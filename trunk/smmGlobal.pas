unit smmGlobal;

interface

uses
  ScaleMM2, smmTypes,
  smmSmallMemory, smmMediumMemory;

type
  /// Global memory manager
  // - a single instance is created for the whole process
  // - caches some memory (blocks + threadmem) for fast reuse
  // - also keeps allocated memory in case an old thread allocated some memory
  // for another thread
  TGlobalMemManager = object
  private {locks}
    FBlockLock: NativeUInt;
    FFreeBlockCount: NativeUInt;
    FThreadLock: NativeUInt;
    FThreadLockRecursion: NativeUInt;
  private {threads}
    /// all thread memory managers
    FFirstThreadMemory: PThreadMemManager;
    /// freed/used thread memory managers
    // - used to cache the per-thread managers in case of multiple threads creation
    FFirstFreedThreadMemory: PThreadMemManager;
  private {small}
    /// global thread manager (owner of all global mem)
    FGlobalThreadMemory: PThreadMemManager;
  private {medium}
    FFirstBlock: PMediumBlockMemory;
  protected
    procedure FreeSmallBlocksFromThreadMemory(aThreadMem: PSmallMemThreadManager);
    procedure FreeMediumBlocksFromThreadMemory(aThreadMem: PMediumThreadManager);

    procedure ProcessFreedMemoryFromOtherThreads;

    procedure ThreadLock;
    procedure ThreadUnLock;
  public
    procedure Init;

    function  GetNewThreadManager: PThreadMemManager;
    procedure AddNewThreadManagerToList(aThreadMem: PThreadMemManager);
    procedure FreeThreadManager(aThreadMem: PThreadMemManager);
    procedure FreeAllMemory;

    procedure FreeMediumBlockMemory(aBlockMem: PMediumBlockMemory);
    function  GetMediumBlockMemory(aNewOwner: PMediumThreadManager): PMediumBlockMemory;

    procedure FreeSmallBlockMemory(aBlockMem: PSmallMemBlock);
    function  GetSmallBlockMemory(aItemSize: NativeUInt): PSmallMemBlock;

    procedure CheckSmallMem;

    procedure FreeInterThreadMemory(aInterThreadMem: PInterThreadMem);
  end;
//{$A+}?

var
  GlobalManager: TGlobalMemManager;

implementation

uses
  smmFunctions;

{ TGlobalManager }

procedure TGlobalMemManager.AddNewThreadManagerToList(aThreadMem: PThreadMemManager);
var
  pprevthreadmem: PThreadMemManager;
begin
  repeat
    pprevthreadmem := FFirstThreadMemory;
    // try to set "result" in global var
    if CAS32(pprevthreadmem, aThreadMem, FFirstThreadMemory) then
      Break;
    if not SwitchToThread then
      sleep(0);
    pprevthreadmem := FFirstThreadMemory;
    if CAS32(pprevthreadmem, aThreadMem, FFirstThreadMemory) then
      Break;
    Sleep(1);
  until false;
  // make linked list: new one is first item (global var), next item is previous item
  aThreadMem.FNextThreadManager := pprevthreadmem;
end;

procedure TGlobalMemManager.CheckSmallMem;
begin
  //FGlobalThreadMemory.FSmallMemManager.
end;

procedure TGlobalMemManager.FreeAllMemory;

(*
  procedure __ProcessBlockMem(aOldBlock: PSmallMemBlockList);
  var
    allmem, oldmem: PSmallMemBlock;
  begin
    if aOldBlock = nil then
      Exit;
    allmem := aOldBlock.FFirstFreedMemBlock;
    while allmem <> nil do
    begin
      // not in use
      if allmem.FUsageCount = allmem.FFreedIndex then
      begin
        oldmem := allmem;
        allmem := allmem.FNextFreedMemBlock;
        Scale_FreeMem(oldmem);
        {FMainThreadMemory.}Scale_FreeMem(oldmem);
      end
      else
        allmem := allmem.FNextFreedMemBlock;
    end;
  end;
  *)

var
  oldthreadmem, tempthreadmem: PThreadMemManager;
//  i: NativeUInt;
begin
  // free internal blocks
//  for i := Low(Self.FFreedMiniMemoryBlocks) to High(Self.FFreedMiniMemoryBlocks) do
//    __ProcessBlockMem(@Self.FFreedMiniMemoryBlocks[i]);
//  for i := Low(Self.FFreedSmallMemoryBlocks) to High(Self.FFreedSmallMemoryBlocks) do
//    __ProcessBlockMem(@Self.FFreedSmallMemoryBlocks[i]);

  { TODO -oAM : release small mem }
  (*
  // free current thread
  tempthreadmem := ScaleMM.GetThreadMemManager;
  with tempthreadmem do
  begin
    for i := Low(tempthreadmem. FMiniMemoryBlocks) to High(tempthreadmem.FMiniMemoryBlocks) do
      __ProcessBlockMem(@tempthreadmem.FMiniMemoryBlocks[i]);
    for i := Low(tempthreadmem.FSmallMemoryBlocks) to High(tempthreadmem.FSmallMemoryBlocks) do
      __ProcessBlockMem(@tempthreadmem.FSmallMemoryBlocks[i]);
  end;
  *)

  { TODO -oAM : release medium mem }

  // free cached threads
  oldthreadmem := Self.FFirstFreedThreadMemory;
  while oldthreadmem <> nil do
  begin
    tempthreadmem := oldthreadmem;
    oldthreadmem  := oldthreadmem.FNextThreadManager;
    VirtualFree(tempthreadmem, 0, MEM_RELEASE);
  end;
end;

procedure TGlobalMemManager.FreeInterThreadMemory(
  aInterThreadMem: PInterThreadMem);
var
  memownerthreads: array[0..31] of PBaseThreadManager;
  i: Integer;
  firstmem, nextmem, tempmem: PBaseMemHeader;
  thread: PBaseThreadManager;
  j: Integer;
  ot: PBaseThreadMemory;
begin
  { TODO -oAM : Make GC thread which processes all freed mem in background, now only one FreeThreadManager or FreeInterThreadMemory can be active at a time}

  //TODO -oAM : LOCK: no threads may be removed/freed now }
  //ThreadLock;

  Assert( Length(memownerthreads) = Length(aInterThreadMem.FMemArray) );
  //fill all ownerthreads of the memory
  for i := Low(memownerthreads) to High(memownerthreads) do
  begin
    tempmem := aInterThreadMem.FMemArray[i];
    if tempmem <> nil then
    begin
      if NativeUInt(tempmem.OwnerBlock) and 3 <> 0 then
        ot := PBaseThreadMemory( NativeUInt(tempmem.OwnerBlock) and -4)
      else
        ot := tempmem.OwnerBlock.OwnerThread;
      memownerthreads[i] := ot.OwnerManager
    end
    else
      memownerthreads[i] := nil;
  end;

  //process mem of each thread
  for i := Low(memownerthreads) to High(memownerthreads) do
  begin
    if aInterThreadMem.FItemCount <= 0 then Break; //nothing left

    thread   := memownerthreads[i];
    if thread = nil then Continue;  //already processed
    firstmem := aInterThreadMem.FMemArray[i];
    nextmem  := firstmem;

    //search mem of same thread
    for j := i+1 to High(memownerthreads) do
    begin
      //same thread?
      if memownerthreads[j] = thread then
      begin
        tempmem := aInterThreadMem.FMemArray[j];
        //link current to next
        PBaseFreeMemHeader(nextmem).NextThreadFree := PBaseFreeMemHeader(tempmem);
        //set next
        nextmem := tempmem;

        //clear, it is processed
        aInterThreadMem.FMemArray[j] := nil;
        memownerthreads[j] := nil;
        dec(aInterThreadMem.FItemCount);
      end;
    end;

    //add linked list of mem to ownerthread
    if not PThreadMemManager(thread).FThreadTerminated then
      PThreadMemManager(thread).AddFreeMemToOwnerThread( PBaseFreeMemHeader(firstmem),
                                                         PBaseFreeMemHeader(nextmem) )
    else
      //thread is terminated, add to global manager
      FGlobalThreadMemory.AddFreeMemToOwnerThread( PBaseFreeMemHeader(firstmem),
                                                   PBaseFreeMemHeader(nextmem) );
    //clear, it is processed
    memownerthreads[i] := nil;
  end;

  //UNLOCK
  //ThreadUnLock;

  aInterThreadMem.FTotalSize := 0;
  aInterThreadMem.FItemCount := 0;

  //in the mean time some inuse memory can be freed in an other thread
  ProcessFreedMemoryFromOtherThreads;
end;

procedure TGlobalMemManager.FreeMediumBlockMemory(
  aBlockMem: PMediumBlockMemory);
var
  firstmem: PMediumHeader;
begin
  //keep max 10 blocks in buffer
  if FFreeBlockCount >= 10 then
  begin
    firstmem := PMediumHeader( NativeUInt(aBlockMem) + SizeOf(TMediumBlockMemory));
    //is free mem?
    //if NativeUInt(firstmem.NextMem) > NativeUInt(1 shl 31) then
    if firstmem.Size and 1 <> 0 then
    begin
      //fully free mem? we can only release fully free mem (duh...)
      if PMediumHeaderExt(firstmem).ArrayPosition = 16 then
      begin
        //RELEASE TO WINDOWS
        VirtualFree(aBlockMem, 0 {all}, MEM_RELEASE);
        //exit!
        Exit;
      end
    end;
  end;

  aBlockMem.ChangeOwnerThread(@Self.FGlobalThreadMemory.FMediumMemManager);

  Threadlock;

  ProcessFreedMemoryFromOtherThreads;

  //LOCK
  while not CAS32(0, 1, FBlockLock) do
  begin
    //small wait: try to swith to other pending thread (if any) else direct continue
    if not SwitchToThread then
      sleep(0);
    //try again
    if CAS32(0, 1, FBlockLock) then
      Break;
    //wait some longer: force swith to any other thread
    sleep(1);
  end;

  //linked list of thread blocks: replace first item
  if FFirstBlock <> nil then
    FFirstBlock.PreviousBlock := aBlockMem;
  aBlockMem.NextBlock         := FFirstBlock;
  aBlockMem.PreviousBlock     := nil;
  FFirstBlock                 := aBlockMem;

  inc(FFreeBlockCount);

  //UNLOCK
  FBlockLock := 0;

  ThreadUnlock;
end;

procedure TGlobalMemManager.FreeMediumBlocksFromThreadMemory(
  aThreadMem: PMediumThreadManager);
var
  threadblock, nextblock: PMediumBlockMemory;
begin
  threadblock := aThreadMem.FFirstBlock;
  aThreadMem.FFirstBlock := nil;

  while threadblock <> nil do
  begin
    nextblock   := threadblock.NextBlock;
    FreeMediumBlockMemory(threadblock);
    threadblock := nextblock;
  end;
end;

procedure TGlobalMemManager.FreeSmallBlockMemory(aBlockMem: PSmallMemBlock);
begin
  Assert( aBlockMem.FFreedIndex = aBlockMem.FUsageCount );
  // dispose
  Scale_FreeMem(aBlockMem);

  ProcessFreedMemoryFromOtherThreads;
end;

procedure TGlobalMemManager.FreeSmallBlocksFromThreadMemory(
  aThreadMem: PSmallMemThreadManager);
begin
  aThreadMem.MoveAllMemToOtherManager(@FGlobalThreadMemory.FSmallMemManager);
end;

procedure TGlobalMemManager.FreeThreadManager(aThreadMem: PThreadMemManager);
//var
//  pprevthreadmem: PThreadMemManager;
begin
  aThreadMem.FThreadTerminated := True;

  { TODO -oAM : Make GC thread which processes all freed mem in background, now only one FreeThreadManager or FreeInterThreadMemory can be active at a time}

  //LOCK: no threads may be proceseed now (e.g. FreeInterThreadMemory)
  ThreadLock;

  // clear mem (partial: add to reuse list, free = free)
  FreeSmallBlocksFromThreadMemory(@aThreadMem.FSmallMemManager);
  FreeMediumBlocksFromThreadMemory(@aThreadMem.FMediumMemManager);
  aThreadMem.Reset;

  { TODO : keep max nr of threads. Remember to lock "FreeInterThreadMemory" then }
  // add to available list
  aThreadMem.FNextThreadManager := FFirstFreedThreadMemory;
  FFirstFreedThreadMemory := aThreadMem;

  //process mem from other threads
  ProcessFreedMemoryFromOtherThreads;

  //UNLOCK
  ThreadUnLock;

  (*
  // add to available list
  repeat
    pprevthreadmem := FFirstFreedThreadMemory;
    // make linked list: new one is first item (global var), next item is previous item
    aThreadMem.FNextThreadManager := pprevthreadmem;
    // try to set "result" in global var
    if CAS32(pprevthreadmem, aThreadMem, FFirstFreedThreadMemory) then
      break;
    if not SwitchToThread then
      sleep(0);
    pprevthreadmem := FFirstFreedThreadMemory;
    aThreadMem.FNextThreadManager := pprevthreadmem;
    if CAS32(pprevthreadmem, aThreadMem, FFirstFreedThreadMemory) then
      break;
    sleep(1);
  until false;
  *)
end;

function TGlobalMemManager.GetSmallBlockMemory(aItemSize: NativeUInt): PSmallMemBlock;
var bl: PSmallMemBlockList;
begin
  Result := nil;

  bl := Self.FGlobalThreadMemory.FSmallMemManager.GetBlockListOfSize(aItemSize - 1);
  if bl.FFirstFreedMemBlock = nil then Exit;

  ThreadLock;

  //in the mean time some inuse memory can be freed in an other thread
  ProcessFreedMemoryFromOtherThreads;

  //LOCK
  while not CAS32(0, 1, FBlockLock) do
  begin
    //small wait: try to swith to other pending thread (if any) else direct continue
    if not SwitchToThread then
      sleep(0);
    //try again
    if CAS32(0, 1, FBlockLock) then
      Break;
    //wait some longer: force swith to any other thread
    sleep(1);
  end;

  // get freed mem from list from front (replace first item)
  if bl.FFirstFreedMemBlock <> nil then
  begin
    Result                 := bl.FFirstFreedMemBlock;
    bl.FFirstFreedMemBlock := Result.FNextFreedMemBlock;

    Result.FNextFreedMemBlock      := nil;
    Result.FNextMemBlock           := nil;
    Result.FPreviousMemBlock       := nil;
    Result.FPreviousFreedMemBlock  := nil;
  end;

  //UNLOCK
  FBlockLock := 0;
  ThreadUnlock;

  if Result <> nil then
  begin
//    Result.OwnerList   := nil;
//    Result.OwnerThread := nil;
    Result.OwnerList   := Pointer(1);
    Result.OwnerThread := Pointer(2);
    {$IFDEF SCALEMM_DEBUG}
    Assert(Result.OwnerThreadId = 1);
    Result.OwnerThreadId := 2;
    {$ENDIF}
    Result.FNextFreedMemBlock      := nil;
    Result.FNextMemBlock           := nil;
    Result.FPreviousMemBlock       := nil;
    Result.FPreviousFreedMemBlock  := nil;
  end;
end;

function TGlobalMemManager.GetMediumBlockMemory(aNewOwner: PMediumThreadManager): PMediumBlockMemory;
begin
  Result := nil;
  if FFirstBlock = nil then Exit;

  Threadlock;

  ProcessFreedMemoryFromOtherThreads;

  //LOCK
  while not CAS32(0, 1, FBlockLock) do
  begin
    //small wait: try to swith to other pending thread (if any) else direct continue
    if not SwitchToThread then
      sleep(0);
    //try again
    if CAS32(0, 1, FBlockLock) then
      Break;
    //wait some longer: force swith to any other thread
    sleep(1);
  end;

  //get block
  Result := FFirstBlock;
  //got a block?
  if Result <> nil then
  begin
    //rearrange linked list (replace first item)
    FFirstBlock := FFirstBlock.NextBlock;
    if FFirstBlock <> nil then
      FFirstBlock.PreviousBlock := nil;

    Result.NextBlock     := nil;
    Result.PreviousBlock := nil;
    dec(FFreeBlockCount);

    //must be locked before owner can be changed (in case some memory is freed in other thread?)
    Result.ChangeOwnerThread(aNewOwner);
  end;

  //UNLOCK
  FBlockLock := 0;

  ThreadUnlock;
end;

function TGlobalMemManager.GetNewThreadManager: PThreadMemManager;
//var
//  pprevthreadmem, newthreadmem: PThreadMemManager;
begin
  Result := nil;

  if FFirstFreedThreadMemory <> nil then
  begin
    ThreadLock;

    Result := FFirstFreedThreadMemory;
    if Result <> nil then
    begin
      FFirstFreedThreadMemory   := Result.FNextThreadManager;
      Result.FNextThreadManager := nil;
    end;

    ThreadUnLock;
  end;

  (*
  // get one cached instance from freed list
  while FFirstFreedThreadMemory <> nil do
  begin
    pprevthreadmem := FFirstFreedThreadMemory;
    if pprevthreadmem <> nil then
      newthreadmem := pprevthreadmem.FNextThreadManager
    else
      newthreadmem := nil;

    // try to set "result" in global var
    if CAS32(pprevthreadmem, newthreadmem, FFirstFreedThreadMemory) then
    begin
      Result := pprevthreadmem;
      if Result <> nil then
        Result.FNextThreadManager := nil;
      break;
    end;
    if not SwitchToThread then
      sleep(0);
    pprevthreadmem := FFirstFreedThreadMemory;
    if pprevthreadmem <> nil then
      newthreadmem := pprevthreadmem.FNextThreadManager
    else
      newthreadmem := nil;
    // try to set "result" in global var
    if CAS32(pprevthreadmem, newthreadmem, FFirstFreedThreadMemory) then
    begin
      Result := pprevthreadmem;
      if Result <> nil then
        Result.FNextThreadManager := nil;
      break;
    end;
    sleep(1);
  end;
  *)
end;

procedure TGlobalMemManager.Init;
begin
    FGlobalThreadMemory := VirtualAlloc( nil,
                            SizeOf(TThreadMemManager),
                            MEM_COMMIT {$ifdef AlwaysAllocateTopDown} or MEM_TOP_DOWN{$endif},
                            PAGE_READWRITE);
    FGlobalThreadMemory.Init;
    FGlobalThreadMemory.FThreadId := 1;
end;

procedure TGlobalMemManager.ProcessFreedMemoryFromOtherThreads;
begin
  if not FGlobalThreadMemory.IsMemoryFromOtherThreadsPresent then Exit;

  //LOCK: no threads may be removed/freed now
  ThreadLock;

  //in the mean time some inuse memory can be freed in an other thread
  FGlobalThreadMemory.ProcessFreedMemFromOtherThreads;

  //UNLOCK
  ThreadUnLock;
end;

procedure TGlobalMemManager.ThreadLock;
var
  iCurrentThreadId: NativeUInt;
begin
  iCurrentThreadId := GetCurrentThreadId;
  if (FThreadLock = iCurrentThreadId) and
     (FThreadLockRecursion > 0) then
  begin
    Assert( CAS32(iCurrentThreadId, iCurrentThreadId, FThreadLock) );
    inc(FThreadLockRecursion);
    Exit;
  end;

  //LOCK: no threads may be removed/freed now
  while not CAS32(0, iCurrentThreadId, FThreadLock) do
  begin
    //small wait: try to swith to other pending thread (if any) else direct continue
    if not SwitchToThread then
      sleep(0);
    //try again
    if CAS32(0, iCurrentThreadId, FThreadLock) then
      Break;
    //wait some longer: force swith to any other thread
    sleep(1);
  end;

  inc(FThreadLockRecursion);
end;

procedure TGlobalMemManager.ThreadUnLock;
begin
  dec(FThreadLockRecursion);

  if FThreadLockRecursion = 0 then
    //UNLOCK
    FThreadLock := 0;
end;

end.
