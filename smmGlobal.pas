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
  private {threads}
    /// all thread memory managers
    FFirstThreadMemory: PThreadMemManager;
    /// freed/used thread memory managers
    // - used to cache the per-thread managers in case of multiple threads creation
    FFirstFreedThreadMemory: PThreadMemManager;
    /// main thread manager (owner of all global mem)
    FMainThreadMemory: PThreadMemManager;
  private {small}
    FSmallCachedMem: TSmallMemThreadManager;
  private {medium}
    FFirstBlock: PMediumBlockMemory;
    FLock: NativeUInt;
    FFreeBlockCount: NativeUInt;
  protected
    procedure FreeSmallBlocksFromThreadMemory(aThreadMem: PSmallMemThreadManager);
    procedure FreeMediumBlocksFromThreadMemory(aThreadMem: PMediumThreadManager);
  public
    procedure Init;

    function  GetNewThreadManager: PThreadMemManager;
    procedure AddNewThreadManagerToList(aThreadMem: PThreadMemManager);
    procedure FreeThreadManager(aThreadMem: PThreadMemManager);
    procedure FreeAllMemory;

    //todo: make copy function?
    procedure FreeMediumBlockMemory(aBlockMem: PMediumBlockMemory);
    function  GetMediumBlockMemory: PMediumBlockMemory;

    procedure FreeSmallBlockMemory(aBlockMem: PSmallMemBlock);
    function  GetSmallBlockMemory(aItemSize: NativeUInt): PSmallMemBlock;
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

  // free cached threads
  oldthreadmem := Self.FFirstFreedThreadMemory;
  while oldthreadmem <> nil do
  begin
    tempthreadmem := oldthreadmem;
    oldthreadmem  := oldthreadmem.FNextThreadManager;
    VirtualFree(tempthreadmem, 0, MEM_RELEASE);
  end;
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
    if NativeUInt(firstmem.NextMem) > NativeUInt(1 shl 31) then
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

  //LOCK
  while not CAS32(0, 1, FLock) do
  begin
    //small wait: try to swith to other pending thread (if any) else direct continue
    if not SwitchToThread then
      sleep(0);
    //try again
    if CAS32(0, 1, FLock) then
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
  FLock := 0;
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
end;

procedure TGlobalMemManager.FreeSmallBlocksFromThreadMemory(
  aThreadMem: PSmallMemThreadManager);
begin
  aThreadMem.MoveAllMemToOtherManager(@FSmallCachedMem);
end;

procedure TGlobalMemManager.FreeThreadManager(aThreadMem: PThreadMemManager);
var
  pprevthreadmem: PThreadMemManager;
begin
  // clear mem (partial: add to reuse list, free = free)
  FreeSmallBlocksFromThreadMemory(@aThreadMem.FSmallMemManager);
  FreeMediumBlocksFromThreadMemory(@aThreadMem.FMediumMemManager);
  aThreadMem.Reset;

  { TODO : keep max nr of threads }
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
end;

function TGlobalMemManager.GetSmallBlockMemory(aItemSize: NativeUInt): PSmallMemBlock;
var bl: PSmallMemBlockList;
    prevmem, nextmem: PSmallMemBlock;
begin
  Result := nil;

  //LOCK
  while not CAS32(0, 1, FLock) do
  begin
    //small wait: try to swith to other pending thread (if any) else direct continue
    if not SwitchToThread then
      sleep(0);
    //try again
    if CAS32(0, 1, FLock) then
      Break;
    //wait some longer: force swith to any other thread
    sleep(1);
  end;

  dec(aItemSize);
  bl := Self.FSmallCachedMem.GetBlockListOfSize(aItemSize);

  // get freed mem from list from front (replace first item)
  if bl.FFirstFreedMemBlock <> nil then
  begin
    prevmem := bl.FFirstFreedMemBlock;
    nextmem := prevmem.FNextFreedMemBlock;
    bl.FFirstFreedMemBlock := nextmem;
    Result := prevmem;
  end;

  //UNLOCK
  FLock := 0;

  if Result <> nil then
  begin
    Result.OwnerList   := bl;
    Result.OwnerThread := nil;
    {$IFDEF SCALEMM_DEBUG}
    Assert(Result.OwnerThreadId = 0);
    Result.OwnerThreadId := GetCurrentThreadId;
    {$ENDIF}
    Result.FNextFreedMemBlock     := nil;
    Result.FNextMemBlock          := nil;
    Result.FPreviousMemBlock      := nil;
    Result.FPreviousFreedMemBlock := nil;
  end;
end;

function TGlobalMemManager.GetMediumBlockMemory: PMediumBlockMemory;
begin
  Result := nil;
  if FFirstBlock = nil then Exit;

  //LOCK
  while not CAS32(0, 1, FLock) do
  begin
    //small wait: try to swith to other pending thread (if any) else direct continue
    if not SwitchToThread then
      sleep(0);
    //try again
    if CAS32(0, 1, FLock) then
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
  end;

  //UNLOCK
  FLock := 0;
end;

function TGlobalMemManager.GetNewThreadManager: PThreadMemManager;
var
  pprevthreadmem, newthreadmem: PThreadMemManager;
begin
  Result := nil;

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
end;

procedure TGlobalMemManager.Init;
begin
  FMainThreadMemory := GetThreadMemManager;
  FSmallCachedMem.Init;
end;

end.
