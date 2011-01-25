unit smmSmallMemory;

interface

{$Include smmOptions.inc}

uses
  smmTypes;

const
  /// alloc memory blocks with 64 memory items each time
  //  64 = 1 shl 6, therefore any multiplication compiles into nice shl opcode
  C_ARRAYSIZE = 32;  //32 instead of 64 -> smaller overhead
  /// Maximum index of 256 bytes granularity Small blocks
  MAX_SMALLMEMBLOCK  = 8;
  /// 0 - 2048 bytes
  C_MAX_SMALLMEM_SIZE = MAX_SMALLMEMBLOCK * 256; //=2048;

type
  PSmallMemHeader        = ^TSmallMemHeader;
  PSmallMemBlock         = ^TSmallMemBlock;
  PSmallMemBlockList     = ^TSmallMemBlockList;
  PSmallMemThreadManager = ^TSmallMemThreadManager;

//? {$A-} { all object/record must be packed }

  /// Header appended to the beginning of every allocated memory block
  TSmallMemHeader = object
    {$IFDEF Align16Bytes}
      {$ifndef CPUX64}
      Filer1: Pointer;  // 16 bytes aligned for 32 bit compiler
      Filer2: Pointer;
      {$endif}
    {$ENDIF}
    {$IFDEF SCALEMM_MAGICTEST}
    Magic1: Integer;
    Magic2: Integer;
    {$ENDIF}

    {$IFDEF Align8Bytes}
    Size: NativeUInt;
    {$ENDIF}
    /// the memory block handler which owns this memory block
    /// must be last item of header (same negative offset from mem as TBaseMemHeader)
    OwnerBlock : PSmallMemBlock;

    {$IFDEF SCALEMM_MAGICTEST}
    procedure SCALEMM_MAGICTEST;
    {$ENDIF}
  end;

  TSmallMemHeaderFree = object
    {$IFDEF Align16Bytes}
      {$ifndef CPUX64}
      Filer1: Pointer;  // 16 bytes aligned for 32 bit compiler
      Filer2: Pointer;
      {$endif}
    {$ENDIF}
    {$IFDEF SCALEMM_MAGICTEST}
    Magic1: Integer;
    Magic2: Integer;
    {$ENDIF}

    {$IFDEF Align8Bytes}
    Size: NativeUInt;
    {$ENDIF}
    /// the memory block handler which owns this memory block
    OwnerBlock: PSmallMemBlock;

    //Extra data of free item:---------------------------------
    /// linked to next single memory item (other thread freem mem)
    NextMem: PSmallMemHeader;
  end;

  /// memory block handler
  TSmallMemBlock = object
    /// Thread owner, must be first item of block (same offset as PBaseBlockMemory)
    OwnerThread: PSmallMemThreadManager;
    /// the memory block list which owns this memory block handler
    OwnerList: PSmallMemBlockList;
    {$IFDEF SCALEMM_DEBUG}
    OwnerThreadId: NativeUInt;
    {$ENDIF}

    /// link to the next list with free memory
    FNextMemBlock: PSmallMemBlock;
    /// link to the previous list with free memory
    // - double linked to be able for fast removal of one block
    FPreviousMemBlock: PSmallMemBlock;

    /// link to the next list with freed memory, in case this list has no more freed mem
    FNextFreedMemBlock: PSmallMemBlock;
    /// link to the previous list with freed memory
    FPreviousFreedMemBlock: PSmallMemBlock;
    /// how much free mem is used, max is C_ARRAYSIZE
    FUsageCount: NativeUInt;

    FFreedIndex: NativeUInt;
    { TODO -oAM : bitmap/bitarray instead of full pointer array }
    FFreedArray: array[0..C_ARRAYSIZE-1] of PSmallMemHeader;

    /// internal storage of the memory blocks
    // - will contain array[0..C_ARRAYSIZE-1] of memory items,
    // i.e. (FItemSize + SizeOf(TSmallMemHeader)) * C_ARRAYSIZE bytes
    FMemoryArray: Pointer;

    function  GetUsedMemoryItem: PSmallMemHeader;    {$ifdef HASINLINE}inline;{$ENDIF}
    procedure FreeMem(aMemoryItem: PSmallMemHeader); {$ifdef HASINLINE}inline;{$ENDIF}

    procedure FreeBlockMemory;
  end;

  /// memory block list
  // - current size if 16 bytes (this is a packed object)
  TSmallMemBlockList = object
    /// the per-thread memory manager which created this block
    OwnerThread: PSmallMemThreadManager;

    /// list containing freed memory (which this block owns)
    // - used to implement a fast caching of memory blocks
    FFirstFreedMemBlock: PSmallMemBlock;
    /// list containing all memory this block owns
    FFirstMemBlock: PSmallMemBlock;

    /// size of memory items (32, 64 etc bytes)
    FItemSize : word;
    /// number of blocks inside FFirstFreedMemBlock
    //FFreeMemCount: byte;
    /// recursive check when we alloc memory for this blocksize (new memory list)
    FRecursive: boolean;

    {$ifdef CPUX64}
    // for faster "array[0..7] of TSmallMemBlockList" calc
    // (for 32 bits, the TSmallMemBlockList instance size if 16 bytes)
    FFiller: array[1..sizeof(NativeInt)-4] of byte;
    {$endif}

    procedure AddNewMemoryBlock;
    function  GetMemFromNewBlock : Pointer;
  end;

  /// handles per-thread memory managment
  TSmallMemThreadManager = object
  public
    SizeType: TSizeType;
    OwnerManager: PBaseThreadManager;
  private
    /// array with memory per block size of 32 bytes (mini blocks)
    // - i.e. 32, 64, 96, 128, 160, 192, 224 bytes
    FMiniMemoryBlocks: array[0..6] of TSmallMemBlockList;
    /// array with memory per block size of 256 bytes (small blocks)
    // - i.e. 256,512,768,1024,1280,1536,1792[,2048] bytes
    FSmallMemoryBlocks: array[0..MAX_SMALLMEMBLOCK] of TSmallMemBlockList;
  public
    procedure Init;
    procedure Reset;

    procedure MoveAllMemToOtherManager(aOtherManager: PSmallMemThreadManager);
    function  GetBlockListOfSize(aItemSize: NativeUInt): PSmallMemBlockList;

    function GetMem(aSize: NativeUInt) : Pointer;
    function FreeMem(aMemory: Pointer): NativeInt;
    function ReallocMem(aMemory: Pointer; aSize: NativeUInt): Pointer;
  end;

implementation

uses
  smmGlobal, ScaleMM2, smmFunctions;

{ TSmallMemHeader }

{$IFDEF SCALEMM_MAGICTEST}
procedure TSmallMemHeader.SCALEMM_MAGICTEST;
begin
  Assert(Magic1 = 1234567890);
  Assert(Magic2 = 1122334455);
end;
{$ENDIF}

{ TSmallMemBlock }

procedure TSmallMemBlock.FreeBlockMemory;
begin
  if OwnerList.FFirstMemBlock = @Self then
    Exit; //keep one block

  // remove ourselves from linked list
  if FPreviousMemBlock <> nil then
    FPreviousMemBlock.FNextMemBlock := Self.FNextMemBlock;
  if FPreviousFreedMemBlock <> nil then
    FPreviousFreedMemBlock.FNextFreedMemBlock := Self.FNextFreedMemBlock;
  if FNextMemBlock <> nil then
    FNextMemBlock.FPreviousMemBlock := Self.FPreviousMemBlock;
  if FNextFreedMemBlock <> nil then
    FNextFreedMemBlock.FPreviousFreedMemBlock := Self.FPreviousFreedMemBlock;

  if OwnerList.FFirstFreedMemBlock = @Self then
    OwnerList.FFirstFreedMemBlock := nil;
  if OwnerList.FFirstMemBlock = @Self then
    OwnerList.FFirstMemBlock := nil;

  PThreadMemManager(OwnerList.OwnerThread.OwnerManager).FMediumMemManager.FreeMem(@Self);
//  GlobalManager.FreeSmallBlockMemory(@Self);
end;

procedure TSmallMemBlock.FreeMem(aMemoryItem: PSmallMemHeader);
begin
  Assert(OwnerList <> nil);

  // first free item of block?
  // then we add this block to (linked) list with available mem
  if FFreedIndex = 0 then
    with OwnerList^ do  //faster
    begin
      {Self.}FNextFreedMemBlock := {Owner}FFirstFreedMemBlock;   //link to first list
      {Self.}FPreviousFreedMemBlock := nil;
      if {Self}FNextFreedMemBlock <> nil then
        {Self}FNextFreedMemBlock.FPreviousFreedMemBlock := @Self; //back link
      {Owner}FFirstFreedMemBlock := @Self; //replace first list
    end;

  // free mem block
  FFreedArray[FFreedIndex] := aMemoryItem;
  inc(FFreedIndex);

  // all memory available?
  if FFreedIndex = C_ARRAYSIZE then
  // if FFreedIndex = FUsageCount then   -> we do not use this one, we want to keep at least one block (?)
  begin
    FreeBlockMemory;
//    with Owner^ do
//    begin
//      if (FFreeMemCount >= C_GLOBAL_BLOCK_CACHE) and
//         ({Owner.}FFirstMemBlock <> @Self)  //keep one block
//      then
//        Self.FreeBlockMemory
//      else
//        inc(FFreeMemCount);
//    end;
  end;
end;

function TSmallMemBlock.GetUsedMemoryItem: PSmallMemHeader;
begin
  Assert(Self.OwnerList <> nil);
  Assert(FFreedIndex > 0);

  dec(FFreedIndex);
  Result := FFreedArray[FFreedIndex];

  if FFreedIndex = 0 then  // no free items left?
  begin
    // set next free memlist
    OwnerList.FFirstFreedMemBlock := {Self.}FNextFreedMemBlock;
    // first one has no previous
    if {Self.}FNextFreedMemBlock <> nil then
    begin
      Assert(FNextFreedMemBlock.FFreedIndex > 0);
      {Self.}FNextFreedMemBlock.FPreviousFreedMemBlock := nil;
    end;

    // remove from free list
    {Self.}FPreviousFreedMemBlock := nil;
    {Self.}FNextFreedMemBlock     := nil;
  end;
  //all memory was available?
  //else if FFreedIndex = C_ARRAYSIZE-1 then
  //  dec(OwnerList.FFreeMemCount);

  {$IFDEF SCALEMM_MAGICTEST}
  Result.SCALEMM_MAGICTEST;
  {$ENDIF}
end;


{ TSmallMemBlockList }

procedure TSmallMemBlockList.AddNewMemoryBlock;
var
  pm: PSmallMemBlock;
begin
  FRecursive := True;
  // get block from cache
  pm := GlobalManager.GetSmallBlockMemory(FItemSize);
  if pm = nil then
  begin
    // create own one
    pm := PThreadMemManager(OwnerThread.OwnerManager).FMediumMemManager.GetMem
//    {$ifdef USEMEDIUM}
//      Owner.GetMem
//    {$else}
      //Owner.GetMedumOrLargeMem // (32+8)*64=2560 > 2048 -> use OldMM
//      Owner.FMediumMemManager.GetMem // (32+8)*64=2560 > 2048 -> use OldMM
//    {$endif}
      ( SizeOf(pm^) + (FItemSize + SizeOf(TSmallMemHeader)) * C_ARRAYSIZE );
    with pm^ do // put zero only to needed properties
    begin
      fillchar(FNextFreedMemBlock,SizeOf(FNextFreedMemBlock)+
        SizeOf(FPreviousFreedMemBlock)+SizeOf(FUsageCount)+
        SizeOf({$ifdef USEBITMAP}FFreed{$else}FFreedIndex{$endif}),0);
      {pm.}FMemoryArray := Pointer(NativeUInt(pm) + SizeOf(pm^));
    end;

    {$IFDEF SCALEMM_DEBUG}
    pm.OwnerThreadId := GetCurrentThreadId;
    {$ENDIF}
  end;

  // init
  with pm^ do
  begin
    {$IFDEF SCALEMM_DEBUG}
    Assert(pm.OwnerThreadId = GetCurrentThreadId);
    {$ENDIF}

    {pm.}OwnerThread       := Self.OwnerThread;
    {pm.}OwnerList         := @Self;
    // set new memlist as first, add link to current item
    {pm.}FNextMemBlock     := {self}FFirstMemBlock;
    // back link to new first item
    if {self}FFirstMemBlock <> nil then
      {self}FFirstMemBlock.FPreviousMemBlock := pm;
    {self}FFirstMemBlock   := pm;
    {pm.}FPreviousMemBlock := nil;

    // if block has already some freed memory (previous used block from cache)
    // then add to used list
    if {pm.}FFreedIndex > 0 then
    begin
      {pm.}FNextFreedMemBlock     := {Self}FFirstFreedMemBlock;  // link to first list
      {pm.}FPreviousFreedMemBlock := nil;
      if {pm}FNextFreedMemBlock <> nil then
        {pm}FNextFreedMemBlock.FPreviousFreedMemBlock := pm; // back link
      {Self}FFirstFreedMemBlock   := pm;                     // replace first list

      //if {pm.}FFreedIndex = C_ARRAYSIZE then
      //  inc({pm.}OwnerList.FFreeMemCount);
    end;
  end;
  FRecursive := False;
end;

function TSmallMemBlockList.GetMemFromNewBlock: Pointer;
var
  pm: PSmallMemBlock;
begin
  // store: first time init?
  if FFirstMemBlock = nil then
  begin
    if FRecursive then
    begin
      Result := PThreadMemManager(OwnerThread.OwnerManager).FMediumMemManager.GetMem(Self.FItemSize);
      Exit;
    end;
    AddNewMemoryBlock;
  end;

  pm := FFirstMemBlock;
  with pm^ do
  begin
    // memlist full? make new memlist
    if FUsageCount >= C_ARRAYSIZE then
    begin
      if FRecursive then
      begin
        Result := PThreadMemManager(Self.OwnerThread.OwnerManager).FMediumMemManager.GetMem(Self.FItemSize);
        Exit;
      end;
      AddNewMemoryBlock;
      pm := FFirstMemBlock;
    end;
  end;

  // get mem from list
  with pm^ do
  begin
    // space left?
    if FUsageCount < C_ARRAYSIZE then
    begin
      Result := FMemoryArray;
      // calc next item
      FMemoryArray := Pointer(NativeUInt(FMemoryArray) + (FItemSize + SizeOf(TSmallMemHeader)) );
      inc(FUsageCount);
      // startheader = link to memlist
      TSmallMemHeader(Result^).OwnerBlock     := pm;

      {$IFDEF SCALEMM_MAGICTEST}
      TSmallMemHeader(Result^).Magic1 := 1234567890;
      TSmallMemHeader(Result^).Magic2 := 1122334455;
      {$ENDIF}
    end
    else
      Result := pm.GetUsedMemoryItem;
  end;

  {$IFDEF SCALEMM_MAGICTEST}
  TSmallMemHeader(Result^).SCALEMM_MAGICTEST;
  {$ENDIF}
  Result  := Pointer(NativeUInt(Result) + SizeOf(TSmallMemHeader));
end;

{ TSmallMemThreadManager }

function TSmallMemThreadManager.FreeMem(aMemory: Pointer): NativeInt;
var
  ph: PSmallMemHeader;
begin
  {$ifdef SCALEMM_DEBUG} Result := 0; try {$ENDIF}
  ph  := PSmallMemHeader(NativeUInt(aMemory) - SizeOf(TSmallMemHeader));

  //if pm <> nil then
  //with pm^ do
//  begin
  {$IFDEF SCALEMM_MAGICTEST}
  ph.SCALEMM_MAGICTEST;
  {$ENDIF}
  // block obtained via Scale_GetMem()
  Assert(ph.OwnerBlock <> nil);
  Assert(ph.OwnerBlock.OwnerList <> nil);
//  Assert(ph.OwnerBlock.OwnerList = @Self);
    // mem of own thread
//    pm.FreeMem(PSmallMemHeader(p));
//    else
      // put mem in lockfree queue of owner thread
//      Owner.Owner.AddFreedMemFromOtherThread(PSmallMemHeader(p));
//  end;

  ph.OwnerBlock.FreeMem(ph);
  Result := 0;

  {$ifdef SCALEMM_DEBUG} except sleep(0); end; {$ENDIF}
end;

function TSmallMemThreadManager.GetBlockListOfSize(
  aItemSize: NativeUInt): PSmallMemBlockList;
begin
  Result := nil;

  if aItemSize < (length(FMiniMemoryBlocks)*32) then
    // blocks of 32: 32, 64, 96, 128, 160, 192, 224
    Result := @FMiniMemoryBlocks[aItemSize shr 5]
  else if aItemSize < (length(FSmallMemoryBlocks)*256) then
    // blocks of 256: 256,512,768,1024,1280,1536,1792[,2048] bytes
    Result := @FSmallMemoryBlocks[aItemSize shr 8]
  else
  begin
    // not allocated by this unit (should not happen)
    Assert(false);
    Exit;
  end;
end;

function TSmallMemThreadManager.GetMem(aSize: NativeUInt): Pointer;
var
  bm: PSmallMemBlockList;
  iNewSize: NativeUInt;
begin
  bm := nil;
  iNewSize := aSize + SizeOf(TSmallMemHeader);

  if iNewSize <= (length(FMiniMemoryBlocks)*32) then
    // blocks of 32: 32, 64, 96, 128, 160, 192, 224
    bm := @FMiniMemoryBlocks[(iNewSize-1) shr 5]
  else if iNewSize <= (length(FSmallMemoryBlocks)*256) then
    // blocks of 256: 256,512,768,1024,1280,1536,1792 bytes
    bm := @FSmallMemoryBlocks[(iNewSize-1) shr 8]
  else
    Error(reInvalidPtr);

  with bm^ do
  begin
    if FFirstFreedMemBlock <> nil then
    begin
    // first get from freed mem (fastest because most chance?)
      Result := FFirstFreedMemBlock.GetUsedMemoryItem;
      Result := Pointer(NativeUInt(Result) + SizeOf(TSmallMemHeader));
    end
    else
      // from normal list
      Result := GetMemFromNewBlock;
  end;
end;

procedure TSmallMemThreadManager.Init;
var i, j: NativeUInt;
begin
  SizeType := stSmall;

  Fillchar(Self, SizeOf(Self), 0);
  j := 32;
  for i := Low(FMiniMemoryBlocks) to High(FMiniMemoryBlocks) do
  begin // 32, 64, 96, 128, 160, 192, 224 bytes
    FMiniMemoryBlocks[i].OwnerThread := @Self;
    FMiniMemoryBlocks[i].FItemSize := j;
    inc(j,32);
  end;
  assert(j=256);
  for i := Low(FSmallMemoryBlocks) to High(FSmallMemoryBlocks) do
  begin // 256,512,768,1024,1280,1536,1792 bytes
    FSmallMemoryBlocks[i].OwnerThread := @Self;
    FSmallMemoryBlocks[i].FItemSize := j;
    inc(j,256);
  end;
  //assert(j=2304);
  assert(j=2560);
end;

procedure TSmallMemThreadManager.MoveAllMemToOtherManager(
  aOtherManager: PSmallMemThreadManager);
var
  i: NativeUInt;

  procedure __ProcessBlockMem(aOldBlock, aGlobalBlock: PSmallMemBlockList);
  var
    allmem, prevmem, tempmem,
    lastinusemem, inusemem: PSmallMemBlock;
  begin
    allmem        := aOldBlock.FFirstMemBlock;
    inusemem      := nil;
    lastinusemem  := nil;

    // scan all memoryblocks and filter unused blocks
    while allmem <> nil do
    begin
      if allmem.OwnerList = nil then
        Break; // loop?

      // fully free, no mem in use?
      if allmem.FFreedIndex = allmem.FUsageCount then
      begin
        // next one
        tempmem := allmem;
        allmem  := allmem.FNextMemBlock;
        // dispose
        //allmem.FreeBlockMemory;
        PThreadMemManager(OwnerManager).FMediumMemManager.FreeMem(tempmem);
        Continue;
      end
      else
      // some items in use (in other thread? or mem leak?)
      begin
        // first item of list?
        if inusemem = nil then
          inusemem := allmem
        else
          // else add to list (link to previous)
          lastinusemem.FNextMemBlock := allmem;
        lastinusemem  := allmem;
      end;

      {$IFDEF SCALEMM_DEBUG}
      Assert(allmem.OwnerThreadId <> 0);
      allmem.OwnerThreadId := 0;
      {$ENDIF}
      allmem.OwnerList              := aGlobalBlock;
      allmem.FNextFreedMemBlock     := nil;
      allmem.FPreviousMemBlock      := nil;
      allmem.FPreviousFreedMemBlock := nil;

      // next one
      allmem := allmem.FNextMemBlock;
    end;

    if inusemem <> nil then
    begin
      assert(lastinusemem <> nil);
      // add freemem list to front (replace first item, link previous to last item)
      repeat
        prevmem := aGlobalBlock.FFirstFreedMemBlock;
        lastinusemem.FNextFreedMemBlock := prevmem;
        if CAS32(prevmem, inusemem, aGlobalBlock.FFirstFreedMemBlock) then
          break;
        if not SwitchToThread then
          sleep(0);
        prevmem := aGlobalBlock.FFirstFreedMemBlock;
        lastinusemem.FNextFreedMemBlock := prevmem;
        if CAS32(prevmem, inusemem, aGlobalBlock.FFirstFreedMemBlock) then
          break;
        sleep(1);
      until false;
    end;
  end;

begin
  for i := Low(Self.FMiniMemoryBlocks) to High(Self.FMiniMemoryBlocks) do
    __ProcessBlockMem( @FMiniMemoryBlocks[i],   @aOtherManager.FMiniMemoryBlocks[i]);
  for i := Low(Self.FSmallMemoryBlocks) to High(Self.FSmallMemoryBlocks) do
    __ProcessBlockMem( @FSmallMemoryBlocks[i],  @aOtherManager.FSmallMemoryBlocks[i]);
end;

function TSmallMemThreadManager.ReallocMem(aMemory: Pointer;
  aSize: NativeUInt): Pointer;
var
  pb: PSmallMemBlock;
  {$IFDEF SCALEMM_MAGICTEST}
  p: Pointer;
  {$ENDIF}
  //iNewSize: NativeUInt;
begin
  pb := PSmallMemHeader(NativeUInt(aMemory) - SizeOf(TSmallMemHeader)).OwnerBlock;
  {$IFDEF SCALEMM_MAGICTEST}
  p  := Pointer(NativeUInt(aMemory) - SizeOf(TSmallMemHeader));
  PSmallMemHeader(p).SCALEMM_MAGICTEST;
  {$ENDIF}
  //iNewSize := aSize + SizeOf(TSmallMemHeader);

//    if (pm <> nil) then
  with pb^ do
  begin
    Assert(pb.OwnerList <> nil);

    //downscaling, new size smaller than current size
    if (NativeUInt(aSize) <= OwnerList.FItemSize) then
    begin
      if NativeUInt(aSize) > (OwnerList.FItemSize shr 2) then
      begin
        Result := aMemory; // no resize needed up to 1/4 the current item size

        {$IFDEF SCALEMM_OUTPUTSTRING}
        OutputDebugString( PChar('small 1 realloc ' + IntToStr(aMemory) + ' -> ' + IntToStr(Result) +
                                   ' (' + IntToStr(OwnerList.FItemSize) + ' -> ' + IntToStr(aSize) + ')' ));
        {$ENDIF}
      end
      else
      // too much downscaling: use move
      begin
        Result := Self.GetMem(aSize); // new mem

        {$IFDEF SCALEMM_OUTPUTSTRING}
        OutputDebugString( PChar('small 2 realloc ' + IntToStr(aMemory) + ' -> ' + IntToStr(Result) +
                                 ' (' + IntToStr(OwnerList.FItemSize) + ' -> ' + IntToStr(aSize) + ')' ));
        {$ENDIF}
        if aMemory <> Result then
        begin
          Move(aMemory^, Result^, aSize); // copy (use smaller new size)
          Self.FreeMem(aMemory); // free old mem
        end;
      end;
    end
    else
    //upscaling, new size bigger than current size
    begin
      Result := PThreadMemManager(Self.OwnerManager).GetMem(aSize); // new mem

      {$IFDEF SCALEMM_OUTPUTSTRING}
      OutputDebugString( PChar('small 3 realloc ' + IntToStr(aMemory) + ' -> ' + IntToStr(Result) +
                                 ' (' + IntToStr(OwnerList.FItemSize) + ' -> ' + IntToStr(aSize) + ')' ));
      {$ENDIF}
      if aMemory <> Result then
      begin
        Move(aMemory^, Result^, OwnerList.FItemSize); // copy (use smaller old size)
        Self.FreeMem(aMemory); // free old mem
      end;
    end;
  end;

  {$IFDEF SCALEMM_MAGICTEST}
  p := PBaseMemHeader(NativeUInt(Result) - SizeOf(TBaseMemHeader));
  if PBaseMemHeader(p).OwnerBlock.OwnerThread = @Self then
  begin
    p := Pointer(NativeUInt(Result) - SizeOf(TSmallMemHeader));
    PSmallMemHeader(p).SCALEMM_MAGICTEST;
    if PSmallMemHeader(p).OwnerBlock <> nil then
      Assert( PSmallMemHeader(p).OwnerBlock.OwnerList <> nil );
  end;
  {$ENDIF}
end;

procedure TSmallMemThreadManager.Reset;
var
  i: NativeUInt;

  procedure __ResetBlocklist(aBlocklist: PSmallMemBlockList);
  begin
    aBlocklist.FFirstFreedMemBlock := nil;
    aBlocklist.FFirstMemBlock := nil;
    aBlocklist.FRecursive := False;
  end;

begin
  for i := Low(FMiniMemoryBlocks) to High(FMiniMemoryBlocks) do
    __ResetBlocklist(@FMiniMemoryBlocks[i]);
  for i := Low(FSmallMemoryBlocks) to High(FSmallMemoryBlocks) do
    __ResetBlocklist(@FSmallMemoryBlocks[i]);
end;

end.
