unit smmSmallMemory;

interface

{$Include smmOptions.inc}

uses
  smmTypes;

const
  /// alloc memory blocks with 64 memory items each time
  //  64 = 1 shl 6, therefore any multiplication compiles into nice shl opcode
  C_ARRAYSIZE         = 32;  //32 instead of 64 -> smaller overhead
  /// Maximum index of 256 bytes granularity Small blocks
  MAX_SMALLMEMBLOCK   = 8;
  /// 0 - 2048 bytes
  C_MAX_SMALLMEM_SIZE = MAX_SMALLMEMBLOCK * 256; //=2048;

type
  PSmallMemHeader        = ^TSmallMemHeader;
  PSmallMemBlock         = ^TSmallMemBlock;
  PSmallMemBlockList     = ^TSmallMemBlockList;
  PSmallMemThreadManager = ^TSmallMemThreadManager;
  PSmallMemHeaderFree    = ^TSmallMemHeaderFree;

//? {$A-} { all object/record must be packed }

  /// Header appended to the beginning of every allocated memory block
  TSmallMemHeader = object
    {$IFDEF SCALEMM_MAGICTEST}
    Magic1: Integer;
    Magic2: Integer;
    {$ELSE}
      {$IFDEF Align16Bytes}
        {$ifndef CPUX64}
        Filer1: Pointer;  // 16 bytes aligned for 32 bit compiler
        Filer2: Pointer;
        {$endif}
      {$ENDIF}
    {$ENDIF}

    Size: NativeUInt;
    /// the memory block handler which owns this memory block
    /// must be last item of header (same negative offset from mem as TBaseMemHeader)
    OwnerBlock : PSmallMemBlock;

    procedure CheckMem;
  end;

  TSmallMemHeaderFree = object
    {$IFDEF SCALEMM_MAGICTEST}
    Magic1: Integer;
    Magic2: Integer;
    {$ELSE}
      {$IFDEF Align16Bytes}
        {$ifndef CPUX64}
        Filer1: Pointer;  // 16 bytes aligned for 32 bit compiler
        Filer2: Pointer;
        {$endif}
      {$ENDIF}
    {$ENDIF}

    Size: NativeUInt;
    /// the memory block handler which owns this memory block
    OwnerBlock: PSmallMemBlock;

    //Extra data of free item:---------------------------------
    /// linked to next single memory item (other thread freem mem)
    NextFreeItem : PSmallMemHeaderFree;
  end;

  /// memory block handler
  TSmallMemBlock = object
    /// Thread owner, must be first item of block (same offset as PBaseBlockMemory)
    OwnerManager: PSmallMemThreadManager;
    /// the memory block list which owns this memory block handler
    OwnerList: PSmallMemBlockList;

    /// how much mem is used, max is C_ARRAYSIZE
    FUsageCount: NativeUInt;
    /// how mem is free
    FFreedIndex: NativeUInt;
    /// internal storage of the memory blocks
    // - will contain array[0..C_ARRAYSIZE-1] of memory items,
    // i.e. (FItemSize + SizeOf(TSmallMemHeader)) * C_ARRAYSIZE bytes
    FMemoryArray: Pointer;
    { TODO -oAM : bitmap/bitarray instead of full pointer array? }
    FFreedArray: array[0..C_ARRAYSIZE] of PSmallMemHeader;

    FNextThreadFreedBlock: PSmallMemBlock;
    FFirstThreadFreed: PSmallMemHeaderFree;
    FThreadFreedCount: Integer;   //negative = lock

    /// link to the next list with free memory
    FNextMemBlock: PSmallMemBlock;
    /// link to the previous list with free memory
    // - double linked to be able for fast removal of one block
    FPreviousMemBlock: PSmallMemBlock;
    /// link to the next list with freed memory, in case this list has no more freed mem
    FNextFreedMemBlock: PSmallMemBlock;
    /// link to the previous list with freed memory
    FPreviousFreedMemBlock: PSmallMemBlock;

    {$IFDEF SCALEMM_DEBUG}
    OwnerThreadId: NativeUInt;
    {$ELSE}
    Filler1: NativeUInt; //8byte aligned
    {$ENDIF}
    {$IFDEF Align16Bytes}
      {$ifndef CPUX64}
      Filler2: Pointer;  // 16 bytes aligned for 32 bit compiler
      Filler3: Pointer;
      {$endif}
    {$ENDIF}

    function  GetUsedMemoryItem: PSmallMemHeader;    {$ifdef HASINLINE}inline;{$ENDIF}
    procedure FreeMem(aMemoryItem: PSmallMemHeader); {$ifdef HASINLINE}inline;{$ENDIF}

    procedure Lock;   {$ifdef HASINLINE}inline;{$ENDIF}
    procedure UnLock; {$ifdef HASINLINE}inline;{$ENDIF}
    procedure ThreadFreeMem(aMemoryItem: PSmallMemHeader);

    procedure FreeBlockMemory;

    procedure CheckMem(aDirection: TScanDirection = sdBoth);
  end;

  /// memory block list
  // - current size if 16 bytes (this is a packed object)
  TSmallMemBlockList = object
    /// the per-thread memory manager which created this block
    OwnerManager: PSmallMemThreadManager;

    /// list containing freed memory (which this block owns)
    // - used to implement a fast caching of memory blocks
    FFirstFreedMemBlock: PSmallMemBlock;
    /// list containing all memory this block owns
    FFirstMemBlock: PSmallMemBlock;

    /// size of memory items (32, 64 etc bytes)
    //FItemSize : word;
    FItemSize : NativeUInt;
    /// recursive check when we alloc memory for this blocksize (new memory list)
    FRecursive: boolean;

    {$ifdef CPUX64}
    // for faster "array[0..7] of TSmallMemBlockList" calc
    // (for 32 bits, the TSmallMemBlockList instance size if 16 bytes)
    FFiller: array[1..sizeof(NativeInt)-4] of byte;
    {$endif}

    procedure AddNewMemoryBlock;
    function  GetMemFromNewBlock: Pointer;

    procedure CheckMem;
  end;

  /// handles per-thread memory managment
  TSmallMemThreadManager = object
  public
    SizeType: TSizeType;
    OwnerThread: PBaseThreadManager;
  private
    FLock: Boolean;
    FFirstThreadFreeBlock: PSmallMemBlock;

    /// array with memory per block size of 32 bytes (mini blocks)
    // - i.e. 32, 64, 96, 128, 160, 192, 224 bytes
    FMiniMemoryBlocks: array[0..6] of TSmallMemBlockList;
    /// array with memory per block size of 256 bytes (small blocks)
    // - i.e. 256,512,768,1024,1280,1536,1792[,2048] bytes
    FSmallMemoryBlocks: array[0..MAX_SMALLMEMBLOCK] of TSmallMemBlockList;

    procedure Lock;
    procedure UnLock;
    procedure FreeThreadFreedMem;
  public
    procedure Init;
    procedure Reset;

    procedure MoveAllMemToOtherManager(aOtherManager: PSmallMemThreadManager);
    function  GetBlockListOfSize(aItemSize: NativeUInt): PSmallMemBlockList;

    procedure CheckAllMem;
    procedure CheckMem(aMemory: Pointer);

    function GetMem(aSize: NativeUInt) : Pointer;                      {$ifdef HASINLINE}inline;{$ENDIF}
    function FreeMem(aMemory: Pointer): NativeInt;                     {$ifdef HASINLINE}inline;{$ENDIF}
    function ReallocMem(aMemory: Pointer; aSize: NativeUInt): Pointer; {$ifdef HASINLINE}inline;{$ENDIF}
  end;

implementation

uses
  smmGlobal, ScaleMM2, smmFunctions;

{ TSmallMemHeader }

procedure TSmallMemHeader.CheckMem;
begin
  {$IFDEF SCALEMM_DEBUG}
  if Self.OwnerBlock.OwnerThreadId > 1 then
    Assert(Self.OwnerBlock.OwnerThreadId = GetCurrentThreadId);
  {$ENDIF}

  {$IFDEF SCALEMM_MAGICTEST}
  Assert(Magic1 = 1234567890);
  Assert(Magic2 = 1122334455);
  {$ENDIF}
  Assert(Size > 0);
  Assert(OwnerBlock <> nil);
  Assert(OwnerBlock.OwnerList <> nil);
  Assert(OwnerBlock.OwnerList.FItemSize = Size);
end;

{ TSmallMemBlock }

procedure TSmallMemBlock.CheckMem(aDirection: TScanDirection = sdBoth);
begin
  {$IFDEF SCALEMM_DEBUG}
  if Self.OwnerThreadId > 1 then
  begin
    Assert(Self.OwnerThreadId = GetCurrentThreadId);
    Assert(Self.OwnerManager.OwnerThread.FThreadId = GetCurrentThreadId);
    Assert(Self.OwnerList.OwnerManager.OwnerThread.FThreadId = GetCurrentThreadId);
  end;
  {$ENDIF}

  //Scale_CheckMem(@Self); owner can be different thread (medium block)

  Assert(FMemoryArray <> nil);
  Assert(FMemoryArray = Pointer(NativeUInt(@Self) + SizeOf(Self)) );

  {$IFDEF SCALEMM_DEBUG}
  if OwnerThreadId = 1 then
  begin
    Assert(OwnerManager <> nil);
    Assert(OwnerManager.OwnerThread <> nil);
    Assert(OwnerThreadId = OwnerManager.OwnerThread.FThreadId);
  end
  else
    Assert(OwnerThreadId = GetCurrentThreadId);
  {$ENDIF}
  Assert(OwnerList <> nil);
  Assert(OwnerList <> Pointer(1));
  Assert(OwnerList.OwnerManager <> nil);
  Assert(OwnerList.OwnerManager.OwnerThread <> nil);
  Assert(not OwnerList.OwnerManager.OwnerThread.FThreadTerminated);
  Assert(OwnerManager <> nil);
  Assert(OwnerManager <> Pointer(2));
  Assert(OwnerManager = OwnerList.OwnerManager);

  Assert(FFreedIndex <= NativeUInt(Length(FFreedArray)));
  Assert(FUsageCount <= NativeUInt(Length(FFreedArray)));
  Assert(FMemoryArray <> nil);

  if FPreviousMemBlock <> nil then
  begin
    Assert(FPreviousMemBlock.FNextMemBlock = @Self);
  end;
  if FPreviousFreedMemBlock <> nil then
  begin
    Assert( (FPreviousFreedMemBlock.FFreedIndex <> 0) or (FPreviousFreedMemBlock.FUsageCount < C_ARRAYSIZE) ); //must have something free or available
    Assert(FPreviousFreedMemBlock.FNextFreedMemBlock = @Self);
  end;
  if (aDirection in [sdBoth, sdPrevious]) then
  begin
    if FPreviousMemBlock <> nil then
      FPreviousMemBlock.CheckMem(sdNone);
    if FPreviousFreedMemBlock <> nil then
      FPreviousFreedMemBlock.CheckMem(sdNone);
  end;

  if FNextMemBlock <> nil then
  begin
    Assert(FNextMemBlock.FPreviousMemBlock = @Self);
  end;
  if FNextFreedMemBlock <> nil then
  begin
    Assert( (FNextFreedMemBlock.FFreedIndex <> 0) or (FNextFreedMemBlock.FUsageCount < C_ARRAYSIZE) ); //must have something free or available
    if Self.OwnerManager.OwnerThread.FThreadId > 1 then
      Assert(FNextFreedMemBlock.FPreviousFreedMemBlock = @Self);
  end;
  if (aDirection in [sdBoth, sdNext]) then
  begin
    if FNextMemBlock <> nil then
      FNextMemBlock.CheckMem(sdNone);
    if FNextFreedMemBlock <> nil then
      FNextFreedMemBlock.CheckMem(sdNone);
  end;
end;

procedure TSmallMemBlock.FreeBlockMemory;
{$IFDEF SCALEMM_DEBUG}
var
  pOwnerList: PSmallMemBlockList;
{$ENDIF}
begin
  if OwnerList.FFirstMemBlock = @Self then
    Exit; //keep one block

  {$IFDEF SCALEMM_DEBUG}
  Self.CheckMem(sdBoth);
  {$ENDIF}

  if OwnerList.FFirstFreedMemBlock = @Self then
    OwnerList.FFirstFreedMemBlock := Self.FNextFreedMemBlock;
  if OwnerList.FFirstMemBlock = @Self then
    OwnerList.FFirstMemBlock := Self.FNextMemBlock;

  // remove ourselves from linked list
  if FPreviousMemBlock <> nil then
    FPreviousMemBlock.FNextMemBlock := Self.FNextMemBlock;
  if FPreviousFreedMemBlock <> nil then
    FPreviousFreedMemBlock.FNextFreedMemBlock := Self.FNextFreedMemBlock;
  if FNextMemBlock <> nil then
    FNextMemBlock.FPreviousMemBlock := Self.FPreviousMemBlock;
  if FNextFreedMemBlock <> nil then
    FNextFreedMemBlock.FPreviousFreedMemBlock := Self.FPreviousFreedMemBlock;

  {$IFDEF SCALEMM_DEBUG}
  FPreviousFreedMemBlock := nil;
  FNextMemBlock := nil;
  FNextFreedMemBlock := nil;
  FPreviousMemBlock := nil;
  Self.CheckMem;
  OwnerList.CheckMem;
  OwnerThreadId := MaxInt;
  pOwnerList := Self.OwnerList;
  OwnerThreadId := MaxInt;
  {$ENDIF}
  FMemoryArray  := nil;
  OwnerList     := nil;
  OwnerManager  := nil;

  //release medium block
  Scale_FreeMem(@Self);

  {$IFDEF SCALEMM_DEBUG}
  pOwnerList.CheckMem;
  {$ENDIF}
end;

procedure TSmallMemBlock.FreeMem(aMemoryItem: PSmallMemHeader);
begin
  {$IFDEF SCALEMM_DEBUG}
  aMemoryItem.CheckMem;
  CheckMem;
  OwnerList.CheckMem;
  {$ENDIF}

  // free mem block
  FFreedArray[FFreedIndex] := aMemoryItem;
  inc(FFreedIndex);

  {$IFDEF SCALEMM_DEBUG}
  aMemoryItem.CheckMem;
  CheckMem;
  OwnerList.CheckMem;
  {$ENDIF}

  // first free item of block?
  // then we add this block to (linked) list with available mem
  if FFreedIndex = 1 then
  begin
    if ({self}FNextFreedMemBlock = nil) and       //not already in free list?
       ({self}FPreviousFreedMemBlock = nil) then
    with OwnerList^ do  //faster
    if {Owner}FFirstFreedMemBlock <> @Self then
    begin
      Assert(FNextFreedMemBlock = nil);
      Assert(FPreviousFreedMemBlock = nil);

      {Self.}FNextFreedMemBlock := {Owner}FFirstFreedMemBlock;   //link to first list
      {Self.}FPreviousFreedMemBlock := nil;
      if {Self}FNextFreedMemBlock <> nil then
        {Self}FNextFreedMemBlock.FPreviousFreedMemBlock := @Self; //back link
      {Owner}FFirstFreedMemBlock := @Self; //replace first list
    end;
  end
  else
    // all memory available?
    if FFreedIndex = C_ARRAYSIZE then
      FreeBlockMemory;
end;

function TSmallMemBlock.GetUsedMemoryItem: PSmallMemHeader;
begin
  {$IFDEF SCALEMM_DEBUG}
  Self.CheckMem;
  OwnerList.CheckMem;
  {$ENDIF}

  if FFreedIndex > 0 then  //free items left?
  begin
    dec(FFreedIndex);
    Result := FFreedArray[FFreedIndex];

    if (FFreedIndex = 0) and              //no free items left?
       (FUsageCount >= C_ARRAYSIZE) then  //and fully used?
    begin
      Assert(OwnerList.FFirstFreedMemBlock = @Self);
      // set next free memlist
      OwnerList.FFirstFreedMemBlock := {Self.}FNextFreedMemBlock;
      // first one has no previous
      if {Self.}FNextFreedMemBlock <> nil then
        {Self.}FNextFreedMemBlock.FPreviousFreedMemBlock := nil;
      // remove from free list
      {Self.}FPreviousFreedMemBlock := nil;
      {Self.}FNextFreedMemBlock     := nil;
    end;
  end
  else  //otherwise use remaining unused mem
  begin
    Assert(FUsageCount < C_ARRAYSIZE);

    Result       := Pointer( NativeUInt(FMemoryArray) +
                             (FUsageCount * (OwnerList.FItemSize + SizeOf(TSmallMemHeader))) );
    inc(FUsageCount);
    Result.OwnerBlock  := @Self;
    Result.Size        := OwnerList.FItemSize;
    {$IFDEF SCALEMM_MAGICTEST}
    Result.Magic1 := 1234567890;
    Result.Magic2 := 1122334455;
    {$ENDIF}

    if (FUsageCount >= C_ARRAYSIZE) and   //fully used?
       (FFreedIndex = 0) then             //no free items left?
    begin
      Assert(OwnerList.FFirstFreedMemBlock = @Self);
      // set next free memlist
      OwnerList.FFirstFreedMemBlock := {Self.}FNextFreedMemBlock;
      // first one has no previous
      if {Self.}FNextFreedMemBlock <> nil then
        {Self.}FNextFreedMemBlock.FPreviousFreedMemBlock := nil;
      // remove from free list
      {Self.}FPreviousFreedMemBlock := nil;
      {Self.}FNextFreedMemBlock     := nil;
    end;
  end;

  {$IFDEF SCALEMM_DEBUG}
  Result.CheckMem;
  Self.CheckMem;
  OwnerList.CheckMem;
  {$ENDIF}
end;

procedure TSmallMemBlock.Lock;
var
  iCount, iNewCount: Integer;
begin
  //unlock
  repeat
    iCount    := FThreadFreedCount;
    iNewCount := (iCount + 1) * -1;
    if (iCount >= 0) and
       CAS32(iCount, iNewCount, @FThreadFreedCount)
    then
      Break;
    //small wait: try to swith to other pending thread (if any) else direct continue
    if not SwitchToThread then
      sleep(0);

    //try again
    iCount    := FThreadFreedCount;
    iNewCount := (iCount + 1) * -1;
    if (iCount >= 0) and
       CAS32(iCount, iNewCount, @FThreadFreedCount)
    then
      Break;
    //wait some longer: force swith to any other thread
    sleep(1);
  until False;
end;

procedure TSmallMemBlock.ThreadFreeMem(aMemoryItem: PSmallMemHeader);
begin
  Lock;

  Dec(FThreadFreedCount);      //-1 is already used for normal locks
  //add new one to front
  PSmallMemHeaderFree(aMemoryItem).NextFreeItem := FFirstThreadFreed;
  FFirstThreadFreed := PSmallMemHeaderFree(aMemoryItem);

  UnLock;

  //8 pending? then notify master thread object
  if FThreadFreedCount = -8 then
  begin
    if NativeUInt(Self.OwnerManager) > 2 then    //not global manager
    begin
      Self.OwnerManager.Lock;
      Self.FNextThreadFreedBlock := Self.OwnerManager.FFirstThreadFreeBlock;
      Self.OwnerManager.FFirstThreadFreeBlock := @Self;
      Self.OwnerManager.UnLock;
    end;
  end;
end;

procedure TSmallMemBlock.UnLock;
begin
  Assert(FThreadFreedCount < 0);
  //unlock
  if not CAS32(FThreadFreedCount, Abs(FThreadFreedCount+1), @FThreadFreedCount) then
    Assert(False);
end;

{ TSmallMemBlockList }

procedure TSmallMemBlockList.AddNewMemoryBlock;
var
  pm: PSmallMemBlock;
begin
  {$IFDEF SCALEMM_DEBUG}
  Self.CheckMem;
  {$ENDIF}

  FRecursive := True;
  // get block from cache
  pm := GlobalManager.GetSmallBlockMemory(FItemSize);
  if pm = nil then
  begin
    // create own one
    pm := PThreadMemManager(OwnerManager.OwnerThread).FMediumMemManager
            .GetMem( SizeOf(pm^) +
                     (FItemSize + SizeOf(TSmallMemHeader) + 1) * C_ARRAYSIZE );
//    with pm^ do // put zero only to needed properties
//    begin
//      fillchar(FNextFreedMemBlock,SizeOf(FNextFreedMemBlock)+
//        SizeOf(FPreviousFreedMemBlock)+SizeOf(FUsageCount)+
//        SizeOf({$ifdef USEBITMAP}FFreed{$else}FFreedIndex{$endif}),0);
    fillchar(pm^, SizeOf(pm^), 0);

    pm.FMemoryArray := Pointer(NativeUInt(pm) + SizeOf(pm^));
    {$IFDEF Align8Bytes}
    Assert( NativeUInt(pm.FMemoryArray) AND 7 = 0);
    {$ENDIF}
    {$IFDEF Align16Bytes}
    Assert( NativeUInt(pm.FMemoryArray) AND 15 = 0);
    {$ENDIF}
    {$IFDEF SCALEMM_DEBUG}
    pm.OwnerList     := Pointer(1);
    pm.OwnerManager  := Pointer(2);
    pm.OwnerThreadId := 2;
    {$ENDIF}
  end;

  {$IFDEF SCALEMM_DEBUG}
  Self.CheckMem;
  {$ENDIF}

  // init
  with pm^ do
  begin
    {$IFDEF SCALEMM_DEBUG}
    Assert(pm.OwnerThreadId <= 2);
    Assert(pm.OwnerManager = Pointer(2));
    Assert(pm.OwnerList = Pointer(1));
    pm.OwnerThreadId := GetCurrentThreadId;
    {$ENDIF}

    //pm.Lock;
    pm.OwnerManager      := Self.OwnerManager;
    pm.OwnerList         := @Self;
    //pm.UnLock;

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
    end;
  end;
  FRecursive := False;

  {$IFDEF SCALEMM_DEBUG}
  pm.CheckMem;
  Self.CheckMem;
  {$ENDIF}
end;

procedure TSmallMemBlockList.CheckMem;
begin
  if FFirstMemBlock <> nil then
  begin
    Assert(FFirstMemBlock.FPreviousMemBlock = nil);
    FFirstMemBlock.CheckMem;
    Assert(FFirstMemBlock.OwnerList <> nil);
    Assert(FFirstMemBlock.OwnerList = @Self);
  end;

  if FFirstFreedMemBlock <> nil then
  begin
    Assert(FFirstFreedMemBlock.FPreviousFreedMemBlock = nil);
    if OwnerManager.OwnerThread.FThreadId > 1 then
      FFirstFreedMemBlock.CheckMem;
    Assert(FFirstFreedMemBlock.OwnerList <> nil);
    Assert(FFirstFreedMemBlock.OwnerList = @Self);
  end;

  Assert(OwnerManager <> nil);
  Assert(OwnerManager.OwnerThread <> nil);
  Assert(not OwnerManager.OwnerThread.FThreadTerminated);
  if OwnerManager.OwnerThread.FThreadId = 1 then
  begin
    //is global mem
  end
  else
    Assert(OwnerManager.OwnerThread.FThreadId = GetCurrentThreadId);
end;

function TSmallMemBlockList.GetMemFromNewBlock: Pointer;
var
  pm: PSmallMemBlock;
begin
  // store: first time init?
  if FFirstMemBlock = nil then
    AddNewMemoryBlock;

  pm := FFirstMemBlock;
  while (pm.FUsageCount >= C_ARRAYSIZE) and
        (pm.FFreedIndex = 0) do              //otherwise "pm.GetUsedMemoryItem" will fail
  begin
    AddNewMemoryBlock;
    pm := FFirstMemBlock;
  end;

  // get mem from list
  with pm^ do
  begin
    // space left?
    if FUsageCount < C_ARRAYSIZE then
    begin
      // calc next item
      //Result := FMemoryArray;
      //FMemoryArray := Pointer( NativeUInt(FMemoryArray) +
      //                         (FItemSize + SizeOf(TSmallMemHeader)) );
      //calculate by multiply to reduce cache writes?
      Result       := Pointer( NativeUInt(FMemoryArray) +
                               (FUsageCount * (FItemSize + SizeOf(TSmallMemHeader))) );
      inc(FUsageCount);
      // startheader = link to memlist
      PSmallMemHeader(Result).OwnerBlock  := pm;
      PSmallMemHeader(Result).Size        := FItemSize;
    end
    else
      Result := pm.GetUsedMemoryItem;
  end;

  {$IFDEF SCALEMM_MAGICTEST}
  TSmallMemHeader(Result^).Magic1 := 1234567890;
  TSmallMemHeader(Result^).Magic2 := 1122334455;
  {$ENDIF}
  {$IFDEF SCALEMM_DEBUG}
  TSmallMemHeader(Result^).CheckMem;
  Self.CheckMem;
  {$ENDIF}
  Result  := Pointer(NativeUInt(Result) + SizeOf(TSmallMemHeader));
end;

{ TSmallMemThreadManager }

procedure TSmallMemThreadManager.CheckAllMem;
var
  i: Integer;
begin
  for i := Low(Self.FMiniMemoryBlocks) to High(Self.FMiniMemoryBlocks) do
    FMiniMemoryBlocks[i].CheckMem;
  for i := Low(Self.FSmallMemoryBlocks) to High(Self.FSmallMemoryBlocks) do
    FSmallMemoryBlocks[i].CheckMem;
end;

procedure TSmallMemThreadManager.CheckMem(aMemory: Pointer);
var
  ph: PSmallMemHeader;
begin
  Assert(Self.OwnerThread.FThreadId = GetCurrentThreadId);

  ph  := PSmallMemHeader(NativeUInt(aMemory) - SizeOf(TSmallMemHeader));
  ph.CheckMem;
  Assert(ph.OwnerBlock <> nil);
  ph.OwnerBlock.CheckMem(sdBoth);
end;

function TSmallMemThreadManager.FreeMem(aMemory: Pointer): NativeInt;
var
  ph: PSmallMemHeader;
begin
  if FFirstThreadFreeBlock <> nil then
    FreeThreadFreedMem;

  {$ifdef SCALEMM_DEBUG} Result := 0; try {$ENDIF}
  ph  := PSmallMemHeader(NativeUInt(aMemory) - SizeOf(TSmallMemHeader));

  {$IFDEF SCALEMM_DEBUG}
  ph.CheckMem;
  {$ENDIF}
  // block obtained via Scale_GetMem()
  Assert(ph.OwnerBlock <> nil);
  ph.OwnerBlock.FreeMem(ph);
  Result := 0;

  {$ifdef SCALEMM_DEBUG} except sleep(0); end; {$ENDIF}
end;

procedure TSmallMemThreadManager.FreeThreadFreedMem;
var
  freeblock, nextblock, nextblock2: PSmallMemBlock;
  pfreemem, nextmem, nextmem2: PSmallMemHeaderFree;
  p:pointer;
begin
  if FFirstThreadFreeBlock = nil then Exit;

  Lock;
  freeblock := FFirstThreadFreeBlock;
  FFirstThreadFreeBlock := nil;
  UnLock;

  nextblock := freeblock;
  while nextblock <> nil do
  begin
    nextblock.Lock;
    pfreemem := nextblock.FFirstThreadFreed;
    nextblock.FFirstThreadFreed := nil;
    nextblock.FThreadFreedCount := -1;  //will be made 0 in unlock
    nextblock2 := nextblock.FNextThreadFreedBlock;
    nextblock.UnLock;
    nextblock := nextblock2;

    nextmem := pfreemem;
    while nextmem <> nil do
    begin
      p := Pointer(NativeUInt(nextmem) + SizeOf(TSmallMemHeader));
      nextmem2 := nextmem;
      nextmem  := nextmem.NextFreeItem;

      if nextmem2.OwnerBlock.OwnerManager = @Self then    //owner can be changed in the mean time!
      begin
        Self.FreeMem(p);
      end
      else
        nextmem2.OwnerBlock.ThreadFreeMem( PSmallMemHeader(nextmem2) );
    end;

  end;
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
  if FFirstThreadFreeBlock <> nil then
    FreeThreadFreedMem;

  bm := nil;
  iNewSize := aSize + (SizeOf(TSmallMemHeader) - 1);

  if iNewSize <= ((length(FMiniMemoryBlocks)*32)-1) then
    // blocks of 32: 32, 64, 96, 128, 160, 192, 224
    bm := @FMiniMemoryBlocks[(iNewSize) shr 5]
  else if iNewSize <= ((length(FSmallMemoryBlocks)*256)-1) then
    // blocks of 256: 256,512,768,1024,1280,1536,1792 bytes
    bm := @FSmallMemoryBlocks[(iNewSize) shr 8]
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

  {$IFDEF SCALEMM_DEBUG}
  PSmallMemHeader(NativeUInt(Result) - SizeOf(TSmallMemHeader)).CheckMem;
  PSmallMemHeader(NativeUInt(Result) - SizeOf(TSmallMemHeader)).OwnerBlock.CheckMem;
  PSmallMemHeader(NativeUInt(Result) - SizeOf(TSmallMemHeader)).OwnerBlock.OwnerList.CheckMem;
  bm.CheckMem;
  {$ENDIF}
end;

procedure TSmallMemThreadManager.Init;
var i, j: NativeUInt;
begin
  SizeType := stSmall;

  Fillchar(Self, SizeOf(Self), 0);
  j := 32;
  for i := Low(FMiniMemoryBlocks) to High(FMiniMemoryBlocks) do
  begin // 32, 64, 96, 128, 160, 192, 224 bytes
    FMiniMemoryBlocks[i].OwnerManager := @Self;
    FMiniMemoryBlocks[i].FItemSize := j;
    inc(j,32);
  end;
  assert(j=256);
  for i := Low(FSmallMemoryBlocks) to High(FSmallMemoryBlocks) do
  begin // 256,512,768,1024,1280,1536,1792 bytes
    FSmallMemoryBlocks[i].OwnerManager := @Self;
    FSmallMemoryBlocks[i].FItemSize := j;
    inc(j,256);
  end;
  //assert(j=2304);
  assert(j=2560);
end;

procedure TSmallMemThreadManager.Lock;
begin
  //LOCK
  while not CAS32(0, 1, @FLock) do
  begin
    //small wait: try to swith to other pending thread (if any) else direct continue
    if not SwitchToThread then
      sleep(0);
    //try again
    if CAS32(0, 1, @FLock) then
      Break;
    //wait some longer: force swith to any other thread
    sleep(1);
  end;
end;

procedure TSmallMemThreadManager.MoveAllMemToOtherManager(
  aOtherManager: PSmallMemThreadManager);
var
  i: NativeUInt;

  procedure __ProcessBlockMem(aOldBlock, aGlobalBlock: PSmallMemBlockList);
  var
    allmem, tempmem,
    lastinusemem, firstinusemem: PSmallMemBlock;
  begin
    allmem        := aOldBlock.FFirstMemBlock;
    firstinusemem := nil;
    lastinusemem  := nil;

    // scan all memoryblocks and filter unused blocks
    while allmem <> nil do
    begin
      allmem.FNextFreedMemBlock     := nil;
      allmem.FPreviousMemBlock      := nil;
      allmem.FPreviousFreedMemBlock := nil;

      // fully free, no mem in use?
      if (allmem.FFreedIndex = allmem.FUsageCount) then
      begin
        // next one
        tempmem := allmem;
        allmem  := allmem.FNextMemBlock;
        // dispose
        Assert(Self.OwnerThread.FThreadId = GetCurrentThreadId);
        PThreadMemManager(Self.OwnerThread).FMediumMemManager.FreeMemOnlyMarked(tempmem);
//        PThreadMemManager(Self.OwnerThread).FMediumMemManager.FreeMemNoCheck(tempmem);
//        Scale_FreeMem(tempmem);

        Continue;
      end
      else
      // some items in use (in other thread? or mem leak?)
      begin
        // first item of list?
        if firstinusemem = nil then
          firstinusemem := allmem
        else
        begin
          // else add to list (link to previous)
          lastinusemem.FNextFreedMemBlock := allmem;
          if firstinusemem.FNextFreedMemBlock = nil then
            firstinusemem.FNextFreedMemBlock := allmem;
        end;

        lastinusemem  := allmem;
      end;

      {$IFDEF SCALEMM_DEBUG}
      Assert(allmem.OwnerThreadId <> 0);
      allmem.OwnerThreadId := 1;
      {$ENDIF}
      //allmem.Lock;
      allmem.OwnerManager  := aOtherManager;
      allmem.OwnerList     := aGlobalBlock;
      //allmem.UnLock;

      // next one
      tempmem := allmem.FNextMemBlock;
      allmem.FNextMemBlock := nil;
      allmem  := tempmem;
    end;

    if firstinusemem <> nil then
    begin
      assert(lastinusemem <> nil);
      // add freemem list to front (replace first item, link previous to last item)
      lastinusemem.FNextFreedMemBlock  := aGlobalBlock.FFirstFreedMemBlock;
      aGlobalBlock.FFirstFreedMemBlock := firstinusemem;
    end;
  end;

begin
  Assert(Self.OwnerThread.FThreadId = GetCurrentThreadId);
  for i := Low(Self.FMiniMemoryBlocks) to High(Self.FMiniMemoryBlocks) do
    __ProcessBlockMem( @FMiniMemoryBlocks[i],   @aOtherManager.FMiniMemoryBlocks[i]);
  for i := Low(Self.FSmallMemoryBlocks) to High(Self.FSmallMemoryBlocks) do
    __ProcessBlockMem( @FSmallMemoryBlocks[i],  @aOtherManager.FSmallMemoryBlocks[i]);
end;

const
  SmallBlockDownsizeCheckAdder = 64;
  SmallBlockUpsizeAdder = 32;

function TSmallMemThreadManager.ReallocMem(aMemory: Pointer;
  aSize: NativeUInt): Pointer;
var
  ph: PSmallMemHeader;
  {$IFDEF SCALEMM_DEBUG}
  p: Pointer;
  {$ENDIF}
begin
  if FFirstThreadFreeBlock <> nil then
    FreeThreadFreedMem;

  ph := PSmallMemHeader(NativeUInt(aMemory) - SizeOf(TSmallMemHeader));
  {$IFDEF SCALEMM_DEBUG}
  Assert(ph.OwnerBlock.OwnerList <> nil);
  p  := Pointer(NativeUInt(aMemory) - SizeOf(TSmallMemHeader));
  PSmallMemHeader(p).CheckMem;
  {$ENDIF}

  with ph^ do
  begin
    //downscaling, new size smaller than current size
    if (NativeUInt(aSize) <= Size) then
    begin
      if NativeUInt(aSize) + SmallBlockDownsizeCheckAdder >= (Size shr 2) then
      begin
        Result := aMemory; // no resize needed up to 1/4 the current item size
      end
      else
      // too much downscaling: use move
      begin
        Result := Self.GetMem(aSize); // new mem
        Move(aMemory^, Result^, aSize); // copy (use smaller new size)
        //Self.FreeMem(aMemory); // free old mem
        ph.OwnerBlock.FreeMem(ph);
      end;
    end
    else
    //upscaling, new size bigger than current size
    begin
      if aSize <= C_MAX_SMALLMEM_SIZE then   //1 till 2048
        Result := Self.GetMem(aSize + SmallBlockUpsizeAdder)
      else  //bigger mem?
        Result := PThreadMemManager(Self.OwnerThread).GetMem(aSize + SmallBlockUpsizeAdder); // new mem
      Move(aMemory^, Result^, Size); // copy (use smaller old size)
      //Self.FreeMem(aMemory); // free old mem
      ph.OwnerBlock.FreeMem(ph);
    end;
  end;

  {$IFDEF SCALEMM_DEBUG}
  p := PBaseMemHeader(NativeUInt(Result) - SizeOf(TBaseMemHeader));
  if PBaseMemHeader(p).OwnerBlock.OwnerManager = @Self then
  begin
    p := Pointer(NativeUInt(Result) - SizeOf(TSmallMemHeader));
    PSmallMemHeader(p).CheckMem;
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

procedure TSmallMemThreadManager.UnLock;
begin
  //UNLOCK
  if not CAS32(1, 0, @FLock) then
    Assert(False);
  //FLock := False;
end;

initialization
  {$IFDEF Align8Bytes}
  Assert( SizeOf(TSmallMemHeader) AND 7 = 0);
  Assert( SizeOf(TSmallMemBlock)  AND 7 = 0);
  {$ENDIF}
  {$IFDEF Align16Bytes}
  Assert( SizeOf(TSmallMemHeader) AND 15 = 0);
  Assert( SizeOf(TSmallMemBlock)  AND 15 = 0);
  {$ENDIF}
  Assert( SizeOf(TBaseMemHeader) = SizeOf(TSmallMemHeader) );

end.
