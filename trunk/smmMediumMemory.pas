unit smmMediumMemory;

//known "bugs":
// - use TMediumHeader.Size instead of .Next (because we use MSB of .Next we
// cannot use mem above 2Gb)

interface

{$Include smmOptions.inc}

uses
  smmTypes;

//const
//  C_MAX_MEDIUMMEM_SIZE = (1 shl 16 shl 4) - //1Mb
//                          SizeOf(TMediumBlockMemory) -
//                          SizeOf(TMediumHeader);

type
  PMediumHeader        = ^TMediumHeader;
  PMediumHeaderExt     = ^TMediumHeaderExt;
  PMediumBlockMemory   = ^TMediumBlockMemory;
  PMediumThreadManager = ^TMediumThreadManager;

  //16 bytes, single memory item
  TMediumHeader = record
    {$IFDEF SCALEMM_MAGICTEST}
    Magic1     : NativeInt;
    {$ENDIF}
    //linked items in one block
    NextMem    : PMediumHeader;
    PrevMem    : PMediumHeader;
    Size       : NativeUInt;
    /// must be last item of header (same negative offset from mem as TBaseMemHeader)
    OwnerBlock : PMediumBlockMemory;

    procedure CheckMem(aDirection: TScanDirection = sdBoth);
  end;
  //40 bytes, first 16 are the same, rest are use for a free block
  TMediumHeaderExt = record
    {$IFDEF SCALEMM_MAGICTEST}
    Magic1       : NativeInt;
    {$ENDIF}
    NextMem      : PMediumHeader;
    PrevMem      : PMediumHeader;
    Size         : NativeUInt;
    OwnerBlock   : PMediumBlockMemory;

    //Extra data of free item:---------------------------------
    //linked list of free mem of the same size of a thread
    NextFreeItem : PMediumHeaderExt;
    PrevFreeItem : PMediumHeaderExt;
    //simple storage of size calculation:
    BlockMask    : NativeUInt;
    ArrayPosition: NativeUInt;
    //linked list of free mem, freed in other thread
    NextThreadFree: PMediumHeaderExt;
    PrevThreadFree: PMediumHeaderExt;

    procedure CheckMem(aDirection: TScanDirection = sdBoth);
  end;

  //Block of 1Mb
  TMediumBlockMemory = record           //16bytes
    /// Thread owner, must be first item of block (same offset as PBaseBlockMemory)
    OwnerThread  : PMediumThreadManager;
    Size         : NativeUInt;
    //linked list of blocks of a thread
    NextBlock,
    PreviousBlock: PMediumBlockMemory;

    procedure CheckMem;
  end;

  //Medium memory manager of a thread
  TMediumThreadManager = record
  public
    SizeType: TSizeType;
    OwnerManager: PBaseThreadManager;
    FFirstBlock : PMediumBlockMemory;
  private
    FFreeMem  : array[0..16] of PMediumHeaderExt;
    FFreeMask : NativeUInt; //word, 16 bit, 65535

    function  AllocBlock(aMinResultSize: NativeUInt): PMediumHeaderExt;
    procedure FreeBlock(aBlock: PMediumBlockMemory);
    function  ScanBlockForFreeItems(aBlock: PMediumBlockMemory; aMinResultSize: NativeUInt): PMediumHeaderExt;

    procedure PutMemToFree(aHeader: PMediumHeader; aSize: NativeUInt);
  public
    procedure Init;
    procedure Reset;

    procedure CheckMem(aMemory: Pointer = nil);

    function GetMem(aSize: NativeUInt) : Pointer;
    function FreeMem(aMemory: Pointer): NativeInt;
    function ReallocMem(aMemory: Pointer; aSize: NativeUInt): Pointer;
  end;

const
  C_MAX_MEDIUMMEM_SIZE = (1 shl 16 shl 4) - //1Mb
                          SizeOf(TMediumBlockMemory) -
                          SizeOf(TMediumHeader) -
                          SizeOf(TMediumHeader);

implementation

uses
  smmGlobal, ScaleMM2, smmFunctions, smmSmallMemory;

////////////////////////////////////////////////////////////////////////////////

{ TMediumThreadManager }

function TMediumThreadManager.AllocBlock(aMinResultSize: NativeUInt): PMediumHeaderExt;
var
  iAllocSize: NativeUInt;
  pheader: PMediumHeaderExt;
  pblock : PMediumBlockMemory;
begin
  pheader := nil;
  pblock  := GlobalManager.GetMediumBlockMemory;
  if pblock <> nil then
  repeat
    pblock.OwnerThread := @Self;
    pheader := ScanBlockForFreeItems(pblock, aMinResultSize);
    //no mem of minimum size?
    if pheader = nil then
      pblock  := GlobalManager.GetMediumBlockMemory;
  until (pheader <> nil) or (pblock = nil);

  //no cached block? then alloc from large mem
  if pblock = nil then
  begin
//    iAllocSize := (1 shl 16 shl 4) {1mb} +
//                  SizeOf(TMediumBlockMemory)  {block header} +
//                  SizeOf(TMediumHeader) {extra beginheader} +
//                  SizeOf(TMediumHeader) {extra endheader} +
//    {$IFDEF SCALEMM_MAGICTEST}
//                  65480; //makes 1114112 = $110000 -> no gap between 2 allocs
//    {$ELSE}
//                  65504; //makes 1114112 = $110000 -> no gap between 2 allocs
//    {$ENDIF}
//                         e.g. $3A50000 - $3B60000 = $110000
//    Assert(iAllocSize <= $110000);
//    Assert(iAllocSize <=  $FFFFF);
    //iAllocSize := $110000;
    //iAllocSize := $FFFFF;
    iAllocSize := (1 shl 16 shl 4) {1mb};  //exact 1mb, so no gap between 2 virtual allocs

    //alloc
    pblock := PThreadMemManager(Self.OwnerManager).FLargeMemManager.GetMem(iAllocSize);
    pblock.Size        := iAllocSize;
    pblock.OwnerThread := @Self;

    //first item
    pheader            := PMediumHeaderExt( NativeUInt(pblock) + SizeOf(TMediumBlockMemory));
    pheader.OwnerBlock := pblock;
    pheader.Size       := iAllocSize -
                          SizeOf(TMediumBlockMemory) -  //block
                          SizeOf(TMediumHeader) -       //start
                          SizeOf(TMediumHeader);        //end(!)
    pheader.NextMem    := PMediumHeader( NativeUInt(pheader) + pheader.Size);
    //reset end
    pheader.NextMem.Size        := 0;
    pheader.NextMem.OwnerBlock  := pblock;
    pheader.NextMem.NextMem     := nil;
    pheader.NextMem.PrevMem     := PMediumHeader(pheader);
    {$IFDEF SCALEMM_MAGICTEST}
    pheader.NextMem.Magic1 := 0;
    pheader.Magic1         := 0;
    {$ENDIF}
    //mark as free
    NativeInt(pheader.NextMem) := NativeInt(pheader.NextMem) or (1 shl 31);
    //init
    pheader.NextFreeItem  := nil;
    pheader.PrevFreeItem  := nil;
    pheader.BlockMask     := 1 shl 16;
    pheader.ArrayPosition := 16;

    //max size is available now
    FFreeMask    := FFreeMask or (1 shl 16);
    FFreeMem[16] := pheader;
  end;
  assert(pheader <> nil);

  //linked list of thread blocks (replace first)
  if FFirstBlock <> nil then
    FFirstBlock.PreviousBlock := pblock;
  pblock.NextBlock            := FFirstBlock;
  FFirstBlock                 := pblock;
  pblock.PreviousBlock        := nil;

  {$IFDEF SCALEMM_DEBUG}
  //pblock.CheckMem;
  CheckMem;
  {$ENDIF}

  Result := pheader;
end;

(*
procedure TMediumThreadManager.FreeMemOtherThread(aHeader: PMediumHeader);
var
  headerext: PMediumHeaderExt;
  iRemainder: NativeInt;
begin
  headerext := PMediumHeaderExt(aHeader);
  with headerext^ do
  begin
    //{headerext.}Size       := NativeUInt({headerext.}NextMem) - NativeUInt(headerext);
    {headerext.}NextFreeItem := nil;
    {headerext.}PrevFreeItem := nil;
  end;

  if headerext.Size < (1 shl 16 shl 4) then  //smaller than 1mb?
  begin
    iRemainder := BitScanLast(                        //get highest bit
                              headerext.Size shr 4);  //div 16 so 1mb fits in 16bit
  end
  else
    iRemainder := 16;

  with headerext^ do
  begin
    {headerext.}BlockMask     :=  (1 shl iRemainder);
    {headerext.}ArrayPosition := iRemainder;

    {$IFDEF SCALEMM_MAGICTEST}
    headerext.Magic1 := 0;//free
    {$ENDIF}

    //as last: set higest bit (mark as free)
    //now owner thread can use it when it scans for marked mem
    NativeInt({headerext.}NextMem) := NativeInt({headerext.}NextMem) or (1 shl 31);
  end;
end;
*)

procedure TMediumThreadManager.CheckMem(aMemory: Pointer = nil);
var
  block: PMediumBlockMemory;
  header: PMediumHeader;
  headerext: PMediumHeaderExt;
  i: Integer;
begin
  if aMemory <> nil then
  begin
    header := PMediumHeader(NativeUInt(aMemory) - SizeOf(TMediumHeader));
    header.CheckMem(sdBoth);
  end
  else  //all
  begin
    block := Self.FFirstBlock;
    while block <> nil do
    begin
      block.CheckMem;
      block := block.NextBlock;
    end;

    //check free mem
    for i := Low(FFreeMem) to High(FFreeMem) do
    begin
      headerext := FFreeMem[i];

      //check mask
      if headerext = nil then
        Assert(FFreeMask and (1 shl i) = 0)
      else
        Assert(FFreeMask and (1 shl i) <> 0);

      //check linked free items
      while headerext <> nil do
      begin
        {$IFDEF SCALEMM_MAGICTEST}
        Assert(headerext.Magic1 = 0); //must be free
        {$ENDIF}
        headerext.CheckMem(sdNone);
        headerext := headerext.NextFreeItem;
      end;
    end;
  end;
end;

procedure TMediumThreadManager.FreeBlock(aBlock: PMediumBlockMemory);
begin
  {$IFDEF SCALEMM_DEBUG}
  aBlock.CheckMem;
  {$ENDIF}

  //remove from linked list
  if FFirstBlock = aBlock then
  begin
    FFirstBlock := aBlock.NextBlock;
    if FFirstBlock <> nil then
      FFirstBlock.PreviousBlock := nil;
  end
  else
  begin
    if aBlock.NextBlock <> nil then
      aBlock.NextBlock.PreviousBlock := aBlock.PreviousBlock;
    if aBlock.PreviousBlock <> nil then
      aBlock.PreviousBlock.NextBlock := aBlock.NextBlock;
  end;
  aBlock.NextBlock     := nil;
  aBlock.PreviousBlock := nil;

  PThreadMemManager(Self.OwnerManager).FLargeMemManager.FreeMem(aBlock);
  //GlobalManager.FreeBlockMemory(aBlock);
end;

function TMediumThreadManager.FreeMem(aMemory: Pointer): NativeInt;
var
  header: PMediumHeader;
begin
  header  := PMediumHeader(NativeUInt(aMemory) - SizeOf(TMediumHeader));
  {$IFDEF SCALEMM_MAGICTEST}
  Assert(header.Magic1 = 123456789); //must be in use!
  {$ENDIF}
  {$IFDEF SCALEMM_DEBUG}
  header.CheckMem(sdNone);
  {$ENDIF}

  PutMemToFree(header, header.Size); // NativeUInt(header.NextMem) - NativeUInt(header))
  Result := 0;
end;

function TMediumThreadManager.GetMem(aSize: NativeUInt): Pointer;
var
  iWordRemainderSize: NativeUInt;
  iMSB: NativeUInt;
  iFreeMemIndex,
  iRemainder: NativeInt;
  remaindersize, allocsize,
  tempmask: NativeUInt;
  pheader : PMediumHeaderExt;
  pnext,
  pheaderremainder: PMediumHeaderExt;
begin
  {$ifdef SCALEMM_DEBUG} Result := nil; try {$ENDIF}

  allocsize := ( aSize +
                 SizeOf(TMediumHeader) +      //alloc extra for header
                 //(aSize div 16) +           //alloc some extra mem (6%) for small grow
                 (aSize shr 4) +              //alloc some extra mem (6%) for small grow
                 8 ); // shr 3 shl 3;         //8byte aligned: add 8 and remove lowest bits (later)
  if allocsize < SizeOf(TMediumHeaderExt) then
    allocsize := SizeOf(TMediumHeaderExt);
  if allocsize > C_MAX_MEDIUMMEM_SIZE then
    allocsize := C_MAX_MEDIUMMEM_SIZE;
  Assert(allocsize <= C_MAX_MEDIUMMEM_SIZE);
  iMSB      := BitScanLast(
                           allocsize shr 4    //div 16 so 1mb fits in 16bit
                          );                  //get highest bit
  allocsize := allocsize shr 3 shl 3;         //8byte aligned: we've add 8 before and remove lowest bits now

  //first without +1 and check if size is OK (otherwise alloc of same size after alloc + free will fail)
  pheader   := FFreeMem[iMSB];
  if (pheader <> nil) and (pheader.Size >= allocsize)  then
  begin
    iFreeMemIndex := iMSB;
  end
  else
  begin
    inc(iMSB);  //+1 for max size
    tempmask := FFreeMask shr iMSB shl iMSB;   //reset lowest bits (shr + shl)

    if tempmask > 0 then
    //get available mem
    begin
      iFreeMemIndex := BitScanFirst(tempmask);        //get lowest bit (smallest size)
      Assert(iFreeMemIndex >= 0);
      pheader       := FFreeMem[iFreeMemIndex];
      Assert(pheader <> nil);
    end
    else
    //alloc new mem (for biggest block)
    begin
      iFreeMemIndex := 16;
      pheader       := AllocBlock(allocsize);
    end;
  end;

  {$IFDEF SCALEMM_DEBUG}
  Assert(pheader <> nil);
  Assert( Cardinal(pheader.NextMem) <> $80808080);
  //check allocated size
  Assert( ((1 shl iFreeMemIndex shl 4) > allocsize) or (pheader.Size >= allocsize) );
  Assert( NativeUInt(pheader.NextMem) > NativeUInt(1 shl 31) ); //higest bit (free mark)
  Assert(pheader.PrevFreeItem = nil);  //must be first one
  {$ENDIF}
  {$IFDEF SCALEMM_MAGICTEST}
  Assert(pheader.Magic1 = 0);  //not in use!
  {$ENDIF}
  {$IFDEF SCALEMM_DEBUG}
  pheader.CheckMem(sdNone);
  {$ENDIF}
  {$IFDEF SCALEMM_MAGICTEST}
  pheader.Magic1 := 123456789; //mark in use
  {$ENDIF}

  //remainder
  remaindersize := pheader.Size - allocsize;
  //if remaindersize > SizeOf(TMediumHeaderExt) then
  //less then "small" mem? then do not free remaining (will never be used)
  if remaindersize > C_MAX_SMALLMEM_SIZE then
  begin
    iWordRemainderSize := remaindersize shr 4;     //div 16 so 1mb fits in 16bit
    iRemainder         := BitScanLast(iWordRemainderSize);  //get highest bit
  end
  else
  begin
    allocsize     := allocsize + remaindersize;   //use all remaining mem too
    remaindersize := 0;
    iRemainder    := 0;
  end;
  {$IFDEF SCALEMM_DEBUG}
  assert(allocsize >= aSize);
  {$ENDIF}

  //same block left?
  if iFreeMemIndex = iRemainder then
  begin
    pheaderremainder := PMediumHeaderExt(NativeUInt(pheader) + allocsize);
    //copy header
    with pheaderremainder^ do
    begin
      {pheaderremainder.}OwnerBlock    := pheader.OwnerBlock;
      {pheaderremainder.}Size          := remaindersize;
      {pheaderremainder.}NextMem       := pheader.NextMem;
      {pheaderremainder.}PrevMem       := PMediumHeader(pheader);
      {pheaderremainder.}NextFreeItem  := pheader.NextFreeItem;
      {pheaderremainder.}PrevFreeItem  := pheader.PrevFreeItem;
      {pheaderremainder.}BlockMask     := pheader.BlockMask;
      {pheaderremainder.}ArrayPosition := pheader.ArrayPosition;
    end;

    //keep same block as free
    //only change to new offset
    FFreeMem[iFreeMemIndex] := pheaderremainder;

    //relink block after remainder
    pnext := PMediumHeaderExt(NativeUInt(pheaderremainder) + remaindersize);
    pnext.PrevMem := PMediumHeader(pheaderremainder);

    with pheader^ do
    begin
      //next = alloc
      {pheader.}NextMem := PMediumHeader(pheaderremainder);
      {pheader.}Size    := allocsize;

      //relink next one to our new offset
      if {pheader.}NextFreeItem <> nil then
        {pheader.}NextFreeItem.PrevFreeItem := pheaderremainder;

      {$IFDEF SCALEMM_DEBUG}
      Assert(iFreeMemIndex = pheader.ArrayPosition);
      {$ENDIF}
      {$IFDEF SCALEMM_MAGICTEST}
      pheaderremainder.Magic1 := 0; //mark as free
      {$ENDIF}
    end;
  end
  else
  //if remaindersize > or < pblock.StepSize then
  {put remainder free block in AllMem array + mask}
  with pheader^ do
  begin
    //replace with next free block
    FFreeMem[iFreeMemIndex] := {pheader.}NextFreeItem;
    {$IFDEF SCALEMM_DEBUG}
    Assert( (pheader.NextFreeItem = nil) or
            (pheader.NextFreeItem.ArrayPosition = iFreeMemIndex) );
    {$ENDIF}
    {$IFDEF SCALEMM_MAGICTEST}
    Assert( (pheader.NextFreeItem = nil) or
            (pheader.NextFreeItem.Magic1 = 0) ); {must be free}
    {$ENDIF}

    //reset bit if nothing free anymore
    if {pheader.}NextFreeItem = nil then
      FFreeMask := FFreeMask xor {pheader.}BlockMask
    else
      pheader.NextFreeItem.PrevFreeItem := nil;

    //alloc size
    {pheader.}Size               := allocsize;
    {pheader.}NextMem            := PMediumHeader(NativeUInt(pheader) + allocsize);
    //set block of next item too
    {pheader.}NextMem.OwnerBlock := {pheader.}OwnerBlock;
    {pheader.}NextMem.PrevMem    := PMediumHeader(pheader);

    //create remainder
    if remaindersize > 0 then
      PutMemToFree({pheader.}NextMem, remaindersize);
  end;

  Result := Pointer(NativeUInt(pheader) + SizeOf(TMediumHeader));

  {$ifdef SCALEMM_MAGICTEST}
  Assert(pmediumheader(pheader).Magic1 = 123456789); //must be in use!
  {$ENDIF}
  {$ifdef SCALEMM_DEBUG}
  PMediumHeader(pheader).CheckMem(sdNone);
  except sleep(0); end;
  {$ENDIF}
end;

procedure TMediumThreadManager.Init;
begin
  SizeType := stMedium;
end;

procedure TMediumThreadManager.PutMemToFree(aHeader: PMediumHeader;
  aSize: NativeUInt);
var
  pnext,
  pheaderremainder: PMediumHeaderExt;
  pprev: PMediumHeader;
  newsize: NativeUInt;
  {$IFDEF SCALEMM_FILLFREEMEM}
  temppointer: Pointer;
  {$ENDIF}
  iRemainder: NativeInt;
begin
  {$ifdef SCALEMM_DEBUG} try {$ENDIF}

  if aSize <= 0 then Exit;
  newsize := aSize;

  {$IFDEF SCALEMM_FILLFREEMEM}
  //reset old mem
  temppointer := Pointer(NativeUInt(aHeader) + SizeOf(TMediumHeader)); //do no reset header
  FillChar( temppointer^, aSize - SizeOf(TMediumHeader), $80);
  {$ENDIF}

  //create remainder
  pheaderremainder := PMediumHeaderExt(aHeader);
  //next
  pnext            := PMediumHeaderExt(NativeUInt(pheaderremainder) + newsize);
  with pheaderremainder^ do
  begin
    {pheaderremainder.}NextMem   := PMediumHeader(pnext);
    {pheaderremainder.}PrevMem   := aHeader.PrevMem;
    {pheaderremainder.}Size      := newsize;
    {pheaderremainder.}BlockMask := 0; //not stored
    {$ifdef SCALEMM_MAGICTEST}
    pheaderremainder.Magic1 := 0;//free
    {$ENDIF}
  end;
  pnext.PrevMem := aHeader;

  //do a backward scanning first to get the first free item...
  pprev := pheaderremainder.PrevMem;
  if (pprev <> nil) and
     //is highest bit set? then previous is a free block
     (NativeUInt(pprev.NextMem) > NativeUInt(1 shl 31)) then
  begin
    //must be marked to be processed in "next" scanning
    NativeInt(pheaderremainder.NextMem) := NativeInt(pheaderremainder.NextMem) or (1 shl 31);

    while (pprev <> nil) and
          //is highest bit set? then previous is a free block
          (NativeUInt(pprev.NextMem) > NativeUInt(1 shl 31)) do
    begin
      {$IFDEF SCALEMM_MAGICTEST}
      Assert(pprev.Magic1 = 0); //must be free
      {$ENDIF}
      pheaderremainder := PMediumHeaderExt(pprev);  //start point!
      pprev            := pprev.PrevMem;            //get previous of previous
      newsize          := 0;                        //re-calc from first block
    end;

    //link (old) next item to new start point
    pnext.PrevMem := PMediumHeader(pheaderremainder);
    //"next" var is new start point
    pnext         := pheaderremainder;
  end;

  //...then do a forward scan to make one block
  while (pnext <> nil) and
        //is highest bit set? then next is a free block
        (NativeUInt(pnext.NextMem) > NativeUInt(1 shl 31)) do
  begin
    Assert( Cardinal(pnext.NextMem) <> $80808080);
    //make one block
    newsize := newsize + pnext.Size;

    {$IFDEF SCALEMM_MAGICTEST}
    Assert(pnext.Magic1 = 0); //must be free
    {$ENDIF}

    //remove old size
    with pnext^ do
    if BlockMask <> 0 then  //skip aHeader itself
    begin
      if {next.}PrevFreeItem = nil then {first?}
      begin
        //replace first item with next
        FFreeMem[{next.}ArrayPosition] := {next.}NextFreeItem;
        if {next.}NextFreeItem = nil then
          FFreeMask := FFreeMask xor {next.}BlockMask
        else
          {next.}NextFreeItem.PrevFreeItem := nil;
      end
      else
      begin
        //remove from linked list
        {next.}PrevFreeItem.NextFreeItem := {next.}NextFreeItem;
        if {next.}NextFreeItem <> nil then
          {next.}NextFreeItem.PrevFreeItem := {next.}PrevFreeItem;
      end;
    end;

    //reset
    {$IFDEF SCALEMM_MAGICTEST}
    pnext.Magic1 := -1;
    {$ENDIF}
    pnext.Size   := MaxInt;

    pnext := PMediumHeaderExt(NativeUInt(pheaderremainder) + newsize);
    //alloc/resize
    with pheaderremainder^ do
    begin
      {pheaderremainder.}Size    := newsize;
      {pheaderremainder.}NextMem := PMediumHeader(pnext);
      {pheaderremainder.}NextMem.PrevMem := PMediumHeader(pheaderremainder);
      {recursive scanning: maybe other blocks freed in other threads etc}
    end;
  end;

  {$ifdef SCALEMM_DEBUG}
  Assert(newsize >= SizeOf(TMediumHeaderExt));
  Assert(newsize <= C_MAX_MEDIUMMEM_SIZE);
  {$ENDIF}
  {$IFDEF SCALEMM_FILLFREEMEM}
  //reset all mem
//  temppointer := Pointer(NativeUInt(pheaderremainder) +
//                 SizeOf(TMediumHeader)); //do no reset header
//  FillChar( temppointer^,
//            newsize - SizeOf(TMediumHeader){offset} - SizeOf(TMediumHeader){own header},
//            $80);
  {$ENDIF}
  {$IFDEF SCALEMM_MAGICTEST}
  pheaderremainder.Magic1 := 0;//free
  {$ENDIF}

  with pheaderremainder^ do
  begin
    //link next item to new start point
    if {pheaderremainder.}NextMem <> nil then
      {pheaderremainder.}NextMem.PrevMem := PMediumHeader(pheaderremainder);
    {pheaderremainder.}Size              := newsize;
    {pheaderremainder.}PrevMem           := pprev;
  end;

  if newsize < C_MAX_MEDIUMMEM_SIZE then  //smaller than 1mb?
  begin
    iRemainder := BitScanLast(                    //get highest bit
                              newsize shr 4);     //div 16 so 1mb fits in 16bit
  end
  else
  begin
    iRemainder := 16;
    if (pheaderremainder.NextMem.NextMem = nil) and    //end?
       (pheaderremainder.PrevMem = nil) then //begin? -> fully free
    //block complete free: release mem back to Windows? {or global manager}
    //we keep one block in cache
    if FFreeMem[iRemainder] <> nil then
    begin
      //set higest bit (mark as free)
      with pheaderremainder^ do
        NativeInt({pheaderremainder.}NextMem) := NativeInt({pheaderremainder.}NextMem) or (1 shl 31);

      FreeBlock(pheaderremainder.OwnerBlock);
      Exit;
    end;
  end;
  //set mask
  FFreeMask := FFreeMask or (1 shl iRemainder);
  with pheaderremainder^ do
    //set higest bit (mark as free)
    NativeInt({pheaderremainder.}NextMem) := NativeInt({pheaderremainder.}NextMem) or (1 shl 31);

  //get first
  pnext     := FFreeMem[iRemainder];
  //replace first
  FFreeMem[iRemainder] := pheaderremainder;
  //set previous of the next
  if pnext <> nil then
  begin
    pnext.PrevFreeItem := pheaderremainder;
    {$IFDEF SCALEMM_MAGICTEST}
    assert(pnext.Magic1 = 0);
    {$ENDIF}
  end;

  with pheaderremainder^ do
  begin
    {pheaderremainder.}NextFreeItem  := pnext;
    {pheaderremainder.}ArrayPosition := iRemainder;
    {pheaderremainder.}BlockMask     := (1 shl iRemainder);
    {pheaderremainder.}PrevFreeItem  := nil; //we are the first
  end;

  {$ifdef SCALEMM_DEBUG}
  pheaderremainder.CheckMem(sdNone);
  //pheaderremainder.OwnerBlock.CheckMem;   //check all because of SCALEMM_FILLFREEMEM
  except sleep(0); end;
  {$ENDIF}
end;

function TMediumThreadManager.ReallocMem(aMemory: Pointer; aSize: NativeUInt): Pointer;
var
  header: PMediumHeader;
  nextfree: PMediumHeaderExt;
  //strippedpointer,
  remaindersize: NativeInt;
  currentsize,
  newsize, nextsize: NativeUInt;
begin
  {$ifdef SCALEMM_DEBUG} Result := nil; try {$ENDIF}

  header      := PMediumHeader(NativeUInt(aMemory) - SizeOf(TMediumHeader));
  newsize     := NativeUInt(aSize) + SizeOf(TMediumHeader);
  currentsize := header.Size;
  {$IFDEF SCALEMM_MAGICTEST}
  Assert(header.Magic1 = 123456789); //must be in use!
  {$ENDIF}
  {$ifdef SCALEMM_DEBUG}
  header.CheckMem(sdNone);
  {$ENDIF}

  //same?
  if newsize = currentsize then
  begin
    Result := aMemory;
    Exit
  end
  //downscaling?
  else if newsize <= currentsize then
  begin
    newsize       := newsize + (aSize div 16);    //alloc some extra mem for small grow
    newsize       := (newsize + 8) shr 3 shl 3;  //8byte aligned: add 8 and remove lowest bits
    //round if downsize a "medium" max size memory
    if newsize > C_MAX_MEDIUMMEM_SIZE then
      newsize := C_MAX_MEDIUMMEM_SIZE;
    //new size is "small"? then alloc small mem
    if newsize < C_MAX_SMALLMEM_SIZE then
    begin
      //alloc new mem ("small") and copy data
      Result := PThreadMemManager(Self.OwnerManager).GetMem(newsize);
      Move(aMemory^, Result^, newsize); // copy (use smaller new size)
      Self.FreeMem(aMemory); // free old mem
      Exit;
    end;
    //if newsize < SizeOf(TMediumHeaderExt) then
    //  newsize := SizeOf(TMediumHeaderExt);

    remaindersize := NativeInt(currentsize) - NativeInt(newsize);
    //less than 32 bytes left after resize? (can be negative!)
    //if (remaindersize < SizeOf(TMediumHeaderExt)) {or less than div 16 change?} then
    //less then "small" mem? then do not free remaining (will never be used)
    if remaindersize < C_MAX_SMALLMEM_SIZE then
    begin
      //do nothing
      Result := aMemory;
      Exit;
    end
    else
    begin
      Result := aMemory;

      //resize
      header.Size               := newsize;
      header.NextMem            := PMediumHeader(NativeUInt(header) + newsize);
      header.NextMem.OwnerBlock := header.OwnerBlock;
      header.NextMem.PrevMem    := header;
      //make new block of remaining
      PutMemToFree(header.NextMem, remaindersize);

      {$IFDEF SCALEMM_MAGICTEST}
      Assert(header.Magic1 = 123456789); //must be in use!
      Assert(header.NextMem.Magic1 = 0);    //must be free
      {$ENDIF}
      {$ifdef SCALEMM_DEBUG}
      header.CheckMem(sdNone);
      {$ENDIF}
    end;
  end
  //upscaling: see if we have next block with some space
  else
  begin
    if newsize <= C_MAX_MEDIUMMEM_SIZE then
    begin
      newsize  := newsize + (aSize div 16);  //alloc some extra mem for small grow
      newsize  := (newsize + 8) shr 3 shl 3;  //8byte aligned: add 8 and remove lowest bits
      if newsize < SizeOf(TMediumHeaderExt) then
        newsize := SizeOf(TMediumHeaderExt);
      if newsize > C_MAX_MEDIUMMEM_SIZE then
        newsize := C_MAX_MEDIUMMEM_SIZE;

      //todo: backwards scanning too?
      nextfree := PMediumHeaderExt(header.NextMem);
    end
    //too big, get new "large" mem
    else
      nextfree := nil;

    //can we use some mem from next free block?
    if (nextfree <> nil) and
       //is highest bit set? then next is a free block
       (NativeUInt(nextfree.NextMem) > NativeUInt(1 shl 31)) then
    begin
      nextsize := nextfree.Size + currentsize;   //next size
      {$IFDEF SCALEMM_MAGICTEST}
      Assert(nextfree.Magic1 = 0); //must be free
      {$ENDIF}

      //large enough?
      if nextsize > newsize then
      begin
        Result := aMemory;
        {$IFDEF SCALEMM_MAGICTEST}
        nextfree.Magic1 := -1; //reset
        {$ENDIF}

        { TODO -oAM : use same "alloc from free block" as GetMem }

        //remove old size
        if nextfree.PrevFreeItem = nil then {first?}
        begin
          //replace first item with nextfree
          FFreeMem[nextfree.ArrayPosition] := nextfree.NextFreeItem;
          //reset bit if nothing free anymore
          if nextfree.NextFreeItem {FreeMem2[nextfree.ArrayPosition]} = nil then
            FFreeMask := FFreeMask xor nextfree.BlockMask
          else
            nextfree.NextFreeItem.PrevFreeItem := nil;
        end
        else
        begin
          //remove from linked list
          nextfree.PrevFreeItem.NextFreeItem := nextfree.NextFreeItem;
          if nextfree.NextFreeItem <> nil then
            nextfree.NextFreeItem.PrevFreeItem := nextfree.PrevFreeItem;
        end;

        //check
        {$IFDEF SCALEMM_DEBUG}
        Assert( (nextfree.NextFreeItem = nil) or
                (nextfree.NextFreeItem.ArrayPosition = nextfree.ArrayPosition) );
        {$ENDIF}
        {$IFDEF SCALEMM_MAGICTEST}
        Assert( (nextfree.NextFreeItem = nil) or
                (nextfree.NextFreeItem.Magic1 = 0) ); {must be free}
        {$ENDIF}

        remaindersize := nextsize - newsize;
        //too small remainder? (can be negative!)
        //if remaindersize < SizeOf(TMediumHeaderExt) then
        //less then "small" mem? then do not free remaining (will never be used)
        if remaindersize < C_MAX_SMALLMEM_SIZE then
        begin
          newsize := NativeInt(newsize) + remaindersize;
          remaindersize := 0;
        end;

        //resize
        header.Size               := newsize;
        header.NextMem            := PMediumHeader(NativeUInt(header) + newsize);
        header.NextMem.OwnerBlock := header.OwnerBlock;
        header.NextMem.PrevMem    := header;
        //make new block of remaining
        if remaindersize > 0 then
          PutMemToFree(header.NextMem, remaindersize);

        {$IFDEF SCALEMM_MAGICTEST}
        Assert(header.Magic1 = 123456789); //must be in use!
        if remaindersize > 0 then
          Assert(header.NextMem.Magic1 = 0);    //must be free
        {$ENDIF}
        {$ifdef SCALEMM_DEBUG}
        header.CheckMem(sdNone);
        {$ENDIF}
      end
      else
      begin
        Assert(newsize > currentsize);
        //alloc new mem and copy data
        Result := PThreadMemManager(Self.OwnerManager).GetMem(newsize);
        Move(aMemory^, Result^, currentsize); // copy (use smaller old size)
        Self.FreeMem(aMemory); // free old mem
      end;
    end
    else
    //alloc new mem and copy data
    begin
      Assert(newsize > currentsize);
      Result := PThreadMemManager(Self.OwnerManager).GetMem(newsize);
      Move(aMemory^, Result^, currentsize); // copy (use smaller old size)
      FreeMem(aMemory); // free old mem
    end;
  end;

  {$ifdef SCALEMM_DEBUG}
  except sleep(0); end;
  {$ENDIF}
end;

procedure TMediumThreadManager.Reset;
var
  i: Integer;
begin
  FFirstBlock := nil;
  FFreeMask   := 0;
  for i := 0 to High(FFreeMem) do
    FFreeMem[i] := nil;
  //FillChar(FFreeMem, Length(FFreeMem), 0);
end;

function TMediumThreadManager.ScanBlockForFreeItems(aBlock: PMediumBlockMemory; aMinResultSize: NativeUInt): PMediumHeaderExt;
var
  firstmem: PMediumHeader;
  prevmem,
  nextmem : PMediumHeaderExt;
  newsize : NativeUInt;
begin
  firstmem := PMediumHeader( NativeUInt(aBlock) + SizeOf(TMediumBlockMemory));
  nextmem  := PMediumHeaderExt(firstmem);
  //prevmem  := nil;
  newsize  := 0;
  Result   := nil;

  while (nextmem <> nil) do
  begin
    prevmem := nextmem;

    //next block is free?
    while (nextmem <> nil) and
          (NativeUInt(nextmem.NextMem) > NativeUInt(1 shl 31)) do
    begin
      Assert( Cardinal(nextmem.NextMem) <> $80808080);
      //make one block
      newsize := newsize + nextmem.Size;
      //get next item
      nextmem := PMediumHeaderExt(NativeUInt(nextmem) + nextmem.Size);
      //note: nextmem.NextFreeBlock cannot be used, points to other thread mem
    end;

    //put mem as free in our mem array
    if (nextmem <> nil) and (newsize > 0) then
    begin
      PutMemToFree(PMediumHeader(prevmem), newsize);
      if Result = nil then
        if newsize >= aMinResultSize then
          Result := prevmem;
    end;

    newsize := 0;
    //next
    if nextmem.NextMem <> nil then
      nextmem := PMediumHeaderExt(NativeInt(nextmem.NextMem) xor (1 shl 31))
    else
      nextmem := nil;
  end;
end;

{ TMediumHeader }

procedure TMediumHeader.CheckMem(aDirection: TScanDirection = sdBoth);
var
  pheader: PMediumHeader;
begin
  {$IFDEF SCALEMM_MAGICTEST}
  Assert( (Magic1 = 0) or (Magic1 = 123456789) );
  {$ENDIF}

  Assert( Size <= C_MAX_MEDIUMMEM_SIZE );
  Assert( NativeUInt(OwnerBlock) < NativeUInt(@Self) );
  Assert( NativeUInt(@Self) - NativeUInt(OwnerBlock) <= C_MAX_MEDIUMMEM_SIZE + SizeOf(TMediumBlockMemory ) );

  if (PrevMem <> nil) then
  begin
    Assert( NativeUInt(PrevMem) < NativeUInt(@Self) );
    Assert( NativeUInt(@Self) - NativeUInt(PrevMem) <= C_MAX_MEDIUMMEM_SIZE );
    Assert( OwnerBlock = PrevMem.OwnerBlock );
    pheader := PMediumHeader(NativeUInt(PrevMem) + PrevMem.Size);
    Assert( pheader = @Self );

    if (aDirection in [sdBoth, sdPrevious]) then
      PrevMem.CheckMem(sdPrevious);
  end;

  if (NextMem <> nil) then
  begin
    pheader := PMediumHeader(NativeUInt(@Self) + Self.Size);
    Assert( NativeUInt(pheader) > NativeUInt(@Self) );
    Assert( NativeUInt(pheader) - NativeUInt(@Self) <= C_MAX_MEDIUMMEM_SIZE );
    Assert( OwnerBlock = pheader.OwnerBlock );
    Assert( pheader.PrevMem = @Self );

    //not a free block?
    if (NativeUInt(NextMem) < NativeUInt(1 shl 31)) then
    begin
      {$IFDEF SCALEMM_MAGICTEST}
      Assert(Magic1 = 123456789); //must be in use
      {$ENDIF}
      if (aDirection in [sdBoth, sdNext]) then
        NextMem.CheckMem(sdNext);
    end
    else
    begin
      {$IFDEF SCALEMM_MAGICTEST}
      Assert(Magic1 = 0); //must be free
      {$ENDIF}
      if (aDirection in [sdBoth, sdNext]) then
      begin
        //pheader := PMediumHeader(NativeUInt(@Self) + Self.Size);
        pheader.CheckMem(sdNext);
      end;
    end;
  end;
end;

{ TMediumHeaderExt }

procedure TMediumHeaderExt.CheckMem(aDirection: TScanDirection = sdBoth);
begin
  PMediumHeader(@Self).CheckMem(aDirection);

  //Assert(BlockMask >= 0);
  Assert( {(ArrayPosition >= 0) and} (ArrayPosition <= 16) );
  Assert( OwnerBlock.OwnerThread.FFreeMem[ArrayPosition] <> nil );
  Assert( OwnerBlock.OwnerThread.FFreeMask and (1 shl ArrayPosition) <> 0 );

  //check linked free items
  if Self.NextFreeItem <> nil then
    Assert( Self.NextFreeItem.PrevFreeItem = @Self );
  if Self.PrevFreeItem <> nil then
    Assert( Self.PrevFreeItem.NextFreeItem = @Self );
end;

{ TMediumBlockMemory }

procedure TMediumBlockMemory.CheckMem;
var
  pheader: PMediumHeader;
  isize: NativeUInt;
begin
  //get first mem item
  pheader := PMediumHeader(NativeUInt(@Self) + SizeOf(TMediumBlockMemory));
  pheader.CheckMem(sdNext);

  //get last one
  isize   := Self.Size -
             SizeOf(TMediumBlockMemory) -  //block
             SizeOf(TMediumHeader) -       //start
             SizeOf(TMediumHeader);        //end(!)
  pheader := PMediumHeader( NativeUInt(pheader) + iSize);
  pheader.CheckMem(sdPrevious);
end;

end.
