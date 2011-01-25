unit smmMediumMemory;

//known "bugs":
// - use TMediumHeader.Size instead of .Next (because we use MSB of .Next we
// cannot use mem above 2Gb)

interface

{$Include smmOptions.inc}

uses
  smmTypes;

const
  C_MAX_MEDIUMMEM_SIZE = (1 shl 16 shl 4); //1Mb

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
    PrevMem   : PMediumHeader;
    Size       : NativeUInt;
    /// must be last item of header (same negative offset from mem as TBaseMemHeader)
    OwnerBlock : PMediumBlockMemory;
  end;
  //40 bytes, first 16 are the same, rest are use for a free block
  TMediumHeaderExt = record
    {$IFDEF SCALEMM_MAGICTEST}
    Magic1       : NativeInt;
    {$ENDIF}
    NextMem      : PMediumHeader;
    PrevMem     : PMediumHeader;
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
  end;

  //Block of 1Mb
  TMediumBlockMemory = record           //16bytes
    /// Thread owner, must be first item of block (same offset as PBaseBlockMemory)
    OwnerThread  : PMediumThreadManager;
    Size         : NativeUInt;
    //linked list of blocks of a thread
    NextBlock,
    PreviousBlock: PMediumBlockMemory;
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

    function GetMem(aSize: NativeUInt) : Pointer;
    function FreeMem(aMemory: Pointer): NativeInt;
    function ReallocMem(aMemory: Pointer; aSize: NativeUInt): Pointer;
  end;

  {
  //4 bytes
  TSmallHeader = record
    offset2block: Word;
    offset2next : Word;  //size
  end;
  }

implementation

uses
  smmGlobal, ScaleMM2, smmFunctions;

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
    pheader := ScanBlockForFreeItems(pblock, aMinResultSize);
    //no mem of minimum size?
    if pheader = nil then
      pblock  := GlobalManager.GetMediumBlockMemory;
  until (pheader <> nil) or (pblock = nil);

  //no cached block? then alloc from large mem
  if pblock = nil then
  begin
    iAllocSize := (1 shl 16 shl 4) {1mb} +
                  SizeOf(TMediumBlockMemory)  {block header} +
                  SizeOf(TMediumHeader) {extra beginheader} +
                  SizeOf(TMediumHeader) {extra endheader} +
    {$IFDEF SCALEMM_MAGICTEST}
                  65480; //makes 1114112 = $110000 -> no gap between 2 allocs
    {$ELSE}
                  65504; //makes 1114112 = $110000 -> no gap between 2 allocs
    {$ENDIF}
                         //e.g. $3A50000 - $3B60000 = $110000
    Assert(iAllocSize <= $110000);
    //iAllocSize := $110000;
    iAllocSize := $FFFFF;       //10kb unused

    //alloc
    pblock := PThreadMemManager(Self.OwnerManager).FLargeMemManager.GetMem(iAllocSize);
    pblock.Size        := iAllocSize;

    //first item
    pheader            := PMediumHeaderExt( NativeUInt(pblock) + SizeOf(TMediumBlockMemory));
    pheader.OwnerBlock := pblock;
    pheader.Size       := iAllocSize -
                          SizeOf(TMediumBlockMemory) -
                          SizeOf(TMediumHeader);
                          //(1 shl 16 shl 4) {1mb} +
                          // SizeOf(TMediumHeader){begin};
    pheader.NextMem    := PMediumHeader( NativeUInt(pheader) + pheader.Size);
//    pheader.NextMem    := PMediumHeader( NativeUInt(pheader) +
//                                         (1 shl 16 shl 4) {1mb} +
//                                         SizeOf(TMediumHeader){begin} );
    //reset end
    pheader.NextMem.OwnerBlock  := nil;
    pheader.NextMem.NextMem     := nil;
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

  pblock.OwnerThread := @Self;

  //linked list of thread blocks (replace first)
  if FFirstBlock <> nil then
    FFirstBlock.PreviousBlock := pblock;
  pblock.NextBlock            := FFirstBlock;
  FFirstBlock                 := pblock;
  pblock.PreviousBlock        := nil;

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

procedure TMediumThreadManager.FreeBlock(aBlock: PMediumBlockMemory);
begin
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

//  if header.OwnerBlock <> nil then
//  begin
//    if header.OwnerBlock.OwnerThread = @Self then
      //put free item to block + allmem array + mask
  PutMemToFree(header, header.Size); // NativeUInt(header.NextMem) - NativeUInt(header))
//    else
//      FreeMemOtherThread(header);
  Result := 0;

  {$IFDEF SCALEMM_MAGICTEST}
  Assert( (header.Magic1 = 0) or (NativeUInt(header.Magic1) = $80808080)); //must be free (or cleared)
  {$ENDIF}
//  end
end;

(*
function TMediumThreadManager.GetBigMem(aSize: NativeInt): Pointer;
var
  pheader: PMediumHeader;
begin
  //alloc
  pheader := VirtualAlloc( nil,
                          aSize + SizeOf(TMediumHeader) {extra beginheader},
                          MEM_COMMIT {$ifdef AlwaysAllocateTopDown} or MEM_TOP_DOWN{$endif},
                          PAGE_READWRITE);
  //reset begin
  pheader.OwnerBlock := nil;
  pheader.NextMem    := nil;
  pheader.Size       := aSize + SizeOf(TMediumHeader);

  {$ifdef SCALEMM_MAGICTEST}
  pheader.Magic1 := 123456789;
  Assert(pmediumheader(pheader).Magic1 = 123456789); //must be in use!
  {$ENDIF}

  Result := Pointer(NativeUInt(pheader) + SizeOf(TMediumHeader));
end;
*)

function TMediumThreadManager.GetMem(aSize: NativeUInt): Pointer;
var
  //iWordSize: Word;
  iWordRemainderSize: NativeUInt;
  iMSB: NativeUInt;

  //i, iStart,
  iFreeMemIndex,
  //iSubblock,
  iRemainder: NativeInt;
  remaindersize, allocsize,
  //tempsize: NativeUInt;
  tempmask: NativeUInt;
  pheader : PMediumHeaderExt;
  pheaderremainder : PMediumHeaderExt;
begin
  {$ifdef SCALEMM_DEBUG} Result := nil; try {$ENDIF}

  allocsize := ( aSize +
                 SizeOf(TMediumHeader) +      //alloc extra for header
                 //(aSize div 16) +           //alloc some extra mem (6%) for small grow
                 (aSize shr 4) +              //alloc some extra mem (6%) for small grow
                 8 ); // shr 3 shl 3;         //8byte aligned: add 8 and remove lowest bits (later)
  if allocsize < SizeOf(TMediumHeaderExt) then
    allocsize := SizeOf(TMediumHeaderExt);
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
//  Assert( NativeUInt(pheader.Next) xor (1 shl 31) //remove higest bit (free mark)
//          - NativeUInt(pheader) > allocsize );
  Assert(pheader.Magic1 = 0);  //not in use!
  Assert( NativeUInt(pheader.NextMem) > NativeUInt(1 shl 31) ); //higest bit (free mark)
  Assert(pheader.PrevFreeItem = nil);  //must be first one
  {$ENDIF}
  {$IFDEF SCALEMM_MAGICTEST}
  pheader.Magic1 := 123456789; //mark in use
  {$ENDIF}

  //remainder
  remaindersize := pheader.Size - allocsize;
  if remaindersize > SizeOf(TMediumHeaderExt) then
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
      {pheaderremainder.}PrevMem      := PMediumHeader(pheader);
      //pheaderremainder.Magic1        := pheader.Magic1;
      {pheaderremainder.}NextFreeItem  := pheader.NextFreeItem;
      {pheaderremainder.}PrevFreeItem  := pheader.PrevFreeItem;
      {pheaderremainder.}BlockMask     := pheader.BlockMask;
      {pheaderremainder.}ArrayPosition := pheader.ArrayPosition;
    end;

    //keep same block as free
    //only change to new offset
    FFreeMem[iFreeMemIndex] := pheaderremainder;

    with pheader^ do
    begin
      //next = alloc
      {pheader.}NextMem := PMediumHeader(pheaderremainder);
      {pheader.}Size    := allocsize;
      Assert(pheader.OwnerBlock <> nil);
      Assert(pheader.NextMem.OwnerBlock <> nil);

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
    if {pheader.}NextFreeItem {FreeMem2[iFreeMemIndex]} = nil then
      FFreeMask := FFreeMask xor {pheader.}BlockMask
    else
    begin
      {$IFDEF SCALEMM_MAGICTEST}
      Assert(pheader.NextFreeItem.Magic1 = 0);
      {$ENDIF}
      pheader.NextFreeItem.PrevFreeItem := nil;
    end;

    //alloc size
    {pheader.}NextMem  := PMediumHeader(NativeUInt(pheader) + allocsize);
    {pheader.}Size     := allocsize;
    //set block of next item too
    {pheader.}NextMem.OwnerBlock := {pheader.}OwnerBlock;
    {pheader.}NextMem.PrevMem   := PMediumHeader(pheader);

    //create remainder
    if remaindersize > 0 then
    begin
      PutMemToFree({pheader.}NextMem, remaindersize);
      {$IFDEF SCALEMM_MAGICTEST}
      Assert(pheader.NextMem.Magic1 = 0); //must be free
      {$ENDIF}
    end;
  end;

  Result := Pointer(NativeUInt(pheader) + SizeOf(TMediumHeader));

  {$ifdef SCALEMM_MAGICTEST}
  Assert(pmediumheader(pheader).Magic1 = 123456789); //must be in use!
  {$ENDIF}
  {$ifdef SCALEMM_DEBUG}
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
  //i: Integer;
//  pblock: PMediumBlockMemory;
  newsize: NativeUInt;
  //strippedpointer : NativeUInt;
  {$IFDEF SCALEMM_FILLFREEMEM}
  temppointer: Pointer;
  {$ENDIF}
  iRemainder: NativeInt;
  //iWordRemainderSize: Word;
begin
  {$ifdef SCALEMM_DEBUG} try {$ENDIF}

  if aSize <= 0 then Exit;
  newsize := aSize;

  //create remainder
  pheaderremainder := PMediumHeaderExt(aHeader);
  //next
  pnext            := PMediumHeaderExt(NativeUInt(pheaderremainder) + newsize);
  with pheaderremainder^ do
  begin
    {pheaderremainder.}NextMem   := PMediumHeader(pnext);
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
    NativeInt(pheaderremainder.NextMem) := NativeInt(pheaderremainder.NextMem) or (1 shl 31);
//    pheaderremainder.NextFreeItem  := nil;
//    pheaderremainder.PrevFreeItem  := nil;
//    pheaderremainder.BlockMask     := nil;
//    pheaderremainder.ArrayPosition := nil;

    while (pprev <> nil) and
          //is highest bit set? then previous is a free block
          (NativeUInt(pprev.NextMem) > NativeUInt(1 shl 31)) do
    begin
      {$IFDEF SCALEMM_MAGICTEST}
      Assert(pprev.Magic1 = 0); //must be free
      {$ENDIF}
      pheaderremainder := PMediumHeaderExt(pprev);  //start point!
      pprev            := pprev.PrevMem;
      newsize          := 0;      //re-calc from first block
    end;

    //link next item to start point
    pnext.PrevMem   := PMediumHeader(pheaderremainder);
    //"next"  var is new start point
    pnext            := pheaderremainder;
  end;

  //...then do a forward scan to make one block
  while (pnext <> nil) and
        //(next.Next <> nil) and  //next is not the end
        //is highest bit set? then next is a free block
        (NativeUInt(pnext.NextMem) > NativeUInt(1 shl 31)) do
        //note: previous block cannot be scanned!?
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
        begin
          {next.}NextFreeItem.PrevFreeItem := nil;
          {$IFDEF SCALEMM_MAGICTEST}
          Assert(pnext.NextFreeItem.Magic1 = 0); //must be free
          {$ENDIF}
        end;
      end
      else
      begin
        {$IFDEF SCALEMM_MAGICTEST}
        Assert(pnext.PrevFreeItem.Magic1 = 0); //must be free
        {$ENDIF}
        //remove from linked list
        {next.}PrevFreeItem.NextFreeItem := {next.}NextFreeItem;
        if {next.}NextFreeItem <> nil then
        begin
          {next.}NextFreeItem.PrevFreeItem := {next.}PrevFreeItem;
          {$IFDEF SCALEMM_MAGICTEST}
          Assert(pnext.NextFreeItem.Magic1 = 0); //must be free
          {$ENDIF}
        end;
      end;
    end;

    //pnext := PMediumHeaderExt(NativeUInt(pheaderremainder) + newsize);
    //alloc/resize
    with pheaderremainder^ do
    begin
      //{pheaderremainder.}Size := newsize;
      //{pheaderremainder.}Next := PMediumHeader(pnext);
      {pheaderremainder.}NextMem := PMediumHeader(NativeUInt(pheaderremainder) + newsize);
      {recursive scanning: maybe other blocks freed in other threads etc}
      pnext := PMediumHeaderExt({pheaderremainder.}NextMem);
      {pheaderremainder.}Size    := newsize;
    end;
  end;

  {$ifdef SCALEMM_DEBUG}
  Assert(newsize >= SizeOf(TMediumHeaderExt));
  {$ENDIF}
  {$IFDEF SCALEMM_FILLFREEMEM}
  //reset all mem!
  temppointer := Pointer(NativeUInt(pheaderremainder) +
                 SizeOf(TMediumHeader)); //do no reset header
  FillChar( temppointer^,
            newsize - SizeOf(TMediumHeader){offset} - SizeOf(TMediumHeader){own header},
            $80);
  {$ENDIF}
  {$IFDEF SCALEMM_MAGICTEST}
  pheaderremainder.Magic1 := 0;//free
  {$ENDIF}

  if newsize < (1 shl 16 shl 4) then  //smaller than 1mb?
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
    if FFreeMem[iRemainder] <> nil then
    begin
      FreeBlock(pheaderremainder.OwnerBlock);
      Exit;
    end;
  end;

  with pheaderremainder^ do
  begin
    //link next item to new start point
    if {pheaderremainder.}NextMem <> nil then
      {pheaderremainder.}NextMem.PrevMem := PMediumHeader(pheaderremainder);

    //set higest bit (mark as free)
    NativeInt({pheaderremainder.}NextMem) := NativeInt({pheaderremainder.}NextMem) or (1 shl 31);
    {pheaderremainder.}Size               := newsize;
    {pheaderremainder.}PrevMem           := pprev;
  end;
  {$IFDEF SCALEMM_MAGICTEST}
  if pprev <> nil then
    Assert(pprev.Magic1 = 123456789);
  {$ENDIF}

  //set mask
  FFreeMask := FFreeMask or (1 shl iRemainder);

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
    //{pheaderremainder.}Size          := newsize;
    {pheaderremainder.}ArrayPosition := iRemainder;
    {pheaderremainder.}BlockMask     := (1 shl iRemainder);
    {pheaderremainder.}PrevFreeItem  := nil; //we are the first
  end;

  {$ifdef SCALEMM_DEBUG} except sleep(0); end; {$ENDIF}
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

  //same?
  if newsize = currentsize then
  begin
    Result := aMemory;

    {$IFDEF SCALEMM_OUTPUTSTRING}
    OutputDebugString( PChar('Medium 1 realloc ' + IntToStr(aMemory) + ' -> ' + IntToStr(Result) +
                             ' (' + IntToStr(currentsize) + ' -> ' + IntToStr(newsize) + ')' ));
    {$ENDIF}
    {$IFDEF SCALEMM_MAGICTEST}
    Assert(header.Magic1 = 123456789); //must be in use!
    {$ENDIF}
    Exit
  end
  //downscaling?
  else if newsize <= currentsize then
  begin
    newsize       := newsize + (aSize div 16);    //alloc some extra mem for small grow
                     //SizeOf(TMediumHeader);
    newsize       := (newsize + 8) shr 3 shl 3;  //8byte aligned: add 8 and remove lowest bits
    if newsize < SizeOf(TMediumHeaderExt) then
      newsize := SizeOf(TMediumHeaderExt);
    remaindersize := NativeInt(currentsize) - NativeInt(newsize);
    //less than 32 bytes left after resize? (can be negative!)
    if (remaindersize < SizeOf(TMediumHeaderExt)) {or less than div 16 change?} then
    begin
      //do nothing
      Result := aMemory;
      {$IFDEF SCALEMM_OUTPUTSTRING}
      OutputDebugString( PChar('Medium 2 realloc ' + IntToStr(aMemory) + ' -> ' + IntToStr(Result) +
                               ' (' + IntToStr(currentsize) + ' -> ' + IntToStr(newsize) + ')' ));
      {$ENDIF}
      {$IFDEF SCALEMM_MAGICTEST}
      Assert(header.Magic1 = 123456789); //must be in use!
      {$ENDIF}
      Exit;
    end
    else
    begin
      Result := aMemory;

      {$IFDEF SCALEMM_OUTPUTSTRING}
      OutputDebugString( PChar('Medium 3 realloc ' + IntToStr(aMemory) + ' -> ' + IntToStr(Result) +
                             ' (' + IntToStr(currentsize) + ' -> ' + IntToStr(newsize) + ')' ));
      {$ENDIF}
      //resize
      header.Size               := newsize;
      header.NextMem            := PMediumHeader(NativeUInt(header) + newsize);
      header.NextMem.OwnerBlock := header.OwnerBlock;
      header.NextMem.PrevMem   := header;
      //make new block of remaining
      PutMemToFree(header.NextMem, remaindersize);

      {$IFDEF SCALEMM_MAGICTEST}
      Assert(header.Magic1 = 123456789); //must be in use!
      Assert(header.NextMem.Magic1 = 0);    //must be free
      {$ENDIF}
    end;
  end
  //upscaling: see if we have next block with some space
  else
  begin
    newsize  := newsize + (aSize div 16);  //alloc some extra mem for small grow
                //SizeOf(TMediumHeader);
    newsize  := (newsize + 8) shr 3 shl 3;  //8byte aligned: add 8 and remove lowest bits

    //todo: backwards scanning too?

    nextfree := PMediumHeaderExt(header.NextMem);
    //can we use some mem from next free block?
    if (nextfree <> nil) and
       //(next.Next <> nil) and  //next is not the end
       //is highest bit set? then next is a free block
       (NativeUInt(nextfree.NextMem) > NativeUInt(1 shl 31)) then
       //note: previous block cannot be scanned!?
    begin
      nextsize := nextfree.Size + currentsize;   //next size
      {$IFDEF SCALEMM_MAGICTEST}
      Assert(nextfree.Magic1 = 0); //must be free
      {$ENDIF}

      //large enough?
      if nextsize > newsize then
      begin
        Result := aMemory;
        {$IFDEF SCALEMM_OUTPUTSTRING}
        OutputDebugString( PChar('Medium 4 realloc ' + IntToStr(aMemory) + ' -> ' + IntToStr(Result) +
                             ' (' + IntToStr(currentsize) + ' -> ' + IntToStr(newsize) + ')' ));
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
          begin
            nextfree.NextFreeItem.PrevFreeItem := nil;
            {$IFDEF SCALEMM_MAGICTEST}
            Assert(nextfree.NextFreeItem.Magic1 = 0); //must be free
            {$ENDIF}
          end;
        end
        else
        begin
          //remove from linked list
          nextfree.PrevFreeItem.NextFreeItem := nextfree.NextFreeItem;
          if nextfree.NextFreeItem <> nil then
          begin
            nextfree.NextFreeItem.PrevFreeItem := nextfree.PrevFreeItem;
            {$IFDEF SCALEMM_MAGICTEST}
            Assert(nextfree.NextFreeItem.Magic1 = 0); //must be free
            {$ENDIF}
          end;
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
        if remaindersize < SizeOf(TMediumHeaderExt) then
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
      end
      else
      begin
        Assert(newsize > currentsize);
        //alloc new mem and copy data
        Result := PThreadMemManager(Self.OwnerManager).GetMem(newsize);
        {$IFDEF SCALEMM_OUTPUTSTRING}
        OutputDebugString( PChar('Medium 5 realloc ' + IntToStr(aMemory) + ' -> ' + IntToStr(Result) +
                                 ' (' + IntToStr(currentsize) + ' -> ' + IntToStr(newsize) + ')' ));
        {$ENDIF}
        if aMemory <> Result then
        begin
          Move(aMemory^, Result^, currentsize); // copy (use smaller old size)
          Self.FreeMem(aMemory); // free old mem
        end;
      end;
    end
    else
    //alloc new mem and copy data
    begin
      Assert(newsize > currentsize);
      Result := PThreadMemManager(Self.OwnerManager).GetMem(newsize);

      {$IFDEF SCALEMM_OUTPUTSTRING}
      OutputDebugString( PChar('Medium 6 realloc ' + IntToStr(aMemory) + ' -> ' + IntToStr(Result) +
                                 ' (' + IntToStr(currentsize) + ' -> ' + IntToStr(newsize) + ')' ));
      {$ENDIF}

      if aMemory <> Result then
      begin
        Move(aMemory^, Result^, currentsize); // copy (use smaller old size)
        FreeMem(aMemory); // free old mem
      end;
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
          //note: previous block cannot be scanned!?
    begin
      Assert( Cardinal(nextmem.NextMem) <> $80808080);
      //make one block
      newsize := newsize + nextmem.Size;
      //get next item
      nextmem := PMediumHeaderExt(NativeUInt(nextmem) + nextmem.Size);
      //nextmem := PMediumHeaderExt(NativeInt(nextmem.Next) xor (1 shl 31));
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

end.
