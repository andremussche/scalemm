unit ScaleMM2;

interface

type
  PMediumHeader    = ^TMediumHeader;
  PMediumHeaderExt = ^TMediumHeaderExt;
  PBlockMemory     = ^TBlockMemory;
  PThreadManager   = ^TThreadManager;

  TBlockMemory = record           //16bytes
    OwnerThread: PThreadManager;
    Size       : NativeUInt;
    NextBlock,
    PreviousBlock: PBlockMemory;
  end;

  TThreadManager = record
  private
    FreeMem2  : array[0..16] of PMediumHeaderExt;
    FreeMask2 : NativeUInt; //word, 16 bit, 65535
    function  AllocBlock2: PMediumHeaderExt;
    procedure PutBlockToFree2(aHeader: PMediumHeader; aSize: NativeUInt);
  public
    function GetMem2(aSize: NativeInt) : Pointer;    {$ifdef HASINLINE}inline;{$ENDIF}
    function FreeMem_2(aMemory: Pointer): NativeInt; {$ifdef HASINLINE}inline;{$ENDIF}
    function ReallocMem2(aMemory: Pointer; aSize: Integer): Pointer;
  end;

  //4 bytes
  TSmallHeader = record
    offset2block: Word;
    offset2next : Word;  //size
  end;

  //8 bytes
  TMediumHeader = record
    Block : PBlockMemory;
    Next  : PMediumHeader; //todo: only size, not fixed pointer
    {$IFDEF DEBUG}
    Magic1: NativeInt;
    {$ENDIF}
  end;
  TMediumHeaderExt = record
    Block        : PBlockMemory;
    Next         : PMediumHeader; //todo: only size, not fixed pointer
    {$IFDEF DEBUG}
    Magic1       : NativeInt;
    {$ENDIF}
    Size         : NativeUInt;
    NextFreeBlock: PMediumHeaderExt;
    PrevFreeBlock: PMediumHeaderExt;
    BlockMask    : NativeUInt;
    ArrayPosition: NativeUInt;
//    CRC: NativeUInt;
  end;

  //16 bytes
  TLargeHeader = record
    //Block: Pointer;
    Next : Pointer;  //size
    AllMemIndex: NativeUInt;
    Mask: NativeUInt;
  end;

threadvar
  GThreadManager: TThreadManager;

implementation

uses
  Windows;

//find first bit (0..31)
//http://www.arl.wustl.edu/~lockwood/class/cs306/books/artofasm/Chapter_6/CH06-4.html
function BitScanFirst(aValue: NativeInt): NativeUInt;
asm
  BSF	EAX, aValue;
end;

function BitScanLast(aValue: Word): NativeUInt;
asm
  BSR	AX, aValue;
end;

{ TThreadManager }

function TThreadManager.AllocBlock2: PMediumHeaderExt;
var
  iAllocSize: NativeUInt;
  pheader: PMediumHeaderExt;
  pblock : PBlockMemory;
begin
  iAllocSize := (1 shl 16 shl 4) {1mb} +
                SizeOf(TBlockMemory)  {block header} +
                SizeOf(TMediumHeader) {extra beginheader} +
                SizeOf(TMediumHeader) {extra endheader};

  //alloc
  pblock := VirtualAlloc( nil,
                          iAllocSize,
                          MEM_COMMIT {$ifdef AlwaysAllocateTopDown} or MEM_TOP_DOWN{$endif},
                          PAGE_READWRITE);
  pblock.Size           := iAllocSize;
  pblock.OwnerThread    := @Self;
  pblock.NextBlock      := nil;
  pblock.PreviousBlock  := nil;

  pheader               := PMediumHeaderExt( NativeUInt(pblock) + SizeOf(TBlockMemory));
  pheader.Block         := pblock;
  pheader.Next          := PMediumHeader( NativeUInt(pheader) +
                                          (1 shl 16 shl 4) {1mb} +
                                          SizeOf(TMediumHeader){begin} );
  pheader.Size          := (1 shl 16 shl 4) {1mb} +
                           SizeOf(TMediumHeader){begin};
  //reset end
  pheader.Next.Block    := nil;
  pheader.Next.Next     := nil;
  {$IFDEF DEBUG}
  pheader.Next.Magic1   := 0;
  pheader.Magic1        := 0;
  {$ENDIF}
  //mark as free
  NativeInt(pheader.Next) := NativeInt(pheader.Next) or (1 shl 31);
  //init
  pheader.NextFreeBlock := nil;
  pheader.PrevFreeBlock := nil;
  pheader.BlockMask     := 1 shl 16;
  pheader.ArrayPosition := 16;

  //max size is available now
  FreeMask2    := FreeMask2 or (1 shl 16);
  FreeMem2[16] := pheader;

  Result := pheader;
end;

function TThreadManager.FreeMem_2(aMemory: Pointer): NativeInt;
var
  header: PMediumHeader;
begin
  header  := PMediumHeader(NativeUInt(aMemory) - SizeOf(TMediumHeader));

  {$IFDEF DEBUG}
  Assert(header.Magic1 = 123456789); //must be in use!
  {$ENDIF}

  //put free item to block + allmem array + mask
  PutBlockToFree2(header, NativeUInt(header.Next) - NativeUInt(header));

  Result  := 0;

  {$IFDEF DEBUG}
  Assert(header.Magic1 = 0); //must be free
  {$ENDIF}
end;

function TThreadManager.GetMem2(aSize: NativeInt): Pointer;
var
  //iWordSize: Word;
  iWordRemainderSize: Word;
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
  {$ifdef DEBUG} try {$ENDIF}

  Result := nil;
  if aSize <= 0 then Exit;
  if aSize >= (1 shl 16 shl 4) then  //bigger than 1mb
  begin
    Result := nil; //GetMem(aSize);   { TODO -oAM : get mem bigger than 1Mb directly from Windows }
    Exit;
  end;

  allocsize     := ( aSize +
                     SizeOf(TMediumHeader) +      //alloc extra for header
                     //(aSize div 16) +           //alloc some extra mem (6%) for small grow
                     (aSize shr 4) +              //alloc some extra mem (6%) for small grow
                     8 ); // shr 3 shl 3;         //8byte aligned: add 8 and remove lowest bits (later)
  iMSB          := BitScanLast(
                               allocsize shr 4    //div 16 so 1mb fits in 16bit
                              );                  //get highest bit
  allocsize     := allocsize shr 3 shl 3;         //8byte aligned: we've add 8 before and remove lowest bits now

  //first without +1 and check if size is OK (otherwise alloc of same size after alloc + free will fail)
  pheader       := FreeMem2[iMSB];
  if (pheader <> nil) and (pheader.Size >= allocsize)  then
  begin
    iFreeMemIndex   := iMSB;
  end
  else
  begin
    inc(iMSB);  //+1 for max size
    tempmask        := FreeMask2 shr iMSB shl iMSB;   //reset lowest bits (shr + shl)

    if tempmask > 0 then
    //get available mem
    begin
      iFreeMemIndex := BitScanFirst(tempmask);        //get lowest bit (smallest size)
      Assert(iFreeMemIndex >= 0);
      pheader       := FreeMem2[iFreeMemIndex];
      Assert(pheader <> nil);
    end
    else
    //alloc new mem (for biggest block)
    begin
      iFreeMemIndex := 16;
      pheader       := AllocBlock2;
    end;
  end;

  {$IFDEF DEBUG}
  Assert( Cardinal(pheader.Next) <> $80808080);
  //check allocated size
  Assert( ((1 shl iFreeMemIndex shl 4) > allocsize) or (pheader.Size >= allocsize) );
//  Assert( NativeUInt(pheader.Next) xor (1 shl 31) //remove higest bit (free mark)
//          - NativeUInt(pheader) > allocsize );
  Assert(pheader.Magic1 = 0);  //not in use!
  Assert( NativeUInt(pheader.Next) > NativeUInt(1 shl 31) ); //higest bit (free mark)
  Assert(pheader.PrevFreeBlock = nil);  //must be first one
  pheader.Magic1 := 123456789; //mark in use
  {$ENDIF}

  //remainder
  remaindersize := pheader.Size - allocsize;
  if remaindersize > 32 then
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
  {$IFDEF DEBUG}
  assert(allocsize >= aSize);
  {$ENDIF}

  //same block left?
  if iFreeMemIndex = iRemainder then
  begin
    pheaderremainder               := PMediumHeaderExt(NativeUInt(pheader) + allocsize);
    with pheaderremainder^ do
    begin
      {pheaderremainder.}Block         := pheader.Block;
      {pheaderremainder.}Next          := pheader.Next;
      {pheaderremainder.}Size          := remaindersize;
      //pheaderremainder.Magic1        := pheader.Magic1;
      {pheaderremainder.}NextFreeBlock := pheader.NextFreeBlock;
      {pheaderremainder.}PrevFreeBlock := pheader.PrevFreeBlock;
      {pheaderremainder.}BlockMask     := pheader.BlockMask;
      {pheaderremainder.}ArrayPosition := pheader.ArrayPosition;
    end;

    //keep same block as free
    //only change to new offset
    FreeMem2[iFreeMemIndex] := pheaderremainder;

    with pheader^ do
    begin
      //next = alloc
      {pheader.}Next := PMediumHeader(pheaderremainder);
      //{pheader.}Size := allocsize; todo

      //relink next one to our new offset
      if {pheader.}NextFreeBlock <> nil then
        {pheader.}NextFreeBlock.PrevFreeBlock := pheaderremainder;

      {$IFDEF DEBUG}
      Assert(iFreeMemIndex = pheader.ArrayPosition);
      pheaderremainder.Magic1 := 0; //mark as free
      {$ENDIF}
    end;
  end
  else
  //if remaindersize > or < pblock.StepSize then
  {put remainder free block in AllMem array + mask}
  with pheader^ do
  begin
    //alloc size
    {pheader.}Next := PMediumHeader(NativeUInt(pheader) + allocsize);
    //pheader.Size := allocsize; todo

    //replace with next free block
    FreeMem2[iFreeMemIndex] := {pheader.}NextFreeBlock;
    {$IFDEF DEBUG}
    Assert( (pheader.NextFreeBlock = nil) or
            ((pheader.NextFreeBlock.ArrayPosition = iFreeMemIndex) and
             (pheader.NextFreeBlock.Magic1 = 0) {must be free})
          );
    {$ENDIF}

    //reset bit if nothing free anymore
    if {pheader.}NextFreeBlock {FreeMem2[iFreeMemIndex]} = nil then
      FreeMask2 := FreeMask2 xor {pheader.}BlockMask
    else
    begin
      {$IFDEF DEBUG}
      Assert(pheader.NextFreeBlock.Magic1 = 0);
      {$ENDIF}
      pheader.NextFreeBlock.PrevFreeBlock := nil;
    end;

    //create remainder
    if remaindersize > 0 then
    begin
      PutBlockToFree2({pheader.}Next, remaindersize);
      {$IFDEF DEBUG}
      Assert(pheader.Next.Magic1 = 0); //must be free
      {$ENDIF}
    end;
  end;

  //Result := pheader;
  Result := Pointer(NativeUInt(pheader) + SizeOf(TMediumHeader));

  {$ifdef DEBUG}
  Assert(pmediumheader(pheader).Magic1 = 123456789); //must be in use!
  except
    sleep(0);
  end;
  {$ENDIF}
end;

procedure TThreadManager.PutBlockToFree2(aHeader: PMediumHeader;
  aSize: NativeUInt);
var
  pnext,
  pheaderremainder: PMediumHeaderExt;
  //i: Integer;
//  pblock: PBlockMemory;
  newsize: NativeUInt;
  //strippedpointer : NativeUInt;
  {$ifdef DEBUG}
  temppointer: Pointer;
  {$ENDIF}
  iRemainder: NativeInt;
  //iWordRemainderSize: Word;
begin
  {$ifdef DEBUG} try {$ENDIF}

  if aSize <= 0 then Exit;
  newsize := aSize;

  //create remainder
  pheaderremainder       := PMediumHeaderExt(aHeader);

  pnext := PMediumHeaderExt(NativeUInt(pheaderremainder) + newsize);
  with pheaderremainder^ do
  begin
    {pheaderremainder.}Next  := PMediumHeader(pnext);
    {pheaderremainder.}Size  := newsize;
  end;
  //can we use some mem from next free block?
  while (pnext <> nil) and
        //(next.Next <> nil) and  //next is not the end
        //is highest bit set? then next is a free block
        (NativeUInt(pnext.Next) > NativeUInt(1 shl 31)) do
        //note: previous block cannot be scanned!?
  begin
    Assert( Cardinal(pnext.Next) <> $80808080);
    //remove highest bit to get real "next" pointer
    //strippedpointer := (NativeInt(next.Next) xor (1 shl 31));
    //make one block
    newsize := newsize + //NativeInt(pheaderremainder.Next) - NativeInt(pheaderremainder) +   //this size
               //strippedpointer - NativeUInt(next);      //next size
               pnext.Size;  //next size

    {$IFDEF DEBUG}
     Assert(pnext.Magic1 = 0); //must be free
    {$ENDIF}

    //remove old size
    with pnext^ do
    begin
      if {next.}PrevFreeBlock = nil then {first?}
      begin
        //replace first item with next
        FreeMem2[{next.}ArrayPosition] := {next.}NextFreeBlock;
        if {next.}NextFreeBlock = nil then
          FreeMask2 := FreeMask2 xor {next.}BlockMask
        else
        begin
          {next.}NextFreeBlock.PrevFreeBlock := nil;
          {$IFDEF DEBUG}
          Assert(pnext.NextFreeBlock.Magic1 = 0); //must be free
          {$ENDIF}
        end;
      end
      else
      begin
        {$IFDEF DEBUG}
        Assert(pnext.PrevFreeBlock.Magic1 = 0); //must be free
        {$ENDIF}
        //remove from linked list
        {next.}PrevFreeBlock.NextFreeBlock := {next.}NextFreeBlock;
        if {next.}NextFreeBlock <> nil then
        begin
          {next.}NextFreeBlock.PrevFreeBlock := {next.}PrevFreeBlock;
          {$IFDEF DEBUG}
          Assert(pnext.NextFreeBlock.Magic1 = 0); //must be free
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
      {pheaderremainder.}Next := PMediumHeader(NativeUInt(pheaderremainder) + newsize);
      {recursive scanning: maybe other blocks freed in other threads etc}
      pnext := PMediumHeaderExt({pheaderremainder.}Next);
      {pheaderremainder.}Size := newsize;
    end;
  end;

  {$ifdef DEBUG}
  Assert(newsize >= 32);
  //reset all mem!
  temppointer := Pointer(NativeUInt(pheaderremainder) +
                 SizeOf(TMediumHeader)); //do no reset header
  FillChar( temppointer^,
            newsize - SizeOf(TMediumHeader){offset} - SizeOf(TMediumHeader){own header},
            $80);
  pheaderremainder.Magic1 := 0;//free
  {$ENDIF}

  with pheaderremainder^ do
  begin
    //set higest bit (mark as free)
    NativeInt({pheaderremainder.}Next) := NativeInt({pheaderremainder.}Next) or (1 shl 31);
    {pheaderremainder.}Size            := newsize;
  end;

  if newsize < (1 shl 16 shl 4) then  //smaller than 1mb?
  begin
    iRemainder := BitScanLast(                    //get highest bit
                              newsize shr 4);     //div 16 so 1mb fits in 16bit
  end
  else
    iRemainder := 16; { TODO -oAM : release mem back to Windows? }

  //set mask
  FreeMask2 := FreeMask2 or (1 shl iRemainder);

  //get first
  pnext     := FreeMem2[iRemainder];
  //replace first
  FreeMem2[iRemainder] := pheaderremainder;
  //set previous of the next
  if pnext <> nil then
  begin
    pnext.PrevFreeBlock := pheaderremainder;
    {$IFDEF DEBUG}
    assert(pnext.Magic1 = 0);
    {$ENDIF}
  end;

  with pheaderremainder^ do
  begin
    {pheaderremainder.}NextFreeBlock := pnext;
    //{pheaderremainder.}Size          := newsize;
    {pheaderremainder.}ArrayPosition := iRemainder;
    {pheaderremainder.}BlockMask     := (1 shl iRemainder);
    {pheaderremainder.}PrevFreeBlock := nil; //we are the first
  end;

  {$ifdef DEBUG} 
  except
    sleep(0);
  end;
  {$ENDIF}
end;

function TThreadManager.ReallocMem2(aMemory: Pointer; aSize: Integer): Pointer;
var
  header: PMediumHeader;
  nextfree: PMediumHeaderExt;
  //strippedpointer,
  currentsize, remaindersize,
  newsize, nextsize: NativeUInt;
begin
  {$ifdef DEBUG} try {$ENDIF}

  // ReAlloc can be misued as GetMem or FreeMem (documented in delphi help) so check what the user wants
  // Normal realloc of exisiting data?
  if (aMemory <> nil) and (aSize > 0) then
  begin
    header      := PMediumHeader(NativeUInt(aMemory) - SizeOf(TMediumHeader));
    {$IFDEF DEBUG}
    Assert(header.Magic1 = 123456789); //must be in use!
    {$ENDIF}
    newsize     := NativeUInt(aSize) + SizeOf(TMediumHeader);
    currentsize := NativeInt(header.Next) - NativeInt(header);

    //same?
    if newsize = currentsize then
    begin
      Result := aMemory;
      {$IFDEF DEBUG}
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
      remaindersize := currentsize - newsize;
      //less than 32 bytes left after resize?
      if (remaindersize < 32) {or less than div 16 change?} then
      begin
        //do nothing
        Result := aMemory;
        {$IFDEF DEBUG}
        Assert(header.Magic1 = 123456789); //must be in use!
        {$ENDIF}
        Exit;
      end
      else
      begin
        Result := aMemory;
        //resize
        header.Next := PMediumHeader(NativeUInt(header) + newsize); // + SizeOf(TMediumHeader));
        //make new block of remaining
        PutBlockToFree2(header.Next, remaindersize);

        {$IFDEF DEBUG}
        Assert(header.Magic1 = 123456789); //must be in use!
        Assert(header.Next.Magic1 = 0);    //must be free
        {$ENDIF}
      end;
    end
    //upscaling: see if we have next block with some space
    else
    begin
      newsize  := newsize + (aSize div 16);  //alloc some extra mem for small grow
                  //SizeOf(TMediumHeader);
      newsize  := (newsize + 8) shr 3 shl 3;  //8byte aligned: add 8 and remove lowest bits

      nextfree := PMediumHeaderExt(header.Next);
      //can we use some mem from next free block?
      if (nextfree <> nil) and
         //(next.Next <> nil) and  //next is not the end
         //is highest bit set? then next is a free block
         (NativeUInt(nextfree.Next) > NativeUInt(1 shl 31)) then
         //note: previous block cannot be scanned!?
      begin
        nextsize := nextfree.Size + currentsize;   //next size

        {$IFDEF DEBUG}
        Assert(nextfree.Magic1 = 0); //must be free
        {$ENDIF}

        //large enough?
        if nextsize > newsize then
        begin
          Result := aMemory;

          remaindersize := nextsize - newsize;
          //too small remainder?
          if remaindersize < 32 then
          begin
            newsize := newsize + remaindersize;
            remaindersize := 0;
          end;
          //resize
          header.Next := PMediumHeader(NativeUInt(header) + newsize);
          //todo: replace with size

          { TODO -oAM : use same "alloc from free block" as GetMem }

          //remove old size
          if nextfree.PrevFreeBlock = nil then {first?}
          begin
            //replace first item with nextfree
            FreeMem2[nextfree.ArrayPosition] := nextfree.NextFreeBlock;
            //reset bit if nothing free anymore
            if nextfree.NextFreeBlock {FreeMem2[nextfree.ArrayPosition]} = nil then
              FreeMask2 := FreeMask2 xor nextfree.BlockMask
            else
            begin
              nextfree.NextFreeBlock.PrevFreeBlock := nil;
              {$IFDEF DEBUG}
              Assert(nextfree.NextFreeBlock.Magic1 = 0); //must be free
              {$ENDIF}
            end;
          end
          else
          begin
            {$IFDEF DEBUG}
            Assert(nextfree.PrevFreeBlock.Magic1 = 0); //must be free
            {$ENDIF}
            //remove from linked list
            nextfree.PrevFreeBlock.NextFreeBlock := nextfree.NextFreeBlock;
            if nextfree.NextFreeBlock <> nil then
            begin
              nextfree.NextFreeBlock.PrevFreeBlock := nextfree.PrevFreeBlock;
              {$IFDEF DEBUG}
              Assert(nextfree.NextFreeBlock.Magic1 = 0); //must be free
              {$ENDIF}
            end;
          end;

          //check
          {$IFDEF DEBUG}
          Assert( (nextfree.NextFreeBlock = nil) or
                  ((nextfree.NextFreeBlock.ArrayPosition = nextfree.ArrayPosition) and
                   (nextfree.NextFreeBlock.Magic1 = 0) {must be free})
                );
          {$ENDIF}

          //make new block of remaining
          if remaindersize > 0 then
            PutBlockToFree2(header.Next, remaindersize);

          {$IFDEF DEBUG}
          Assert(header.Magic1 = 123456789); //must be in use!
          if remaindersize > 0 then
            Assert(header.Next.Magic1 = 0);    //must be free
          {$ENDIF}
        end
        else
        begin
          //alloc new mem and copy data
          Result := Self.GetMem2(newsize);
          if aMemory <> Result then
          begin
            Move(aMemory^, Result^, currentsize); // copy (use smaller old size)
            FreeMem_2(aMemory); // free old mem
          end;
        end;
      end
      else
      //alloc new mem and copy data
      begin
        Result := Self.GetMem2(newsize);
        if aMemory <> Result then
        begin
          Move(aMemory^, Result^, currentsize); // copy (use smaller old size)
          FreeMem_2(aMemory); // free old mem
        end;
      end;
    end;
  end
  else
  begin
    if (aMemory = nil) and (aSize > 0) then
      // GetMem disguised as ReAlloc
      Result := Self.GetMem2(aSize)
    else
    begin
      //FreeMem disguised as ReAlloc
      Result := nil;
      Self.FreeMem_2(aMemory);
    end;
  end;

  {$ifdef DEBUG} 
  except
    sleep(0);
  end;
  {$ENDIF}
end;

procedure MemTest;
var p1, p2, p3: pointer;
  i: NativeInt;
begin
  i := 1 shl 4;
  i := 1 shl 16;
  i := i shl 4;

  p1 := GThreadManager.GetMem2(100);
  p2 := GThreadManager.GetMem2(100);
  p3 := GThreadManager.GetMem2(100);
  GThreadManager.FreeMem_2(p2);
  p2 := GThreadManager.GetMem2(100);
  p2 := GThreadManager.ReallocMem2(p2, 150);
  p2 := GThreadManager.ReallocMem2(p2, 50);
  p2 := GThreadManager.ReallocMem2(p2, 100);

  GThreadManager.FreeMem_2(p3);
  GThreadManager.FreeMem_2(p1);
end;

initialization
  MemTest;

end.
