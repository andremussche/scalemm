unit ScaleMM2;

interface

type
  PMediumHeader    = ^TMediumHeader;
  PMediumHeaderExt = ^TMediumHeaderExt;
  PBlockMemory     = ^TBlockMemory;
  PThreadManager   = ^TThreadManager;

  TBlockMemory = record
    OwnerThread: PThreadManager;
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
    Next  : PMediumHeader; //size
    Magic1: NativeInt;
  end;
  TMediumHeaderExt = record
    Block        : PBlockMemory;
    Next         : PMediumHeader; //size
    Magic1       : NativeInt;
    //todo: plain size
    NextFreeBlock: PMediumHeaderExt;
    PrevFreeBlock: PMediumHeaderExt;
    BlockMask    : NativeInt;
    ArrayPosition: NativeUInt;
    CRC: NativeUInt;
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
  pheader : PMediumHeaderExt;
begin
  //alloc
  pheader := VirtualAlloc( nil,
                           (1 shl 16 shl 4) {1mb} +
                           SizeOf(TMediumHeader) {extra beginheader} +
                           SizeOf(TMediumHeader) {extra endheader},
                           MEM_COMMIT {$ifdef AlwaysAllocateTopDown} or MEM_TOP_DOWN{$endif},
                           PAGE_READWRITE);
//  pheader.Block         := nil;
  pheader.Next          := PMediumHeader( NativeUInt(pheader) + (1 shl 16 shl 4) + SizeOf(TMediumHeader){begin} );
  //reset end
  pheader.Next.Block    := nil;
  pheader.Next.Next     := nil;
  pheader.Next.Magic1   := 0;
  //mark as free
  NativeInt(pheader.Next) := NativeInt(pheader.Next) or (1 shl 31);
  pheader.Magic1        := 0;
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
  header, next: PMediumHeader;
begin
  Result  := 0;
  header  := PMediumHeader(NativeUInt(aMemory) - SizeOf(TMediumHeader));

  Assert(header.Magic1 = 123456789); //must be in use!

  //put free item to block + allmem array + mask
  PutBlockToFree2(header, NativeInt(header.Next) - NativeInt(header));

  Assert(header.Magic1 = 0); //must be free
end;

function TThreadManager.GetMem2(aSize: NativeInt): Pointer;
var
  iWordSize: Word;
  iWordRemainderSize: Word;
  iMSB: NativeUInt;

  i, iStart, iFreeMemIndex,
  iSubblock, iRemainder: NativeInt;
  remaindersize, allocsize,
  tempsize: NativeInt;
  tempmask: NativeInt;
  pheader : PMediumHeaderExt;
  pheaderremainder : PMediumHeaderExt;
begin
  {$ifdef DEBUG} try {$ENDIF}

  Result := nil;
  if aSize <= 0 then Exit;
  if aSize >= (1 shl 16 shl 4) then  //bigger than 1mb
  begin
    Result := nil; //GetMem(aSize);   todo
    Exit;
  end;

  allocsize     := aSize + (aSize div 16) +       //alloc some extra mem (6%) for small grow
                   SizeOf(TMediumHeader);         //alloc extra for header
  allocsize     := (allocsize + 8) shr 3 shl 3;   //8byte aligned: add 8 and remove lowest bits
  iWordSize     := allocsize shr 4;               //div 16 so 1mb fits in 16bit
  iMSB          := BitScanLast(iWordSize) + 1;    //get highest bit (+1 for max size)
  //todo: first without +1 and check if size is OK (otherwise alloc of same size after alloc + free will fail)

  tempmask      := FreeMask2 shr iMSB shl iMSB;   //reset lowest bits (shr + shl)

  //alloc new mem? (for biggest block)
  if tempmask = 0 then
  begin
    pheader       := AllocBlock2;
    iFreeMemIndex := 16;
  end
  else
  //get available mem
  begin
    iFreeMemIndex := BitScanFirst(tempmask);        //get lowest bit (smallest size)
    Assert(iFreeMemIndex >= 0);
    pheader       := FreeMem2[iFreeMemIndex];
    Assert( pheader <> nil);
  end;

  Assert( Cardinal(pheader.Next) <> $80808080);
  //check allocated size
  Assert( (1 shl iFreeMemIndex shl 4) > allocsize );
  Assert( NativeInt(pheader.Next) xor (1 shl 31) //remove higest bit (free mark)
          - NativeInt(pheader) > allocsize );
  Result    := pheader;

  Assert(pheader.Magic1 = 0);  //not in use!
  Assert( NativeUInt(pheader.Next) > NativeUInt(1 shl 31) ); //higest bit (free mark)
  Assert(pheader.PrevFreeBlock = nil);  //must be first one
  pheader.Magic1 := 123456789; //mark in use

  //remainder
  remaindersize := NativeInt(pheader.Next) xor (1 shl 31) //remove higest bit (free mark)
                   - NativeInt(pheader) - allocsize;
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
  assert(allocsize >= aSize);

  //same block left?
  if iFreeMemIndex = iRemainder then
  begin
    pheaderremainder               := PMediumHeaderExt(NativeUInt(pheader) + allocsize);
    pheaderremainder.Block         := pheader.Block;
    pheaderremainder.Next          := pheader.Next;
    //pheaderremainder.Magic1        := pheader.Magic1;
    pheaderremainder.NextFreeBlock := pheader.NextFreeBlock;
    pheaderremainder.PrevFreeBlock := pheader.PrevFreeBlock;
    pheaderremainder.BlockMask     := pheader.BlockMask;
    pheaderremainder.ArrayPosition := pheader.ArrayPosition;
    Assert(iFreeMemIndex = pheader.ArrayPosition);

    //keep same block as free
    //only change to new offset
    FreeMem2[iFreeMemIndex] := pheaderremainder;
    //relink next one to our new offset
    if pheader.NextFreeBlock <> nil then
      pheader.NextFreeBlock.PrevFreeBlock := pheaderremainder;

    pheaderremainder.Magic1 := 0; //mark as free

    //next = alloc
    pheader.Next := PMediumHeader(pheaderremainder);
  end
  else
  //if remaindersize > or < pblock.StepSize then
  {put remainder free block in AllMem array + mask}
  begin
    //alloc size
    pheader.Next := PMediumHeader(NativeUInt(pheader) + allocsize);

    //replace with next free block
    FreeMem2[iFreeMemIndex] := pheader.NextFreeBlock;
    Assert( (pheader.NextFreeBlock = nil) or
            ((pheader.NextFreeBlock.ArrayPosition = iFreeMemIndex) and
             (pheader.NextFreeBlock.Magic1 = 0) {must be free})
          );

     //reset bit if nothing free anymore
//    if FreeMem2[iFreeMemIndex] = nil then
//      FreeMask2 := FreeMask2 xor (1 shl iFreeMemIndex);
    if pheader.NextFreeBlock {FreeMem2[iFreeMemIndex]} = nil then
      FreeMask2 := FreeMask2 xor pheader.BlockMask
    else
    begin
      Assert(pheader.NextFreeBlock.Magic1 = 0);
      pheader.NextFreeBlock.PrevFreeBlock := nil;
    end;

    //create remainder
    if remaindersize > 0 then
    begin
      PutBlockToFree2(pheader.Next, remaindersize);
      Assert(pheader.Next.Magic1 = 0); //must be free
    end;
  end;

  Assert(pmediumheader(Result).Magic1 = 123456789); //must be in use!
  Result := Pointer(NativeUInt(Result) + SizeOf(TMediumHeader));

  {$ifdef DEBUG} 
  except
    sleep(0);
  end;
  {$ENDIF}
end;

procedure TThreadManager.PutBlockToFree2(aHeader: PMediumHeader;
  aSize: NativeUInt);
var
  next,
  pheaderremainder: PMediumHeaderExt;
  i: Integer;
//  pblock: PBlockMemory;
  newsize, tempsize: NativeUInt;
  strippedpointer : NativeUInt;
  temppointer: Pointer;
var
  iRemainder: NativeInt;
  iWordRemainderSize: Word;
begin
  {$ifdef DEBUG} try {$ENDIF}

  if aSize <= 0 then Exit;

  //create remainder
  pheaderremainder       := PMediumHeaderExt(aHeader);
  pheaderremainder.Next  := PMediumHeader(NativeUInt(pheaderremainder) + aSize);

  next := PMediumHeaderExt(pheaderremainder.Next);
  //can we use some mem from next free block?
  if (next <> nil) and
     //(next.Next <> nil) and  //next is not the end
     //is highest bit set? then next is a free block
     (NativeUInt(next.Next) > NativeUInt(1 shl 31)) then
     //note: previous block cannot be scanned!?
  begin
    Assert( Cardinal(next.Next) <> $80808080);
    //remove highest bit to get real "next" pointer
    strippedpointer := (NativeInt(next.Next) xor (1 shl 31));
    //make one block
    newsize := aSize + //NativeInt(pheaderremainder.Next) - NativeInt(pheaderremainder) +   //this size
               strippedpointer - NativeUInt(next);      //next size

    Assert(next.Magic1 = 0); //must be free

    //remove old size
    if next.PrevFreeBlock = nil then {first?}
    begin
      //replace first item with next
      FreeMem2[next.ArrayPosition] := next.NextFreeBlock;
      if next.NextFreeBlock {FreeMem2[next.ArrayPosition]} = nil then
        FreeMask2 := FreeMask2 xor next.BlockMask
      else
      begin
        next.NextFreeBlock.PrevFreeBlock := nil;
        Assert(next.NextFreeBlock.Magic1 = 0); //must be free
      end;
    end
    else
    begin
      Assert(next.PrevFreeBlock.Magic1 = 0); //must be free
      //remove from linked list
      next.PrevFreeBlock.NextFreeBlock := next.NextFreeBlock;
      if next.NextFreeBlock <> nil then
      begin
        next.NextFreeBlock.PrevFreeBlock := next.PrevFreeBlock;
        Assert(next.NextFreeBlock.Magic1 = 0); //must be free
      end;
    end;

    //alloc/resize
    pheaderremainder.Next := PMediumHeader(NativeUInt(pheaderremainder) + newsize);

    { TODO : recursive scanning: maybe other blocks freed in other threads etc }
  end
  else
    newsize := aSize;

  Assert(newsize >= 32);
  //reset all mem!
  temppointer := Pointer(NativeUInt(pheaderremainder) + SizeOf(TMediumHeader));
//  FillChar( temppointer^,
//            newsize - SizeOf(TMediumHeader){offset} - SizeOf(TMediumHeader){own header},
//            $80);

  //set higest bit (mark as free)
  NativeInt(pheaderremainder.Next) := NativeInt(pheaderremainder.Next) or (1 shl 31);

  pheaderremainder.Magic1 := 0;//free

  if newsize < (1 shl 16 shl 4) then  //smaller than 1mb
  begin
    iWordRemainderSize := newsize shr 4;     //div 16 so 1mb fits in 16bit
    iRemainder         := BitScanLast(iWordRemainderSize);  //get highest bit
  end
  else
    iRemainder := 16;

  pheaderremainder.ArrayPosition := iRemainder;
  pheaderremainder.BlockMask     := (1 shl iRemainder);
  //then set mask
  FreeMask2   := FreeMask2 or pheaderremainder.BlockMask;
  //set next
  pheaderremainder.NextFreeBlock := FreeMem2[iRemainder];
  //set previous of the next
  if pheaderremainder.NextFreeBlock <> nil then
  begin
    pheaderremainder.NextFreeBlock.PrevFreeBlock := pheaderremainder;
    assert(pheaderremainder.NextFreeBlock.Magic1 = 0);
  end;
  //replace first
  FreeMem2[iRemainder] := pheaderremainder;
  pheaderremainder.PrevFreeBlock := nil; //we are the first

  {$ifdef DEBUG} 
  except
    sleep(0);
  end;
  {$ENDIF}
end;

function TThreadManager.ReallocMem2(aMemory: Pointer; aSize: Integer): Pointer;
var
  header, next: PMediumHeader;
  nextfree: PMediumHeaderExt;
  strippedpointer,
  currentsize, remaindersize,
  newsize, nextsize: NativeInt;
begin
  {$ifdef DEBUG} try {$ENDIF}

  // ReAlloc can be misued as GetMem or FreeMem (documented in delphi help) so check what the user wants
  // Normal realloc of exisiting data?
  if (aMemory <> nil) and (aSize > 0) then
  begin
    header      := PMediumHeader(NativeUInt(aMemory) - SizeOf(TMediumHeader));
    Assert(header.Magic1 = 123456789); //must be in use!
    currentsize := NativeInt(header.Next) - NativeInt(header) - SizeOf(TMediumHeader);

    //same?
    if aSize = currentsize then
    begin
      Result := aMemory;

      Assert(header.Magic1 = 123456789); //must be in use!

      Exit
    end
    //downscaling?
    else if aSize <= currentsize then
    begin
      newsize       := aSize + (aSize div 16) +    //alloc some extra mem for small grow
                       SizeOf(TMediumHeader);
      newsize       := (newsize + 8) shr 3 shl 3;  //8byte aligned: add 8 and remove lowest bits
      remaindersize := currentsize + SizeOf(TMediumHeader) - newsize;
      //less than 32 bytes left after resize?
      if (remaindersize < 32) {or less than div 16 change?} then
      begin
        //do nothing
        Result := aMemory;

        Assert(header.Magic1 = 123456789); //must be in use!

        Exit;
      end
      else
      begin
        Result := aMemory;
        //resize
        header.Next := PMediumHeader(NativeUInt(header) + newsize); // + SizeOf(TMediumHeader));
        //make new block of remaining
        PutBlockToFree2(header.Next, remaindersize);

        Assert(header.Magic1 = 123456789); //must be in use!
        Assert(header.Next.Magic1 = 0);    //must be free
      end;
    end
    //upscaling: see if we have next block with some space
    else
    begin
      newsize := aSize + (aSize div 16) +  //alloc some extra mem for small grow
                 SizeOf(TMediumHeader);
      newsize := (newsize + 8) shr 3 shl 3;  //8byte aligned: add 8 and remove lowest bits

      next    := header.Next;
      //can we use some mem from next free block?
      if (next <> nil) and
         //(next.Next <> nil) and  //next is not the end
         //is highest bit set? then next is a free block
         (NativeUInt(next.Next) > NativeUInt(1 shl 31)) then
         //note: previous block cannot be scanned!?
      begin
        //remove highest bit to get real "next" pointer
        strippedpointer := (NativeInt(next.Next) xor (1 shl 31));
        nextsize := currentsize     + SizeOf(TMediumHeader) - //this size
                    strippedpointer - NativeInt(next);        //next size

        Assert(next.Magic1 = 0); //must be free

        //large enough?
        if nextsize > newsize then
        begin
          Result := aMemory;

          remaindersize := nextsize - newsize;
          if remaindersize < 32 then
          begin
            newsize := newsize + remaindersize;
            remaindersize := 0;
          end;
          
          //resize
          header.Next := PMediumHeader(NativeUInt(header) + newsize); // + SizeOf(TMediumHeader));

          nextfree := PMediumHeaderExt(next);
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
              Assert(nextfree.NextFreeBlock.Magic1 = 0); //must be free
            end;
          end
          else
          begin
            Assert(nextfree.PrevFreeBlock.Magic1 = 0); //must be free
            //remove from linked list
            nextfree.PrevFreeBlock.NextFreeBlock := nextfree.NextFreeBlock;
            if nextfree.NextFreeBlock <> nil then
            begin
              nextfree.NextFreeBlock.PrevFreeBlock := nextfree.PrevFreeBlock;
              Assert(nextfree.NextFreeBlock.Magic1 = 0); //must be free
            end;
          end;

          //check
          Assert( (nextfree.NextFreeBlock = nil) or
                  ((nextfree.NextFreeBlock.ArrayPosition = nextfree.ArrayPosition) and
                   (nextfree.NextFreeBlock.Magic1 = 0) {must be free})
                );

          //make new block of remaining
          if remaindersize > 0 then
            PutBlockToFree2(header.Next, remaindersize);
                
          Assert(header.Magic1 = 123456789); //must be in use!
          Assert(header.Next.Magic1 = 0);    //must be free
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
