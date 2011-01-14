unit ScaleMM2;

//known "bugs":
// - virtual mem is allocated on 1mb boundary by Windows, so we need to alloc
// little bit less than 2mb instead of little bit more than 1mb
// (999kb between blocks -> never used -> fragmentation)
// - use TMediumHeader.Size instead of .Next (because we use MSB of .Next we
// cannot use mem above 2Gb)
// - Check for minimum free mem size when reusing previous blocks of GlobalManager

interface

{$IFDEF DEBUG}
  {.$DEFINE FILLFREEMEM}
{$ENDIF}

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
    FFreeMem  : array[0..16] of PMediumHeaderExt;
    FFreeMask : NativeUInt; //word, 16 bit, 65535

    FFirstBlock: PBlockMemory;

    function  AllocBlock(aMinResultSize: NativeUInt): PMediumHeaderExt;
    procedure FreeBlock(aBlock: PBlockMemory);
    function  ScanBlockForFreeItems(aBlock: PBlockMemory; aMinResultSize: NativeUInt): PMediumHeaderExt;

    procedure PutMemToFree(aHeader: PMediumHeader; aSize: NativeUInt);
    procedure FreeMemOtherThread(aHeader: PMediumHeader);

    function  GetBigMem(aSize: NativeInt): Pointer;
  public
    function GetMem(aSize: NativeInt) : Pointer;    //{$ifdef HASINLINE}inline;{$ENDIF}
    function FreeMem(aMemory: Pointer): NativeInt;  //{$ifdef HASINLINE}inline;{$ENDIF}
    function ReallocMem(aMemory: Pointer; aSize: Integer): Pointer;
  end;

  /// Global memory manager
  // - a single instance is created for the whole process
  // - caches some memory (blocks + threadmem) for fast reuse
  // - also keeps allocated memory in case an old thread allocated some memory
  // for another thread
  TGlobalMemManager = object
  private
    /// main thread manager (owner of all global mem)
    FMainThreadMemory: PThreadManager;

    FFirstBlock: PBlockMemory;
    FLock: NativeUInt;
    FFreeBlockCount: NativeUInt;

    procedure Init;
    procedure FreeBlocksFromThreadMemory(aThreadMem: PThreadManager);
  public
    procedure FreeAllMemory;

    procedure FreeBlockMemory(aBlockMem: PBlockMemory);
    function  GetBlockMemory: PBlockMemory;
  end;

  {
  //4 bytes
  TSmallHeader = record
    offset2block: Word;
    offset2next : Word;  //size
  end;
  }

  //16 bytes
  TMediumHeader = record
    Block     : PBlockMemory;
    TodoSize  : NativeUInt;    //todo: use size
    Next      : PMediumHeader;
    TodoPrev  : PMediumHeader; //todo: use prev for backwards scanning
    {$IFDEF DEBUG}
    Magic1    : NativeInt;
    {$ENDIF}
  end;
  TMediumHeaderExt = record
    Block     : PBlockMemory;
    TodoSize  : NativeUInt;    //todo: use size
    Next      : PMediumHeader;
    TodoPrev  : PMediumHeader;
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

  {
  //16 bytes
  TLargeHeader = record
    //Block: Pointer;
    Next : Pointer;  //size
    AllMemIndex: NativeUInt;
    Mask: NativeUInt;
  end;
  }

//threadvar
//  GThreadManager: TThreadManager;
var
  GGlobalManager: TGlobalMemManager;
(*
{$ifdef USEMEMMANAGEREX}
  OldMM: TMemoryManagerEx;
{$else}
  OldMM: TMemoryManager;
{$endif}
*)

implementation

////////////////////////////////////////////////////////////////////////////////
//Inline Windows functions

//uses
// Windows.pas unit dependency should be not used -> code inlined here

type
  DWORD = LongWord;
  BOOL  = LongBool;
const
  PAGE_EXECUTE_READWRITE = $40;
  kernel32  = 'kernel32.dll';
  MEM_COMMIT = $1000;
  PAGE_READWRITE = 4;
  MEM_RELEASE = $8000;

function  TlsAlloc: DWORD; stdcall; external kernel32 name 'TlsAlloc';
function  TlsGetValue(dwTlsIndex: DWORD): Pointer; stdcall; external kernel32 name 'TlsGetValue';
function  TlsSetValue(dwTlsIndex: DWORD; lpTlsValue: Pointer): BOOL; stdcall; external kernel32 name 'TlsSetValue';
function  TlsFree(dwTlsIndex: DWORD): BOOL; stdcall; external kernel32 name 'TlsFree';
procedure Sleep(dwMilliseconds: DWORD); stdcall; external kernel32 name 'Sleep';
function  SwitchToThread: BOOL; stdcall; external kernel32 name 'SwitchToThread';
function  FlushInstructionCache(hProcess: THandle; const lpBaseAddress: Pointer; dwSize: DWORD): BOOL; stdcall; external kernel32 name 'FlushInstructionCache';
function  GetCurrentProcess: THandle; stdcall; external kernel32 name 'GetCurrentProcess';
function  GetCurrentThreadId: DWORD; stdcall; external kernel32 name 'GetCurrentThreadId';
function  Scale_VirtualProtect(lpAddress: Pointer; dwSize, flNewProtect: DWORD;
            var OldProtect: DWORD): BOOL; stdcall; overload; external kernel32 name 'VirtualProtect';
procedure ExitThread(dwExitCode: DWORD); stdcall; external kernel32 name 'ExitThread';
function VirtualAlloc(lpvAddress: Pointer; dwSize, flAllocationType, flProtect: DWORD): Pointer; stdcall; external kernel32 name 'VirtualAlloc';
function VirtualFree(lpAddress: Pointer; dwSize, dwFreeType: DWORD): BOOL; stdcall; external kernel32 name 'VirtualFree';

function SetPermission(Code: Pointer; Size, Permission: Cardinal): Cardinal;
begin
  Assert(Assigned(Code) and (Size > 0));
  { Flush the instruction cache so changes to the code page are effective immediately }
  if Permission <> 0 then
    if FlushInstructionCache(GetCurrentProcess, Code, Size) then
      Scale_VirtualProtect(Code, Size, Permission, Longword(Result));
end;

(*
procedure NewEndThread(ExitCode: Integer); //register; // ensure that calling convension matches EndThread
begin
  // free all thread mem
  GGlobalManager.FreeBlocksFromThreadMemory(@GThreadManager);
  // OldEndThread(ExitCode);  todo: make trampoline with original begin etc
  // code of original EndThread;
  ExitThread(ExitCode);
end;

type
  PJump = ^TJump;
  TJump = packed record
    OpCode: Byte;
    Distance: Integer;
  end;
  TEndThread = procedure(ExitCode: Integer);
var
  NewCode: TJump = (OpCode  : $E9;
                    Distance: 0);
  OldEndThread: TEndThread;

// redirect calls to System.EndThread to NewEndThread
procedure PatchThread;
var
  pEndThreadAddr: PJump;
  iOldProtect: DWord;
begin
  pEndThreadAddr := Pointer(@EndThread);
  Scale_VirtualProtect(pEndThreadAddr, 5, PAGE_EXECUTE_READWRITE, iOldProtect);
  // calc jump to new function
  NewCode.Distance := Cardinal(@NewEndThread) - (Cardinal(@EndThread) + 5);
  // store old
  OldEndThread := TEndThread(pEndThreadAddr);
  // overwrite with jump to new function
  pEndThreadAddr^  := NewCode;
  // flush CPU
  FlushInstructionCache(GetCurrentProcess, pEndThreadAddr, 5);
end;
*)

////////////////////////////////////////////////////////////////////////////////
//Assembly functions

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

//compare oldvalue with destination: if equal then newvalue is set
function CAS32(const oldValue: NativeUInt; newValue: NativeUInt; var destination): boolean;
asm // eax=oldValue, edx=newValue, ecx=Destination
  lock cmpxchg dword ptr [Destination],newValue
  setz  al
{$ifdef SPINWAITBACKOFF}
  jz @ok
  pause // let the CPU know this thread is in a Spin Wait loop
@ok:
{$endif}
end;

////////////////////////////////////////////////////////////////////////////////

{ TThreadManager }

function TThreadManager.AllocBlock(aMinResultSize: NativeUInt): PMediumHeaderExt;
var
  iAllocSize: NativeUInt;
  pheader: PMediumHeaderExt;
  pblock : PBlockMemory;
begin
  pblock := GGlobalManager.GetBlockMemory;
  if pblock <> nil then
  repeat
    pheader := ScanBlockForFreeItems(pblock, aMinResultSize);
    //no mem of minimum size?
    if pheader = nil then
      pblock  := GGlobalManager.GetBlockMemory;
  until (pheader <> nil) or (pblock = nil);

  if pblock = nil then
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
    if pblock = nil then
    begin
      Result := nil;
      Exit;
    end;
    pblock.Size           := iAllocSize;

    //first item
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
    FFreeMask    := FFreeMask or (1 shl 16);
    FFreeMem[16] := pheader;
  end;
  assert(pheader <> nil);

  pblock.OwnerThread    := @Self;

  //linked list of thread blocks (replace first)
  if FFirstBlock <> nil then
    FFirstBlock.PreviousBlock := pblock;
  pblock.NextBlock            := FFirstBlock;
  FFirstBlock                 := pblock;
  pblock.PreviousBlock        := nil;

  Result := pheader;
end;

procedure TThreadManager.FreeMemOtherThread(aHeader: PMediumHeader);
var
  headerext: PMediumHeaderExt;
  iRemainder: NativeInt;
begin
  headerext := PMediumHeaderExt(aHeader);
  with headerext^ do
  begin
    {headerext.}Size          := NativeUInt({headerext.}Next) - NativeUInt(headerext);
    {headerext.}NextFreeBlock := nil;
    {headerext.}PrevFreeBlock := nil;
  end;

  if headerext.Size < (1 shl 16 shl 4) then  //smaller than 1mb?
  begin
    iRemainder := BitScanLast(                        //get highest bit
                              headerext.Size shr 4);  //div 16 so 1mb fits in 16bit
  end
  else
    iRemainder := 16; { TODO -oAM : release mem back to Windows? }

  with headerext^ do
  begin
    {headerext.}BlockMask     :=  (1 shl iRemainder);
    {headerext.}ArrayPosition := iRemainder;

    //as last: set higest bit (mark as free)
    //now owner thread can use it when it scans for marked mem
    NativeInt({headerext.}Next) := NativeInt({headerext.}Next) or (1 shl 31);
  end;
end;

procedure TThreadManager.FreeBlock(aBlock: PBlockMemory);
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

  GGlobalManager.FreeBlockMemory(aBlock);
end;

function TThreadManager.FreeMem(aMemory: Pointer): NativeInt;
var
  header: PMediumHeader;
begin
  header  := PMediumHeader(NativeUInt(aMemory) - SizeOf(TMediumHeader));

  {$IFDEF DEBUG}
  Assert(header.Magic1 = 123456789); //must be in use!
  {$ENDIF}

  //not large mem?
  if header.Block <> nil then
  begin
    if header.Block.OwnerThread = @Self then
      //put free item to block + allmem array + mask
      PutMemToFree(header, NativeUInt(header.Next) - NativeUInt(header))
    else
      FreeMemOtherThread(header);
    Result := 0;

    {$IFDEF DEBUG}
    Assert(header.Magic1 = 0); //must be free
    {$ENDIF}
  end
  else
  begin
    Result  := 0;
    if not VirtualFree(header, 0, MEM_RELEASE) then
      Result := 1;
  end;
end;

function TThreadManager.GetBigMem(aSize: NativeInt): Pointer;
var
  pheader: PMediumHeader;
begin
  //alloc
  pheader := VirtualAlloc( nil,
                          aSize + SizeOf(TMediumHeader) {extra beginheader},
                          MEM_COMMIT {$ifdef AlwaysAllocateTopDown} or MEM_TOP_DOWN{$endif},
                          PAGE_READWRITE);
  //reset begin
  pheader.Block := nil;
  pheader.Next  := nil;

  {$ifdef DEBUG}
  pheader.Magic1 := 123456789;
  Assert(pmediumheader(pheader).Magic1 = 123456789); //must be in use!
  {$ENDIF}

  Result := Pointer(NativeUInt(pheader) + SizeOf(TMediumHeader));
end;

function TThreadManager.GetMem(aSize: NativeInt): Pointer;
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
  {$ifdef DEBUG} Result := nil; try {$ENDIF}

  if aSize <= 0 then  //zero or less?
  begin
    Result := nil;
    Exit;
  end
  else if aSize >= (1 shl 16 shl 4) then  //bigger than 1mb
  begin
    Result := GetBigMem(aSize);
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
  pheader       := FFreeMem[iMSB];
  if (pheader <> nil) and (pheader.Size >= allocsize)  then
  begin
    iFreeMemIndex   := iMSB;
  end
  else
  begin
    inc(iMSB);  //+1 for max size
    tempmask        := FFreeMask shr iMSB shl iMSB;   //reset lowest bits (shr + shl)

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
      if pheader = nil then  //out of memory?
      begin
        Result := nil;
        Exit;
      end;
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
    pheaderremainder := PMediumHeaderExt(NativeUInt(pheader) + allocsize);
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
    FFreeMem[iFreeMemIndex] := pheaderremainder;

    with pheader^ do
    begin
      //next = alloc
      {pheader.}Next := PMediumHeader(pheaderremainder);
      //{pheader.}Size := allocsize; todo
      Assert(pheader.Block <> nil);
      Assert(pheader.Next.Block <> nil);

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
    //replace with next free block
    FFreeMem[iFreeMemIndex] := {pheader.}NextFreeBlock;
    {$IFDEF DEBUG}
    Assert( (pheader.NextFreeBlock = nil) or
            ((pheader.NextFreeBlock.ArrayPosition = iFreeMemIndex) and
             (pheader.NextFreeBlock.Magic1 = 0) {must be free})
          );
    {$ENDIF}

    //reset bit if nothing free anymore
    if {pheader.}NextFreeBlock {FreeMem2[iFreeMemIndex]} = nil then
      FFreeMask := FFreeMask xor {pheader.}BlockMask
    else
    begin
      {$IFDEF DEBUG}
      Assert(pheader.NextFreeBlock.Magic1 = 0);
      {$ENDIF}
      pheader.NextFreeBlock.PrevFreeBlock := nil;
    end;

    //alloc size
    {pheader.}Next       := PMediumHeader(NativeUInt(pheader) + allocsize);
    //pheader.Size       := allocsize; todo
    //set block of next item too
    {pheader.}Next.Block := {pheader.}Block;

    //create remainder
    if remaindersize > 0 then
    begin
      PutMemToFree({pheader.}Next, remaindersize);
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

procedure TThreadManager.PutMemToFree(aHeader: PMediumHeader;
  aSize: NativeUInt);
var
  pnext,
  pheaderremainder: PMediumHeaderExt;
  //i: Integer;
//  pblock: PBlockMemory;
  newsize: NativeUInt;
  //strippedpointer : NativeUInt;
  {$IFDEF FILLFREEMEM}
  temppointer: Pointer;
  {$ENDIF}
  iRemainder: NativeInt;
  //iWordRemainderSize: Word;
begin
  {$ifdef DEBUG} try {$ENDIF}

  if aSize <= 0 then Exit;
  newsize := aSize;

  //create remainder
  pheaderremainder := PMediumHeaderExt(aHeader);
  //next
  pnext            := PMediumHeaderExt(NativeUInt(pheaderremainder) + newsize);
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
        FFreeMem[{next.}ArrayPosition] := {next.}NextFreeBlock;
        if {next.}NextFreeBlock = nil then
          FFreeMask := FFreeMask xor {next.}BlockMask
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
  {$IFDEF FILLFREEMEM}
  //reset all mem!
  temppointer := Pointer(NativeUInt(pheaderremainder) +
                 SizeOf(TMediumHeader)); //do no reset header
  FillChar( temppointer^,
            newsize - SizeOf(TMediumHeader){offset} - SizeOf(TMediumHeader){own header},
            $80);
  {$ENDIF}
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
  begin
    iRemainder := 16;
    //block complete free: release mem back to Windows? {or global manager}
    if FFreeMem[iRemainder] <> nil then
    begin
      FreeBlock(pheaderremainder.Block);
      Exit;
    end;
  end;

  //set mask
  FFreeMask := FFreeMask or (1 shl iRemainder);

  //get first
  pnext     := FFreeMem[iRemainder];
  //replace first
  FFreeMem[iRemainder] := pheaderremainder;
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

  {$ifdef DEBUG} except sleep(0); end; {$ENDIF}
end;

function TThreadManager.ReallocMem(aMemory: Pointer; aSize: Integer): Pointer;
var
  header: PMediumHeader;
  nextfree: PMediumHeaderExt;
  //strippedpointer,
  remaindersize: NativeInt;
  currentsize,
  newsize, nextsize: NativeUInt;
begin
  {$ifdef DEBUG} Result := nil; try {$ENDIF}

  // ReAlloc can be misued as GetMem or FreeMem (documented in delphi help) so check what the user wants
  // Normal realloc of exisiting data?
//  if (aMemory <> nil) and (aSize > 0) then
//  begin
    header      := PMediumHeader(NativeUInt(aMemory) - SizeOf(TMediumHeader));
    newsize     := NativeUInt(aSize) + SizeOf(TMediumHeader);

    //todo: use size etc with smart up/downsize
    //large mem?
    if header.Block = nil then
    begin
      Result := Self.GetMem(newsize);
      if aMemory <> Result then
      begin
        Move(aMemory^, Result^, currentsize); // copy (use smaller old size)
        FreeMem(aMemory); // free old mem
      end;
      Exit;
    end;

    {$IFDEF DEBUG}
    Assert(header.Magic1 = 123456789); //must be in use!
    {$ENDIF}
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
      remaindersize := NativeInt(currentsize) - NativeInt(newsize);
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
        header.Next       := PMediumHeader(NativeUInt(header) + newsize); // + SizeOf(TMediumHeader));
        header.Next.Block := header.Block;
        //make new block of remaining
        PutMemToFree(header.Next, remaindersize);

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

          { TODO -oAM : use same "alloc from free block" as GetMem }

          //remove old size
          if nextfree.PrevFreeBlock = nil then {first?}
          begin
            //replace first item with nextfree
            FFreeMem[nextfree.ArrayPosition] := nextfree.NextFreeBlock;
            //reset bit if nothing free anymore
            if nextfree.NextFreeBlock {FreeMem2[nextfree.ArrayPosition]} = nil then
              FFreeMask := FFreeMask xor nextfree.BlockMask
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

          remaindersize := nextsize - newsize;
          //too small remainder?
          if remaindersize < 32 then
          begin
            newsize := newsize + remaindersize;
            remaindersize := 0;
          end;

          //resize
          header.Next       := PMediumHeader(NativeUInt(header) + newsize);
          header.Next.Block := header.Block;
          //todo: replace with size

          //make new block of remaining
          if remaindersize > 0 then
            PutMemToFree(header.Next, remaindersize);

          {$IFDEF DEBUG}
          Assert(header.Magic1 = 123456789); //must be in use!
          if remaindersize > 0 then
            Assert(header.Next.Magic1 = 0);    //must be free
          {$ENDIF}
        end
        else
        begin
          //alloc new mem and copy data
          Result := Self.GetMem(newsize);
          if aMemory <> Result then
          begin
            Move(aMemory^, Result^, currentsize); // copy (use smaller old size)
            FreeMem(aMemory); // free old mem
          end;
        end;
      end
      else
      //alloc new mem and copy data
      begin
        Result := Self.GetMem(newsize);
        if aMemory <> Result then
        begin
          Move(aMemory^, Result^, currentsize); // copy (use smaller old size)
          FreeMem(aMemory); // free old mem
        end;
      end;
    end;
{
  end
  else
  begin
    if (aMemory = nil) and (aSize > 0) then
      // GetMem disguised as ReAlloc
      Result := Self.GetMem(aSize)
    else
    begin
      //FreeMem disguised as ReAlloc
      Result := nil;
      Self.FreeMem(aMemory);
    end;
  end;            }

  {$ifdef DEBUG} 
  except
    sleep(0);
  end;
  {$ENDIF}
end;

function TThreadManager.ScanBlockForFreeItems(aBlock: PBlockMemory; aMinResultSize: NativeUInt): PMediumHeaderExt;
var
  firstmem: PMediumHeader;
  prevmem,
  nextmem : PMediumHeaderExt;
  newsize : NativeUInt;
begin
  firstmem := PMediumHeader( NativeUInt(aBlock) + SizeOf(TBlockMemory));
  nextmem  := PMediumHeaderExt(firstmem);
  prevmem  := nil;
  newsize  := 0;
  Result   := nil;

  while (nextmem <> nil) do
  begin
    prevmem := nextmem;

    //next block is free?
    while (nextmem <> nil) and
          (NativeUInt(nextmem.Next) > NativeUInt(1 shl 31)) do
          //note: previous block cannot be scanned!?
    begin
      Assert( Cardinal(nextmem.Next) <> $80808080);
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
    if nextmem.Next <> nil then
      nextmem := PMediumHeaderExt(NativeInt(nextmem.Next) xor (1 shl 31))
    else
      nextmem := nil;
  end;
end;

(*
procedure MemTest;
var p1, p2, p3: pointer;
  i: NativeInt;
begin
  i := 1 shl 4;
  i := 1 shl 16;
  i := i shl 4;

  p1 := GThreadManager.GetMem(100);
  p2 := GThreadManager.GetMem(100);
  p3 := GThreadManager.GetMem(100);
  GThreadManager.FreeMem(p2);
  p2 := GThreadManager.GetMem(100);
  p2 := GThreadManager.ReallocMem(p2, 150);
  p2 := GThreadManager.ReallocMem(p2, 50);
  p2 := GThreadManager.ReallocMem(p2, 100);

  GThreadManager.FreeMem(p3);
  GThreadManager.FreeMem(p1);
end;
*)

{ TGlobalMemManager }

procedure TGlobalMemManager.FreeAllMemory;
begin
{ TODO -oAM : TGlobalMemManager.FreeAllMemory }
end;

procedure TGlobalMemManager.FreeBlockMemory(aBlockMem: PBlockMemory);
var
  firstmem: PMediumHeader;
begin
  //keep max 10 blocks in buffer
  if FFreeBlockCount >= 10 then
  begin
    firstmem := PMediumHeader( NativeUInt(aBlockMem) + SizeOf(TBlockMemory));
    //is free mem?
    if NativeUInt(firstmem.Next) > NativeUInt(1 shl 31) then
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

procedure TGlobalMemManager.FreeBlocksFromThreadMemory(aThreadMem: PThreadManager);
var
  threadblock, nextblock: PBlockMemory;
begin
  threadblock := aThreadMem.FFirstBlock;
  while threadblock <> nil do
  begin
    nextblock   := threadblock.NextBlock;
    FreeBlockMemory(threadblock);
    threadblock := nextblock;
  end;
end;

function TGlobalMemManager.GetBlockMemory: PBlockMemory;
//var
//  firstmem: PMediumHeader;
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

    //firstmem := PMediumHeader( NativeUInt(Result) + SizeOf(TBlockMemory));
    //is free mem?
    //if firstmem.Next > (1 shl 31) then
    //begin
      //fully free mem?
      //if PMediumHeaderExt(firstmem).ArrayPosition = 16 then
        dec(FFreeBlockCount);
    //end;
  end;

  //UNLOCK
  FLock := 0;
end;

procedure TGlobalMemManager.Init;
begin
//  FMainThreadMemory := GetSmallMemManager;
{ TODO -oAM : TGlobalMemManager.Init }
end;

(*
const
{$ifdef USEMEMMANAGEREX}
  ScaleMM_Ex: TMemoryManagerEx = (
    GetMem: Scale_GetMem;
    FreeMem: Scale_FreeMem;
    ReallocMem: Scale_ReallocMem;
    AllocMem: Scale_AllocMem;
    RegisterExpectedMemoryLeak: Scale_RegisterMemoryLeak;
    UnregisterExpectedMemoryLeak: Scale_UnregisterMemoryLeak );
{$else}
  ScaleMM_Ex: TMemoryManager = (
    GetMem: Scale_GetMem;
    FreeMem: Scale_FreeMem;
    ReallocMem: Scale_ReallocMem );
{$endif}
*)

(*
procedure ScaleMM2_Install;
begin
  (*
  {$IFnDEF PURE_PASCAL}
  // get TLS slot
  GOwnTlsIndex  := TlsAlloc;
  // write fixed offset to TLS slot (instead calc via GOwnTlsIndex)
  _FixedOffset;
  {$ENDIF}

  // Hook memory Manager
  GetMemoryManager(OldMM);
  if @OldMM <> @ScaleMM_Ex then
    SetMemoryManager(ScaleMM_Ex);
  )

  // init main thread manager
  GGlobalManager.Init;

  // we need to patch System.EndThread to properly mark memory to be freed
  PatchThread;
end;
*)

//initialization
//  MemTest;
//  ScaleMM2_Install;


end.
