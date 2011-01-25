unit smmLargeMemory;

interface

{$Include smmOptions.inc}

uses
  smmTypes;

type
  PLargeHeader           = ^TLargeHeader;
  PLargeBlockMemory      = ^TLargeBlockMemory;
  PLargeMemThreadManager = ^TLargeMemThreadManager;

  //16 bytes
  {
  TLargeHeader = record
    //Block: Pointer;
    Next : Pointer;  //size
    AllMemIndex: NativeUInt;
    Mask: NativeUInt;
  end;
  }
  //16 bytes, single memory item
  TLargeHeader = record
    //linked items in one block
    //NextMem    : PLargeHeader;
    //PrevMem    : PLargeHeader;
    Size       : NativeUInt;
    /// must be last item of header (same negative offset from mem as TBaseMemHeader)
    OwnerBlock : PLargeBlockMemory;
  end;

  TLargeBlockMemory = object
    OwnerThread: PLargeMemThreadManager;
    Size       : NativeUInt;
    //linked list of blocks of a thread
    //NextBlock,
    //PreviousBlock: PLargeBlockMemory;
  end;

  TLargeMemThreadManager = object
  public
    SizeType: TSizeType;
    OwnerManager: PBaseThreadManager;
  public
    procedure Init;

    function GetMem(aSize: NativeUInt) : Pointer;
    function FreeMem(aMemory: Pointer): NativeInt;
    //function ReallocMem(aMemory: Pointer; aSize: NativeUInt): Pointer;

    function GetMemWithHeader(aSize: NativeUInt) : Pointer;
    function FreeMemWithHeader(aMemory: Pointer): NativeInt;
    function ReallocMemWithHeader(aMemory: Pointer; aSize: NativeUInt): Pointer;
  end;

implementation

uses
  smmFunctions;

{ TLargeMemThreadManager }

function TLargeMemThreadManager.FreeMem(aMemory: Pointer): NativeInt;
begin
  Result  := 0;
  if not VirtualFree(aMemory, 0, MEM_RELEASE) then
    Result := 1;
end;

function TLargeMemThreadManager.FreeMemWithHeader(aMemory: Pointer): NativeInt;
var
  pblock : PLargeBlockMemory;
begin
  pblock := PLargeBlockMemory(NativeUInt(aMemory) - SizeOf(TLargeBlockMemory) - SizeOf(TLargeHeader));
  Result := Self.FreeMem(pblock);
end;

function TLargeMemThreadManager.GetMem(aSize: NativeUInt): Pointer;
begin
  Result := VirtualAlloc( nil,
                          aSize,
                          MEM_COMMIT {$ifdef AlwaysAllocateTopDown} or MEM_TOP_DOWN{$endif},
                          PAGE_READWRITE);
  if Result = nil then
    System.Error(reOutOfMemory);
  if NativeUInt(Result) > NativeUInt(1 shl 31) then
    System.Error(reInvalidPtr);
end;

function TLargeMemThreadManager.GetMemWithHeader(aSize: NativeUInt): Pointer;
var
  iAllocSize: NativeUInt;
  pheader: PLargeHeader;
  pblock : PLargeBlockMemory;
begin
  iAllocSize    := aSize + SizeOf(TLargeBlockMemory) + SizeOf(TLargeHeader);
  //block
  pblock        := Self.GetMem(iAllocSize);
  pblock.OwnerThread   := @Self;
  pblock.Size          := iAllocSize;
//  pblock.NextBlock     := nil;
//  pblock.PreviousBlock := nil;

  //first item
  pheader            := PLargeHeader( NativeUInt(pblock) + SizeOf(TLargeBlockMemory));
  pheader.OwnerBlock := pblock;
  pheader.Size       := aSize + SizeOf(TLargeHeader);
//  pheader.NextMem    := nil;
//  pheader.PrevMem    := nil;

  Result := Pointer(NativeUInt(pheader) + SizeOf(TLargeHeader));
end;

procedure TLargeMemThreadManager.Init;
begin
  SizeType := stLarge;
end;

function TLargeMemThreadManager.ReallocMemWithHeader(aMemory: Pointer;
  aSize: NativeUInt): Pointer;
var
  iAllocSize, iOldSize: NativeUInt;
  //pheader: PLargeHeader;
  pblock : PLargeBlockMemory;
begin
  pblock     := PLargeBlockMemory(NativeUInt(aMemory) - SizeOf(TLargeBlockMemory) - SizeOf(TLargeHeader));
  iAllocSize := aSize + SizeOf(TLargeBlockMemory) + SizeOf(TLargeHeader);
  iOldSize   := pblock.Size - SizeOf(TLargeBlockMemory) - SizeOf(TLargeHeader);

  //upscale?
  if iAllocSize > pblock.Size then
  begin
    Result := GetMemWithHeader(iAllocSize);
    Move(aMemory^, Result^, iOldSize); // copy (use smaller old size)
    FreeMem(pblock);
  end
  //downscale: less than 1/2?
  else if iAllocSize > (pblock.Size shr 1) then
    Result := aMemory
  else
  begin
    Result := GetMemWithHeader(iAllocSize);
    Move(aMemory^, Result^, aSize); // copy (use smaller new size)
    FreeMem(pblock);
  end;
end;

end.
