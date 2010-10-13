unit ScaleMM;

interface

const
  CARRAYSIZE = 25;  //alloc memory blocks with 25 memory items each time

{$ifdef RELEASE}
  {$Optimization ON}
  {$STACKFRAMES OFF}
{$ENDIF}

//todo: prefetch memory after alloc ivm class create, strings etc zodat ervolgcode sneller is?

type
  PMemoryList   = ^TMemoryList;
  PBlockMemory  = ^TBlockMemory;
  PThreadMemory = ^TThreadMemory;
  PMemHeader    = ^TMemHeader;

  TMemHeader = record
    //owner
    MemList  : PMemoryList;

    //NextMem  : PMemHeader;  //linked to next single memory item (freed?)
    Filer: Pointer;           //16byte aligned
  end;

  TMemoryList = record
    //linked to next freed item (for fast GetMem)
    //FNextFreedItem: PMemHeader;

    //owner
    BlockMemory: PBlockMemory;

    //linked to next list with freed memory, in case this list has no more freed mem
    FNextFreedMemList: PMemoryList;
    //linked to next list with free memory
    FNextMemList     : PMemoryList;

    //FFreeItem: Pointer;  //no index but moving pointer, pointing to an array item
    FFreeIndex: Integer;
    FFreeArray: array[0..CARRAYSIZE] of Pointer;

    FUsageCount : Integer;    //how much free mem is used, max is CARRAYSIZE
    FMemoryArray: Pointer;    //array[CARRAYSIZE] of memory items

    procedure AllocListMemory;
    function  GetFreedMemoryItem: PMemHeader;   {$ifdef RELEASE}inline;{$ENDIF}
    procedure FreeMem(aMemoryItem: PMemHeader); {$ifdef RELEASE}inline;{$ENDIF}
  end;

  //todo: voor arrays van 10?
  //TSmallMemoryList = record

  //todo: voor arrays van 10?
  //TMediumMemoryList = record

  TBlockMemory = record
    //first list with freed memory (which this block owns)
    FFirstFreeMemLists: PMemoryList;
    //first list with free memory (which this block owns)
    FFirstMemLists    : PMemoryList;

    //size of memory items (32, 64 etc bytes)
    FBlockSize   : Word;   //0..65535
    //owner
    ThreadMemory : PThreadMemory;

    //recursive check when we alloc memory for this blocksize (new memory list)
    FRecursive: boolean;

    //no filer needed, is faster this way
    //FFiler: array[0..2] of Pointer;  //make 32byte aligned instead of 20 bytes

    procedure AddNewMemoryList;
    function  GetMemFromList: Pointer;
    function  GetFreedMemFromList: Pointer; {$ifdef RELEASE}inline;{$ENDIF}
  end;

  TThreadMemory = record
  private
    //linked list of mem freed in other thread
    FOtherThreadFreedMemory: PMemHeader;
    procedure ProcessFreedMemFromOtherThreads;

    //array with memory per block size (32 - 256 bytes)
    var FMiniMemoryBlocks: array[0..7] of TBlockMemory;
    //array with memory per block size (256 - 2048 bytes)
    var FSmallMemoryBlocks: array[0..7] of TBlockMemory;
    //array with (dynamic) memory per block size 256 (2048 - 16384/18432 bytes)
    var FMediumMemoryBlocks: array[0..63] of PBlockMemory;

    function  GetLargerMem(aSize: Integer): Pointer;
    function  GetOldMem(aSize: Integer): Pointer;
    function  FreeOldMem(aMemory: Pointer): Integer;
  public
    procedure Init;

    function GetMem(aSize: Integer): Pointer;    {$ifdef RELEASE}inline;{$ENDIF}
    function FreeMem(aMemory: Pointer): Integer; {$ifdef RELEASE}inline;{$ENDIF}
  end;

implementation

uses
  uWinApiFunctions;
//  Windows, SysUtils, Classes, Contnrs;  do not use other units!

var
  OldMM: TMemoryManagerEx;
  GOwnTlsIndex,
  GOwnTlsOffset: Cardinal;

//ScaleMM.pas.85: MOV   EAX, GOwnTlsOffset                     //todo: calc once and inject runtime
//0040A344 A1646C4B00       mov eax,[$004b6c64]
//ScaleMM.pas.89: MOV   EAX, 123456789                     //todo: calc once and inject runtime
//0040A344 B815CD5B07       mov eax,$075bcd15

function _GetOwnTls: Pointer;
asm
//  MOV   EAX, GOwnTlsIndex
  MOV   EAX, 123456789                     //dummy value: calc once and inject runtime
  MOV   ECX, fs:[$00000018]
//  MOV   EAX, [ECX+ EAX*4+$0e10]
  MOV   EAX, [ECX + EAX]
  RET
end;

procedure _FixedOffset;
var p: pointer;
begin
  GOwnTlsOffset := GOwnTlsIndex*4+$0e10;

  p  := @_GetOwnTls;
  SetPermission(p, 5, PAGE_EXECUTE_READWRITE);
  inc(PByte(p));
  PCardinal(p)^ := GOwnTlsOffset;  //write fixed offset (so no calculation + slow global var value fetch)
end;

{ TSmallMemManager }

function CreateSmallMemManager: PThreadMemory;
begin
  Result := OldMM.GetMem( SizeOf(TThreadMemory) );
  InitializeArray(Result, TypeInfo(TThreadMemory),1);
  Result.Init;

  TlsSetValue(GOwnTLSIndex, Result);
end;

function GetSmallMemManager: PThreadMemory;inline;
begin
  Result := _GetOwnTls;
  if Result <> nil then
    Exit
  else
    Result := CreateSmallMemManager;
end;

procedure TThreadMemory.Init;
var i: Integer;
begin
  for i := Low(FMiniMemoryBlocks) to High(FMiniMemoryBlocks) do
  begin
    FMiniMemoryBlocks[i].ThreadMemory       := @Self;
    FMiniMemoryBlocks[i].FFirstMemLists     := nil;
    FMiniMemoryBlocks[i].FFirstFreeMemLists := nil;
    FMiniMemoryBlocks[i].FBlockSize         := (i+1) * 32;
  end;
  for i := Low(FMiniMemoryBlocks) to High(FMiniMemoryBlocks) do
  begin
    FSmallMemoryBlocks[i].ThreadMemory       := @Self;
    FSmallMemoryBlocks[i].FFirstMemLists     := nil;
    FSmallMemoryBlocks[i].FFirstFreeMemLists := nil;
    FSmallMemoryBlocks[i].FBlockSize         := (i+1) * 256;
  end;
end;

procedure TThreadMemory.ProcessFreedMemFromOtherThreads;
begin
  //todo
end;

function TThreadMemory.FreeMem(aMemory: Pointer): Integer;
var
  pm: PMemoryList;
  p: Pointer;
begin
  p  := Pointer(Cardinal(aMemory) - SizeOf(TMemHeader));
  pm := PMemHeader(p).MemList;

  Result := 0; // No Error result for Delphi

  if FOtherThreadFreedMemory <> nil then
    ProcessFreedMemFromOtherThreads;

  //oldmm?
  if pm <> nil then
  with pm^ do
  begin
    if BlockMemory.ThreadMemory = @Self then  //mem of own thread?
      FreeMem(PMemHeader(p))
    else
    //mem of other thread
    begin
      //todo: put mem in lockfree queue of owner thread?
      Sleep(0);
    end;
  end
  else
    Result := FreeOldMem(p);
end;

function TThreadMemory.FreeOldMem(aMemory: Pointer): Integer;
begin
 Result := OldMM.FreeMem(aMemory);
end;

function TThreadMemory.GetLargerMem(aSize: Integer): Pointer;
begin
  Result := GetOldMem(aSize);

  //FMediumMemoryBlocks (64x 256 + 2048 bytes)
  //if block not yet then add, else normal stuff
end;

function TThreadMemory.GetMem(aSize: Integer): Pointer;
var
  i: Integer;
  bm: PBlockMemory;
begin
  //Result := nil;
//  i := aSize div 32;       //blocks of 32: 32, 64, 96, etc till 256

  //mini block?
  if aSize <= 8 * 32 then
  begin
    i  := aSize div 32;       //blocks of 32: 32, 64, 96, etc till 256
    bm := @FMiniMemoryBlocks[i]
  end
  //small block?
  else if aSize <= 8 * 256 then
  begin
    i  := aSize div 256;    //blocks of 256: 256, 512, 768, etc till 2048
    bm := @FSmallMemoryBlocks[i]
  end
  else
  //medium or larger blocks
  begin
    Result := GetLargerMem(aSize);
    Exit;
  end;

  if FOtherThreadFreedMemory <> nil then
    ProcessFreedMemFromOtherThreads;

  (*
  //mini block?
  if i <= High(FMiniMemoryBlocks) then
    bm := @FMiniMemoryBlocks[i]
  else
  begin
    j := aSize div 256;    //blocks of 256: 256, 512, 768, etc till 2048
    //small block?
    if j <= High(FSmallMemoryBlocks) then
      bm := @FSmallMemoryBlocks[j]
    else
    //medium or larger blocks
    begin
      Result := GetLargerMem(aSize);
      Exit;
    end;
  end;
  *)

  with bm^ do
  begin
    //first get from freed mem (fastest because most chance?)
    if FFirstFreeMemLists <> nil then
      Result := GetFreedMemFromList
    else
      //from normal list
      Result := GetMemFromList;
  end;

  Result  := Pointer(Cardinal(Result) + SizeOf(TMemHeader));
end;

function TThreadMemory.GetOldMem(aSize: Integer): Pointer;
begin
  Result := OldMM.GetMem(aSize + SizeOf(TMemHeader));
  TMemHeader(Result^).MemList := nil;  //not our memlist, so nil, so we can check on this
  Result := Pointer(Cardinal(Result) + SizeOf(TMemHeader) );
end;

function Scale_GetMem(aSize: Integer): Pointer;
begin
  Result := GetSmallMemManager.GetMem(aSize);
end;

function Scale_AllocMem(aSize: Cardinal): Pointer;
begin
  Result := GetSmallMemManager.GetMem(aSize);
  ZeroMemory(Result, aSize);
end;

function Scale_FreeMem(aMemory: Pointer): Integer;
begin
  Result := GetSmallMemManager.FreeMem(aMemory);
end;

function Scale_ReallocMem(aMemory: Pointer; aSize: Integer): Pointer;
var
  pm: PMemoryList;
  p: Pointer;
begin
  // ReAlloc can be misued as GetMem or FreeMem (documented in delphi help) so check what the user wants

  // Normal realloc of exisiting data?
  if (aMemory <> nil) and (aSize > 0) then
  begin
    p  := Pointer(Cardinal(aMemory) - SizeOf(TMemHeader));
    pm := PMemHeader(p).MemList;

    if pm <> nil then
    with pm^ do
    begin
      //new size smaller then current size?
      if (aSize <= BlockMemory.FBlockSize) then    //we have already incremented blocksize in "TMemList.AllocMem" and "TMemStore.GetMemFromList"
      begin
        if (aSize + BlockMemory.FBlockSize >= BlockMemory.FBlockSize) then
          Result := aMemory //no resize needed
        else
        //too much downscaling
        begin
          Result := Scale_GetMem(aSize);   //new mem
          if aMemory <> Result then
          begin
            Move(aMemory^, Result^,
                 aSize);                   //copy (use smaller new size)
            Scale_FreeMem(aMemory);        //free old mem
          end;
        end;
      end
      else
      //new size bigger then current size?
      begin
        Result := Scale_GetMem(aSize);     //new mem
        if aMemory <> Result then
        begin
          Move(aMemory^, Result^,
               BlockMemory.FBlockSize);    //copy (use smaller old size)
          Scale_FreeMem(aMemory);          //free old mem
        end;
      end;
    end
    //oldmm
    else
    begin
      Result := OldMM.ReallocMem(p, aSize + SizeOf(TMemHeader));
      TMemHeader(Result^).MemList := nil;  //not our memlist, so nil, so we can check on this
      Result := Pointer(Cardinal(Result) + SizeOf(TMemHeader) );
    end;
  end
  else
  begin
    if (aMemory = nil) and (aSize > 0) then
    begin // GetMem disguised as ReAlloc
      Result := Scale_GetMem(aSize);
    end
    else
    begin // FreeMem disguised as ReAlloc
      Result := nil;
      Scale_FreeMem(aMemory);
    end;
  end;
end;

function Scale_RegisterMemoryLeak(P: Pointer): Boolean;
begin
  Result := False;
end;

function Scale_UnregisterMemoryLeak(P: Pointer): Boolean;
begin
  Result := False;
end;

const
  ScaleMM_Ex: TMemoryManagerEx = (
    GetMem     : Scale_GetMem;
    FreeMem    : Scale_FreeMem;
    ReallocMem : Scale_ReallocMem;
    AllocMem   : Scale_AllocMem;
    RegisterExpectedMemoryLeak  : Scale_RegisterMemoryLeak;
    UnregisterExpectedMemoryLeak: Scale_UnregisterMemoryLeak);

{ TMemoryList }

procedure TMemoryList.AllocListMemory;
begin
  FMemoryArray := OldMM.GetMem( (BlockMemory.FBlockSize + SizeOf(TMemHeader))
                                * CARRAYSIZE );
end;

procedure ScaleMMInstall;
var
  iSize: integer;
begin
  iSize := SizeOf(TMemHeader);        //8
  iSize := SizeOf(TMemoryList);       //132
  iSize := SizeOf(TBlockMemory);      //20
  iSize := SizeOf(TThreadMemory);     //580

  //get TLS slot
  GOwnTlsIndex  := TlsAlloc;
  //write fixed offset to TLS slot (instead calc via GOwnTlsIndex)
  _FixedOffset;

  // Hook memory Manager
  GetMemoryManager(OldMM);
  if @OldMM <> @ScaleMM_Ex then
    SetMemoryManager(ScaleMM_Ex);
end;

procedure TMemoryList.FreeMem(aMemoryItem: PMemHeader);
begin
  //first free item of memlist?
  (*
  if FNextFreedItem = nil then
    with BlockMemory^ do  //faster
    begin{
      Self.}FNextFreedMemList   := {Store}FFirstFreeMemLists;  //link to first list
      {Store}FFirstFreeMemLists := @Self;                      //replace first list
    end;
  aMemoryItem.NextMem := FNextFreedItem;    //link to first item
  FNextFreedItem      := aMemoryItem;         //replace first item
  *)

  (*
  if FFreeItem <> nil then
    inc(PPointer(FFreeItem))
  else
  begin
    FFreeItem := @FFreeArray[1];

    with BlockMemory^ do  //faster
    begin{
      Self.}FNextFreedMemList   := {Store}FFirstFreeMemLists;  //link to first list
      {Store}FFirstFreeMemLists := @Self;                      //replace first list
    end;
  end;

  PMemHeader(FFreeItem^) := aMemoryItem;   //moving pointer instead of cacl offset in array[]
  *)

  //free mem block
  FFreeArray[FFreeIndex] := aMemoryItem;

  //first free item of memlist?
  if FFreeIndex = 0 then
    with BlockMemory^ do  //faster
    begin{
      Self.}FNextFreedMemList   := {Store}FFirstFreeMemLists;  //link to first list
      {Store}FFirstFreeMemLists := @Self;                      //replace first list
    end;

  inc(FFreeIndex);
end;

function TMemoryList.GetFreedMemoryItem: PMemHeader;
begin
  dec(FFreeIndex);
  Result := FFreeArray[FFreeIndex];
  if FFreeIndex = 0 then  //no free items left?
    //set next free memlist
    {Self.}BlockMemory.FFirstFreeMemLists := {Self.}FNextFreedMemList;

  (*
  Result         := PMemHeader(FFreeItem^);
  dec(PPointer(FFreeItem));

  //first item?
  if FFreeItem = @FFreeArray[0] then
  begin
    //reset
    FFreeItem := nil;
    //set next free memlist
    {Self.}BlockMemory.FFirstFreeMemLists := {Self.}FNextFreedMemList;
  end;
  *)

  {
  Result         := FNextFreedItem;
  FNextFreedItem := Result.NextMem;
  }

//  dec(FFreeIndex);
//  Result := FFreeArray[FFreeIndex];
//  if FFreeIndex = 0 then  //no free items left?
//  if FNextFreedItem = nil then
end;

{ TBlockMemory }

procedure TBlockMemory.AddNewMemoryList;
var
  pm: PMemoryList;
begin
  New(pm);
  ZeroMemory(pm, SizeOf(TMemoryList));

  with pm^ do
  begin
    BlockMemory      := @Self;
    FBlockSize := Self.FBlockSize;
    AllocListMemory;                  //init
  end;

  //set new memlist as first, add link to current item
  pm.FNextMemList := FFirstMemLists;
  FFirstMemLists  := pm;
end;

function TBlockMemory.GetFreedMemFromList: Pointer;
begin
  Result := FFirstFreeMemLists.GetFreedMemoryItem;
end;

function TBlockMemory.GetMemFromList: Pointer;
var
  pm: PMemoryList;
begin
  //store: first time init?
  if FFirstMemLists = nil then
  begin
    if FRecursive then
    begin
      Result := Self.ThreadMemory.GetOldMem(Self.FBlockSize);
      Exit;
    end;
    FRecursive := True;
    AddNewMemoryList;
    FRecursive := False;
  end;

  pm := FFirstMemLists;
  with pm^ do
  begin
    //memlist full? make new memlist
    if FUsageCount >= CARRAYSIZE then
    begin
      if FRecursive then
      begin
        Result := Self.ThreadMemory.GetOldMem(Self.FBlockSize);
        Exit;
      end;
      FRecursive := True;
      AddNewMemoryList;
      FRecursive := False;

      pm := FFirstMemLists;
    end;
  end;

  //get mem from list
  with pm^ do
  begin
    //space left?
    if FUsageCount < CARRAYSIZE then
    begin
      //calc next item
      Result := Pointer(Cardinal(FMemoryArray) +
                                (FUsageCount *
                                  (FBlockSize + SizeOf(TMemHeader))
                                 ) );
      inc(FUsageCount);
      //startheader = link to memlist
      TMemHeader(Result^).MemList := pm;
    end
    else
      Result := nil;
  end;
end;

initialization
  ScaleMMInstall;
//  Test;

end.
