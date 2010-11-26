// fast scaling memory manager for Delphi
// licensed under a MPL/GPL/LGPL tri-license; version 0.1
unit ScaleMM;

{
  ScaleMM: Fast scaling memory manager for Delphi
  by André Mussche (andre.mussche@gmail.com)
  http://code.google.com/p/scalemm
  Released under Mozilla Public License 1.1

  Simple, small and compact MM, built on top of the main Memory Manager
  (FastMM4 is a good candidate, standard since Delphi 2007), architectured
  in order to scale on multi core CPU's (which is what FastMM4 is lacking).

  Modifications by A.Bouchez - http://synopse.info:
  - Compiles from Delphi 6 up to Delphi XE;
  - Some pascal code converted to faster asm;
  - Some code refactoring, a lot of comments added;
  - Added (experimental) medium block handling from 2048 bytes up to 16384;
  - Released under MPL 1.1/GPL 2.0/LGPL 2.1 tri-license.

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is ScaleMM - Fast scaling memory manager for Delphi.

  The Initial Developer of the Original Code is André Mussche.

  Portions created by the Initial Developer are Copyright (C) 2010
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  - Arnaud Bouchez http://synopse.info
  Portions created by each contributor are Copyright (C) 2010
  each contributor. All Rights Reserved.

  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. If you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. If you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****
}

interface

const
  /// alloc memory blocks with 64 memory items each time
  // - 64 = 1 shl 6, therefore any multiplication compiles into nice shl opcode
  // - on a heavily multi-threaded application, with USEMEDIUM defined below,
  // a lower value (like 32) could be used instead (maybe dedicated value for
  // medium blocks would be even better)
  C_ARRAYSIZE = 64;
  /// keep 10 free blocks in cache
  C_GLOBAL_BLOCK_CACHE = 10;

/// internal GetSmallMemManager function is 2% faster with an injected offset
{$DEFINE SCALE_INJECT_OFFSET}

// experimental inlined TLS access
// - injected offset + GetSmallMemManager call can be slower than offset loading
{$define INLINEGOWN}
{$ifdef INLINEGOWN}
  {$UNDEF SCALE_INJECT_OFFSET}
{$endif}


// other posible defines:
{.$DEFINE PURE_PASCAL}    // no assembly, pure delphi code
{.$DEFINE Align16Bytes}   // 16 byte aligned header, so some more overhead
{.$DEFINE DEBUG_SCALEMM}  // slower but better debugging (no inline functions etc)
{$DEFINE USEMEDIUM}      // handling of 2048..16384 bytes blocks


{$IFDEF DEBUG_SCALEMM}
  {$OPTIMIZATION   OFF}
  {$STACKFRAMES    ON}
  {$ASSERTIONS     ON}
  {$DEBUGINFO      ON}
  {$OVERFLOWCHECKS ON}
  {$RANGECHECKS    ON}
{$ELSE}      // default "release" mode, much faster!
  {$OPTIMIZATION   ON}         // 235% faster!
  {$STACKFRAMES    OFF}        // 12% faster
  {$ASSERTIONS     OFF}
  {$OVERFLOWCHECKS OFF}
  {$RANGECHECKS    OFF}
  {$if CompilerVersion >= 17}
    {$define HASINLINE}        // Delphi 2005 or newer
  {$ifend}
{$ENDIF}

{$if CompilerVersion < 19}
  type // from Delphi 6 up to Delphi 2007
    NativeUInt = Cardinal;
    NativeInt  = Integer;
{$ifend}

{$if CompilerVersion >= 17}
  {$define USEMEMMANAGEREX}
{$ifend}


const
{$ifdef USEMEDIUM}
  /// Maximum index of 2048 bytes granularity Medium blocks
  // - 63488 could have been the upper limit because 65536=63488+2048 won't fit in
  // a FItemSize: word, but it will allocate 63488*C_ARRAYSIZE=4 MB per thread!
  // - so we allocate here up to 16384 bytes, i.e. 1 MB, which sounds
  // reasonable
  // - a global VirtualAlloc() bigger block, splitted into several medium blocks,
  // via a double-linked list (see FastMM4 algorithm) could be implemented instead
  MAX_MEDIUMMEMBLOCK = 7;
  /// Maximum index of 256 bytes granularity Small blocks
  MAX_SMALLMEMBLOCK  = 6;
{$else}
  /// Maximum index of 256 bytes granularity Small blocks
  // - Small blocks will include 2048 if Medium Blocks not handled
  MAX_SMALLMEMBLOCK  = 7;
{$endif}


type
  PMemHeader        = ^TMemHeader;
  PMemBlock         = ^TMemBlock;
  PMemBlockList     = ^TMemBlockList;
  PThreadMemManager = ^TThreadMemManager;

{$A-} { all object/record must be packed }
  /// Header appended to the beginning of every allocated memory block
  TMemHeader = object
    /// the memory block handler which owns this memory block
    Owner: PMemBlock;
    /// linked to next single memory item (other thread freem mem)
    NextMem: PMemHeader;
    {$IFDEF Align16Bytes}
      {$ifndef CPUX64}
      Filer1: Pointer;  // 16 bytes aligned for 32 bit compiler
      Filer2: Pointer;
      {$endif}
    {$ENDIF}
  end;

  /// memory block handler
  TMemBlock = object
    /// the memory block list which owns this memory block handler
    Owner: PMemBlockList;

    { TODO -oAM : todo: one double linked list? }
    /// link to the next list with freed memory, in case this list has no more freed mem
    FNextFreedMemBlock: PMemBlock;
    /// link to the next list with free memory
    FNextMemBlock: PMemBlock;
    /// link to the previous list with free memory
    // - double linked to be able for fast removal of one block
    FPreviousMemBlock: PMemBlock;
    /// link to the previous list with freed memory
    FPreviousFreedMemBlock: PMemBlock;
    /// internal storage of the memory blocks
    // - will contain array[0..C_ARRAYSIZE-1] of memory items,
    // i.e. (FItemSize + SizeOf(TMemHeader)) * C_ARRAYSIZE bytes
    FMemoryArray: Pointer;
    /// how much free mem is used, max is C_ARRAYSIZE
    FUsageCount: NativeUInt;

    FFreedIndex: NativeUInt;
    FFreedArray: array[0..C_ARRAYSIZE-1] of Pointer;

    function GetUsedMemoryItem: PMemHeader;     {$ifdef HASINLINE}inline;{$ENDIF}
    procedure FreeMem(aMemoryItem: PMemHeader); {$ifdef HASINLINE}inline;{$ENDIF}

    procedure FreeBlockMemoryToGlobal;
  end;

  /// memory block list
  // - current size if 16 bytes (this is a packed object)
  TMemBlockList = object
    /// list containing freed memory (which this block owns)
    // - used to implement a fast caching of memory blocks
    FFirstFreedMemBlock: PMemBlock;
    /// list containing all memory this block owns
    FFirstMemBlock: PMemBlock;
    { TODO -oAM : also last block to append free block etc instead of front? }
    /// the per-thread memory manager which created this block
    Owner: PThreadMemManager;

    /// size of memory items (32, 64 etc bytes)
    FItemSize : word;
    /// number of blocks inside FFirstFreedMemBlock
    FFreeMemCount: byte;
    /// recursive check when we alloc memory for this blocksize (new memory list)
    FRecursive: boolean;

    {$ifdef CPUX64}
    // for faster "array[0..7] of TMemBlockList" calc
    // (for 32 bits, the TMemBlockList instance size if 16 bytes)
    FFiller: array[1..sizeof(NativeInt)-4] of byte;
    {$endif}

    procedure AddNewMemoryBlock;
    function  GetMemFromNewBlock : Pointer;
  end;

  /// handles per-thread memory managment
  TThreadMemManager = object
  private
    /// link to the list of mem freed in other thread
    FOtherThreadFreedMemory: PMemHeader;
    /// array with memory per block size of 32 bytes (mini blocks)
    // - i.e. 32, 64, 96, 128, 160, 192, 224 bytes
    FMiniMemoryBlocks: array[0..6] of TMemBlockList;
    /// array with memory per block size of 256 bytes (small blocks)
    // - i.e. 256,512,768,1024,1280,1536,1792[,2048] bytes
    FSmallMemoryBlocks: array[0..MAX_SMALLMEMBLOCK] of TMemBlockList;
{$ifdef USEMEDIUM}
    /// array with memory per block size of 2048 bytes (medium blocks)
    // - i.e. 2048,4096,6144,8192,10240,12288,14336,16384 bytes
    FMediumMemoryBlocks: array[0..MAX_MEDIUMMEMBLOCK] of TMemBlockList;
{$endif}

    // link to list of items to reuse after thread terminated
    FNextThreadManager: PThreadMemManager;

    procedure ProcessFreedMemFromOtherThreads;
    procedure AddFreedMemFromOtherThread(aMemory: PMemHeader);
  public
    FThreadId: LongWord;
    FThreadTerminated: Boolean;  //is this thread memory available to new thread?

    procedure Init;
    procedure Reset;

    function GetMem(aSize: NativeInt) : Pointer;   {$ifdef HASINLINE}inline;{$ENDIF}
    function FreeMem(aMemory: Pointer): NativeInt; {$ifdef HASINLINE}inline;{$ENDIF}
  end;

  /// Global memory manager
  // - a single instance is created for the whole process
  // - caches some memory (blocks + threadmem) for fast reuse
  // - also keeps allocated memory in case an old thread allocated some memory
  // for another thread
  TGlobalMemManager = object
  private
    /// all thread memory managers
    FFirstThreadMemory: PThreadMemManager;
    /// freed/used thread memory managers
    // - used to cache the per-thread managers in case of multiple threads creation
    FFirstFreedThreadMemory: PThreadMemManager;
    /// main thread manager (owner of all global mem)
    FMainThreadMemory: PThreadMemManager;

    /// Freed/used memory: array with memory per 32 bytes block size
    // - i.e. 32, 64, 96, 128, 160, 192, 224 bytes
    FFreedMiniMemoryBlocks  : array[0..6] of TMemBlockList;
    /// Freed/used memory: array with memory per 256 bytes block size
    // - i.e. 256,512,768,1024,1280,1536,1792[,2048] bytes
    FFreedSmallMemoryBlocks : array[0..MAX_SMALLMEMBLOCK] of TMemBlockList;
{$ifdef USEMEDIUM}
    /// Freed/used memory: array with memory per block size of 2048 bytes
    // - i.e. 2048,4096,6144,8192,10240,12288,14336,16384 bytes
    FFreedMediumMemoryBlocks: array[0..MAX_MEDIUMMEMBLOCK] of TMemBlockList;
{$endif}

    procedure Init;
    procedure FreeBlocksFromThreadMemory(aThreadMem: PThreadMemManager);
  public
    procedure AddNewThreadManagerToList(aThreadMem: PThreadMemManager);
    procedure FreeThreadManager(aThreadMem: PThreadMemManager);
    function  GetNewThreadManager: PThreadMemManager;
    procedure FreeAllMemory;

    procedure FreeBlockMemory(aBlockMem: PMemBlock);
    function  GetBlockMemory(aItemSize: NativeUInt): PMemBlock;
  end;
{$A+}

function Scale_GetMem(aSize: Integer): Pointer;
function Scale_AllocMem(aSize: Cardinal): Pointer;
function Scale_FreeMem(aMemory: Pointer): Integer;
function Scale_ReallocMem(aMemory: Pointer; aSize: Integer): Pointer;

var
  GlobalManager: TGlobalMemManager;

/// Points to the Memory Manager on which ScaleMM is based
// - ScaleMM works on top of a main MM, which is FastMM4 since Delphi 2007
// - ScaleMM will handle blocks up to 2048 bytes (or 16384 is medium blocks
// are enabled)
// - but larger blocks are delegated to OldMM
// - you can explicitely use OldMM on purpose (but it doesn't seem to be a good idea)
// - note that also "root" block memory is allocated by OldMM if ScaleMM needs
// memory itself (to populate its internal buffers): there is not direct call
// to the VirtualAlloc() API, for instance
var
{$ifdef USEMEMMANAGEREX}
  OldMM: TMemoryManagerEx;
{$else}
  OldMM: TMemoryManager;
{$endif}


implementation

// Windows.pas unit is better not used -> code inlined here

type
  DWORD = LongWord;
  BOOL  = LongBool;
const
  PAGE_EXECUTE_READWRITE = $40;
  kernel32  = 'kernel32.dll';

function  TlsAlloc: DWORD; stdcall; external kernel32 name 'TlsAlloc';
function  TlsGetValue(dwTlsIndex: DWORD): Pointer; stdcall; external kernel32 name 'TlsGetValue';
function  TlsSetValue(dwTlsIndex: DWORD; lpTlsValue: Pointer): BOOL; stdcall; external kernel32 name 'TlsSetValue';
function  TlsFree(dwTlsIndex: DWORD): BOOL; stdcall; external kernel32 name 'TlsFree';
procedure Sleep(dwMilliseconds: DWORD); stdcall; external kernel32 name 'Sleep';
function  FlushInstructionCache(hProcess: THandle; const lpBaseAddress: Pointer; dwSize: DWORD): BOOL; stdcall; external kernel32 name 'FlushInstructionCache';
function  GetCurrentProcess: THandle; stdcall; external kernel32 name 'GetCurrentProcess';
function  GetCurrentThreadId: DWORD; stdcall; external kernel32 name 'GetCurrentThreadId';
function  Scale_VirtualProtect(lpAddress: Pointer; dwSize, flNewProtect: DWORD;
            var OldProtect: DWORD): BOOL; stdcall; overload; external kernel32 name 'VirtualProtect';
procedure ExitThread(dwExitCode: DWORD); stdcall; external kernel32 name 'ExitThread';

function SetPermission(Code: Pointer; Size, Permission: Cardinal): Cardinal;
begin
  Assert(Assigned(Code) and (Size > 0));
  { Flush the instruction cache so changes to the code page are effective immediately }
  if Permission <> 0 then
    if FlushInstructionCache(GetCurrentProcess, Code, Size) then
      if not Scale_VirtualProtect(Code, Size, Permission, Longword(Result)) then
        sleep(0);
end;

function CreateSmallMemManager: PThreadMemManager; forward;


{$IFDEF PURE_PASCAL}

threadvar
  GCurrentThreadManager: PThreadMemManager;

function GetSmallMemManager: PThreadMemManager; {$ifdef HASINLINE}inline;{$ENDIF}
begin
  Result := GCurrentThreadManager;
  if Result = nil then
    Result := CreateSmallMemManager;
end;

{$ELSE}
var
  GOwnTlsIndex,
  GOwnTlsOffset: NativeUInt;

function GetSmallMemManager: PThreadMemManager;
asm
{$IFDEF SCALE_INJECT_OFFSET}
  mov eax,123456789        // dummy value: calc once and inject at runtime
{$ELSE}
  mov eax,GOwnTlsOffset    // 2% slower, so we default use injected offset
{$ENDIF}
  mov ecx,fs:[$00000018]
  mov eax,[ecx+eax]      // fixed offset, calculated only once
  or eax,eax
  jz CreateSmallMemManager
end;

procedure _FixedOffset;
{$IFDEF SCALE_INJECT_OFFSET}
var p: PAnsiChar;
{$ENDIF}
begin
  GOwnTlsOffset := GOwnTlsIndex * 4 + $0e10;
  {$IFDEF SCALE_INJECT_OFFSET}
  p  := @GetSmallMemManager;
  SetPermission(p, 5, PAGE_EXECUTE_READWRITE);
  PCardinal(p+1)^ := GOwnTlsOffset;  // write fixed offset
  {$ENDIF}
end;

{$ENDIF PURE_PASCAL}

function CreateSmallMemManager: PThreadMemManager;
begin
  Result := GlobalManager.GetNewThreadManager;
  if Result = nil then
  begin
    Result := OldMM.GetMem( SizeOf(TThreadMemManager) );
    Result.Init;
  end
  else
  begin
    Result.FThreadId := GetCurrentThreadId;
    Result.FThreadTerminated := False;
  end;

  {$IFDEF PURE_PASCAL}
  GCurrentThreadManager := Result;
  {$ELSE}
  TlsSetValue(GOwnTLSIndex, Result);
  {$ENDIF}
end;

// compare oldvalue with destination: if equal then newvalue is set
function CAS32(const oldValue: pointer; newValue: pointer; var destination): boolean;
asm
  lock cmpxchg dword ptr [destination], newValue
  setz  al
end; { CAS32 }

{Compare [AAddress], CompareVal:
 If Equal: [AAddress] := NewVal and result = CompareVal
 If Unequal: Result := [AAddress]}
function LockCmpxchg(CompareVal, NewVal: Byte; AAddress: PByte): Byte;
asm
  {On entry:
    al = CompareVal,
    dl = NewVal,
    ecx = AAddress}
  lock cmpxchg [ecx], dl
end;

{$ifNdef DEBUG_SCALEMM}
procedure DummyAssert(aBoolean: boolean);
begin
  //
end;
{$ELSE}
procedure Assert(aCondition: boolean);
begin
  if not aCondition then
  begin
    asm
      int 3;
    end;
    Sleep(0);  //no exception, just dummy for breakpoint
  end;
end;
{$ENDIF}


function GetOldMem(aSize: NativeUInt): Pointer; {$ifdef HASINLINE}inline;{$ENDIF}
begin
  Result := OldMM.GetMem(aSize + SizeOf(TMemHeader));
  TMemHeader(Result^).Owner := nil;  // not our memlist, so mark as such
  Result := Pointer(NativeUInt(Result) + SizeOf(TMemHeader) );
end;

{ TThreadMemManager }

procedure TThreadMemManager.Init;
var i, j: NativeUInt;
begin
  fillchar(self,sizeof(self),0);
  FThreadId := GetCurrentThreadId;
  j := 32;
  for i := Low(FMiniMemoryBlocks) to High(FMiniMemoryBlocks) do
  begin // 32, 64, 96, 128, 160, 192, 224 bytes
    FMiniMemoryBlocks[i].Owner := @Self;
    FMiniMemoryBlocks[i].FItemSize := j;
    inc(j,32);
  end;
  assert(j=256);
  for i := Low(FSmallMemoryBlocks) to High(FSmallMemoryBlocks) do
  begin // 256,512,768,1024,1280,1536,1792 bytes
    FSmallMemoryBlocks[i].Owner := @Self;
    FSmallMemoryBlocks[i].FItemSize := j;
    inc(j,256);
  end;
{$ifdef USEMEDIUM}
  assert(j=2048);
  for i := Low(FMediumMemoryBlocks) to High(FMediumMemoryBlocks) do
  begin // 2048, 4096...16384 bytes
    FMediumMemoryBlocks[i].Owner := @Self;
    FMediumMemoryBlocks[i].FItemSize := j;
    inc(j,2048);
  end;
  assert(j=(MAX_MEDIUMMEMBLOCK+2)*2048);
{$else}
  assert(j=2304);
{$endif}
end;

procedure TThreadMemManager.ProcessFreedMemFromOtherThreads;
var
  pcurrentmem, ptempmem: PMemHeader;
begin
  //reset first item (to get all mem in linked list)
  repeat
    pcurrentmem := FOtherThreadFreedMemory;
    if CAS32(pcurrentmem, nil, FOtherThreadFreedMemory) then
      Break;
    Sleep(0);
  until false;

  //free all mem in linked list
  while pcurrentmem <> nil do
  begin
    ptempmem    := pcurrentmem;
    pcurrentmem := pcurrentmem.NextMem;

    //free
    ptempmem.Owner.FreeMem(ptempmem);
  end;
end;

procedure TThreadMemManager.Reset;
var
  i: NativeUInt;

  procedure __ResetBlocklist(aBlocklist: PMemBlockList);
  begin
    aBlocklist.FFirstFreedMemBlock := nil;
    aBlocklist.FFirstMemBlock := nil;
    aBlocklist.FRecursive := False;
  end;

begin
  FThreadId := 0;
  FThreadTerminated := True;
  FOtherThreadFreedMemory := nil;
  FNextThreadManager := nil;

  for i := Low(FMiniMemoryBlocks) to High(FMiniMemoryBlocks) do
    __ResetBlocklist(@FMiniMemoryBlocks[i]);
  for i := Low(FSmallMemoryBlocks) to High(FSmallMemoryBlocks) do
    __ResetBlocklist(@FSmallMemoryBlocks[i]);
{$ifdef USEMEDIUM}
  for i := Low(FMediumMemoryBlocks) to High(FMediumMemoryBlocks) do
    __ResetBlocklist(@FMediumMemoryBlocks[i]);
{$endif}
end;

procedure TThreadMemManager.AddFreedMemFromOtherThread(aMemory: PMemHeader);
var
  poldmem: PMemHeader;
begin
  repeat
    poldmem          := FOtherThreadFreedMemory;
    aMemory.NextMem  := poldmem;       //link to current next BEFORE the swap!
    // set new item as first (to created linked list)
    if CAS32(poldmem, aMemory, FOtherThreadFreedMemory) then
      Break;
    Sleep(0);  //retry
  until false;
end;

function TThreadMemManager.FreeMem(aMemory: Pointer): NativeInt;
var
  pm: PMemBlock;
  p: Pointer;
begin
  p  := Pointer(NativeUInt(aMemory) - SizeOf(TMemHeader));
  pm := PMemHeader(p).Owner;

  Result := 0; // No Error result for Delphi

  if FOtherThreadFreedMemory <> nil then
    ProcessFreedMemFromOtherThreads;

  if pm <> nil then
  // block obtained via Scale_GetMem()
  with pm^ do
  begin
    Assert(Owner <> nil);
    Assert(Owner.Owner <> nil);

    if Owner.Owner = @Self then  // mem of own thread?
      FreeMem(PMemHeader(p))
    else
    //mem of other thread
    begin
      //put mem in lockfree queue of owner thread
      Owner.Owner.AddFreedMemFromOtherThread(PMemHeader(p));
    end;
  end
  else
    Result := OldMM.FreeMem(p);
end;

function TThreadMemManager.GetMem(aSize: NativeInt): Pointer;
var
  bm: PMemBlockList;
begin
  if aSize <= (length(FMiniMemoryBlocks)*32) then
  begin
    if aSize > 0 then
    begin
      // blocks of 32: 32, 64, 96, 128, 160, 192, 224
      bm := @FMiniMemoryBlocks[(aSize-1) shr 5];
    end else
    begin
      Result := nil;
      Exit;
    end;
  end
  else if aSize <= (length(FSmallMemoryBlocks)*256) then
  begin
    // blocks of 256: 256,512,768,1024,1280,1536,1792 bytes
    bm := @FSmallMemoryBlocks[(aSize-1) shr 8];
  end
{$ifdef USEMEDIUM}
  else if aSize <= (length(FMediumMemoryBlocks)*2048) then
  begin
    // blocks of 2048: 2048, 4096... bytes
    bm := @FMediumMemoryBlocks[(aSize-1) shr 11];
  end
{$endif}
  else
  begin
    // larger blocks are allocated via the old Memory Manager
    Result := GetOldMem(aSize);
    Exit;
  end;

  if FOtherThreadFreedMemory <> nil then
    ProcessFreedMemFromOtherThreads;

  with bm^ do
  begin
    // first get from freed mem (fastest because most chance?)
    if FFirstFreedMemBlock <> nil then
      Result := FFirstFreedMemBlock.GetUsedMemoryItem
    else
      // from normal list
      Result := GetMemFromNewBlock;
  end;

  Assert(NativeUInt(Result) > $10000);
  Result  := Pointer(NativeUInt(Result) + SizeOf(TMemHeader));
end;


{ TMemBlock }

procedure TMemBlock.FreeBlockMemoryToGlobal;
begin
  if Owner.FFirstMemBlock = @Self then Exit; //keep one block

  //remove ourselves from linked list
  if FPreviousMemBlock <> nil then
    FPreviousMemBlock.FNextMemBlock := Self.FNextMemBlock;
  if FPreviousFreedMemBlock <> nil then
    FPreviousFreedMemBlock.FNextFreedMemBlock := Self.FNextFreedMemBlock;
  if FNextMemBlock <> nil then
    FNextMemBlock.FPreviousMemBlock := Self.FPreviousMemBlock;
  if FNextFreedMemBlock <> nil then
    FNextFreedMemBlock.FPreviousFreedMemBlock := Self.FPreviousFreedMemBlock;

  if Owner.FFirstFreedMemBlock = @Self then
    Owner.FFirstFreedMemBlock := nil;
  if Owner.FFirstMemBlock = @Self then
    Owner.FFirstMemBlock := nil;

  GlobalManager.FreeBlockMemory(@Self);
end;

procedure TMemBlock.FreeMem(aMemoryItem: PMemHeader);
begin
  // first free item of block?
  // then we add this block to (linked) list with available mem
  if FFreedIndex = 0 then
    with Owner^ do  //faster
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
  // if FFreedIndex = FUsageCount then we do not use this one, we want to keep at least one block (?)
  begin
    with Owner^ do
    begin
      Self.FreeBlockMemoryToGlobal;
    end;
  end;
end;

function TMemBlock.GetUsedMemoryItem: PMemHeader;
begin
  Assert(Self.Owner <> nil);
  Assert(FFreedIndex > 0);

  dec(FFreedIndex);
  Result := FFreedArray[FFreedIndex];

  if FFreedIndex = 0 then  // no free items left?
  begin
    //set next free memlist
    Owner.FFirstFreedMemBlock := {Self.}FNextFreedMemBlock;
    //first one has no previous
    if {Self.}FNextFreedMemBlock <> nil then
      {Self.}FNextFreedMemBlock.FPreviousFreedMemBlock := nil;

    //remove from free list
    {Self.}FPreviousFreedMemBlock := nil;
    {Self.}FNextFreedMemBlock     := nil;
  end
  //all memory was available?
//  else if FFreedIndex = CARRAYSIZE-1 then
//    dec(Owner.FFullyFreedMemListCount);
end;


{ TMemBlockList }

procedure TMemBlockList.AddNewMemoryBlock;
var
  pm: PMemBlock;
begin
  // get block from cache
  pm := GlobalManager.GetBlockMemory(FItemSize);
  if pm = nil then
  begin
    // create own one
    pm := Owner.GetMem(SizeOf(pm^));
    fillchar(pm^,SizeOf(pm^),0);
    with pm^ do
    begin
      {pm.}Owner     := @Self;
      {pm.}FItemSize := Self.FItemSize;
      {pm.}FMemoryArray :=
{$ifdef USEMEDIUM}
      Self.Owner.GetMem {$else}
      GetOldMem // (32+8)*64=2560 > 2048 -> use OldMM
{$endif} ( (FItemSize + SizeOf(TMemHeader)) * C_ARRAYSIZE );
    end;
  end;

  // init
  with pm^ do
  begin
    {pm.}Owner             := @Self;
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
end;

function TMemBlockList.GetMemFromNewBlock: Pointer;
var
  pm: PMemBlock;
begin
  // store: first time init?
  if FFirstMemBlock = nil then
  begin
    if FRecursive then
    begin
      Result := GetOldMem(Self.FItemSize);
      Exit;
    end;
    FRecursive := True;
    AddNewMemoryBlock;
    FRecursive := False;
//    if FFirstMemBlock.FUsageCount = 0 then
      // always keep at least one in buffer, so we do this cheat here...
//      inc(FFirstMemBlock.FUsageCount);
  end;

  pm := FFirstMemBlock;
  with pm^ do
  begin
    // memlist full? make new memlist
    if FUsageCount >= C_ARRAYSIZE then
    begin
      if FRecursive then
      begin
        Result := GetOldMem(Self.FItemSize);
        Exit;
      end;
      FRecursive := True;
      AddNewMemoryBlock;
      FRecursive := False;
      pm := FFirstMemBlock;
    end;
  end;

  // get mem from list
  with pm^ do
  begin
    // space left?
    if FUsageCount < C_ARRAYSIZE then
    begin
      // calc next item
      Result := Pointer(
        NativeUInt(FMemoryArray) + FUsageCount * (FItemSize + SizeOf(TMemHeader)) );
      inc(FUsageCount);
      // startheader = link to memlist
      TMemHeader(Result^).Owner     := pm;
    end
    else
      Result := pm.GetUsedMemoryItem;
  end;

  Assert(NativeUInt(Result) > $10000);
end;


{ TGlobalManager }

procedure TGlobalMemManager.AddNewThreadManagerToList(aThreadMem: PThreadMemManager);
var
  pprevthreadmem: PThreadMemManager;
begin
  repeat
    pprevthreadmem := FFirstThreadMemory;
    // try to set "result" in global var
    if CAS32(pprevthreadmem, aThreadMem, FFirstThreadMemory) then
      break;
    sleep(0);
  until false;
  // make linked list: new one is first item (global var), next item is previous item
  aThreadMem.FNextThreadManager := pprevthreadmem;
end;

procedure TGlobalMemManager.FreeAllMemory;

  procedure __ProcessBlockMem(aOldBlock: PMemBlockList);
  var
    allmem, oldmem: PMemBlock;
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
        FMainThreadMemory.FreeMem(oldmem.FMemoryArray);
        FMainThreadMemory.FreeMem(oldmem);
      end
      else
        allmem := allmem.FNextFreedMemBlock;
    end;
  end;

var
  oldthreadmem, tempthreadmem: PThreadMemManager;
  i: NativeUInt;
begin
  // free internal blocks
  for i := Low(Self.FFreedMiniMemoryBlocks) to High(Self.FFreedMiniMemoryBlocks) do
    __ProcessBlockMem(@Self.FFreedMiniMemoryBlocks[i]);
  for i := Low(Self.FFreedSmallMemoryBlocks) to High(Self.FFreedSmallMemoryBlocks) do
    __ProcessBlockMem(@Self.FFreedSmallMemoryBlocks[i]);
{$ifdef USEMEDIUM}
  for i := Low(Self.FFreedMediumMemoryBlocks) to High(Self.FFreedMediumMemoryBlocks) do
    __ProcessBlockMem(@Self.FFreedMediumMemoryBlocks[i]);
{$endif}

  // free current thread
  tempthreadmem := GetSmallMemManager;
  for i := Low(tempthreadmem.FMiniMemoryBlocks) to High(tempthreadmem.FMiniMemoryBlocks) do
    __ProcessBlockMem(@tempthreadmem.FMiniMemoryBlocks[i]);
  for i := Low(tempthreadmem.FSmallMemoryBlocks) to High(tempthreadmem.FSmallMemoryBlocks) do
    __ProcessBlockMem(@tempthreadmem.FSmallMemoryBlocks[i]);
{$ifdef USEMEDIUM}
  for i := Low(tempthreadmem.FMediumMemoryBlocks) to High(tempthreadmem.FMediumMemoryBlocks) do
    __ProcessBlockMem(@tempthreadmem.FMediumMemoryBlocks[i]);
{$endif}

  // free cached threads
  oldthreadmem := Self.FFirstFreedThreadMemory;
  while oldthreadmem <> nil do
  begin
    tempthreadmem := oldthreadmem;
    oldthreadmem  := oldthreadmem.FNextThreadManager;
    OldMM.FreeMem(tempthreadmem);
  end;
end;

procedure TGlobalMemManager.FreeBlockMemory(aBlockMem: PMemBlock);
var bl: PMemBlockList;
    prevmem: PMemBlock;
begin
  Assert( aBlockMem.FFreedIndex = aBlockMem.FUsageCount );

  with aBlockMem.Owner^ do
  begin
    if FItemSize <= (length(Self.FFreedMiniMemoryBlocks)*32) then
    begin
      // blocks of 32: 32, 64, 96, 128, 160, 192, 224
      bl := @Self.FFreedMiniMemoryBlocks[(FItemSize-1) shr 5];
    end
    else if FItemSize <= (length(Self.FFreedSmallMemoryBlocks)*256) then
    begin
      // blocks of 256: 256,512,768,1024,1280,1536,1792[,2048] bytes
      bl := @Self.FFreedSmallMemoryBlocks[(FItemSize-1) shr 8];
    end
{$ifdef USEMEDIUM}
    else if FItemSize <= (length(Self.FFreedMediumMemoryBlocks)*2048) then
    begin
      // blocks of 2048: 2048,4096,6144,8192,10240,12288,14336,16384 bytes
      bl := @Self.FFreedMediumMemoryBlocks[(FItemSize-1) shr 11];
    end
{$endif}
    else begin
      // large block
      FreeMem(aBlockMem.FMemoryArray);
      FreeMem(aBlockMem);
      Exit;
    end;
  end;

  // lock
  while bl.FRecursive or (LockCmpxchg(0, 1, @bl.FRecursive) <> 0) do
    Sleep(0);
  // too much cached?
  if bl.FFreeMemCount > C_GLOBAL_BLOCK_CACHE then
  begin
    // dispose
    Scale_FreeMem(aBlockMem.FMemoryArray);
    Scale_FreeMem(aBlockMem);
    // unlock
    bl.FRecursive := False;
    Exit;
  end;

  // add freemem block to front (replace first item, link previous to first items)
  prevmem := bl.FFirstFreedMemBlock;
  aBlockMem.FNextFreedMemBlock := prevmem;
  if prevmem <> nil then
    prevmem.FPreviousFreedMemBlock := aBlockMem;
  bl.FFirstFreedMemBlock := aBlockMem;
  // inc items cached
  inc(bl.FFreeMemCount);
  // unlock
  bl.FRecursive := False;
  // prepare block content
  aBlockMem.Owner := bl;
  aBlockMem.FNextMemBlock := nil;
  aBlockMem.FPreviousMemBlock := nil;
  aBlockMem.FPreviousFreedMemBlock := nil;
end;

procedure TGlobalMemManager.FreeBlocksFromThreadMemory(aThreadMem: PThreadMemManager);
var
  i: NativeUInt;

  procedure __ProcessBlockMem(aOldBlock, aGlobalBlock: PMemBlockList);
  var
    allmem, prevmem, tempmem,
    lastunusedmem, lastinusemem,
    unusedmem, inusemem: PMemBlock;
  begin
    allmem        := aOldBlock.FFirstMemBlock;
    unusedmem     := nil;
    lastunusedmem := nil;
    inusemem      := nil;
    lastinusemem  := nil;

    // scan all memoryblocks and filter unused blocks
    while allmem <> nil do
    begin
      if allmem.Owner = nil then
        Break; // loop?

      // fully free, no mem in use?
      if allmem.FFreedIndex = allmem.FUsageCount then
      begin

        if aGlobalBlock.FFreeMemCount > C_GLOBAL_BLOCK_CACHE then
        begin
          // next one
          tempmem := allmem;
          allmem  := allmem.FNextMemBlock;
          // dispose
          Scale_FreeMem(tempmem.FMemoryArray);
          Scale_FreeMem(tempmem);
          Continue;
        end;

        // first item of list?
        if unusedmem = nil then
          unusedmem := allmem
        else
          // else add to list (link to previous)
          lastunusedmem.FNextMemBlock := allmem;
        lastunusedmem  := allmem;

        // update number of items cached
        inc(aGlobalBlock.FFreeMemCount);
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

        // update number of items cached
        inc(aGlobalBlock.FFreeMemCount);
      end;

      allmem.Owner                  := aGlobalBlock;
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
        sleep(0);
      until false;
    end;

    if unusedmem <> nil then
    begin
      assert(lastunusedmem <> nil);
      //add unusedmem list to front (replace first item, link previous to last item)
      repeat
        prevmem                     := aGlobalBlock.FFirstMemBlock;
        lastunusedmem.FNextMemBlock := prevmem;
        if CAS32(prevmem, unusedmem, aGlobalBlock.FFirstMemBlock) then break;
        sleep(0);
      until false;
    end;
  end;

begin
  for i := Low(aThreadMem.FMiniMemoryBlocks) to High(aThreadMem.FMiniMemoryBlocks) do
    __ProcessBlockMem( @aThreadMem.FMiniMemoryBlocks[i],   @Self.FFreedMiniMemoryBlocks[i]);
  for i := Low(aThreadMem.FSmallMemoryBlocks) to High(aThreadMem.FSmallMemoryBlocks) do
    __ProcessBlockMem( @aThreadMem.FSmallMemoryBlocks[i],  @Self.FFreedSmallMemoryBlocks[i]);
{$ifdef USEMEDIUM}
  for i := Low(aThreadMem.FMediumMemoryBlocks) to High(aThreadMem.FMediumMemoryBlocks) do
    __ProcessBlockMem( @aThreadMem.FMediumMemoryBlocks[i], @Self.FFreedMediumMemoryBlocks[i]);
{$endif}
end;

procedure TGlobalMemManager.FreeThreadManager(aThreadMem: PThreadMemManager);
var
  pprevthreadmem: PThreadMemManager;
begin
  // clear mem (partial: add to reuse list, free = free)
  FreeBlocksFromThreadMemory(aThreadMem);
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
    sleep(0);
  until false;
end;

function TGlobalMemManager.GetBlockMemory(aItemSize: NativeUInt): PMemBlock;
var bl: PMemBlockList;
    prevmem, nextmem: PMemBlock;
begin
  Result := nil;

  dec(aItemSize);
  if aItemSize < (length(Self.FFreedMiniMemoryBlocks)*32) then
  begin
    // blocks of 32: 32, 64, 96, 128, 160, 192, 224
    bl := @Self.FFreedMiniMemoryBlocks[aItemSize shr 5];
  end
  else if aItemSize < (length(Self.FFreedSmallMemoryBlocks)*256) then
  begin
    // blocks of 256: 256,512,768,1024,1280,1536,1792[,2048] bytes
    bl := @Self.FFreedSmallMemoryBlocks[aItemSize shr 8];
  end
{$ifdef USEMEDIUM}
  else if aItemSize < (length(Self.FFreedMediumMemoryBlocks)*2048) then
  begin
    // blocks of 2048: 2048,4096,6144,8192,10240,12288,14336,16384 bytes
    bl := @Self.FFreedMediumMemoryBlocks[aItemSize shr 11];
  end
{$endif}
  else begin
    // not allocated by this unit (should not happen)
    assert(false);
    Exit;
  end;

  // lock
  while bl.FRecursive or (LockCmpxchg(0, 1, @bl.FRecursive) <> 0) do
    Sleep(0);
  // get freed mem from list from front (replace first item)
  if bl.FFirstFreedMemBlock <> nil then
  begin
    prevmem := bl.FFirstFreedMemBlock;
    nextmem := prevmem.FNextFreedMemBlock;
    bl.FFirstFreedMemBlock := nextmem;
    if nextmem <> nil then
      nextmem.FPreviousFreedMemBlock := nil;
    Result := prevmem;
  end
  // get free mem from list from front (replace first item)
  else if bl.FFirstMemBlock <> nil then
  begin
    prevmem := bl.FFirstMemBlock;
    nextmem := prevmem.FNextMemBlock;
    bl.FFirstMemBlock := nextmem;
    if nextmem <> nil then
      nextmem.FPreviousMemBlock := nil;
    Result := prevmem;
  end;
  // unlock
  bl.FRecursive := False;

  if Result <> nil then
  begin
    dec(bl.FFreeMemCount);
    Result.Owner := bl;
    Result.FNextFreedMemBlock := nil;
    Result.FNextMemBlock := nil;
    Result.FPreviousMemBlock := nil;
    Result.FPreviousFreedMemBlock := nil;
  end;
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
      Result.FNextThreadManager := nil;
      break;
    end;
    sleep(0);
  end;
end;

procedure TGlobalMemManager.Init;
var i, j: NativeUInt;
begin
  fillchar(self,SizeOf(self),0);
  j := 32;
  for i := Low(FFreedMiniMemoryBlocks) to High(FFreedMiniMemoryBlocks) do
  begin
    FFreedMiniMemoryBlocks[i].Owner := @Self;
    FFreedMiniMemoryBlocks[i].FItemSize := j;
    inc(j,32);
  end;
  Assert(j=256);
  for i := Low(FFreedSmallMemoryBlocks) to High(FFreedSmallMemoryBlocks) do
  begin
    FFreedSmallMemoryBlocks[i].Owner := @Self;
    FFreedSmallMemoryBlocks[i].FItemSize := j;
    inc(j,256);
  end;
{$ifdef USEMEDIUM}
  Assert(j=2048);
  for i := Low(FFreedMediumMemoryBlocks) to High(FFreedMediumMemoryBlocks) do
  begin
    FFreedMediumMemoryBlocks[i].Owner := @Self;
    FFreedMediumMemoryBlocks[i].FItemSize := j;
    inc(j,2048);
  end;
  assert(j=18432);
{$else}
  assert(j=2304);
{$endif}
  FMainThreadMemory := GetSmallMemManager;
end;

{$ifndef PURE_PASCAL}
{$if CompilerVersion < 19}
procedure Move(const Source; var Dest; Count: Integer);
asm // eax=source edx=dest ecx=count
  // original code by John O'Harrow - included since Delphi 2007
  cmp     ecx, 32
  ja      @@LargeMove {Count > 32 or Count < 0}
  sub     ecx, 8
  jg      @@SmallMove
  jmp     dword ptr [@@JumpTable+32+ecx*4] {0..8 Byte Move}
@@SmallMove: {9..32 Byte Move}
  fild    qword ptr [eax+ecx] {Load Last 8}
  fild    qword ptr [eax] {Load First 8}
  cmp     ecx, 8
  jle     @@Small16
  fild    qword ptr [eax+8] {Load Second 8}
  cmp     ecx, 16
  jle     @@Small24
  fild    qword ptr [eax+16] {Load Third 8}
  fistp   qword ptr [edx+16] {Save Third 8}
@@Small24:
  fistp   qword ptr [edx+8] {Save Second 8}
@@Small16:
  fistp   qword ptr [edx] {Save First 8}
  fistp   qword ptr [edx+ecx] {Save Last 8}
@@Exit:
  ret
  lea eax,eax+0 // for alignment of @@JumpTable
@@JumpTable: {4-Byte Aligned}
  dd      @@Exit, @@M01, @@M02, @@M03, @@M04, @@M05, @@M06, @@M07, @@M08
@@LargeForwardMove: {4-Byte Aligned}
  push    edx
  fild    qword ptr [eax] {First 8}
  lea     eax, [eax+ecx-8]
  lea     ecx, [ecx+edx-8]
  fild    qword ptr [eax] {Last 8}
  push    ecx
  neg     ecx
  and     edx, -8 {8-Byte Align Writes}
  lea     ecx, [ecx+edx+8]
  pop     edx
@FwdLoop:
  fild    qword ptr [eax+ecx]
  fistp   qword ptr [edx+ecx]
  add     ecx, 8
  jl      @FwdLoop
  fistp   qword ptr [edx] {Last 8}
  pop     edx
  fistp   qword ptr [edx] {First 8}
  ret
@@LargeMove:
  jng     @@LargeDone {Count < 0}
  cmp     eax, edx
  ja      @@LargeForwardMove
  sub     edx, ecx
  cmp     eax, edx
  lea     edx, [edx+ecx]
  jna     @@LargeForwardMove
  sub     ecx, 8 {Backward Move}
  push    ecx
  fild    qword ptr [eax+ecx] {Last 8}
  fild    qword ptr [eax] {First 8}
  add     ecx, edx
  and     ecx, -8 {8-Byte Align Writes}
  sub     ecx, edx
@BwdLoop:
  fild    qword ptr [eax+ecx]
  fistp   qword ptr [edx+ecx]
  sub     ecx, 8
  jg      @BwdLoop
  pop     ecx
  fistp   qword ptr [edx] {First 8}
  fistp   qword ptr [edx+ecx] {Last 8}
@@LargeDone:
  ret
@@M01:
  movzx   ecx, [eax]
  mov     [edx], cl
  ret
@@M02:
  movzx   ecx, word ptr [eax]
  mov     [edx], cx
  ret
@@M03:
  mov     cx, [eax]
  mov     al, [eax+2]
  mov     [edx], cx
  mov     [edx+2], al
  ret
@@M04:
  mov     ecx, [eax]
  mov     [edx], ecx
  ret
@@M05:
  mov     ecx, [eax]
  mov     al, [eax+4]
  mov     [edx], ecx
  mov     [edx+4], al
  ret
@@M06:
  mov     ecx, [eax]
  mov     ax, [eax+4]
  mov     [edx], ecx
  mov     [edx+4], ax
  ret
@@M07:
  mov     ecx, [eax]
  mov     eax, [eax+3]
  mov     [edx], ecx
  mov     [edx+3], eax
  ret
@@M08:
  fild    qword ptr [eax]
  fistp   qword ptr [edx]
end;
{$ifend}
{$endif PURE_PASCAL}

function Scale_ReallocMem(aMemory: Pointer; aSize: Integer): Pointer;
var
  pm: PMemBlock;
  p: Pointer;
begin
  // ReAlloc can be misued as GetMem or FreeMem (documented in delphi help) so check what the user wants
  Assert(NativeUInt(aMemory) > $10000);

  // Normal realloc of exisiting data?
  if (aMemory <> nil) and (aSize > 0) then
  begin
    p  := Pointer(NativeUInt(aMemory) - SizeOf(TMemHeader));
    pm := PMemHeader(p).Owner;

    if pm <> nil then
    with pm^ do
    begin
      if (NativeUInt(aSize) <= Owner.FItemSize) then
      begin
        // new size smaller than current size
        if NativeUInt(aSize) >= (Owner.FItemSize shr 1) then
          Result := aMemory // no resize needed up to half the current item size
        else
        // too much downscaling: use move
        with GetSmallMemManager^ do
        begin
          Result := GetMem(aSize); // new mem
          if aMemory <> Result then
          begin
            Move(aMemory^, Result^, aSize); // copy (use smaller new size)
            FreeMem(aMemory); // free old mem
          end;
        end;
      end
      else
      with GetSmallMemManager^ do
      begin
        // new size bigger than current size
        Result := GetMem(aSize); // new mem
        if aMemory <> Result then
        begin
          Move(aMemory^, Result^, Owner.FItemSize); // copy (use smaller old size)
          FreeMem(aMemory); // free old mem
        end;
      end;
    end
    // was allocated via OldMM -> rely on OldMM for reallocation
    else
    begin
      Result := OldMM.ReallocMem(p, aSize + SizeOf(TMemHeader));
      TMemHeader(Result^).Owner := nil; // mark not from our memlist
      Result := Pointer(NativeUInt(Result) + SizeOf(TMemHeader) );
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


function Scale_GetMem(aSize: Integer): Pointer;
{$IFDEF HASINLINE}
begin
  Result := GetSmallMemManager.GetMem(aSize);
  Assert(NativeUInt(Result) > $10000);
end;
{$ELSE}
  {$IFDEF PURE_PASCAL}
  begin
    Result := GetSmallMemManager.GetMem(aSize);
    Assert(NativeUInt(Result) > $10000);
  end;
  {$ELSE}
  asm
    {$IFDEF INLINEGOWN}
    mov edx,eax
    mov eax,GOwnTlsOffset
    mov ecx,fs:[$00000018]
    mov eax,[ecx+eax]      // fixed offset, calculated only once
    or eax,eax
    jnz TThreadMemManager.GetMem
    push edx
    call CreateSmallMemManager
    pop edx
    jmp TThreadMemManager.GetMem
    {$ELSE}
    push eax
    call GetSmallMemManager
    pop edx
    jmp TThreadMemManager.GetMem
    {$endif}
  end;
  {$ENDIF}
{$ENDIF}

function Scale_AllocMem(aSize: Cardinal): Pointer;
begin
  Result := GetSmallMemManager.GetMem(aSize);
  Assert(NativeUInt(Result) > $10000);
  fillchar(Result^, aSize, 0); // AllocMem() = GetMem()+ZeroMemory()
end;

function Scale_FreeMem(aMemory: Pointer): Integer;
{$IFDEF HASINLINE}
begin
  Assert(NativeUInt(aMemory) > $10000);
  Result := GetSmallMemManager.FreeMem(aMemory);
end;
{$ELSE}
  {$IFDEF PURE_PASCAL}
  begin
    Assert(NativeUInt(aMemory) > $10000);
    Result := GetSmallMemManager.FreeMem(aMemory);
  end;
  {$ELSE}
  asm
    {$IFDEF INLINEGOWN}
    mov edx,eax
    mov eax,GOwnTlsOffset
    mov ecx,fs:[$00000018]
    mov eax,[ecx+eax]      // fixed offset, calculated only once
    or eax,eax
    jnz TThreadMemManager.FreeMem
    push edx
    call CreateSmallMemManager
    pop edx
    jmp TThreadMemManager.FreeMem
    {$ELSE}
    push eax
    call GetSmallMemManager
    pop edx
    jmp TThreadMemManager.FreeMem
    {$endif}
  end;
  {$ENDIF}
{$ENDIF}

{$ifdef USEMEMMANAGEREX}
function Scale_RegisterMemoryLeak(P: Pointer): Boolean;
begin
  { TODO : implement memory leak checking }
  Result := OldMM.RegisterExpectedMemoryLeak(p);
end;

function Scale_UnregisterMemoryLeak(P: Pointer): Boolean;
begin
  Result := OldMM.UnregisterExpectedMemoryLeak(p);
end;
{$endif}

type
  TEndThread = procedure(ExitCode: Integer);
var
  OldEndThread: TEndThread;

procedure NewEndThread(ExitCode: Integer); //register; // ensure that calling convension matches EndThread
begin
  // free all thread mem
  GlobalManager.FreeThreadManager( GetSmallMemManager );
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
var
  NewCode: TJump = (OpCode  : $E9;
                    Distance: 0);

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

procedure ScaleMMInstall;
begin
  {$IFnDEF PURE_PASCAL}
  //get TLS slot
  GOwnTlsIndex  := TlsAlloc;
  //write fixed offset to TLS slot (instead calc via GOwnTlsIndex)
  _FixedOffset;
  {$ENDIF}

  // Hook memory Manager
  GetMemoryManager(OldMM);
  if @OldMM <> @ScaleMM_Ex then
    SetMemoryManager(ScaleMM_Ex);

  // init main thread manager
  GlobalManager.Init;

  // we need to patch System.EndThread to properly mark memory to be freed
  PatchThread;
end;

initialization
  ScaleMMInstall;

finalization
  { TODO : check for memory leaks }
  //GlobalManager.FreeAllMemory;

end.



