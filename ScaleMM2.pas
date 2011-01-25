// fast scaling memory manager for Delphi
// licensed under a MPL/GPL/LGPL tri-license; version 0.1
unit ScaleMM2;

{$Include smmOptions.inc}

{
Fast Scaling Memory Manager 1.1 for Delphi

Description:
  Simple, small and compact MM, built on top of the main Memory Manager
  (FastMM4 is a good candidate, standard since Delphi 2007), architectured
  in order to scale on multi core CPU's (which is what FastMM4 is lacking).

Homepage:
  http://code.google.com/p/scalemm
  by André Mussche (andre.mussche@gmail.com)

Usage:
 - Delphi 6 up to Delphi 2005 with FastMM4:
   Place FastMM4 as the very first unit under the "uses" clause of your
   project's .dpr file THEN add SynScaleMM to the "uses" clause
 - Delphi 6 up to Delphi 2005 with no FastMM4 or Delphi 2006 up to Delphi XE:
   Place SynScaleMM as the very first unit under the "uses" clause of your
   project's .dpr file.

License:
  Released under Mozilla Public License 1.1

  Modifications by A.Bouchez - http://synopse.info:
  - Compiles from Delphi 6 up to Delphi XE;
  - Some pascal code converted to faster asm;
  - Some code refactoring, a lot of comments added;
  - Added medium block handling from 2048 bytes up to 16384;
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

Change log:
 Version 1.0 (3 December 2010):
  - First stable version
 Version 1.1 (6 December 2010):
  - Some optimizations for better "Fast Code MM Challenge Benchmark" results
    (lower memory overhead, more memory reuse, less locking)
 Version 2.0a (25 Januari 2011):
  - added medium memory handling (<1Mb), large memory is direct done via VirtualAlloc etc
  - splitted in seperate units to make developing/testing easier
  = empty units for stats and logging (will be implemented later)
}

interface

uses
  smmStatistics, smmLogging,
  smmSmallMemory, smmMediumMemory, smmLargeMemory;

type
  PThreadMemManager = ^TThreadMemManager;

  /// handles per-thread memory managment
  TThreadMemManager = object
  public
    FThreadId: LongWord;
    FThreadTerminated: Boolean;  //is this thread memory available to new thread?

    FSmallMemManager : TSmallMemThreadManager;
    FMediumMemManager: TMediumThreadManager;
    FLargeMemManager : TLargeMemThreadManager;

    {$IFDEF SCALEMM_STATISTICS}
    FStatistic: TMemoryStatistics;
    {$ENDIF}
    {$IFDEF SCALEMM_LOGGING}
    FLogging: TMemoryLogging;
    {$ENDIF}

    // link to list of items to reuse after thread terminated
    FNextThreadManager: PThreadMemManager;
  protected
    FOtherThreadFreeLock: Boolean;
    /// link to the list of mem freed in other thread
    FOtherThreadFreedMemory: Pointer;
    procedure ProcessFreedMemFromOtherThreads;

    procedure AddFreedMemFromOtherThread(aMemory: Pointer);
    function  ReallocMemFromOtherThread(aMemory: Pointer; aSize: Integer): Pointer;
  public
    procedure Init;
    procedure Reset;

    function GetMem(aSize: NativeInt) : Pointer;                       {$ifdef HASINLINE}inline;{$ENDIF}
    function FreeMem(aMemory: Pointer): NativeInt;                     {$ifdef HASINLINE}inline;{$ENDIF}
    function ReallocMem(aMemory: Pointer; aSize: NativeUInt): Pointer; {$ifdef HASINLINE}inline;{$ENDIF}
  end;

  function Scale_GetMem(aSize: Integer): Pointer;
  function Scale_AllocMem(aSize: Cardinal): Pointer;
  function Scale_FreeMem(aMemory: Pointer): Integer;
  function Scale_ReallocMem(aMemory: Pointer; aSize: Integer): Pointer;

  function GetThreadMemManager: PThreadMemManager;

implementation

// Windows.pas unit dependency should be not used -> seperate file
uses
  smmFunctions, smmGlobal, smmTypes;

/// internal GetThreadMemManager function is 2% faster with an injected offset
{$DEFINE SCALE_INJECT_OFFSET}

// inlined TLS access
// - injected offset + GetThreadMemManager call can be slower than offset loading
{$ifdef INLINEGOWN}
  {$ifndef HASINLINE} // inlined Getmem/Freemem will call GetThreadMemManager
    {$UNDEF SCALE_INJECT_OFFSET}
  {$endif}
{$endif}

{$if CompilerVersion >= 17}
  {$define USEMEMMANAGEREX}
{$ifend}

function CreateMemoryManager: PThreadMemManager; forward;

{$IFDEF PURE_PASCAL}
threadvar
  GCurrentThreadManager: PThreadMemManager;

function GetThreadMemManager: PThreadMemManager; {$ifdef HASINLINE}inline;{$ENDIF}
begin
  Result := GCurrentThreadManager;
  if Result = nil then
    Result := CreateMemoryManager;
end;
{$ELSE}
var
  GOwnTlsIndex,
  GOwnTlsOffset: NativeUInt;

function GetThreadMemManager: PThreadMemManager;
asm
{$IFDEF SCALE_INJECT_OFFSET}
  mov eax,123456789        // dummy value: calc once and inject at runtime
{$ELSE}
  mov eax,GOwnTlsOffset    // 2% slower, so we default use injected offset
{$ENDIF}
  mov ecx,fs:[$00000018]
  mov eax,[ecx+eax]      // fixed offset, calculated only once
  or eax,eax
  jz CreateMemoryManager
end;

procedure _FixedOffset;
{$IFDEF SCALE_INJECT_OFFSET}
var p: PAnsiChar;
{$ENDIF}
begin
  GOwnTlsOffset := GOwnTlsIndex * 4 + $0e10;
  {$IFDEF SCALE_INJECT_OFFSET}
  p  := @GetThreadMemManager;
  SetPermission(p, 5, PAGE_EXECUTE_READWRITE);
  PCardinal(p+1)^ := GOwnTlsOffset;  // write fixed offset
  {$ENDIF}
end;
{$ENDIF PURE_PASCAL}

function CreateMemoryManager: PThreadMemManager;
begin
  Result := GlobalManager.GetNewThreadManager;
  if Result = nil then
  begin
    Result := VirtualAlloc( nil,
                            SizeOf(TThreadMemManager),
                            MEM_COMMIT {$ifdef AlwaysAllocateTopDown} or MEM_TOP_DOWN{$endif},
                            PAGE_READWRITE);
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

{ TThreadMemManager }

procedure TThreadMemManager.ProcessFreedMemFromOtherThreads;
//var
//  pcurrentmem, ptempmem: PSmallMemHeader;
begin
  { TODO -oAM : process interthread mem }
  (*
  //reset first item (to get all mem in linked list)
  repeat
    pcurrentmem := FOtherThreadFreedMemory;
    if CAS32(pcurrentmem, nil, FOtherThreadFreedMemory) then
      Break;
    if not SwitchToThread then
      sleep(0);
    pcurrentmem := FOtherThreadFreedMemory;
    if CAS32(pcurrentmem, nil, FOtherThreadFreedMemory) then
      Break;
    sleep(1);
  until false;

  //free all mem in linked list
  while pcurrentmem <> nil do
  begin
    ptempmem    := pcurrentmem;
    pcurrentmem := pcurrentmem.NextMem;

    //free
    ptempmem.Owner.FreeMem(ptempmem);
  end;
  *)
end;

function TThreadMemManager.ReallocMem(aMemory: Pointer;
  aSize: NativeUInt): Pointer;
var
  pm: PBaseMemHeader;
  ot: PBaseThreadMemory;
begin
  if FOtherThreadFreedMemory <> nil then
    ProcessFreedMemFromOtherThreads;

  pm := PBaseMemHeader(NativeUInt(aMemory) - SizeOf(TBaseMemHeader));
  ot := pm.OwnerBlock.OwnerThread;

  if ot = @FSmallMemManager then
    Result := FSmallMemManager.ReallocMem(aMemory, aSize)
  else if ot = @FMediumMemManager then
    Result := FMediumMemManager.ReallocMem(aMemory, aSize)
  else if ot = @FLargeMemManager then
    Result := FLargeMemManager.ReallocMemWithHeader(aMemory, aSize)
  else
    Result := ReallocMemFromOtherThread(aMemory, aSize);
end;

function TThreadMemManager.ReallocMemFromOtherThread(aMemory: Pointer;
  aSize: Integer): Pointer;
begin
  Result := Scale_GetMem(aSize);
  { TODO -oAM : use header.size? }
  Move(aMemory^, Result^, aSize); // copy (use bigger? new size)
  Scale_FreeMem(aMemory);
end;

procedure TThreadMemManager.Reset;
begin
  FThreadId := 0;
  FThreadTerminated := True;
  FOtherThreadFreedMemory := nil;
  FNextThreadManager := nil;

  FSmallMemManager.Reset;
  FMediumMemManager.Reset;
end;

procedure TThreadMemManager.AddFreedMemFromOtherThread(aMemory: Pointer);
var
  ph: PBaseMemHeader;
begin
  ph := PBaseMemHeader(NativeUInt(aMemory) - SizeOf(TBaseMemHeader));

  //large mem can be direct freed
  if ph.OwnerBlock.OwnerThread.SizeType = stLarge then
  begin
    FLargeMemManager.FreeMem(aMemory);
    Exit;
  end;

  { TODO -oAM : interthread memory, buffer 25 or 1mb of mem, then release via
    globalmanager with a lock}

  //LOCK
  (*
  while not CAS32(0, 1, ph.OwnerBlock.OwnerThread.FOtherThreadFreeLock) do
  begin
    //small wait: try to swith to other pending thread (if any) else direct continue
    if not SwitchToThread then
      sleep(0);
    //try again
    if CAS32(0, 1, FOtherThreadFreeLock) then
      Break;
    //wait some longer: force swith to any other thread
    sleep(1);
  end;
  FOtherThreadFreeLock := False;
  *)
end;

function TThreadMemManager.FreeMem(aMemory: Pointer): NativeInt;
var
  pm: PBaseMemHeader;
  ot: PBaseThreadMemory;
begin
  if FOtherThreadFreedMemory <> nil then
    ProcessFreedMemFromOtherThreads;

  pm := PBaseMemHeader(NativeUInt(aMemory) - SizeOf(TBaseMemHeader));
  ot := pm.OwnerBlock.OwnerThread;

  if ot = @FSmallMemManager then
    Result := FSmallMemManager.FreeMem(aMemory)
  else if ot = @FMediumMemManager then
    Result := FMediumMemManager.FreeMem(aMemory)
  else if ot = @FLargeMemManager then
    Result := FLargeMemManager.FreeMemWithHeader(aMemory)
  else
  begin
    AddFreedMemFromOtherThread(aMemory);
    Result := 0;
  end;
end;

function TThreadMemManager.GetMem(aSize: NativeInt): Pointer;
begin
  if FOtherThreadFreedMemory <> nil then
    ProcessFreedMemFromOtherThreads;

  if aSize <= C_MAX_SMALLMEM_SIZE then   //-1 till 2048
  begin
    if aSize > 0 then
      Result := FSmallMemManager.GetMem(aSize)
    else
    begin
      Result := nil;
      Exit;
    end
  end
  else if aSize <= C_MAX_MEDIUMMEM_SIZE then  //till 1Mb
    Result := FMediumMemManager.GetMem(aSize)
  else
  begin
    Result := FLargeMemManager.GetMemWithHeader(aSize);
  end;
end;

procedure TThreadMemManager.Init;
begin
  FThreadId := GetCurrentThreadId;

  FSmallMemManager.Init;
  FSmallMemManager.OwnerManager  := @Self;

  FMediumMemManager.Init;
  FMediumMemManager.OwnerManager := @Self;

  FLargeMemManager.Init;
  FLargeMemManager.OwnerManager  := @Self;
end;

function Scale_ReallocMem(aMemory: Pointer; aSize: Integer): Pointer;
begin
  // ReAlloc can be misued as GetMem or FreeMem (documented in delphi help) so check what the user wants
  // Normal realloc of exisiting data?
  if (aMemory <> nil) and (aSize > 0) then
  begin
    Result := GetThreadMemManager.ReallocMem(aMemory, aSize);
  end
  else
  begin
    if (aMemory = nil) and (aSize > 0) then
      // GetMem disguised as ReAlloc
      Result := Scale_GetMem(aSize)
    else
    begin
      // FreeMem disguised as ReAlloc
      Result := nil;
      Scale_FreeMem(aMemory);
    end;
  end;
end;

function Scale_GetMem(aSize: Integer): Pointer;
{$IFDEF HASINLINE}
begin
  Result := GetThreadMemManager.GetMem(aSize);
end;
{$ELSE}
  {$IFDEF PURE_PASCAL}
  begin
    Result := GetThreadMemManager.GetMem(aSize);
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
    call CreateMemoryManager
    pop edx
    jmp TThreadMemManager.GetMem
    {$ELSE}
    push eax
    call GetThreadMemManager
    pop edx
    jmp TThreadMemManager.GetMem
    {$endif}
  end;
  {$ENDIF}
{$ENDIF}

function Scale_AllocMem(aSize: Cardinal): Pointer;
begin
  Result := Scale_GetMem(aSize);
  fillchar(Result^, aSize, 0); // AllocMem() = GetMem()+ZeroMemory()
end;

function Scale_FreeMem(aMemory: Pointer): Integer;
{$IFDEF HASINLINE}
begin
  Result := GetThreadMemManager.FreeMem(aMemory);
end;
{$ELSE}
  {$IFDEF PURE_PASCAL}
  begin
    Result := GetThreadMemManager.FreeMem(aMemory);
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
    call CreateMemoryManager
    pop edx
    jmp TThreadMemManager.FreeMem
    {$ELSE}
    push eax
    call GetThreadMemManager
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
//  Result := OldMM.RegisterExpectedMemoryLeak(p);
  Result := True;
end;

function Scale_UnregisterMemoryLeak(P: Pointer): Boolean;
begin
//  Result := OldMM.UnregisterExpectedMemoryLeak(p);
  Result := True;
end;
{$endif}

type
  TEndThread = procedure(ExitCode: Integer);
var
  OldEndThread: TEndThread;

procedure NewEndThread(ExitCode: Integer); //register; // ensure that calling convension matches EndThread
begin
  // free all thread mem
  GlobalManager.FreeThreadManager( GetThreadMemManager );
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

var
{$ifdef USEMEMMANAGEREX}
  OldMM: TMemoryManagerEx;
{$else}
  OldMM: TMemoryManager;
{$endif}

procedure ScaleMMInstall;
begin
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

  // init main thread manager
  GlobalManager.Init;

  // we need to patch System.EndThread to properly mark memory to be freed
  PatchThread;
end;

initialization
  ScaleMMInstall;

finalization
  { TODO : check for memory leaks }
  // GlobalManager.FreeAllMemory;

end.
