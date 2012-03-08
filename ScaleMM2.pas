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
 Version 2.02a (25 Januari 2011):
  - added medium memory handling (<1Mb), large memory is direct done via VirtualAlloc etc
  - splitted in seperate units to make developing/testing easier
  - empty units for stats and logging (will be implemented later)
 Version 2.04b (23 Februari 2011):
  - realloc optimizations
  - lots of internal CheckMem's (for validation)
  - interthread memory is now handled (alloc in thread 1, free in thread 2)
  - small (difficult to find) bugs fixed and other optimalizations
  - check for 8byte alignment (needed for OmniThreadLibrary etc)
 Version 2.05 (19 March 2011):
  - small size realloc bug fixed, now passes FastCode validations (D2007 and D2010)
 Version 2.10 (06 March 2012):
  - small bugs fixed
  - many additional checks added
  - interthread memory finally stable
  Note: can leak memory (or have increased mem usage) over time
 Version 2.11 (08 March 2012):
  - 64bit version (Delphi XE2) 
}

interface

uses
  smmStatistics, smmLogging,
  smmTypes,
  smmSmallMemory, smmMediumMemory, smmLargeMemory;

type
  PThreadMemManager = ^TThreadMemManager;

  /// handles per-thread memory managment
  TThreadMemManager = object
  public
    /// link to the list of mem freed in other thread
    FOtherThreadFreedMemory: PBaseFreeMemHeader;
    FOtherThreadFreeLock: Boolean;

    function  TryLock: boolean;
    procedure Lock;
    procedure UnLock;
  public
    FThreadId: NativeUint;
    FThreadTerminated: Boolean;  //is this thread memory available to new thread?

    // link to list of items to reuse after thread terminated
    FNextThreadManager: PThreadMemManager;

    //procedure AddFreeMemToOwnerThread(aFirstMem, aLastMem: PBaseFreeMemHeader);
  public
    FSmallMemManager : TSmallMemThreadManager;
    FMediumMemManager: TMediumThreadManager;
    FLargeMemManager : TLargeMemThreadManager;
  protected
    {$IFDEF SCALEMM_STATISTICS}
    FStatistic: TMemoryStatistics;
    {$ENDIF}
    {$IFDEF SCALEMM_LOGGING}
    FLogging: TMemoryLogging;
    {$ENDIF}
  protected
    procedure FreeMemOfOtherThread(aMemory: PBaseMemHeader);
    function  ReallocMemOfOtherThread(aMemory: Pointer; aSize: NativeUInt): Pointer;

    function  FreeMemFromOtherThread(aMemory: PBaseMemHeader): NativeInt;
  public
    procedure Init;
    procedure Reset;

    procedure CheckMem(aMemory: Pointer);

    function  IsMemoryFromOtherThreadsPresent: Boolean;
    procedure ProcessFreedMemFromOtherThreads;

    function GetMem(aSize: NativeInt) : Pointer;                       {$ifdef HASINLINE}inline;{$ENDIF}
    function FreeMem(aMemory: Pointer): NativeInt;                     {$ifdef HASINLINE}inline;{$ENDIF}
    function ReallocMem(aMemory: Pointer; aSize: NativeUInt): Pointer; {$ifdef HASINLINE}inline;{$ENDIF}
  end;

  {$if CompilerVersion >= 23}  //Delphi XE2
  function Scale_GetMem(aSize: NativeInt)   : Pointer;
  function Scale_AllocMem(aSize: NativeInt): Pointer;
  function Scale_ReallocMem(aMemory: Pointer; aSize: NativeInt): Pointer;
  {$else}
  function Scale_GetMem(aSize: Integer)   : Pointer;
  function Scale_AllocMem(aSize: Cardinal): Pointer;
  function Scale_ReallocMem(aMemory: Pointer; aSize: Integer): Pointer;
  {$ifend}
  function Scale_FreeMem(aMemory: Pointer): Integer;

  procedure Scale_CheckMem(aMemory: Pointer);

  function GetThreadMemManager: PThreadMemManager;
  function CreateMemoryManager: PThreadMemManager;

  procedure ScaleMMInstall;

{$IFDEF PURE_PASCAL}
threadvar
  GCurrentThreadManager: PThreadMemManager;
{$ENDIF}
implementation

// Windows.pas unit dependency should be not used -> seperate file
uses
  smmFunctions, smmGlobal;

{$IFDEF PURE_PASCAL}
function GetThreadMemManager: PThreadMemManager; {$ifdef HASINLINE}inline;{$ENDIF}
begin
  Result := GCurrentThreadManager;
  if Result = nil then
  begin
    Result := CreateMemoryManager;
    Assert(not Result.FThreadTerminated);
    Assert(not Result.FSmallMemManager.OwnerThread.FThreadTerminated);
  end;
  Assert(Result.FThreadId = GetCurrentThreadId);
  Assert(Result.FSmallMemManager.OwnerThread = PBaseThreadManager(Result));
  Assert(Result.FMediumMemManager.OwnerThread = PBaseThreadManager(Result));
  Assert(Result.FSmallMemManager.OwnerThread.FThreadId = GetCurrentThreadId);
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
                            //64 * 1024,
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

  {$IFDEF SCALEMM_DEBUG}
  Result.CheckMem(nil);
  {$ENDIF}

  {$IFDEF PURE_PASCAL}
  GCurrentThreadManager := Result;
  {$ELSE}
  TlsSetValue(GOwnTLSIndex, Result);
  {$ENDIF}
end;

{ TThreadMemManager }

procedure TThreadMemManager.ProcessFreedMemFromOtherThreads;
var
  pcurrentmem, ptempmem: PBaseFreeMemHeader;
begin
  if FOtherThreadFreedMemory = nil then Exit;
  //Assert(Self.FThreadId > 1);

  //LOCK
  Lock;

  pcurrentmem := FOtherThreadFreedMemory;
  FOtherThreadFreedMemory := nil;

  //UNLOCK
  UnLock;

  //free all mem in linked list
  while pcurrentmem <> nil do
  begin
    ptempmem    := pcurrentmem;
    pcurrentmem := pcurrentmem.NextThreadFree;

    //free
    Self.FreeMemFromOtherThread( PBaseMemHeader(ptempmem) );
  end;
end;

function TThreadMemManager.ReallocMem(aMemory: Pointer;
  aSize: NativeUInt): Pointer;
var
  pm: PBaseMemHeader;
  ot: PBaseSizeManager;
begin
  if FOtherThreadFreedMemory <> nil then
    ProcessFreedMemFromOtherThreads;

  pm := PBaseMemHeader(NativeUInt(aMemory) - SizeOf(TBaseMemHeader));
  //check realloc of freed mem
  if (pm.Size and 1 = 0) then
  begin
    //medium+large mem has ownerthread instead of ownerblock (optimization)
    if NativeUInt(pm.OwnerBlock) and 3 <> 0 then
    begin
      //other thread?
      if PThreadMemManager( NativeUInt(pm.OwnerBlock) and -4) <> @Self then
      begin
        Result := ReallocMemOfOtherThread(aMemory, aSize);
        Exit;
      end;

      //large or medium?
      if NativeUInt(pm.OwnerBlock) and 2 = 0 then
        Result := FMediumMemManager.ReallocMem(aMemory, aSize)
      else
        Result := FLargeMemManager.ReallocMemWithHeader(aMemory, aSize)
    end
    else
    //small mem
    begin
      ot := pm.OwnerBlock.OwnerManager;

      if ot = @FSmallMemManager then
        Result := FSmallMemManager.ReallocMem(aMemory, aSize)
      else
        Result := ReallocMemOfOtherThread(aMemory, aSize);
    end
  end
  else
  begin
    Result := nil;
    Error(reInvalidPtr);  //double free?
  end;

  {$IFDEF SCALEMM_DEBUG}
  CheckMem(nil);
  {$ENDIF}
end;

function TThreadMemManager.ReallocMemOfOtherThread(aMemory: Pointer;
  aSize: NativeUInt): Pointer;
var
  pm: PBaseMemHeader;
begin
  Result := Self.GetMem(aSize);

  pm := PBaseMemHeader(NativeUInt(aMemory) - SizeOf(TBaseMemHeader));
  if aSize > pm.Size then
    Move(aMemory^, Result^, pm.Size)  // copy (use smaller old size)
  else
    Move(aMemory^, Result^, aSize);   // copy (use smaller new size)

  Self.FreeMemOfOtherThread(pm);
end;

procedure TThreadMemManager.Reset;
begin
  FThreadId := 0;
  FThreadTerminated := True;
  //FOtherThreadFreedMemory := nil;
  FNextThreadManager := nil;

  FSmallMemManager.Reset;
  FMediumMemManager.Reset;
end;

function TThreadMemManager.TryLock: boolean;
begin
  Result := CAS32(0, 1, @FOtherThreadFreeLock);
end;

procedure TThreadMemManager.UnLock;
begin
  //if not CAS32(1, 0, @FOtherThreadFreeLock) then
  //  Assert(False);
  FOtherThreadFreeLock := False;
end;

procedure TThreadMemManager.Lock;
begin
  //unlock
  repeat
    if CAS32(0, 1, @FOtherThreadFreeLock) then
      Break;
    //small wait: try to swith to other pending thread (if any) else direct continue
    if not SwitchToThread then
      sleep(0);

    //try again
    if CAS32(0, 1, @FOtherThreadFreeLock) then
      Break;
    //wait some longer: force swith to any other thread
    sleep(1);
  until False;
end;

procedure TThreadMemManager.FreeMemOfOtherThread(aMemory: PBaseMemHeader);
var
  p: Pointer;
  pm: PMediumHeader;
begin
  //large mem can be direct freed
  if NativeUInt(aMemory.OwnerBlock) and 2 <> 0 then
  //if aMemory.OwnerBlock.OwnerThread.SizeType = stLarge then
  begin
    //convert to "client" pointer again to be able to use the normal functions
    p  := Pointer(NativeUInt(aMemory) + SizeOf(TBaseMemHeader));
    FLargeMemManager.FreeMem(p);
    Exit;
  end;

  //medium mem
  if NativeUInt(aMemory.OwnerBlock) and 3 <> 0 then
  begin
    pm := PMediumHeader( NativeUInt(aMemory) + SizeOf(TBaseMemHeader) - SizeOf(TMediumHeader));
    pm.ThreadFree;
  end
  //small mem
  else
    PSmallMemHeader(aMemory).OwnerBlock.ThreadFreeMem(PSmallMemHeader(aMemory));
end;

procedure TThreadMemManager.CheckMem(aMemory: Pointer);
var
  pm: PBaseMemHeader;
  ot: PBaseSizeManager;
  tm: PThreadMemManager;
begin
  if aMemory = nil then
  begin
    FSmallMemManager.CheckAllMem;
    FMediumMemManager.CheckMem(nil);
    Exit;
  end;

  Assert(aMemory <> nil);
  pm := PBaseMemHeader(NativeUInt(aMemory) - SizeOf(TBaseMemHeader));
  Assert(pm.OwnerBlock <> nil);  

  //medium or large mem?
  if NativeUInt(pm.OwnerBlock) and 3 <> 0 then
  begin
    //other thread?
    tm := PThreadMemManager( NativeUInt(pm.OwnerBlock) and -4);
    if tm <> @Self then
      Exit;  //cannot check mem of other thread!
    Assert(tm <> nil);

    //large or medium?
    if NativeUInt(pm.OwnerBlock) and 2 = 0 then
      tm.FMediumMemManager.CheckMem(aMemory)
    else
      tm.FLargeMemManager.CheckMem(aMemory);
  end
  else
  //small mem
  begin
    ot := pm.OwnerBlock.OwnerManager;
    PThreadMemManager(ot.OwnerThread).FSmallMemManager.CheckMem(aMemory);
  end;
end;

function TThreadMemManager.FreeMem(aMemory: Pointer): NativeInt;
var
  pm: PBaseMemHeader;
  ot: PBaseSizeManager;
  pt: PThreadMemManager;
begin
  if FOtherThreadFreedMemory <> nil then
    ProcessFreedMemFromOtherThreads;

  pm := PBaseMemHeader(NativeUInt(aMemory) - SizeOf(TBaseMemHeader));
  //check double free
  if (pm.Size and 1 <> 0) then
    Error(reInvalidPtr);

  //medium or large mem?
  if NativeUInt(pm.OwnerBlock) and 3 <> 0 then
  begin
    pt := PThreadMemManager( NativeUInt(pm.OwnerBlock) and -4);
    if pt <> @Self then
    //other thread?
    begin
      FreeMemOfOtherThread(pm);
      Result := 0;
      Exit;
    end;

    //large or medium?
    if NativeUInt(pm.OwnerBlock) and 2 = 0 then
      Result := FMediumMemManager.FreeMem(aMemory)
    else
      Result := FLargeMemManager.FreeMemWithHeader(aMemory)
  end
  else
  //small mem
  begin
    ot := pm.OwnerBlock.OwnerManager;

    if ot = @FSmallMemManager then
      Result := FSmallMemManager.FreeMem(aMemory)
    else
    begin
      FreeMemOfOtherThread(pm);
      Result := 0;
    end;
  end;

  {$IFDEF SCALEMM_DEBUG}
  CheckMem(nil);
  {$ENDIF}
end;

function TThreadMemManager.FreeMemFromOtherThread(
  aMemory: PBaseMemHeader): NativeInt;
var
  ot: PBaseSizeManager;
  op: PThreadMemManager;
  p:  Pointer;
begin
  //check double free
  if (aMemory.Size and 1 <> 0) then
    Error(reInvalidPtr);

  //convert to "client" pointer again to be able to use the normal functions
  p  := Pointer(NativeUInt(aMemory) + SizeOf(TBaseMemHeader));

  //large or medium?
  if NativeUInt(aMemory.OwnerBlock) and 3 <> 0 then
  begin
    op := PThreadMemManager( NativeUInt(aMemory.OwnerBlock) and -4);
    //check owner (can be changed in the meantime!)
    if op <> @Self then
    begin
      FreeMemOfOtherThread(aMemory);
      Result := 0;
      Exit;
    end;

    //large or medium?
    if NativeUInt(aMemory.OwnerBlock) and 2 = 0 then
      Result := FMediumMemManager.FreeMem(p)
    else
      Result := FLargeMemManager.FreeMemWithHeader(p)
  end
  else
  begin
    ot := aMemory.OwnerBlock.OwnerManager;
    if ot = @FSmallMemManager then
      Result := FSmallMemManager.FreeMem(p)
    else
    begin
      FreeMemOfOtherThread(aMemory);
      Result := 0;
    end;
  end;

  {$ifdef SCALEMM_DEBUG}
  if not Self.FThreadTerminated then
    Self.CheckMem(nil);
  {$ENDIF}
end;

{
procedure TThreadMemManager.AddFreeMemToOwnerThread(aFirstMem,
  aLastMem: PBaseFreeMemHeader);
begin
  //LOCK
  Lock;

  //put new mem to front of linked list
  aLastMem.NextThreadFree := FOtherThreadFreedMemory;
  FOtherThreadFreedMemory := aFirstMem;

  //UNLOCK
  Unlock;
end;
}

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
  else if aSize <= C_MAX_MEDIUMMEM_SIZE - SizeOf(TMediumHeader) then  //till 1Mb
    Result := FMediumMemManager.GetMem(aSize)
  else
  begin
    Result := FLargeMemManager.GetMemWithHeader(aSize);
  end;

  {$IFDEF Align8Bytes}
  Assert( NativeUInt(Result) AND 7 = 0);
  {$ENDIF}
  {$IFDEF Align16Bytes}
  Assert( NativeUInt(Result) AND 15 = 0);
  {$ENDIF}

  {$IFDEF SCALEMM_DEBUG}
  CheckMem(nil);
  {$ENDIF}
end;

procedure TThreadMemManager.Init;
begin
  FThreadId := GetCurrentThreadId;

  FSmallMemManager.Init;
  FSmallMemManager.OwnerThread  := @Self;

  FMediumMemManager.Init;
  FMediumMemManager.OwnerThread := @Self;

  FLargeMemManager.Init;
  FLargeMemManager.OwnerThread  := @Self;

  {$IFDEF SCALEMM_DEBUG}
  CheckMem(nil);
  {$ENDIF}
end;

function TThreadMemManager.IsMemoryFromOtherThreadsPresent: Boolean;
begin
  Result := FOtherThreadFreedMemory <> nil;
end;

{$if CompilerVersion >= 23}  //Delphi XE2
function Scale_ReallocMem(aMemory: Pointer; aSize: NativeInt): Pointer;
{$else}
function Scale_ReallocMem(aMemory: Pointer; aSize: Integer): Pointer;
{$ifend}
var
  pm: PBaseMemHeader;
  iSize: NativeUInt;
begin
  // ReAlloc can be misued as GetMem or FreeMem (documented in delphi help) so check what the user wants
  // Normal realloc of exisiting data?
  if (aMemory <> nil) and (aSize > 0) then
  begin
    //general resize: if size within 1/4 we do nothing (also possible in other thread!)
    //iSize := NativeUInt(Pointer(NativeUInt(aMemory) - SizeOf(TBaseMemHeader))^);
    pm    := PBaseMemHeader(NativeUInt(aMemory) - SizeOf(TBaseMemHeader));
    iSize := pm.Size;

    //downsize...
    if (NativeUInt(aSize) <= iSize) then
    begin
      Result := aMemory;
      if iSize <= C_MAX_SMALLMEM_SIZE then  //small mem?
      begin
        //within 1/4?
        if (NativeUInt(aSize) + 32 > iSize shr 2) then
          Exit;
      end
      else
      begin
        //medium + large mem has included their header size in the size too
        if iSize <= C_MAX_MEDIUMMEM_SIZE then  //medium mem?
        begin
          Assert( NativeUInt(PBaseMemHeader(NativeUInt(aMemory) - SizeOf(TBaseMemHeader)).OwnerBlock) and 2 = 0 ); //must be marked as medium!
          //within 1/2?
          if (NativeUInt(aSize) + SizeOf(TMediumHeader) <= iSize) then
          begin
             if (NativeUInt(aSize) > iSize shr 1) then
               Exit
          end
          else
          begin
            Result := GetThreadMemManager.ReallocMem(aMemory, aSize + (aSize shr 3));  //add extra size (12,5%)
            Exit;
          end;
        end
        else                                  //large mem
        begin
          Assert( NativeUInt(PBaseMemHeader(NativeUInt(aMemory) - SizeOf(TBaseMemHeader)).OwnerBlock) and 2 <> 0); //must marked as large!
          //within 1/2?
          if (NativeUInt(aSize) + SizeOf(TLargeHeader) {+ SizeOf(TLargeBlockMemory)} <= iSize) then
          begin
            if (NativeUInt(aSize) > iSize shr 1) then
              Exit
          end
          else
          begin
            Result := GetThreadMemManager.ReallocMem(aMemory, aSize + (aSize shr 3));  //add extra size (12,5%)
            Exit;
          end;
        end;
      end;

      //too much downsize: realloc anyway
      Result := GetThreadMemManager.GetMem(aSize);
      Move(aMemory^, Result^, aSize); // copy (use smaller new size)
      Scale_FreeMem(aMemory);  //free mem (possible from other thread!)
      Exit;
    end;

    //normal realloc
    Result := GetThreadMemManager.ReallocMem(aMemory, aSize + (aSize shr 2) );
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

{$if CompilerVersion >= 23}  //Delphi XE2
function Scale_GetMem(aSize: NativeInt): Pointer;
{$else}
function Scale_GetMem(aSize: Integer): Pointer;
{$ifend}
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

{$if CompilerVersion >= 23}  //Delphi XE2
function Scale_AllocMem(aSize: NativeInt): Pointer;
{$else}
function Scale_AllocMem(aSize: Cardinal): Pointer;
{$ifend}
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

procedure Scale_CheckMem(aMemory: Pointer);
begin
  GetThreadMemManager.CheckMem(aMemory);
end;

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
//var
//  OldEndThread: TEndThread;

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
    OpCode  : Byte;
    Distance: Integer;
  end;
//var
//  NewCode: TJump = (OpCode  : $E9;
//                    Distance: 0);

procedure FastcodeAddressPatch(const ASource, ADestination: Pointer);
const
  Size: Cardinal = SizeOf(TJump);
var
  NewJump: PJump;
  OldProtect: Cardinal;
begin
  if VirtualProtect(ASource, Size, PAGE_EXECUTE_READWRITE, OldProtect) then
  begin
    NewJump := PJump(ASource);
    NewJump.OpCode := $E9;
    NewJump.Distance := NativeUInt(ADestination) - NativeUInt(ASource) - Size;

    FlushInstructionCache(GetCurrentProcess, ASource, SizeOf(TJump));
    VirtualProtect(ASource, Size, OldProtect, OldProtect);
  end;
end;

// redirect calls to System.EndThread to NewEndThread
procedure PatchThread;
begin
  FastcodeAddressPatch(@EndThread, @NewEndThread);
end;
{
procedure PatchThread;
var
  pEndThreadAddr: PJump;
  iOldProtect: DWord;
begin
  pEndThreadAddr := PJump(@EndThread);
  Scale_VirtualProtect(pEndThreadAddr, SizeOf(TJump), PAGE_EXECUTE_READWRITE, iOldProtect);
  // calc jump to new function
  NewCode.Distance := Cardinal(@NewEndThread) - (Cardinal(@EndThread) + SizeOf(TJump));
  // store old
  OldEndThread := TEndThread(pEndThreadAddr);
  // overwrite with jump to new function
  pEndThreadAddr^  := NewCode;
  // flush CPU
  FlushInstructionCache(GetCurrentProcess, pEndThreadAddr, 5);
end;
}

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
  SetMemoryManager(OldMM);
  { TODO : check for memory leaks }
  GlobalManager.FreeAllMemory;

end.
