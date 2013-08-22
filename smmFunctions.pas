unit smmFunctions;

interface

{$Include smmOptions.inc}
{$I smmOptions.inc}

//uses
//  smmTypes;

type
  DWORD = LongWord;
  BOOL  = LongBool;

  PMemoryBasicInformation = ^TMemoryBasicInformation;
  _MEMORY_BASIC_INFORMATION = record
    BaseAddress : Pointer;
    AllocationBase : Pointer;
    AllocationProtect : DWORD;
    RegionSize : DWORD;
    State : DWORD;
    Protect : DWORD;
    Type_9 : DWORD;
  end;
  TMemoryBasicInformation = _MEMORY_BASIC_INFORMATION;

  PListEntry = ^TListEntry;
  _LIST_ENTRY = record
    Flink: PListEntry;
    Blink: PListEntry;
  end;
  TListEntry = _LIST_ENTRY;

  PRTLCriticalSection = ^TRTLCriticalSection;
  PRTLCriticalSectionDebug = ^TRTLCriticalSectionDebug;
  _RTL_CRITICAL_SECTION_DEBUG = record
    Type_18: Word;
    CreatorBackTraceIndex: Word;
    CriticalSection: PRTLCriticalSection;
    ProcessLocksList: TListEntry;
    EntryCount: DWORD;
    ContentionCount: DWORD;
    Spare: array[0..1] of DWORD;
  end;
  TRTLCriticalSectionDebug = _RTL_CRITICAL_SECTION_DEBUG;

  _RTL_CRITICAL_SECTION = record
    DebugInfo: PRTLCriticalSectionDebug;
    LockCount: Longint;
    RecursionCount: Longint;
    OwningThread: THandle;
    LockSemaphore: THandle;
    Reserved: DWORD;
  end;
  TRTLCriticalSection = _RTL_CRITICAL_SECTION;

const
  kernel32  = 'kernel32.dll';
  PAGE_EXECUTE_READWRITE = $40;
  PAGE_READWRITE = 4;
  MEM_COMMIT     = $1000;
  MEM_RESERVE    = $2000;
  MEM_DECOMMIT   = $4000;
  MEM_RELEASE    = $8000;
  MEM_FREE       = $10000;
  MEM_RESET      = $80000;
  MEM_TOP_DOWN   = $100000;

  function  TlsAlloc: DWORD; stdcall; external kernel32 name 'TlsAlloc';
  function  TlsGetValue(dwTlsIndex: DWORD): Pointer; stdcall; external kernel32 name 'TlsGetValue';
  function  TlsSetValue(dwTlsIndex: DWORD; lpTlsValue: Pointer): BOOL; stdcall; external kernel32 name 'TlsSetValue';
  function  TlsFree(dwTlsIndex: DWORD): BOOL; stdcall; external kernel32 name 'TlsFree';
  procedure Sleep(dwMilliseconds: DWORD); stdcall; external kernel32 name 'Sleep';
  function  SwitchToThread: BOOL; stdcall; external kernel32 name 'SwitchToThread';
  function  FlushInstructionCache(hProcess: THandle; const lpBaseAddress: Pointer; dwSize: DWORD): BOOL; stdcall; external kernel32 name 'FlushInstructionCache';
  function  GetCurrentProcess: THandle; stdcall; external kernel32 name 'GetCurrentProcess';
  function  GetCurrentThreadId: DWORD; stdcall; external kernel32 name 'GetCurrentThreadId';
  function  GetCurrentThread: THandle; stdcall; external kernel32 name 'GetCurrentThread';
  function  Scale_VirtualProtect(lpAddress: Pointer; dwSize, flNewProtect: DWORD;
              var OldProtect: DWORD): BOOL; stdcall; overload; external kernel32 name 'VirtualProtect';
  procedure ExitThread(dwExitCode: DWORD); stdcall; external kernel32 name 'ExitThread';
  function  VirtualAlloc(lpvAddress: Pointer; dwSize, flAllocationType, flProtect: DWORD): Pointer; stdcall; external kernel32 name 'VirtualAlloc';
  function  VirtualFree(lpAddress: Pointer; dwSize, dwFreeType: DWORD): BOOL; stdcall; external kernel32 name 'VirtualFree';
  function  VirtualQuery(lpAddress: Pointer; var lpBuffer: TMemoryBasicInformation; dwLength: DWORD): DWORD; stdcall; external kernel32 name 'VirtualQuery';

  procedure OutputDebugString(lpOutputString: PWideChar); stdcall; external kernel32 name 'OutputDebugStringW';
//  function  IntToStr(Value: Integer): string;overload;
//  function  IntToStr(Value: Pointer): string;overload;

  function SetPermission(Code: Pointer; Size, Permission: Cardinal): Cardinal;

  function  CAS32(aOldValue, aNewValue: Byte; aDestination: Pointer): boolean;overload;
  function  CAS32(aOldValue, aNewValue: NativeUInt; aDestination: Pointer): boolean;overload;
  function  CAS32(aOldValue, aNewValue: Pointer; aDestination: Pointer): boolean;overload;
  function  CAS64(const aOldData: Pointer; aOldReference: Cardinal; aNewData: Pointer; aNewReference: Cardinal; var aDestination): Boolean;
  procedure Move64(aNewData: Pointer; aNewReference: Cardinal; var aDestination);

  procedure InterlockedExchange(aTarget: Pointer; aValue: NativeUInt);
//  procedure InterlockedIncrement(var Value: Byte);overload;
//  procedure InterlockedDecrement(var Value: Byte);overload;
//  procedure InterlockedIncrement(var Value: Integer);overload;
//  procedure InterlockedDecrement(var Value: Integer);overload;
  function  InterlockedAdd(var Addend: Integer): Integer;
  function  BitScanLast(aValue: Word): NativeUInt;
  function  BitScanFirst(aValue: NativeInt): NativeUInt;

  function InterlockedExchangeAdd(var Addend: Longint; Value: Longint): Longint; stdcall; external kernel32 name 'InterlockedExchangeAdd';

  procedure InitializeCriticalSection(var lpCriticalSection: TRTLCriticalSection); stdcall; external kernel32 name 'InitializeCriticalSection';
  procedure EnterCriticalSection(var lpCriticalSection: TRTLCriticalSection); stdcall; external kernel32 name 'EnterCriticalSection';
  procedure LeaveCriticalSection(var lpCriticalSection: TRTLCriticalSection); stdcall; external kernel32 name 'LeaveCriticalSection';
  function  InitializeCriticalSectionAndSpinCount(var lpCriticalSection: TRTLCriticalSection; dwSpinCount: DWORD): BOOL; stdcall; external kernel32 name 'InitializeCriticalSectionAndSpinCount';
  function  SetCriticalSectionSpinCount(var lpCriticalSection: TRTLCriticalSection; dwSpinCount: DWORD): DWORD; stdcall; external kernel32 name 'SetCriticalSectionSpinCount';
  function  TryEnterCriticalSection(var lpCriticalSection: TRTLCriticalSection): BOOL; stdcall; external kernel32 name 'TryEnterCriticalSection';
  procedure DeleteCriticalSection(var lpCriticalSection: TRTLCriticalSection); stdcall; external kernel32 name 'DeleteCriticalSection';

  {$ifdef SCALEMM_DEBUG}
  procedure Assert(aCondition: boolean);
  {$ENDIF}

  {$ifndef PURE_PASCAL}
  {$if CompilerVersion < 19}
  procedure Move(const Source; var Dest; Count: Integer);
  {$ifend}
  {$endif PURE_PASCAL}


implementation

//uses
//  SysUtils;

function SetPermission(Code: Pointer; Size, Permission: Cardinal): Cardinal;
begin
  Assert(Assigned(Code) and (Size > 0));
  { Flush the instruction cache so changes to the code page are effective immediately }
  if Permission <> 0 then
    if FlushInstructionCache(GetCurrentProcess, Code, Size) then
      Scale_VirtualProtect(Code, Size, Permission, Longword(Result));
end;

{
function  IntToStr(Value: Integer): string;
begin
  Result := SysUtils.IntToHex(Value, 8);
end;

function  IntToStr(Value: Pointer): string;overload;
begin
  Result := SysUtils.IntToHex(Integer(Value), 8);
end;
}

////////////////////////////////////////////////////////////////////////////////
//Assembly functions

procedure InterlockedExchange(aTarget: Pointer; aValue: NativeUInt);
asm
{ -> EAX Target }
{ EDX Value }
{ <- EAX Result }
//{$IFDEF INTERLOCKEDWIN32}
//          MOV ECX, EAX
//          MOV EAX, [ECX]
//@@1:
//LOCK CMPXCHG [ECX], EDX
//          JNZ @@1
//{$ELSE}
//          XCHG EAX, EDX
  LOCK XCHG [aTarget], aValue // LOCK is implicit on XCHG with memory
//{$ENDIF}
end;

function  CAS32(aOldValue, aNewValue: Pointer; aDestination: Pointer): boolean;overload;
asm
  //                          Win32 Win64
  // aOldValue     : byte    EAX   RCX
  // aNewValue     : byte    EDX   RDX
  // aDestination  : pointer ECX   R8
{$IFDEF CPU386}
  lock cmpxchg [aDestination], aNewValue
  setz al
{$ELSE} .NOFRAME
  mov  rax, aOldValue
  lock cmpxchg [aDestination], aNewValue
  setz  al
{$endif}
end;

function  CAS32(aOldValue, aNewValue: NativeUInt; aDestination: Pointer): boolean;overload;
asm
  //                          Win32 Win64
  // aOldValue     : byte    EAX   RCX
  // aNewValue     : byte    EDX   RDX
  // aDestination  : pointer ECX   R8
{$IFDEF CPU386}
  lock cmpxchg [aDestination], aNewValue
  setz al
{$ELSE} .NOFRAME
  mov  rax, aOldValue
  lock cmpxchg [aDestination], aNewValue
  setz  al
{$endif}
end;

function CAS32(aOldValue: Byte; aNewValue: Byte; aDestination: Pointer): boolean;overload;
asm
  //                          Win32 Win64
  // aOldValue     : byte    AL    CL
  // aNewValue     : byte    DL    DL
  // aDestination  : pointer ECX   R8
{$IFDEF CPU386}
  lock cmpxchg [aDestination],dl
  setz al
{$ELSE} .NOFRAME
  mov  al, cl
  lock cmpxchg [aDestination], aNewValue
  setz  al
{$endif}
end;

function  CAS64(const aOldData: Pointer; aOldReference: Cardinal; aNewData: Pointer; aNewReference: Cardinal; var aDestination): Boolean;
asm
  push  edi
  push  ebx
  mov   ebx, aNewData
  mov   ecx, aNewReference
  mov   edi, aDestination
  lock cmpxchg8b qword ptr [edi]
  setz  al
  pop   ebx
  pop   edi
end; { CAS64 }

procedure Move64(aNewData: Pointer; aNewReference: Cardinal; var aDestination);
//Move 8 bytes atomically into 8-byte Destination!
asm
  movd  xmm0, eax
  movd  xmm1, edx
  punpckldq xmm0, xmm1
  movq  qword [aDestination], xmm0
end; { Move64 }

(*
procedure InterlockedIncrement(var Value: Byte);
asm
  lock inc byte [Value]
end;

procedure InterlockedDecrement(var Value: Byte);
asm
  lock dec byte [Value]
end;

procedure InterlockedIncrement(var Value: Integer);
asm
  lock inc [Value]
end;
*)

function InterlockedAdd(var Addend: Integer): Integer;
{ @Addend: EAX }
{ Result:  EAX }
asm
      MOV  EDX, EAX
      MOV  EAX, 1
 LOCK XADD [EDX], EAX
      INC  EAX
end;

procedure InterlockedDecrement(var Value: Integer);
asm
  lock dec [Value]
end;

//find first bit (0..31)
//http://www.arl.wustl.edu/~lockwood/class/cs306/books/artofasm/Chapter_6/CH06-4.html
function BitScanFirst(aValue: NativeInt): NativeUInt;
asm
{$IFDEF CPU386}
  BSF	EAX, aValue;
{$ELSE} .NOFRAME
  BSF	RAX, aValue;
{$ENDIF}
end;

function BitScanLast(aValue: Word): NativeUInt;
asm
{$IFDEF CPU386}
  BSR	AX, aValue;
{$ELSE} .NOFRAME
  BSR	AX, aValue;
{$ENDIF}
end;

////////////////////////////////////////////////////////////////////////////////

{$ifNdef SCALEMM_DEBUG}
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
      int 3;   // breakpoint
    end;
    //Sleep(0);  // no exception, just dummy for breakpoint
    {$WARN SYMBOL_PLATFORM OFF}
    if DebugHook = 0 then
      Error(reInvalidPtr);
  end;
end;
{$ENDIF}

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

end.
