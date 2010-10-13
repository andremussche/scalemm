unit uWinApiFunctions;

interface

type
  DWORD = LongWord;
  BOOL  = LongBool;

const
  PAGE_EXECUTE_READWRITE = $40;

function  TlsAlloc: DWORD; stdcall;
function  TlsGetValue(dwTlsIndex: DWORD): Pointer; stdcall;
function  TlsSetValue(dwTlsIndex: DWORD; lpTlsValue: Pointer): BOOL; stdcall;
function  TlsFree(dwTlsIndex: DWORD): BOOL; stdcall;
procedure Sleep(dwMilliseconds: DWORD); stdcall;
function  FlushInstructionCache(hProcess: THandle; const lpBaseAddress: Pointer; dwSize: DWORD): BOOL; stdcall;
function  GetCurrentProcess: THandle; stdcall;
function VirtualProtect(lpAddress: Pointer; dwSize, flNewProtect: DWORD;
  var OldProtect: DWORD): BOOL; stdcall; overload;

procedure ZeroMemory(Destination: Pointer; Length: DWORD); inline;

function  SetPermission(Code: Pointer; Size, Permission: Cardinal): Cardinal;

implementation

const
  kernel32  = 'kernel32.dll';

function  TlsAlloc; external kernel32 name 'TlsAlloc';
function  TlsFree; external kernel32 name 'TlsFree';
function  TlsGetValue; external kernel32 name 'TlsGetValue';
function  TlsSetValue; external kernel32 name 'TlsSetValue';
procedure Sleep; external kernel32 name 'Sleep';
function FlushInstructionCache; external kernel32 name 'FlushInstructionCache';
function GetCurrentProcess; external kernel32 name 'GetCurrentProcess';
//function VirtualProtect(lpAddress: Pointer; dwSize, flNewProtect: DWORD;
//  lpflOldProtect: Pointer): BOOL; external kernel32 name 'VirtualProtect';
function VirtualProtect(lpAddress: Pointer; dwSize, flNewProtect: DWORD;
  var OldProtect: DWORD): BOOL; external kernel32 name 'VirtualProtect';

procedure ZeroMemory(Destination: Pointer; Length: DWORD);
begin
  FillChar(Destination^, Length, 0);
end;

function SetPermission(Code: Pointer; Size, Permission: Cardinal): Cardinal;
begin
  Assert(Assigned(Code) and (Size > 0));
  { Flush the instruction cache so changes to the code page are effective
    immediately }
  if Permission <> 0 then
    if FlushInstructionCache(GetCurrentProcess, Code, Size) then
      if not VirtualProtect(Code, Size, Permission, Longword(Result)) then
        sleep(0);
end;

end.
