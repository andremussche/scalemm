unit ThreadHook;

interface

uses
  Windows;

type
  TEndThread = procedure(ExitCode: Integer);
var
  OldEndThread: TEndThread;

implementation

uses
  ScaleMM;

procedure NewEndThread(ExitCode: Integer); //register; // ensure that calling convension matches EndThread
begin
  //free all thread mem
  GlobalManager.FreeThreadManager( GetCurrentThreadManager );

  //OldEndThread(ExitCode);  todo: make trampoline with original begin etc
  //code of original EndThread;
  ExitThread(ExitCode);
end;

type
  PJump = ^TJump;

  TJump = packed record
    OpCode: Byte;
    Distance: Integer;
  end;

var
  //OldCode: TJump;
  NewCode: TJump = (OpCode  : $E9;
                    Distance: 0);

// redirect calls to System.EndThread to NewEndThread
procedure PatchThread;
var
  pEndThreadAddr: PJump;
  iOldProtect: DWord;
begin
  pEndThreadAddr   := Pointer(@EndThread);
  VirtualProtect(pEndThreadAddr, 5, PAGE_EXECUTE_READWRITE, iOldProtect);
  //calc jump to new function
  NewCode.Distance := Cardinal(@NewEndThread) - (Cardinal(@EndThread) + 5);
  //store old
  OldEndThread     := TEndThread(pEndThreadAddr);
  //overwrite with jump to new function
  pEndThreadAddr^  := NewCode;
  //flush CPU
  FlushInstructionCache(GetCurrentProcess, pEndThreadAddr, 5);
end;

initialization
  //PatchThread;

end.
