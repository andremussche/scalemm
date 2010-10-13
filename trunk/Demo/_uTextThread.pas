unit _uTextThread;

interface

uses
  Math, SysUtils,
  Classes, Windows;

type
  TDummyStringThread = class(TThread)
  private
    FDuration_msec: integer;
    procedure SetDuration_msec(const Value: integer);
  protected
  public
    procedure Execute; override;

    constructor Create;overload;
    property Duration_msec: integer read FDuration_msec write SetDuration_msec;
  end;

implementation

uses DateUtils, AnsiStrings;

{ TDummyStringThread }

//var
//  h1, h2, h3: PHeader;

//compare oldvalue with destination: if equal then newvalue is set
function CAS32(const oldValue: pointer; newValue: pointer; var destination): boolean;
asm
  lock cmpxchg dword ptr [destination], newValue
  setz  al
end; { CAS32 }

procedure TDummyStringThread.Execute;
var
  s, stest: AnsiString;
  pa, pb, pc: pointer;
  p1, p2, p3: pointer;
  i,j:integer;
  tStart: TDateTime;
//  pOld, pNew, pDest: pointer;
//  ia: array of integer;
begin
//  SetThreadPriority( Self.Handle, THREAD_PRIORITY_ABOVE_NORMAL);

  tStart := now;
  stest  := '12345678901234567890';

  pa := GetMemory(10);
  pb := GetMemory(40);
  pc := GetMemory(80);

  for j := 0 to 1000 do
    for i := 0 to 10000 do
    begin
//      s := '  ';
//      s := s + 'test';
//      s := 'bla';
//      s := '  ';
//      s := s + 'test';
//      s := 'bla';

{
      s := Copy(stest, 1, 10);
      s := Copy(s, 2, 8);
      s := Copy(s, 3, 4);
      stest := s + ' ' + s + ' ' + s;
      }

      p1 := GetMemory(10);
      p2 := GetMemory(40);
      p3 := GetMemory(80);

      p1 := ReallocMemory(p1, 30);
      p2 := ReallocMemory(p2, 60);
      p3 := ReallocMemory(p3, 120);

      FreeMem(p1);
      FreeMem(p2);
      FreeMem(p3);
    end;

  FDuration_msec := MilliSecondsBetween(now, tStart);
end;

constructor TDummyStringThread.Create;
begin
  Create(True);
end;

procedure TDummyStringThread.SetDuration_msec(const Value: integer);
begin
  FDuration_msec := Value;
end;

end.

