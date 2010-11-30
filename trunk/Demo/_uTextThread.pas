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

uses
  DateUtils, AnsiStrings;

{ TDummyStringThread }

//var
//  h1, h2, h3: PHeader;

//compare oldvalue with destination: if equal then newvalue is set
function CAS32nolock(const oldValue: Cardinal; newValue: Cardinal; var destination): boolean;
asm
  cmpxchg dword ptr [destination], newValue
  setz  al
end; { CAS32 }


//compare oldvalue with destination: if equal then newvalue is set
function CAS32(const oldValue: pointer; newValue: pointer; var destination): boolean;
asm
  lock cmpxchg dword ptr [destination], newValue
  setz  al
end; { CAS32 }

type
  PTestRecord = ^TTestRecord;
  TTestRecord = record
    Next: PTestRecord;
  end;

procedure LinkTest;
var
  p1, p2, p3: PTestRecord;
begin
  new(p1);
  ZeroMemory(p1, SizeOf(TTestRecord));

  new(p2);
  ZeroMemory(p1, SizeOf(TTestRecord));

  while not CAS32(p1.Next, p2, p1.Next) do
    sleep(0);

//  cool!
//  p3 := p1;
//  new(p3);

end;

//var
//  GInnerLock,
//  GOuterLock: Cardinal;

procedure TDummyStringThread.Execute;
var
//  s, stest: AnsiString;
  s, stest: String;
  pa, pb, pc: pointer;
  p1, p2, p3: pointer;
  i,j:integer;
  tStart: TDateTime;
  obj1, obj2, obj3: TObject;
  ws: WideString;

  iCurrentTID: Cardinal;
//  pOld, pNew, pDest: pointer;
//  ia: array of integer;
begin
//  SetThreadPriority( Self.Handle, THREAD_PRIORITY_ABOVE_NORMAL);

  iCurrentTID := GetCurrentThreadId;

  p1 := GetMemory(255);
  FreeMem(p1);
//  FreeMem(p1);
{
      p1 := GetMemory(10);
      p2 := GetMemory(40);
      p3 := GetMemory(80);

      p1 := ReallocMemory(p1, 30);
      p2 := ReallocMemory(p2, 60);
      p3 := ReallocMemory(p3, 120);

      FreeMem(p1);
      FreeMem(p2);
      FreeMem(p3);
      }

  tStart := now;
  stest  := '12345678901234567890';

//  pa := GetMemory(10);
//  pb := GetMemory(40);
//  pc := GetMemory(80);

  for j := 0 to 1000 do
    for i := 0 to 10000 do
//    for i := 0 to 100 do
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

      {
      ws := Copy(stest, 1, 10);
      ws := Copy(ws, 2, 8);
      ws := Copy(ws, 3, 4);
      stest := ws + ' ' + ws + ' ' + ws;
      }

//      {
      p1 := GetMemory(10);
      p2 := GetMemory(40);
      p3 := GetMemory(80);

      p1 := ReallocMemory(p1, 30);
      p2 := ReallocMemory(p2, 60);
      p3 := ReallocMemory(p3, 120);

      FreeMem(p1);
      FreeMem(p2);
      FreeMem(p3);
//      }

      {
      p1 := Scale_GetMem(10);
      p2 := Scale_GetMem(40);
      p3 := Scale_GetMem(80);

      p1 := Scale_ReallocMem(p1, 30);
      p2 := Scale_ReallocMem(p2, 60);
      p3 := Scale_ReallocMem(p3, 120);

      Scale_FreeMem(p1);
      Scale_FreeMem(p2);
      Scale_FreeMem(p3);
      }

      {
      obj1 := TObject.Create;
      obj2 := TObject.Create;
      obj3 := TObject.Create;
      obj1.Free;
      obj2.Free;
      obj3.Free;
      }
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

initialization
  LinkTest;

end.

