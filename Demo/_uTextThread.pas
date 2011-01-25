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

type
  TTestRecord2 = record
    P1: Integer;
    P2: Integer;
    P3: Integer;
    P4: Integer;
    P5: Integer;
    P6: Integer;
    P7: Integer;
    P8: Integer;

    procedure Init;
  end;
  PTestRecord2 = ^TTestRecord2;

//var
//  r1, r2, r3: PTestRecord2;

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

function CAS32i(const oldValue: integer; newValue: integer; var destination): boolean;
asm
  lock cmpxchg dword ptr [destination], newValue
  setz  al
  jz @ok
  pause // let the CPU know this thread is in a Spin Wait loop
@ok:
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

var
//  GInnerLock,
//  GOuterLock: Cardinal;
  ia: array of Integer;

procedure test;
var
  i: integer;
begin
  SetLength(ia, 10000);
  for i := 0 to 10000-1 do
    ia[i] := i;
end;

procedure TDummyStringThread.Execute;
var
//  s, stest: AnsiString;
  s, stest: String;
  pa, pb, pc: pointer;
  p1, p2, p3: pointer;
  i,j,k,l:integer;
  tStart: TDateTime;
  obj1, obj2, obj3: TObject;
  ws: WideString;

  r1, r2, r3: PTestRecord2;

  iCurrentTID: Cardinal;
//  pOld, pNew, pDest: pointer;
//  ia: array of integer;
begin
    //  SetThreadPriority( Self.Handle, THREAD_PRIORITY_ABOVE_NORMAL);

  iCurrentTID := GetCurrentThreadId;

//  p1 := Scale_GetMem(100 * 1000);
//  Scale_FreeMem(p1);

//  p1 := GetMemory(255);
//  FreeMem(p1);
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

  (*
  for l := 0 to 1000 do
  for i := 0 to 10000-1 do
  begin
    j := ia[i];
    k := j+1;
    while not CAS32i(j, k, ia[i]) do
    begin
      if not SwitchToThread then
        Sleep(0);
      j := ia[i];
      k := j+1;
      if CAS32i(j, k, ia[i]) then
        Break
      else
         Sleep(1);
    end;
  end;
  FDuration_msec := MilliSecondsBetween(now, tStart);
  Exit;
  *)


//  pa := GetMemory(10);
//  pb := GetMemory(40);
//  pc := GetMemory(80);

//  GThreadManager.Init;

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

      {
      p1 := GetMemory(10);
      p2 := GetMemory(40);
      p3 := GetMemory(80);

      p1 := ReallocMemory(p1, 30);
      p2 := ReallocMemory(p2, 60);
      p3 := ReallocMemory(p3, 120);

      FreeMemory(p1);
      FreeMemory(p2);
      FreeMemory(p3);
      }

//      {
      p1 := GetMemory(10 * 1024);
      p2 := GetMemory(40 * 1024);
      p3 := GetMemory(80 * 1024);

      p1 := ReallocMemory(p1, 10 * 1024 + 10);
      p2 := ReallocMemory(p2, 40 * 1024 + 10);
      p3 := ReallocMemory(p3, 80 * 1024 + 10);

      FreeMemory(p1);
      FreeMemory(p2);
      FreeMemory(p3);
//      }


      (*
      r1 := ScaleMM2.GThreadManager.GetMem(SizeOf(TTestRecord2) + 8);
//      r1.Init;
//      assert(r1.P1 = 111111111);
      r2 := ScaleMM2.GThreadManager.GetMem(40 + 8);
//      r2.Init;
//      assert(r1.P1 = 111111111);
      r3 := ScaleMM2.GThreadManager.GetMem(80 + 8);
//      r3.Init;
//      assert(r1.P1 = 111111111);

      r1 := ScaleMM2.GThreadManager.ReallocMem(r1, 40 + 8);
      r2 := ScaleMM2.GThreadManager.ReallocMem(r2, 60 + 8);
      r3 := ScaleMM2.GThreadManager.ReallocMem(r3, 120 + 8);

      ScaleMM2.GThreadManager.FreeMem(r1);
      ScaleMM2.GThreadManager.FreeMem(r2);
      ScaleMM2.GThreadManager.FreeMem(r3);
      *)

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

{ TTestRecord2 }

procedure TTestRecord2.Init;
begin
  P1 := 111111111;
  P2 := 222222222;
  P3 := 333333333;
  P4 := 444444444;
  P5 := 555555555;
  P6 := 666666666;
  P7 := 777777777;
  P8 := 888888888;
end;

initialization
  test;
  LinkTest;

end.

