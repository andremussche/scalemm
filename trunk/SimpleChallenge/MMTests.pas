unit MMTests;

interface

uses
  Classes;

  procedure ExecuteTests(const aName: string);
  function  ExecuteAllTests_msec(const aThreadCount: integer): Double;

  procedure AllocAndFree(const aSize: Integer);
  procedure AllocAndFree_Count(const aSize, aCount: Integer);

type
  TTestThread_AF = class(TThread)
  protected
    procedure Execute;override;
  public
    Loop,
    AllocSize: Integer;

    constructor Create(aLoop, aAllocSize: integer);
  end;

  TTestThread_AFC = class(TThread)
  protected
    procedure Execute;override;
  public
    Loop,
    AllocSize: Integer;
    Count: Integer;

    constructor Create(aLoop, aAllocSize, aCount: integer);
  end;

implementation

uses
  DateUtils, SysUtils, Windows;

procedure ExecuteTests(const aName: string);
var
  tDuration_msec: Double;
begin
  tDuration_msec := MMTests.ExecuteAllTests_msec(1);
  Writeln(Format('%s, 1 thread  = %4.2f',[aName, tDuration_msec]));
  tDuration_msec := MMTests.ExecuteAllTests_msec(2);
  Writeln(Format('%s, 2 threads = %4.2f',[aName, tDuration_msec]));
  tDuration_msec := MMTests.ExecuteAllTests_msec(4);
  Writeln(Format('%s, 4 threads = %4.2f',[aName, tDuration_msec]));
  tDuration_msec := MMTests.ExecuteAllTests_msec(8);
  Writeln(Format('%s, 8 threads = %4.2f',[aName, tDuration_msec]));

  Writeln('');
end;

procedure AllocAndFree(const aSize: Integer);
var
  p: Pointer;
begin
  p := AllocMem(aSize);
  FreeMem(p);
end;

procedure AllocAndFree_Count(const aSize, aCount: Integer);
var
  ap: array of Pointer;
  i: Integer;
begin
  SetLength(ap, aCount);
  for i := 0 to High(ap) do
    ap[i] := AllocMem(aSize);
  for i := 0 to High(ap) do
    FreeMem(ap[i]);
  SetLength(ap, 0);
end;

{ TTestThread_AF }

constructor TTestThread_AF.Create(aLoop, aAllocSize: integer);
begin
  inherited Create(True);
  Loop := aLoop;
  AllocSize := aAllocSize;
end;

procedure TTestThread_AF.Execute;
var i: Integer;
begin
  for i := 1 to Loop do
    AllocAndFree(AllocSize);
end;

{ TTestThread_AFC }

constructor TTestThread_AFC.Create(aLoop, aAllocSize, aCount: integer);
begin
  inherited Create(True);
  Loop := aLoop;
  AllocSize := aAllocSize;
  Count := aCount;
end;

procedure TTestThread_AFC.Execute;
var i: Integer;
begin
  for i := 1 to Loop do
    AllocAndFree_Count(AllocSize, Count);
end;

function ExecuteAllTests_msec(const aThreadCount: integer): Double;
var
  at: array of TThread;

  procedure __StartThreadsForSize(aSize: integer);
  var i: Integer;
  begin
    for i := 0 to aThreadCount-1 do
      at[i] := TTestThread_AF.Create(100 * 1000, aSize);
    for i := 0 to aThreadCount-1 do at[i].Start;
    for i := 0 to aThreadCount-1 do at[i].WaitFor;
    for i := 0 to aThreadCount-1 do at[i].Free;

    for i := 0 to aThreadCount-1 do
      at[i] := TTestThread_AFC.Create(10 * 1000, aSize, 32);
    for i := 0 to aThreadCount-1 do at[i].Start;
    for i := 0 to aThreadCount-1 do at[i].WaitFor;
    for i := 0 to aThreadCount-1 do at[i].Free;
  end;

var
  tStart, tStop: TDateTime;
  iStart, iEnd, iFreq: Int64;
begin
  SetLength(at, aThreadCount);

  QueryPerformanceCounter(iStart);
  tStart := Now;
  try
    __StartThreadsForSize(1);
    __StartThreadsForSize(10);
    __StartThreadsForSize(100);
    __StartThreadsForSize(1000);
    __StartThreadsForSize(10000);
  except
  end;
  QueryPerformanceCounter(iEnd);
  tStop  := Now;
  Result := MilliSecondSpan(tStop, tStart);

  QueryPerformanceFrequency(iFreq);
  Result := ((iEnd - iStart) / iFreq) * 1000;
end;

end.
