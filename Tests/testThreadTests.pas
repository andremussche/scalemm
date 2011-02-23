unit testThreadTests;

interface

uses
  TestFramework, Classes, smmThreadTest;

type
  TThreadTests = class(TTestCase)
  strict private
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure MediumMem_AllocFree_Test;
    procedure InterThreadAllocFree_Test;
    procedure DoubleInterThreadAllocFree_Test;
  end;

implementation

uses
  Windows;

procedure TThreadTests.DoubleInterThreadAllocFree_Test;
begin
  InterThreadAllocFree_Test;
  InterThreadAllocFree_Test;
end;

procedure TThreadTests.InterThreadAllocFree_Test;
var
  t1, t2: TInterThreadMemTestThread;
begin
  t1 := TInterThreadMemTestThread.Create(True);
  t2 := TInterThreadMemTestThread.Create(True);

  t1.OtherThread := t2;
  t2.OtherThread := t1;

  t1.Start;
  t2.Start;

  Sleep(10 * 1000);

  t1.Terminate;
  t2.Terminate;
  t1.WaitFor;
  t2.WaitFor;
  t1.Free;
  t2.Free;
end;

procedure TThreadTests.MediumMem_AllocFree_Test;
var
  _TestThread: TMediumAllocTestThread;
begin
  _TestThread := TMediumAllocTestThread.Create(False);
  _TestThread.WaitFor;
  _TestThread.Free;
end;

procedure TThreadTests.SetUp;
begin
end;

procedure TThreadTests.TearDown;
begin
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TThreadTests.Suite);
end.

