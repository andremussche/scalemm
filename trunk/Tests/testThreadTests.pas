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
  end;

implementation

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

