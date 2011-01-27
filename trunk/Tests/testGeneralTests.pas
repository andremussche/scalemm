unit testGeneralTests;

interface

uses
  TestFramework, Classes, smmArrayTest;

type
  TGeneralTests = class(TTestCase)
  strict private
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ZeroOrNegativeAlloc_Test;
    procedure AllocAllThenFreeAll_Test;
    procedure AllocAllReallocThenFreeAll_Test;
    procedure MediumBlockPreviousMemFree_Test;
    procedure ReallocFromSmallToLargeToSmall_Test;
  end;

implementation

procedure TGeneralTests.AllocAllReallocThenFreeAll_Test;
begin
  AllocAllReallocThenFreeAll(0, 1024 * 10, 1, 1);  //0-10kb
end;

procedure TGeneralTests.AllocAllThenFreeAll_Test;
begin
  AllocAllThenFreeAll(0, 1024 * 20, 1);  //0 - 20kb
end;

procedure TGeneralTests.MediumBlockPreviousMemFree_Test;
begin
  MediumBlockPreviousMemFree;
end;

procedure TGeneralTests.ReallocFromSmallToLargeToSmall_Test;
begin
  ReallocFromSmallToLargeToSmall;
end;

procedure TGeneralTests.SetUp;
begin
end;

procedure TGeneralTests.TearDown;
begin
end;

procedure TGeneralTests.ZeroOrNegativeAlloc_Test;
var p: Pointer;
begin
  p := GetMemory(0);
  Assert(p = nil);
  p := GetMemory(-1);
  Assert(p = nil);
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TGeneralTests.Suite);
end.


