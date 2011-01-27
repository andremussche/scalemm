unit smmThreadTest;

interface

uses
  Classes;

type
  TMediumAllocTestThread = class(TThread)
  protected
    procedure Execute;override;
  end;

implementation

{ TTestTest }

procedure TMediumAllocTestThread.Execute;
var
  p1, p2, p3, p4: Pointer;
begin
  p1 := GetMemory(10 * 1024);
  p2 := GetMemory(10 * 1024);
  p3 := GetMemory(10 * 1024);
  p4 := GetMemory(10 * 1024);

  FreeMemory(p1);
  FreeMemory(p2);
  FreeMemory(p3);
  FreeMemory(p4);
end;

end.
