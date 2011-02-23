unit smmThreadTest;

interface

uses
  Classes, SyncObjs;

type
  TMediumAllocTestThread = class(TThread)
  protected
    procedure Execute;override;
  end;

  TInterThreadMemTestThread = class(TThread)
  private
    FOtherThread: TInterThreadMemTestThread;
    procedure SetOtherThread(const Value: TInterThreadMemTestThread);
  protected
//    FOtherThreadMem: array[0..63] of Pointer;
//    FOtherThreadMemCount: Integer;
    FLock: TCriticalSection;
    FMemory: TList;
    templist: TList;
    procedure ClearMem;

    procedure Execute;override;
  public
    procedure  AfterConstruction;override;
    destructor Destroy; override;

    procedure AddMemToOtherThread(aMemory: Pointer);

    property OtherThread: TInterThreadMemTestThread read FOtherThread write SetOtherThread;
  end;

implementation

uses
  smmFunctions;

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

{ TInterThreadMemTestThread }

procedure TInterThreadMemTestThread.AddMemToOtherThread(aMemory: Pointer);
//var i: Integer;
begin
  FLock.Enter;
  try
    FMemory.Add(aMemory);
  finally
    FLock.Leave;
  end;

  {
  i := InterlockedAdd(FOtherThreadMemCount);
  while i > High(FOtherThreadMem) do
  begin
    //reset
    //CAS32(i, 0, FOtherThreadMemCount);
    //wait till list is cleared
    while FOtherThreadMemCount > High(FOtherThreadMem) do
      Sleep(1);

    //get new position
    i := InterlockedAdd(FOtherThreadMemCount);
  end;

  FOtherThreadMem[i] := aMemory;
  }
end;

procedure TInterThreadMemTestThread.AfterConstruction;
begin
  inherited;
  FLock := TCriticalSection.Create;
  templist := TList.Create;
  FMemory  := TList.Create;
end;

procedure TInterThreadMemTestThread.ClearMem;
var
  i:integer;
begin
  //get all mem in one lock
  FLock.Enter;
  try
    templist.Assign(FMemory, laCopy);
    FMemory.Clear;
  finally
    FLock.Leave;
  end;

  //free mem from other thread
  for i := 0 to templist.Count-1 do
    FreeMemory(templist.Items[i]);
  templist.Clear;
end;

destructor TInterThreadMemTestThread.Destroy;
begin
  FLock.Enter;
  ClearMem;
  templist.Free;
  FLock.Free;
  FMemory.Free;
  inherited;
end;

function AllocAndFill(aSize: NativeUInt): Pointer;
begin
  Result := GetMemory(aSize);
  FillChar(Result^, aSize, 81);
end;

procedure TInterThreadMemTestThread.Execute;
begin
  while not Terminated do
  begin
    //mem added?
    if FMemory.Count > 0 then
      ClearMem;

    Sleep(1);

    if OtherThread <> nil then
    begin
      OtherThread.AddMemToOtherThread( AllocAndFill(100) );
//      OtherThread.AddMemToOtherThread( GetMemory(500) );
//      OtherThread.AddMemToOtherThread( GetMemory(1000) );
//      OtherThread.AddMemToOtherThread( GetMemory(2000) );
//      OtherThread.AddMemToOtherThread( GetMemory(8000) );
//      OtherThread.AddMemToOtherThread( GetMemory(48000) );
//      OtherThread.AddMemToOtherThread( GetMemory(100000) );
//      OtherThread.AddMemToOtherThread( GetMemory(2000000) );

      Sleep(10);
    end;

  end;

  ClearMem;
end;

procedure TInterThreadMemTestThread.SetOtherThread(
  const Value: TInterThreadMemTestThread);
begin
  FOtherThread := Value;
end;

end.
