unit smmGlobal;

interface

uses
  ScaleMM3;

type
  TGlobalMemManager = record
  public
    FirstSmallSlot: PSmallBlock;
    FirstMediumBlock: PMediumBlock;

    FirstThreadManager,
    FirstFreeThreadManager: PThreadManager;

    FThreadLock: NativeUInt;
  public
    procedure Init;

    procedure Lock;
    procedure UnLock;

    procedure FreeThreadManager(aThreadMem: PThreadManager);
    function  GetThreadManager: PThreadManager;

    function  GetSmallMemory : PSmallBlock;
    function  GetMediumMemory: PMediumBlock;
  end;

var
  GlobalManager: TGlobalMemManager;

implementation

uses
  smmFunctions;

{ TGlobalMemManager }

procedure TGlobalMemManager.FreeThreadManager(aThreadMem: PThreadManager);
var
  firstsmallmem, lastsmallmem, smallmem: PSmallBlock;
  firstmedmem, lastmedmem, medmem: PMediumBlock;
begin
  firstsmallmem := nil;
  lastsmallmem  := nil;
  smallmem := aThreadMem.FirstSmallSlot;
  if smallmem <> nil then
  begin
    firstsmallmem := smallmem;
    while smallmem <> nil do
    begin
      smallmem.ThreadOwner := nil;      //detach
      lastsmallmem := smallmem;
      smallmem     := smallmem.NextSmallSlot;
    end;
  end;

  firstmedmem := nil;
  lastmedmem  := nil;
  medmem := aThreadMem.FirstMediumBlock;
  if medmem <> nil then
  begin
    firstmedmem := medmem;
    while medmem <> nil do
    begin
      medmem.ThreadOwner := nil;     //detach
      lastmedmem := medmem;
      medmem     := medmem.NextBlock;
    end;
  end;

  aThreadMem.Reset;

  //link all mem to global manager
  begin
    Lock;
    try
      if firstsmallmem <> nil then
      begin
        Assert(lastsmallmem <> nil);
        if Self.FirstSmallSlot = nil then
          FirstSmallSlot := firstsmallmem
        else
        begin
          smallmem            := Self.FirstSmallSlot;
          Self.FirstSmallSlot := firstsmallmem;
          lastsmallmem.NextSmallSlot := smallmem;
          smallmem.PrevSmallSlot     := lastsmallmem;
        end;
      end;

      if firstmedmem <> nil then
      begin
        Assert(lastmedmem <> nil);
        if Self.FirstMediumBlock = nil then
          FirstMediumBlock := firstmedmem
        else
        begin
          medmem                := Self.FirstMediumBlock;
          Self.FirstMediumBlock := firstmedmem;
          lastmedmem.NextBlock  := medmem;
          medmem.PrevBlock      := lastmedmem;
        end;
      end;

      aThreadMem.NextFreeThreadManager := FirstFreeThreadManager;
      FirstFreeThreadManager           := aThreadMem;
    finally
      UnLock;
    end;
  end;
end;

function TGlobalMemManager.GetMediumMemory: PMediumBlock;
begin
  Result := nil;
  if FirstMediumBlock = nil then Exit;

  Lock;
  try
    if FirstMediumBlock = nil then Exit;
    Result := FirstMediumBlock;
    FirstMediumBlock := Result.NextBlock;
    if FirstMediumBlock <> nil then
      FirstMediumBlock.PrevBlock := nil;
  finally
    UnLock;
  end;

  Result.NextBlock := nil;
  Result.PrevBlock := nil;
end;

function TGlobalMemManager.GetSmallMemory: PSmallBlock;
begin
  Result := nil;
  if FirstSmallSlot = nil then Exit;

  Lock;
  try
    if FirstSmallSlot = nil then Exit;
    Result := FirstSmallSlot;
    FirstSmallSlot := Result.NextSmallSlot;
    if FirstSmallSlot <> nil then
      FirstSmallSlot.PrevSmallSlot := nil;
  finally
    UnLock;
  end;

  Result.NextSmallSlot := nil;
  Result.PrevSmallSlot := nil;
end;

function TGlobalMemManager.GetThreadManager: PThreadManager;
begin
  Result := nil;
  if FirstFreeThreadManager = nil then Exit;

  Lock;
  try
    if FirstFreeThreadManager = nil then Exit;
    Result := FirstFreeThreadManager;
    FirstFreeThreadManager := Result.NextFreeThreadManager;
  finally
    UnLock;
  end;

  Result.NextFreeThreadManager := nil;
end;

procedure TGlobalMemManager.Init;
begin
  FillChar(Self, SizeOf(Self), 0);
end;

procedure TGlobalMemManager.Lock;
var
  iCurrentThreadId: NativeUInt;
begin
  iCurrentThreadId := GetCurrentThreadId;

  while (FThreadLock <> 0) and
        not CAS32(0, iCurrentThreadId, @FThreadLock) do
  begin
    //small wait: try to swith to other pending thread (if any) else direct continue
    if not SwitchToThread then
      Sleep(0);
    //try again
    if (FThreadLock = 0) and
       CAS32(0, iCurrentThreadId, @FThreadLock)
    then
      Break;
    //wait some longer: force swith to any other thread
    Sleep(1);
  end;
end;

procedure TGlobalMemManager.UnLock;
begin
  FThreadLock := 0;
end;

end.
