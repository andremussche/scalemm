unit smmArrayTest;

interface

  procedure AllocAllThenFreeAll(aStart, aCount, aStep: NativeUInt);
  procedure AllocAllReallocThenFreeAll(aStart, aCount, aStep, aReallocInc: NativeUInt);
  procedure MediumBlockPreviousMemFree;
  procedure ReallocFromSmallToLargeToSmall;

implementation

uses
  smmMediumMemory;

procedure AllocAllThenFreeAll(aStart, aCount, aStep: NativeUInt);
var
  i: Integer;
  iAlloc: NativeUInt;
  ia: array of Pointer;
begin
  SetLength(ia, aCount);
  iAlloc := aStart;

  //alloc
  for i := 0 to aCount - 1 do
  begin
    ia[i] := GetMemory(iAlloc);
    inc(iAlloc, aStep);
  end;

  //free
  for i := 0 to aCount - 1 do
  begin
    FreeMemory(ia[i]);
  end;

  SetLength(ia, 0);
end;

procedure AllocAllReallocThenFreeAll(aStart, aCount, aStep, aReallocInc: NativeUInt);
var
  i,j: Integer;
  iOldSize,
  iAlloc: NativeUInt;
  ia: array of array of Integer;
begin
  SetLength(ia, aCount);
  iAlloc := aStart;

  //alloc
  for i := 0 to aCount - 1 do
  begin
    SetLength(ia[i], iAlloc);

    //fill marker
    for j := Low(ia[i]) to High(ia[i]) do
      ia[i,j] := iAlloc;

    inc(iAlloc, aStep);
  end;

  //realloc +
  for i := 0 to aCount - 1 do
  begin
    iOldSize := Length(ia[i]);
    SetLength(ia[i], iOldSize + aReallocInc);

    //check marker
    for j := Low(ia[i]) to iOldSize-1 do
      if ia[i,j] <> iOldSize then
        Assert(False);

    //fill marker
    iAlloc := iOldSize + aReallocInc;
    for j := Low(ia[i]) to High(ia[i]) do
      ia[i,j] := iAlloc;
  end;

  //realloc -
  for i := 0 to aCount - 1 do
  begin
    iOldSize := Length(ia[i]);
    SetLength(ia[i], iOldSize - aReallocInc);

    //check marker
    iAlloc := iOldSize - aReallocInc;
    for j := Low(ia[i]) to iAlloc-1 do
      if ia[i,j] <> iOldSize then
        Assert(False);
  end;

  //free
  for i := 0 to aCount - 1 do
  begin
    SetLength(ia[i], 0);
  end;

  SetLength(ia, 0);
end;

procedure MediumBlockPreviousMemFree;
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

procedure ReallocFromSmallToLargeToSmall;
var
  i, j: Integer;
  iCount, iStep: Integer;
  iAlloc: NativeUInt;
  ia: array of Integer;
begin
  iAlloc := 0;
  SetLength(ia, 0);
  iStep  := 128;
  iCount := (C_MAX_MEDIUMMEM_SIZE + 100) div iStep;

  for i := 0 to iCount do
  begin
    //check marker
    for j := Low(ia) to High(ia) do
      if ia[j] <> iAlloc then
        Assert(False);

    //inc
    inc(iAlloc, iStep);
    SetLength(ia, iAlloc);

    //fill marker
    for j := Low(ia) to High(ia) do
      ia[j] := iAlloc;
  end;

  for i := iCount downto 0 do
  begin
    //check marker
    for j := Low(ia) to High(ia) do
      if ia[j] <> iAlloc then
        Assert(False);

    //dec
    dec(iAlloc, iStep);
    SetLength(ia, iAlloc);

    //fill marker
    for j := Low(ia) to High(ia) do
      ia[j] := iAlloc;
  end;

end;


end.
