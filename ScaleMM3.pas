// fast scaling memory manager for Delphi
// licensed under a MPL/GPL/LGPL tri-license; version 0.1
unit ScaleMM3;

{$MESSAGE WARN  'Warning: Experimental version! Only use if you really want to, but normally you should use ScaleMM2 (see website with .zip download or "\Version2" branch in SVN).' +
				'I tried a different architecture for ScaleMM3, which turned out to be slower...' +
				'Note: Only ScaleMM2 can be used in production.'}

{$Include smmOptions.inc}
{$A4}
//{$CODEALIGN 4}

interface

type
  PThreadManager        = ^TThreadManager;           //manager of 1 thread, contains all mem of the thread
  PMediumLargeMemHeader = ^TMediumLargeMemHeader;    //header of a single allocated medium or large memory
  PSmallFreeMemHeader   = ^TSmallFreeMemHeader;      //header of a single allocated small memory, which is freed (allocated mem is used a header, normal small mem has no header!)
  PSmallBlock           = ^TSmallBlock;             //header of a small memory block, same as one medium memory item (4kb), contains "pages" of 32 bytes
  PMediumBlock          = ^TMediumBlock;             //header of a medium memory block, contains "pages" of 4kb
  PMediumSlot           = ^TMediumSlot;              //quick acces "slot" of one medium page (not stored in header but in array for to keep all page info near each other: less cpu cache fetches(?)

  TSlotFlag   = (sfFree, sfInUse, sfFiller, sfFreePending);
  TMemoryType = (mtSmall, mtMedium, mtLarge);

  TSmallFreeMemHeader = record
  public
    PrevMem,
    NextMem     : PSmallFreeMemHeader;
    Slot        : PShortint;
    ThreadFreed : Boolean;
  end;

  //128 pages * 32 bytes = 4kb (= one medium page)
  TSmallBlock = record
  public
    //first part same as TMediumLargeHeader!
    Flag: TMemoryType;      //todo: use as internal lock too
    Slot: PMediumSlot;
  public
    ThreadOwner    : PThreadManager;
    //small mem. specific
    NextSmallSlot  : PSmallBlock;
    PrevSmallSlot  : PSmallBlock;
    //128 slots x 32 bytes = 4kb
    FreeSlots: Integer;
    FreeSlotsFromOtherThreads: Integer;
    Slots : array[4..127] of Shortint;
    Filler: array[0..4] of Byte;     //32 byte aligned

//    firstfreeslotfromotherthread
//    lock

    procedure Init;
    procedure ClearSmallSlots(aSlotIndex: Integer);
    procedure CheckMem;
  public
    class function AllocSlot: PSmallBlock;static;
    function ReallocSmallMem(APointer: Pointer; ANewSize: NativeInt): Pointer;
    function GetSizeOfmem(APointer: Pointer): NativeUInt;
  end;

  //Large: mem bigger than 1Mb
  TMediumLargeMemHeader = record
  public
    class function GetLargeMem(aSize: NativeUInt): Pointer;static;
    function ReallocLargeMem(APointer: Pointer; ANewSize: NativeInt): Pointer;
  public
    Flag: TMemoryType;
    case Integer of
      //medium mem
      0: (MediumSlot   : PMediumSlot;
          PrevMediumMem,
          NextMediumMem: PMediumLargeMemHeader);
      //large mem
      1: (LargeMemSize : NativeUInt);
  end;

  TMediumSlot = packed record
  public
    Flag: TSlotFlag;
    Size: Byte;         //nr of pages
  end;

  //255 pages * 4kb = 1Mb
  TMediumBlock = record
  public
    ThreadOwner   : PThreadManager;
    Flags         : TMemoryType;       //small, medium, large
                                       //todo: use as lock too?
    PrevBlock,
    NextBlock     : PMediumBlock;

    FreeSlots     : Integer;
    FreeSlotsFromOtherThreads: Integer;
    Slots         : array[0..255] of TMediumSlot;

//    firstfreeslotsfromotherthreads

    procedure Init;
    procedure ClearMediumSlots(aSlotIndex: Integer);
    procedure CheckMem;
  public
    class function  AllocBlock: PMediumBlock;static;
    class procedure FreeBlock(aBlock: PMediumBlock);static;
    function  ReallocMediumMem(APointer: Pointer; ANewSize: NativeInt): Pointer;
  end;

  TThreadManager = record
    FirstMediumBlock : PMediumBlock;
    FirstSmallSlot   : PSmallBlock;

    SmallSlotCount,
    MediumBlockCount: NativeUInt;

    FFreeSmallMem : array[0..123] of PSmallFreeMemHeader;
    FFreeMediumMem: array[0..255] of PMediumLargeMemHeader;

    function  GetMediumMem(ASize: NativeInt): Pointer;
    function  GetSmallMem(aSize: NativeUInt): Pointer;
    procedure RemoveSmallMem(aSmallMem: PSmallBlock);

    procedure CheckAllMem;
  public
    NextThreadManager,
    NextFreeThreadManager: PThreadManager;

    procedure Init;  {$ifdef HASINLINE}inline;{$ENDIF}
    procedure Reset; {$ifdef HASINLINE}inline;{$ENDIF}

    procedure AddSmallMem;
    procedure AddMedMem;
    function  FreeSmallMem(aSmallMem : PSmallBlock): Boolean;
    function  FreeMedMem  (aMediumMem: PMediumBlock): Boolean;
    procedure FreeLargeMem(aLargeMem : PMediumLargeMemHeader);
  end;

  {$IF CompilerVersion <= 22}   //till Delphi XE it is Integer
  function Scale_GetMem(ASize: Integer): Pointer;
  function Scale_FreeMem(APointer: Pointer): Integer;
  function Scale_ReallocMem(APointer: Pointer; ANewSize: Integer): Pointer;
  function Scale_AllocMem(ASize: Cardinal): Pointer;
  {$ELSE}
  function Scale_GetMem(ASize: NativeInt): Pointer;
  function Scale_FreeMem(APointer: Pointer): Integer;
  function Scale_ReallocMem(APointer: Pointer; ANewSize: NativeInt): Pointer;
  function Scale_AllocMem(ASize: NativeInt): Pointer;
  {$IFEND}
  function Scale_CheckMem(APointer: Pointer): Boolean;

  function GetCurrentThreadManager: PThreadManager;

const
  C_Small_FirstSlots     = SizeOf(TSmallBlock) div 32;
  C_Small_FirstSlots_Rem = SizeOf(TSmallBlock) mod 32;

  C_SmallPageSize   = 32;             //32 bytes
  //
  C_MediumBlockSize = 1024 * 1024;    //1Mb
  C_MediumPageSize  = 4 * 1024;       //4kb
  //
  C_LargeBlockGranularity = 65536;    //64k

implementation

// Windows.pas unit dependency should be not used -> seperate file
uses
  smmFunctions, smmGlobal;

threadvar
  _ThreadManager: PThreadManager;

function CreateThreadManager: PThreadManager;
//var
//  medmem: PMediumBlock;
begin
//  medmem := TMediumBlock.AllocBlock;
//  Result := medmem.GetMediumMem( SizeOf(TThreadManager) );

//todo  GlobalManager.GetThreadManager

  Result := VirtualAlloc(nil, SizeOf(TThreadManager), MEM_COMMIT, PAGE_READWRITE);
  Assert(Result <> nil);
  Result.Init;
//  Result.FirstMediumBlock     := medmem;
//  Result.FirstFreeMediumBlock := medmem;

//  medmem.ThreadOwner := Result;
  _ThreadManager     := Result;
end;

function GetThreadManager: PThreadManager; {$ifdef HASINLINE}inline;{$ENDIF}
begin
  Result := _ThreadManager;
  if Result = nil then
    Result := CreateThreadManager;
end;

function GetCurrentThreadManager: PThreadManager;
begin
  Result := GetThreadManager;
end;

{$IF CompilerVersion <= 22}
function Scale_GetMem(ASize: Integer): Pointer;
{$ELSE}
function Scale_GetMem(ASize: NativeInt): Pointer;
{$IFEND}
begin
  with GetThreadManager^ do
  begin
    //small
    if ASize < 3 * 1024 then          //3kb
    begin
      if ASize <= 0 then
      begin
        Result := nil;
        Exit;
      end;
      Result := GetSmallMem(ASize);
    end
    else if ASize < 900 * 1024 then   //900kb
    begin
      Result := GetMediumMem(ASize);
    end
    else
    begin
      //large
      Result := TMediumLargeMemHeader.GetLargeMem(ASize);
    end;
  end;

  Assert( Scale_CheckMem(Result) );
end;

{$IF CompilerVersion <= 22}
function Scale_FreeMem(APointer: Pointer): Integer;
{$ELSE}
function Scale_FreeMem(APointer: Pointer): Integer;
{$IFEND}
var
  threadmanager: PThreadManager;
  block: PMediumBlock;
  slotoffset: NativeUInt;
  iSlot, iSlot2, iSlotSize, iSlotSize2: Integer;
  prevmed,
  pheader: PMediumLargeMemHeader;
  psmallslot: PSmallBlock;
  pfreesmall, pprevsmall: PSmallFreeMemHeader;
begin
  Assert( Scale_CheckMem(APointer) );

  threadmanager := GetThreadManager;
  with threadmanager^ do
  begin
    Result  := 0;
    //get header in front of 4k slot
    pheader := PMediumLargeMemHeader( NativeUInt(APointer) and
                                      -C_MediumPageSize );
    Assert(Scale_CheckMem(APointer));
    //get block = front of slot array in first 4k slot
    block := PMediumBlock( NativeUInt(pheader.MediumSlot) and
                           -C_MediumPageSize );

    {
    todo: if ThreadFreeCount+currentfree = max then
        GC thread.threadedfreeblock++
    }

    //SMALL
    if pheader.Flag = mtSmall then
    begin
      Assert(pheader.MediumSlot <> nil);
      //get block = front of slot array in first 4k slot
      psmallslot := PSmallBlock(pheader);

      //todo: psmallslot.ThreadOwner

      slotoffset := NativeUInt(APointer) - NativeUInt(psmallslot);
      iSlot      := slotoffset div C_SmallPageSize;
      Assert(psmallslot.Slots[iSlot] < 0);                     //in use
      Assert(psmallslot.Slots[iSlot] > Low(Shortint));

      {$IFDEF SCALEMM_FILLFREEMEM}
      FillChar(APointer^, Abs(psmallslot.Slots[iSlot]) * C_SmallPageSize, $81);
      {$ENDIF}
      {$IFDEF SCALEMM_DEBUG}
      psmallslot.ClearSmallSlots(iSlot);
      {$ENDIF}

      //same thread?
      if block.ThreadOwner = threadmanager then
      begin
        with psmallslot^ do
        begin
          iSlotSize := Abs({psmallslot.}Slots[iSlot]);   //make positive = free
          inc({psmallslot.}FreeSlots, iSlotSize);
          {psmallslot.}Slots[iSlot] := iSlotSize;

          //release small mem?
          if FreeSlots = (Length(Slots) - C_Small_FirstSlots + Low(Slots)) then
          begin
            pfreesmall      := PSmallFreeMemHeader(APointer);
            pfreesmall.Slot := nil;  //reset, needed for free check
            pfreesmall.PrevMem := nil;  //reset, needed for free check
            pfreesmall.NextMem := nil;  //reset, needed for free check
            if FreeSmallMem(psmallslot) then
              Exit;
          end;
        end;

        iSlot2     := iSlot + iSlotSize;
        //make one block if next item also free
        if iSlot2 < High(psmallslot.Slots) then
        begin
          iSlotSize2 := psmallslot.Slots[iSlot + iSlotSize];
          while (iSlotSize2 > 0) and
                (FFreeSmallMem[iSlotSize] <> nil) do   //no mem for current size? then skip one big block and keep at least one small size
          begin
            pprevsmall := PSmallFreeMemHeader( NativeUInt(APointer) + NativeUInt(iSlotSize * C_SmallPageSize) );
            if not pprevsmall.ThreadFreed then  //items freed in other thread are not in linked list
            begin
              //first one? then remove from linked list array
              if pprevsmall.PrevMem = nil then
              begin
                Assert(FFreeSmallMem[iSlotSize2] = pprevsmall);
                FFreeSmallMem[iSlotSize2] := pprevsmall.NextMem;
                //unlink from next
                if pprevsmall.NextMem <> nil then
                  pprevsmall.NextMem.PrevMem := nil;
              end
              else
              //else unlink from anywhere in the linked list
              begin
                pprevsmall.PrevMem.NextMem := pprevsmall.NextMem;
                if pprevsmall.NextMem <> nil then
                  pprevsmall.NextMem.PrevMem := pprevsmall.PrevMem;
              end;
            end;
            iSlotSize               := iSlotSize + iSlotSize2;
            psmallslot.Slots[iSlot] := iSlotSize;

            iSlot2     := iSlot + iSlotSize;
            if iSlot2 >= High(psmallslot.Slots) then Break;
            iSlotSize2 := psmallslot.Slots[iSlot2];  //recalc next item, for recursive linking
          end;
        end;

        pfreesmall      := PSmallFreeMemHeader(APointer);
        pfreesmall.Slot := @psmallslot.Slots[iSlot];
        //add to "all free list by size"
        pprevsmall               := FFreeSmallMem[iSlotSize];
        FFreeSmallMem[iSlotSize] := pfreesmall;
        if pprevsmall <> nil then
        begin
          pprevsmall.PrevMem  := pfreesmall;
          pfreesmall.NextMem  := pprevsmall;
        end
        else
          pfreesmall.NextMem   := nil;
        pfreesmall.PrevMem     := nil;
        pfreesmall.ThreadFreed := False;

        {$IFDEF SCALEMM_DEBUG}
        psmallslot.CheckMem;
        {$ENDIF}
      end
      else
      begin
        pfreesmall         := PSmallFreeMemHeader(APointer);
        pfreesmall.Slot    := @psmallslot.Slots[iSlot];
        pfreesmall.PrevMem := nil;
        pfreesmall.NextMem := nil;
        pfreesmall.ThreadFreed := True;

        with psmallslot^ do
        begin
          iSlotSize := Abs({psmallslot.}Slots[iSlot]);   //make positive = free
          InterlockedExchangeAdd({psmallslot.}FreeSlotsFromOtherThreads, iSlotSize);
          {psmallslot.}Slots[iSlot] := iSlotSize;
        end;
      end;
    end
    //MEDIUM
    else if pheader.Flag = mtMedium then
    begin
      {$IFDEF SCALEMM_FILLFREEMEM}
      FillChar(APointer^, (pheader.MediumSlot.Size * C_MediumPageSize) - SizeOf(TMediumLargeMemHeader), $82);
      {$ENDIF}
      Assert(pheader.MediumSlot.Flag= sfInUse);

      //same thread?
      if block.ThreadOwner = threadmanager then
      begin
        inc(block.FreeSlots, pheader.MediumSlot.Size);
        //release block?
        if block.FreeSlots = (Length(block.Slots) - 1) then
        begin
          pheader.MediumSlot.Flag:= sfFreePending;
          if FreeMedMem(block) then
          begin
            {$IFDEF SCALEMM_DEBUG}
            CheckAllMem;
            {$ENDIF}
            Exit;
          end;
        end;

        {$IFDEF SCALEMM_DEBUG}
        slotoffset := NativeUInt(APointer) - NativeUInt(block);
        iSlot      := slotoffset div C_MediumPageSize;
        Assert(@block.Slots[iSlot] = pheader.MediumSlot);
        //Assert(block.Slots[iSlot].Flags = TSlot.C_FlagInUse);
        {$ENDIF}

        //todo: check next + prevmem if free too, to make one big block

        //add to "all free list by size"
        with pheader.MediumSlot^ do
        begin
          prevmed := FFreeMediumMem[{pheader.Slot.}Size];
          FFreeMediumMem[{pheader.Slot.}Size] := pheader;
        end;
        if prevmed <> nil then
        begin
          prevmed.PrevMediumMem  := pheader;
          pheader.NextMediumMem  := prevmed;
        end
        else
          pheader.NextMediumMem  := nil;
        pheader.PrevMediumMem    := nil;
        pheader.MediumSlot.Flag:= sfFree;
      end
      else
      begin
        pheader.PrevMediumMem := nil;
        pheader.NextMediumMem := nil;
        InterlockedExchangeAdd(block.FreeSlotsFromOtherThreads, pheader.MediumSlot.Size);
        pheader.MediumSlot.Flag:= sfFree;
      end;

      {$IFDEF SCALEMM_DEBUG}
//      if block.ThreadOwner = threadmanager then
        //block.CheckMem;
//        CheckAllMem;
      {$ENDIF}
    end
    //LARGE
    else if pheader.Flag = mtLarge then
    begin
      threadmanager.FreeLargeMem(pheader);
    end;
  end;

  Assert(Result = 0);
end;

{$IF CompilerVersion <= 22}
function Scale_ReallocMem(APointer: Pointer; ANewSize: Integer): Pointer;
{$ELSE}
function Scale_ReallocMem(APointer: Pointer; ANewSize: NativeInt): Pointer;
{$IFEND}
var
  threadmanager: PThreadManager;
  block: PMediumBlock;
  pheader: PMediumLargeMemHeader;
  psmallslot: PSmallBlock;
  iOldSize: NativeInt;
begin
  Assert( Scale_CheckMem(APointer) );

  // ReAlloc can be misued as GetMem or FreeMem (documented in delphi help) so check what the user wants
  // Normal realloc of exisiting data?
  if (APointer <> nil) and (ANewSize > 0) then
  begin
    threadmanager := GetThreadManager;
    with threadmanager^ do
    begin
      Result := nil;
      //get header in front of 4k slot
      pheader    := PMediumLargeMemHeader(
                        NativeUInt(APointer) and
                        -C_MediumPageSize );
      Assert(Scale_CheckMem(APointer));
      //get block = front of slot array in first 4k slot
      block := PMediumBlock( NativeUInt(pheader.MediumSlot) and
                             -C_MediumPageSize );

      if pheader.Flag = mtLarge then
      begin
        Result := pheader.ReallocLargeMem(APointer, ANewSize);
        Assert( Scale_CheckMem(Result) );
        Exit;
      end
      //same thread?
      else if block.ThreadOwner = threadmanager then
      begin
        if pheader.Flag = mtSmall then
        begin
          Assert(pheader.MediumSlot <> nil);
          //get block = front of slot array in first 4k slot
          psmallslot := PSmallBlock(pheader);

          Result := psmallslot.ReallocSmallMem(APointer, ANewSize);
//          {$IFDEF SCALEMM_DEBUG}
//          psmallslot.CheckMem;   can be reallocated
//          {$ENDIF}
        end
        else if pheader.Flag = mtMedium then
        begin
          Result := block.ReallocMediumMem(APointer, ANewSize);
//          {$IFDEF SCALEMM_DEBUG}
//          block.CheckMem;        can be reallocated
//          {$ENDIF}
        end
        else if pheader.Flag = mtLarge then
        begin
          Result := pheader.ReallocLargeMem(APointer, ANewSize);
        end;
        Assert( Scale_CheckMem(Result) );
      end
      else
      begin
        //get old size
        if pheader.Flag = mtSmall then
          iOldSize := PSmallBlock(pheader).GetSizeOfmem(APointer)
        else
          iOldSize := pheader.MediumSlot.Size * C_MediumPageSize;

        //needs realloc? or only downsize but larger than 1/4?
        if (iOldSize > ANewSize) and
           ( (iOldSize / 4) > ANewSize) then
        begin
          Result := APointer;
          Assert( Scale_CheckMem(Result) );
          Exit;
        end;

        //alloc new mem from own thread
        Result := Scale_GetMem(ANewSize);
        //copy old contents
        if ANewSize < iOldSize then
          Move(APointer^, Result^, ANewSize)
        else
          Move(APointer^, Result^, iOldSize);
        //free old mem
        Scale_FreeMem(APointer);
        Assert( Scale_CheckMem(Result) );
        Exit;
      end;
    end;
  end
  else
  begin
    if (APointer = nil) and (ANewSize > 0) then
      // GetMem disguised as ReAlloc
      Result := Scale_GetMem(ANewSize)
    else
    begin
      // FreeMem disguised as ReAlloc
      Result := nil;
      Scale_FreeMem(APointer);
    end;
  end;
end;

{$IF CompilerVersion <= 22}
function Scale_AllocMem(ASize: Cardinal): Pointer;
{$ELSE}
function Scale_AllocMem(ASize: NativeInt): Pointer;
{$IFEND}
begin
  Result := Scale_GetMem(aSize);
  Fillchar(Result^, aSize, 0); // AllocMem() = GetMem()+ZeroMemory()
end;

function Scale_CheckMem(APointer: Pointer): Boolean;
var
  block: PMediumBlock;
  pheader: PMediumLargeMemHeader;
  psmallslot: PSmallBlock;
  slotoffset: NativeUInt;
  iSlot: Integer;
begin
  Result := True;

  Assert(APointer <> nil);
  Assert(NativeUInt(APointer) > $1000);

  //get header in front of 4k slot
  pheader := PMediumLargeMemHeader( NativeUInt(APointer) and
                                 -C_MediumPageSize );

  Assert(pheader.MediumSlot <> nil);
  Assert(pheader.Flag in [mtSmall, mtMedium, mtLarge] );
  if pheader.Flag = mtLarge then Exit;

  //get block = front of slot array in first 4k slot
  block := PMediumBlock( NativeUInt(pheader.MediumSlot) and
                         -C_MediumPageSize );

  Assert( NativeUInt(pheader) >= NativeUInt(block) );
  Assert( NativeUInt(pheader) - NativeUInt(block) < C_MediumBlockSize );

  if pheader.Flag = mtSmall then
  begin
    Assert(pheader.MediumSlot <> nil);
    //get block = front of slot array in first 4k slot
    psmallslot := PSmallBlock(pheader);

    slotoffset := NativeUInt(APointer) - NativeUInt(psmallslot);
    iSlot      := slotoffset div C_SmallPageSize;
    Assert(psmallslot.Slots[iSlot] < 0);                     //in use
    Assert(psmallslot.Slots[iSlot] > Low(Shortint));
  end
  else if pheader.Flag = mtMedium then
  begin
    slotoffset := NativeUInt(APointer) - NativeUInt(block);
    iSlot      := slotoffset div C_MediumPageSize;
    Assert(@block.Slots[iSlot] = pheader.MediumSlot);
    Assert(block.Slots[iSlot].Flag= sfInUse);
  end;
end;

{TThreadManager}
(*=========================================*)

procedure TThreadManager.Init;
begin
  FillChar(Self, SizeOf(Self), 0);
end;

procedure TThreadManager.AddSmallMem;
var
  smallmem: PSmallBlock;
  firstmem, prevmem: PSmallFreeMemHeader;
begin
  smallmem                         := TSmallBlock.AllocSlot;
  smallmem.NextSmallSlot           := FirstSmallSlot;
  if FirstSmallSlot <> nil then
    FirstSmallSlot.PrevSmallSlot   := smallmem;
  FirstSmallSlot                   := smallmem;

  //get first mem
  firstmem       := PSmallFreeMemHeader( NativeUInt(smallmem) + NativeUInt(C_Small_FirstSlots * C_SmallPageSize) );
  firstmem.Slot  := @smallmem.Slots[C_Small_FirstSlots];

  //add to list of "all free mem by size"
  prevmem        := FFreeSmallMem[High(FFreeSmallMem)];
  //first block not uses, so -1
  FFreeSmallMem[High(FFreeSmallMem)] := firstmem;
  Assert( High(FFreeSmallMem) = firstmem.Slot^ );
  if prevmem <> nil then
  begin
    prevmem.PrevMem  := firstmem;
    firstmem.NextMem := prevmem;
  end
  else
    firstmem.NextMem := nil;
  firstmem.PrevMem   := nil;
  firstmem.ThreadFreed := False;

  inc(SmallSlotCount);
  {$IFDEF SCALEMM_DEBUG}
//  CheckAllMem;
  {$ENDIF}
end;

procedure TThreadManager.AddMedMem;
var
  prevmem, firstmem: PMediumLargeMemHeader;
  medmem: PMediumBlock;
begin
  medmem                          := TMediumBlock.AllocBlock;
  medmem.NextBlock                := FirstMediumBlock;
  if FirstMediumBlock <> nil then
    FirstMediumBlock.PrevBlock    := medmem;
  FirstMediumBlock                := medmem;

  //get first mem
  firstmem       := PMediumLargeMemHeader( NativeUInt(medmem) + C_MediumPageSize );
  firstmem.MediumSlot  := @medmem.Slots[1];
  firstmem.Flag := mtMedium;

  //add to list of "all free mem by size"
  prevmem        := FFreeMediumMem[High(FFreeMediumMem)];
  //first block not uses, so -1
  FFreeMediumMem[High(FFreeMediumMem)] := firstmem;
  Assert( High(FFreeMediumMem) = firstmem.MediumSlot.Size );
  if prevmem <> nil then
  begin
    prevmem.PrevMediumMem  := firstmem;
    firstmem.NextMediumMem := prevmem;
  end
  else
    firstmem.NextMediumMem := nil;
  firstmem.PrevMediumMem   := nil;

  inc(MediumBlockCount);
  {$IFDEF SCALEMM_DEBUG}
//  CheckAllMem;
  {$ENDIF}
end;

function TThreadManager.FreeSmallMem(aSmallMem : PSmallBlock): Boolean;
var
  p: Pointer;
begin
  Result := False;
  //keep 1 block
  if FirstSmallSlot = aSmallMem then Exit;
  //if SmallSlotCount = 1 then Exit;
  dec(SmallSlotCount);

  //remove from "all free list"
  RemoveSmallMem(aSmallMem);

  if aSmallMem = nil then
    Sleep(0);
  //remove from lists
  if aSmallMem.NextSmallSlot <> nil then
    aSmallMem.NextSmallSlot.PrevSmallSlot := aSmallMem.PrevSmallSlot;
  if aSmallMem.PrevSmallSlot <> nil then
    aSmallMem.PrevSmallSlot.NextSmallSlot := aSmallMem.NextSmallSlot;

  //mark as medium mem again
  aSmallMem.Flag := mtMedium;
  p := Pointer( NativeUInt(aSmallMem) + SizeOf(TMediumLargeMemHeader) );
  //release
  Scale_FreeMem(p);

  Result := True;
end;

procedure TThreadManager.RemoveSmallMem(aSmallMem: PSmallBlock);
var
  i, iSize: Integer;
  pheader: PSmallFreeMemHeader;
begin
  {$ifdef SCALEMM_DEBUG}try{$endif}

  i := C_Small_FirstSlots;
  with aSmallMem^ do
  while i <= High(Slots) do
  begin
    Assert(Slots[i] > 0);
    iSize := Abs(Slots[i]);

    pheader := PSmallFreeMemHeader( NativeUInt(aSmallMem) + NativeUInt(i * C_SmallPageSize) );
    //if pheader.Slot <> nil then
    if not pheader.ThreadFreed then  //items freed in other thread are not in linked list
    begin
      Assert(pheader.Slot = @Slots[i]);
      //first one? then remove from linked list array
      if pheader.PrevMem = nil then
      begin
        Assert(FFreeSmallMem[iSize] = pheader);
        FFreeSmallMem[iSize] := pheader.NextMem;
        //unlink from next
        if pheader.NextMem <> nil then
          pheader.NextMem.PrevMem := nil;
      end
      else
      //else unlink from anywhere in the linked list
      begin
        pheader.PrevMem.NextMem := pheader.NextMem;
        if pheader.NextMem <> nil then
          pheader.NextMem.PrevMem := pheader.PrevMem;
      end;
    end;

    Inc(i, iSize);
  end;

  {$ifdef SCALEMM_DEBUG}except sleep(0); end;{$endif}
end;


procedure TThreadManager.Reset;
begin
  FillChar(Self,
           SizeOf(Self) - 2*(SizeOf(PThreadManager)),  //keep NextThreadManager and NextFreeThreadManager
           0);
end;

function TThreadManager.FreeMedMem(aMediumMem: PMediumBlock): Boolean;
begin
  Result := False;
  //keep 1 block
  if FirstMediumBlock = aMediumMem then Exit;
  //if MediumBlockCount = 1 then Exit;
  dec(MediumBlockCount);

  //remove from lists
  if aMediumMem.NextBlock <> nil then
    aMediumMem.NextBlock.PrevBlock := aMediumMem.PrevBlock;
  if aMediumMem.PrevBlock <> nil then
    aMediumMem.PrevBlock.NextBlock := aMediumMem.NextBlock;

  if FirstMediumBlock = aMediumMem then
    FirstMediumBlock := aMediumMem.NextBlock;

  //release
  TMediumBlock.FreeBlock(aMediumMem);

  Result := True;
end;

procedure TThreadManager.FreeLargeMem(aLargeMem: PMediumLargeMemHeader);
var
  meminfo: TMemoryBasicInformation;
  iRemainderSize: NativeUInt;
  pnextmem: Pointer;
begin
  iRemainderSize := aLargeMem.LargeMemSize;
  VirtualQuery(aLargeMem, meminfo, SizeOf(meminfo));
  //threadsafe direct release
  if not VirtualFree(aLargeMem, 0, MEM_RELEASE) then
  begin
    Assert(False);
    System.Error(reInvalidPtr);
  end;

  pnextmem       := aLargeMem;
  iRemainderSize := iRemainderSize - meminfo.RegionSize;
  while iRemainderSize > 0 do
  begin
    pnextmem := Pointer(NativeUInt(pnextmem) + meminfo.RegionSize);
    VirtualQuery(pnextmem, meminfo, SizeOf(meminfo));
    if not VirtualFree(pnextmem, 0, MEM_RELEASE) then
    begin
      Assert(False);
      System.Error(reInvalidPtr);
    end;
    iRemainderSize := iRemainderSize - meminfo.RegionSize;
  end;
end;

function TThreadManager.GetMediumMem(ASize: NativeInt): Pointer;
var
  i, iTotalSize, iDiff,
  iSlotsNeeded: Integer;
  pheader, prevmem, remainder: PMediumLargeMemHeader;
  medmem: PMediumBlock;
begin
  iTotalSize   := ASize + SizeOf(TMediumLargeMemHeader);
  iSlotsNeeded := (iTotalSize div C_MediumPageSize);
  if (iTotalSize mod C_MediumPageSize) > 0 then
    inc(iSlotsNeeded);
  Assert(iSlotsNeeded > 0);

  //exact size available?
  pheader := FFreeMediumMem[iSlotsNeeded];
  if pheader <> nil then
  begin
    //replace with next free mem
    FFreeMediumMem[iSlotsNeeded] := pheader.NextMediumMem;
    if pheader.NextMediumMem <> nil then
      pheader.NextMediumMem.PrevMediumMem   := nil;
  end
  //else search for bigger size
  else
  begin
    //any mem allocated for this thread?
    if FirstMediumBlock <> nil then
      //search till end
      for i := iSlotsNeeded to High(FFreeMediumMem) do
      begin
        if FFreeMediumMem[i] <> nil then
        begin
          pheader          := FFreeMediumMem[i];
          FFreeMediumMem[i] := pheader.NextMediumMem;
          if pheader.NextMediumMem <> nil then
            pheader.NextMediumMem.PrevMediumMem := nil;
          Break;
        end;
      end;
    //nothing? than alloc new mem
    if pheader = nil then
    begin
      AddMedMem;
      pheader := FFreeMediumMem[High(FFreeMediumMem)];
      FFreeMediumMem[High(FFreeMediumMem)] := pheader.NextMediumMem;
      if pheader.NextMediumMem <> nil then
        pheader.NextMediumMem.PrevMediumMem          := nil;
    end;
    Assert(pheader <> nil);
    Assert(pheader.MediumSlot <> nil);

    //split remainder
    iDiff   := pheader.MediumSlot.Size - iSlotsNeeded;
    if iDiff > 0 then
    begin
      //create remainder (after current mem)
      remainder := PMediumLargeMemHeader( NativeUInt(pheader) + NativeUInt(iSlotsNeeded * C_MediumPageSize) );
      with remainder^ do
      begin
        {remainder.}Flag := mtMedium;
        {remainder.}MediumSlot  := PMediumSlot( NativeUInt(pheader.MediumSlot) + NativeUInt(iSlotsNeeded * SizeOf(TMediumSlot)) );
        with MediumSlot^ do
        begin
          {remainder.Slot.}Flag := sfFree;
          {remainder.Slot.}Size := iDiff;
        end;
      end;

      //add to free linked list
      prevmem               := FFreeMediumMem[iDiff];
      FFreeMediumMem[iDiff] := remainder;
      if prevmem <> nil then
      begin
        prevmem.PrevMediumMem    := remainder;
        remainder.NextMediumMem  := prevmem;
      end
      else
        remainder.NextMediumMem  := nil;
      remainder.PrevMediumMem    := nil;
    end;
  end;
  Assert(pheader.Flag = mtMedium);
  Assert(pheader.MediumSlot <> nil);
  Assert(pheader.MediumSlot.Flag= sfFree);
  Assert(pheader.MediumSlot.Size >= iSlotsNeeded);

  //fill header
  with pheader^ do
  begin
    //{pheader.}Flags := C_FlagMedium;
    //alloc c.q. use
    with MediumSlot^ do
    begin
      {pheader.Slot.}Flag:= sfInUse;
      {pheader.Slot.}Size  := iSlotsNeeded;
    end;
  end;

  //decrement free slot count
  medmem := PMediumBlock( NativeUInt(pheader.MediumSlot) and
                          -C_MediumPageSize );
  Dec(medmem.FreeSlots, iSlotsNeeded);

  //result without header
  Result        := Pointer( NativeUInt(pheader) + SizeOf(TMediumLargeMemHeader) );

  {$IFDEF SCALEMM_FILLFREEMEM}
  FillChar(Result^, (iSlotsNeeded * C_MediumPageSize) - SizeOf(TMediumLargeMemHeader), $FF);
  {$ENDIF}
  {$IFDEF SCALEMM_DEBUG}
//  medmem.ClearMediumSlots(iFirstFree);
//  CheckAllMem;
  {$ENDIF}
end;

function TThreadManager.GetSmallMem(aSize: NativeUInt): Pointer;
var
  smallmem: PSmallBlock;
  iSlotsNeeded, iDiff,
  i: Integer;
  remainder, prevmem,
  pheader: PSmallFreeMemHeader;
begin
  iSlotsNeeded := (aSize div C_SmallPageSize);
  if (aSize mod C_SmallPageSize) > 0 then
    inc(iSlotsNeeded);

  //exact size available?
  pheader := FFreeSmallMem[iSlotsNeeded];
  if pheader <> nil then
  begin
    //replace with next free mem
    FFreeSmallMem[iSlotsNeeded] := pheader.NextMem;
    if pheader.NextMem <> nil then
      pheader.NextMem.PrevMem   := nil;
  end
  //else search for bigger size
  else
  begin
    //any mem allocated for this thread?
    if FirstSmallSlot <> nil then
      //search till end
      for i := iSlotsNeeded to High(FFreeSmallMem) do
      begin
        if FFreeSmallMem[i] <> nil then
        begin
          pheader          := FFreeSmallMem[i];
          FFreeSmallMem[i] := pheader.NextMem;
          if pheader.NextMem <> nil then
            pheader.NextMem.PrevMem := nil;
          Break;
        end;
      end;
    //nothing? than alloc new mem
    if pheader = nil then
    begin
      AddSmallMem;
      pheader := FFreeSmallMem[High(FFreeSmallMem)];
      FFreeSmallMem[High(FFreeSmallMem)] := pheader.NextMem;
      if pheader.NextMem <> nil then
        pheader.NextMem.PrevMem          := nil;
    end;
    Assert(pheader <> nil);
    Assert(pheader.Slot <> nil);
    Assert(pheader.Slot^ > 0);

    //split remainder
    iDiff   := pheader.Slot^ - iSlotsNeeded;
    if iDiff > 0 then
    begin
      //create remainder (after current mem)
      remainder := PSmallFreeMemHeader( NativeUInt(pheader) + NativeUInt(iSlotsNeeded * C_SmallPageSize) );
      with remainder^ do
      begin
        {remainder.}Slot  := PShortInt( NativeUInt(pheader.Slot) + NativeUInt(iSlotsNeeded * SizeOf(ShortInt)) );
        {remainder.}Slot^ := iDiff;
      end;

      //add to free linked list
      prevmem              := FFreeSmallMem[iDiff];
      FFreeSmallMem[iDiff] := remainder;
      if prevmem <> nil then
      begin
        prevmem.PrevMem    := remainder;
        remainder.NextMem  := prevmem;
      end
      else
        remainder.NextMem  := nil;
      remainder.PrevMem    := nil;
      remainder.ThreadFreed := False;
    end;
  end;
  Assert(pheader.Slot <> nil);
  Assert(pheader.Slot^ >= iSlotsNeeded);

  //fill header
  with pheader^ do
  begin
    //alloc c.q. use
    Slot^ := - iSlotsNeeded;

    //todo: threadowner
  end;

  //decrement free slot count
  smallmem := PSmallBlock( NativeUInt(pheader) and
                            -C_MediumPageSize );
  Dec(smallmem.FreeSlots, iSlotsNeeded);

  //result without header
  Result   := pheader;

  {$IFDEF SCALEMM_FILLFREEMEM}
  FillChar(Result^, (iSlotsNeeded * C_SmallPageSize), $FF);
  {$ENDIF}
  {$IFDEF SCALEMM_DEBUG}
//  CheckAllMem;
  {$ENDIF}
end;

procedure TThreadManager.CheckAllMem;
var
  iCount: NativeUInt;
  smallmem: PSmallBlock;
  medmem: PMediumBlock;
  pheader: PMediumLargeMemHeader;
  i: Integer;
begin
  //small: check infinite loops
  iCount := 0;
  smallmem := FirstSmallSlot;
  while smallmem <> nil do
  begin
    smallmem := smallmem.NextSmallSlot;
    inc(iCount);
    Assert(iCount <= SmallSlotCount);
    if iCount > SmallSlotCount then Break;
  end;

  //medium: check infinite loops
  iCount := 0;
  medmem := FirstMediumBlock;
  while medmem <> nil do
  begin
    medmem := medmem.NextBlock;
    inc(iCount);
    Assert(iCount <= MediumBlockCount);
    if iCount > MediumBlockCount then Break;
  end;

  for i := Low(FFreeMediumMem) to High(FFreeMediumMem) do
  begin
    if FFreeMediumMem[i] <> nil then
    begin
      pheader := FFreeMediumMem[i];
      Assert(pheader.PrevMediumMem = nil);
      while pheader <> nil do
      begin
        Assert(pheader.Flag = mtMedium);
        Assert(pheader.MediumSlot.Size  = i);
        Assert(pheader.MediumSlot.Flag= sfFree);

        if pheader.NextMediumMem <> nil then
          Assert(pheader.NextMediumMem.PrevMediumMem = pheader);
        if pheader.PrevMediumMem <> nil then
          Assert(pheader.PrevMediumMem.NextMediumMem = pheader);

        pheader := pheader.NextMediumMem;
      end;
    end;
  end;

  //check all small mem
  smallmem := FirstSmallSlot;
  while smallmem <> nil do
  begin
    smallmem.CheckMem;
    smallmem := smallmem.NextSmallSlot;
  end;

  //check all medium mem
  medmem := FirstMediumBlock;
  while medmem <> nil do
  begin
    medmem.CheckMem;
    medmem := medmem.NextBlock;
  end;

end;

{TMediumBlock}
(*=========================================*)

class function TMediumBlock.AllocBlock: PMediumBlock;
begin
  Result := VirtualAlloc( nil,
                          C_MediumBlockSize,
                          MEM_COMMIT {$ifdef AlwaysAllocateTopDown} or MEM_TOP_DOWN{$endif},
                          PAGE_READWRITE);

  if Result = nil then
  begin
    Assert(False);
    System.Error(reOutOfMemory);
  end;
  Result.Init;
end;

class procedure TMediumBlock.FreeBlock(aBlock: PMediumBlock);
var
  i, iSize: Integer;
  pheader: PMediumLargeMemHeader;
begin
  i := 1;
  with aBlock^ do
  while i <= High(Slots) do
  begin
    Assert( (Slots[i].Flag= sfFree) or (Slots[i].Flag= sfFreePending) );

    pheader := Pointer( NativeUInt(aBlock) + NativeUInt(i * C_MediumPageSize) );
    Assert(pheader.MediumSlot = @Slots[i]);
    Assert(pheader.Flag = mtMedium);

    iSize := Slots[i].Size;
    if pheader.MediumSlot.Flag<> sfFreePending then
    begin
      //first one? then remove from linked list array
      if pheader.PrevMediumMem = nil then
      begin
        Assert(ThreadOwner.FFreeMediumMem[iSize] = pheader);
        ThreadOwner.FFreeMediumMem[iSize] := pheader.NextMediumMem;
        //unlink from next
        if pheader.NextMediumMem <> nil then
          pheader.NextMediumMem.PrevMediumMem := nil;
      end
      else
      //else unlink from anywhere in the linked list
      begin
        pheader.PrevMediumMem.NextMediumMem := pheader.NextMediumMem;
        if pheader.NextMediumMem <> nil then
          pheader.NextMediumMem.PrevMediumMem := pheader.PrevMediumMem;
      end;
    end;

    Inc(i, iSize);
  end;

  if not VirtualFree(aBlock, 0, MEM_RELEASE) then
  begin
    Assert(False);
    System.Error(reInvalidPtr);
  end;
end;

procedure TMediumBlock.Init;
begin
  FillChar(Self, SizeOf(Self), 0);

  Self.ThreadOwner := _ThreadManager;
  Self.Flags       := mtMedium;
  Self.FreeSlots   := Length(Slots) - 1;  //first slot not used

  //first slot not used
  Self.Slots[0].Flag := sfInUse;
  Self.Slots[0].Size := 1;

  Self.Slots[1].Flag := sfFree;
  Self.Slots[1].Size := Length(Slots) - 1;  //first slot not used
end;

function TMediumBlock.ReallocMediumMem(APointer: Pointer; ANewSize: NativeInt): Pointer;
var
  slotoffset: NativeUInt;
  iSlot: Integer;
  prevmem, newmem,
  pheader: PMediumLargeMemHeader;
  iTotalSize: NativeInt;
  iNextSlot,
  iNextSlot2,
  iCurrentSlots,
  iDiff,
  iOldSize,
  iSlotsNeeded: Integer;
begin
  //get header in front of 4k slot
  pheader    := PMediumLargeMemHeader(
                    NativeUInt(APointer) and
                    -C_MediumPageSize);

  iTotalSize    := ANewSize + SizeOf(TMediumLargeMemHeader);
  iSlotsNeeded  := (iTotalSize div C_MediumPageSize);
  if (iTotalSize mod C_MediumPageSize) > 0 then
    inc(iSlotsNeeded);
  iCurrentSlots := pheader.MediumSlot.Size;
  Assert( (iSlotsNeeded > 0) and (iCurrentSlots > 0) );

  //same size?
  if iSlotsNeeded = iCurrentSlots then
    Result := APointer
  //smaller size
  else if iSlotsNeeded < iCurrentSlots then
  begin
    //too much downsize? then switch to small mem
    if ANewSize <= 2 * 1024 then          //2kb
    begin
      //alloc new mem from own thread
      Result := Scale_GetMem(ANewSize);
      //copy old contents
      Move(APointer^, Result^, ANewSize);
      //free old mem
      Scale_FreeMem(APointer);
      Exit;
    end;

    //else just downsize in place and release unused mem
    iDiff  := iCurrentSlots - iSlotsNeeded;
    //more than 1 slot free? (1 slot as buffer so less resizes)
    if iDiff > 1 then
    begin
      slotoffset := NativeUInt(APointer) - NativeUInt(@Self);
      iSlot      := slotoffset div C_MediumPageSize;
      Assert(@Self.Slots[iSlot] = pheader.MediumSlot);

      //downsize
      Slots[iSlot].Size := iSlotsNeeded;
      Inc(Self.FreeSlots, iDiff);

      //remainder
      iNextSlot := iSlot + iSlotsNeeded;
      //create remainder
      newmem    := PMediumLargeMemHeader( NativeUInt(@Self) + NativeUInt(iNextSlot * C_MediumPageSize));
      with newmem^ do
      begin
        {newmem.}Flag := mtMedium;
        {newmem.}MediumSlot  := PMediumSlot( NativeUInt(pheader.MediumSlot) + NativeUInt(iSlotsNeeded * SizeOf(TMediumSlot)) );
        with MediumSlot^ do
        begin
          {newmem.Slot.}Flag := sfFree;
          {newmem.Slot.}Size := iDiff;
        end;
      end;

      //add remainder to "all free list by size"
      prevmem := Self.ThreadOwner.FFreeMediumMem[iDiff];
      Self.ThreadOwner.FFreeMediumMem[iDiff] := newmem;
      if prevmem <> nil then
      begin
        prevmem.PrevMediumMem := newmem;
        newmem.NextMediumMem  := prevmem;
      end
      else
        newmem.NextMediumMem  := nil;
      newmem.PrevMediumMem    := nil;

      {$IFDEF SCALEMM_DEBUG}
      ClearMediumSlots(iNextSlot);
//      ThreadOwner.CheckAllMem;
      //CheckMem;
      {$ENDIF}
    end;

    Result := APointer;
  end
  //upsize
  else
  begin
    slotoffset := NativeUInt(APointer) - NativeUInt(@Self);
    iSlot      := slotoffset div C_MediumPageSize;
    Assert(@Self.Slots[iSlot] = pheader.MediumSlot);

    iNextSlot := iSlot + iCurrentSlots;
    iDiff     := iSlotsNeeded - iCurrentSlots;
    //extend current slot if room available after it
    if (iNextSlot <= High(Slots)) and
       (Self.Slots[iNextSlot].Flag = sfFree) and
       (Self.Slots[iNextSlot].Size >= iDiff) then
    begin
      iNextSlot2 := iNextSlot + iDiff;
      iOldSize   := Self.Slots[iNextSlot].Size;
      iDiff      := iOldSize - iDiff;

      prevmem    := PMediumLargeMemHeader( NativeUInt(@Self) + NativeUInt(iNextSlot * C_MediumPageSize));
      //first one? then remove from linked list array
      if prevmem.PrevMediumMem = nil then
      begin
        Assert(ThreadOwner.FFreeMediumMem[iOldSize] = prevmem);
        ThreadOwner.FFreeMediumMem[iOldSize] := prevmem.NextMediumMem;
        //unlink from next
        if prevmem.NextMediumMem <> nil then
          prevmem.NextMediumMem.PrevMediumMem := nil;
      end
      else
      //else unlink from anywhere in the linked list
      begin
        prevmem.PrevMediumMem.NextMediumMem := prevmem.NextMediumMem;
        if prevmem.NextMediumMem <> nil then
          prevmem.NextMediumMem.PrevMediumMem := prevmem.PrevMediumMem;
      end;

      //has (big enough) remainder?
//      if iDiff > 1 then
      if iDiff > 0 then
      begin
        //create remainder
        newmem  := PMediumLargeMemHeader( NativeUInt(@Self) + NativeUInt(iNextSlot2 * C_MediumPageSize));
        with newmem^ do
        begin
          {newmem.}Flag := mtMedium;
          {newmem.}MediumSlot  := PMediumSlot( NativeUInt(pheader.MediumSlot) + NativeUInt(iSlotsNeeded * SizeOf(TMediumSlot)) );
          with MediumSlot^ do
          begin
            {newmem.Slot.}Flag := sfFree;
            {newmem.Slot.}Size := iDiff;
          end;
        end;
        {$IFDEF SCALEMM_DEBUG}
        ClearMediumSlots(iNextSlot2);
        {$ENDIF}
        Assert(Slots[iNextSlot2].Size > 0);

        //add remainder to "all free list by size"
        prevmem := Self.ThreadOwner.FFreeMediumMem[iDiff];
        Self.ThreadOwner.FFreeMediumMem[iDiff] := newmem;
        if prevmem <> nil then
        begin
          prevmem.PrevMediumMem := newmem;
          newmem.NextMediumMem  := prevmem;
        end
        else
          newmem.NextMediumMem  := nil;
        newmem.PrevMediumMem    := nil;
      end;

      //upsize
      Slots[iSlot].Size := iSlotsNeeded;
      //decrement with diff
      Dec(Self.FreeSlots, iSlotsNeeded - iCurrentSlots);
      {$IFDEF SCALEMM_DEBUG}
      ClearMediumSlots(iSlot);
      //CheckMem;
//      ThreadOwner.CheckAllMem;
      {$ENDIF}

      //in place resize
      Result := APointer;
    end
    else
    //cannot upsize inplace, so alloc new mem and copy contents
    begin
      //alloc new mem from own thread (+ some extra)
      Result := Scale_GetMem(ANewSize + C_MediumPageSize);

      iOldSize := (iCurrentSlots * C_MediumPageSize) - SizeOf(TMediumLargeMemHeader);
      //copy old contents
      Move(APointer^, Result^, iOldSize);
      //free old mem
      Scale_FreeMem(APointer);
    end;
  end;
end;

procedure TMediumBlock.ClearMediumSlots(aSlotIndex: Integer);
var
  i: Integer;
begin
  if (aSlotIndex < 1) or (aSlotIndex > High(Slots)) then Exit;

  for i := aSlotIndex+1 to Slots[aSlotIndex].Size + aSlotIndex - 1 do
  begin
    Self.Slots[i].Flag:= sfFiller;
    Self.Slots[i].Size  := 0; //-1;
  end;

  i := Slots[aSlotIndex].Size + aSlotIndex;
  if i <= High(Slots) then
    Assert( Slots[i].Flag in [sfFree, sfInUse] )
end;

procedure TMediumBlock.CheckMem;
var
  iFree,
  i: Integer;
  pheader: PMediumLargeMemHeader;
begin
  Assert(Self.Flags = mtMedium);
  Assert(Self.FreeSlots < Length(Slots) );
  Assert(Self.FreeSlotsFromOtherThreads < Length(Slots) );

  i     := 1;
  while i <= High(Slots) do
  begin
    Assert( (Slots[i].Flag = sfInUse) or (Slots[i].Flag = sfFree) );

    if Slots[i].Flag = sfInUse then
    begin
      pheader := Pointer( NativeUInt(@Self) + NativeUInt(i * C_MediumPageSize) );
      Assert(pheader.MediumSlot = @Slots[i]);
      Assert( (pheader.Flag = mtMedium) or
              (pheader.Flag = mtSmall) );

      if (pheader.Flag = mtSmall) then
      begin
        PSmallBlock(pheader).CheckMem;
      end;
    end
    else if Slots[i].Flag = sfFree then
    begin
      Inc(iFree, (Slots[i].Size));
    end;

    Inc(i, (Slots[i].Size));
  end;
  {$MESSAGE WARN 'enable again?'}
  //Assert(iFree = Self.FreeSlots + Self.FreeSlotsFromOtherThreads);

  if Self.NextBlock <> nil then
    Assert( @Self = Self.NextBlock.PrevBlock);
  if Self.PrevBlock <> nil then
    Assert( @Self = Self.PrevBlock.NextBlock);
end;

{ TMediumLargeHeader }
(*=========================================*)

class function TMediumLargeMemHeader.GetLargeMem(aSize: NativeUInt): Pointer;
var
  pheader: PMediumLargeMemHeader;
  iTotalSize: NativeUInt;
begin
  iTotalSize := (aSize + SizeOf(TMediumLargeMemHeader) + C_LargeBlockGranularity) and -C_LargeBlockGranularity;   //round to 64k

  pheader    := VirtualAlloc( nil,
                           iTotalSize,
                           MEM_COMMIT {$ifdef AlwaysAllocateTopDown} or MEM_TOP_DOWN{$endif},
                           PAGE_READWRITE);
  if pheader = nil then
  begin
    Assert(False);
    System.Error(reOutOfMemory);
  end;

  pheader.LargeMemSize  := iTotalSize;
  pheader.Flag := mtLarge;
  Result := Pointer( NativeUInt(pheader) + SizeOf(TMediumLargeMemHeader) );
end;

function TMediumLargeMemHeader.ReallocLargeMem(APointer: Pointer; ANewSize: NativeInt): Pointer;
var
  pheader: PMediumLargeMemHeader;
  iTotalSize, iCurrentSize: NativeInt;
  iExtraSize: Integer;
  pNextMem: Pointer;
  meminfo: TMemoryBasicInformation;
begin
  //get header in front of 4k slot
  pheader    := PMediumLargeMemHeader(
                    NativeUInt(APointer) and
                    -C_MediumPageSize);

  iCurrentSize  := pheader.LargeMemSize;
  iTotalSize    := ANewSize + SizeOf(TMediumLargeMemHeader);

  //same size?
  if iTotalSize = iCurrentSize then
    Result := APointer
  //smaller size
  else if iTotalSize < iCurrentSize then
  begin
    //too much downsize? so switch to medium or small mem
    if (ANewSize <  900 * 1024) or         //900kb
       (ANewSize < iTotalSize div 2) then  //more than 50% smaller
    begin
      //alloc new mem from own thread
      Result := Scale_GetMem(ANewSize);
      //copy old contents
      Move(APointer^, Result^, ANewSize);
      //free old mem
      Scale_FreeMem(APointer);
      Exit;
    end;

    Result := APointer;
  end
  //upsize
  else
  begin
    pNextMem := Pointer( NativeUInt(pheader) + NativeUInt(iCurrentSize) );
    //try to expand current mem (in place)
    VirtualQuery(pNextMem, meminfo, SizeOf(meminfo));
    //next mem is free?
    if (meminfo.State = MEM_FREE) then
    begin
      iTotalSize := (iTotalSize + C_LargeBlockGranularity) and -C_LargeBlockGranularity;   //round to 64k
      iExtraSize := iTotalSize - iCurrentSize;
      //Enough mem to grow in place?
      if (meminfo.RegionSize >= Cardinal(iExtraSize)) then
      begin
        {Attempy to reserve the address range (which will fail if another
         thread has just reserved it) and commit it immediately afterwards.}
        if (VirtualAlloc(pNextMem, iExtraSize, MEM_RESERVE, PAGE_READWRITE) <> nil)
          and (VirtualAlloc(pNextMem, iExtraSize, MEM_COMMIT, PAGE_READWRITE) <> nil) then
        begin
          pheader.LargeMemSize := iTotalSize;
          Result       := APointer;
          Exit;
        end;
      end;
    end;

    //alloc new mem from own thread
    Result := GetLargeMem(iTotalSize);

    iCurrentSize := iCurrentSize - SizeOf(TMediumLargeMemHeader);
    //copy old contents
    Move(APointer^, Result^, iCurrentSize);
    //free old mem
    _ThreadManager.FreeLargeMem(pheader);
  end;
end;

{ TSmallHeader }
(*=========================================*)

class function TSmallBlock.AllocSlot: PSmallBlock;
begin
  Result := Scale_GetMem( C_MediumPageSize - SizeOf(TMediumLargeMemHeader) ); //alloc 1 page of 4kb
  if Result = nil then
    System.Error(reOutOfMemory);

  Result := PSmallBlock( NativeUInt(Result) - SizeOf(TMediumLargeMemHeader) );
  Result.Init;
end;

procedure TSmallBlock.Init;
var
  tempslot: PMediumSlot;
begin
  tempslot := Self.Slot;
  FillChar(Self, SizeOf(Self), 0);

  Self.Slot  := tempslot;
  Self.Flag := mtSmall;

  Slots[Low(Slots)]         := -1 * (C_Small_FirstSlots - Low(Slots));  //-1 = in use
  FreeSlots                 := Length(Slots) - C_Small_FirstSlots + Low(Slots);
  Slots[C_Small_FirstSlots] := FreeSlots;
end;

function TSmallBlock.GetSizeOfmem(APointer: Pointer): NativeUInt;
var
  slotoffset: NativeUInt;
  iSlot: Integer;
begin
  slotoffset := NativeUInt(APointer) - NativeUInt(@Self);
  iSlot      := slotoffset div C_SmallPageSize;
  Result     := Abs(Self.Slots[iSlot]) * C_SmallPageSize;
end;

function TSmallBlock.ReallocSmallMem(APointer: Pointer; ANewSize: NativeInt): Pointer;
var
  slotoffset: NativeUInt;
  iSlot: Integer;
  iNextSlot, iNextSlot2,
  iDiff,
  iCurrentSlots,
  iSlotsNeeded: Integer;
  iOldSize: Integer;
  psmallfree, pprevfree: PSmallFreeMemHeader;
  block: PMediumBlock;
begin
  slotoffset := NativeUInt(APointer) - NativeUInt(@Self);
  iSlot      := slotoffset div C_SmallPageSize;
  Assert(Self.Slots[iSlot] < 0); //in use
  iCurrentSlots := Abs(Self.Slots[iSlot]);

  iSlotsNeeded  := (ANewSize div C_SmallPageSize);
  if (ANewSize mod C_SmallPageSize) > 0 then
    inc(iSlotsNeeded);

  //same size?
  if iSlotsNeeded = iCurrentSlots then
    Result := APointer
  //smaller size
  else if iSlotsNeeded < iCurrentSlots then
  begin
    Result := APointer;

    //more than 1 slot free?
    //downsize in place and release unused mem
    iDiff  := iCurrentSlots - iSlotsNeeded;
    if iDiff > 1 then
    begin
      //downsize
      Slots[iSlot] := - iSlotsNeeded;
      Inc(Self.FreeSlots, iDiff);

      //remainder
      iNextSlot        := iSlot + iSlotsNeeded;
      Slots[iNextSlot] := iDiff;
      //create remainder
      psmallfree       := PSmallFreeMemHeader( NativeUInt(APointer) + NativeUInt(iSlotsNeeded * C_SmallPageSize));
      psmallfree.Slot  := @Slots[iNextSlot];

      //get block = front of slot array in first 4k slot
      block := PMediumBlock( NativeUInt(Self.Slot) and
                             -C_MediumPageSize );
      //add remainder to "all free list by size"
      pprevfree := block.ThreadOwner.FFreeSmallMem[iDiff];
      block.ThreadOwner.FFreeSmallMem[iDiff] := psmallfree;
      if pprevfree <> nil then
      begin
        pprevfree.PrevMem := psmallfree;
        psmallfree.NextMem  := pprevfree;
      end
      else
        psmallfree.NextMem  := nil;
      psmallfree.PrevMem    := nil;
      psmallfree.ThreadFreed := False;

      {$IFDEF SCALEMM_DEBUG}
//      ClearMediumSlots(iNextSlot);
//      ThreadOwner.CheckAllMem;
      //CheckMem;
      {$ENDIF}
    end;
  end
  //upsize
  else
  begin
    iNextSlot := iSlot + iCurrentSlots;
    iDiff     := iSlotsNeeded - iCurrentSlots;
    //extend current slot if room available after it
    if (iNextSlot < High(Slots)) and
       ((Self.Slots[iNextSlot]) >= iDiff) then
    begin
      //new (bigger) size
      Slots[iSlot] := -1 * iSlotsNeeded;
      Dec(Self.FreeSlots, iSlotsNeeded - iCurrentSlots);

      //remove extra free mem
      iDiff      := Self.Slots[iNextSlot] - iDiff;
      psmallfree := PSmallFreeMemHeader( NativeUInt(@Self) + NativeUInt(iNextSlot * C_SmallPageSize) );
      Assert(psmallfree.Slot = @Slots[iNextSlot]);
      //get block = front of slot array in first 4k slot
      block := PMediumBlock( NativeUInt(Self.Slot) and
                             -C_MediumPageSize );

      if not psmallfree.ThreadFreed then  //items freed in other thread are not in linked list
      begin
        //first one? then remove from linked list array
        if psmallfree.PrevMem = nil then
        begin
          iOldSize := Self.Slots[iNextSlot];
          Assert(block.ThreadOwner.FFreeSmallMem[iOldSize] = psmallfree);
          block.ThreadOwner.FFreeSmallMem[iOldSize] := psmallfree.NextMem;
          //unlink from next
          if psmallfree.NextMem <> nil then
            psmallfree.NextMem.PrevMem := nil;
        end
        else
        //else unlink from anywhere in the linked list
        begin
          psmallfree.PrevMem.NextMem := psmallfree.NextMem;
          if psmallfree.NextMem <> nil then
            psmallfree.NextMem.PrevMem := psmallfree.PrevMem;
        end;
      end;

      //remainder after upsize?
      if iDiff > 0 then
      begin
        iNextSlot2        := iSlot + iSlotsNeeded;
        Slots[iNextSlot2] := iDiff;

        //create remainder
        psmallfree       := PSmallFreeMemHeader( NativeUInt(@Self) + NativeUInt(iNextSlot2 * C_SmallPageSize) );
        psmallfree.Slot  := @Slots[iNextSlot2];
        //add remainder to "all free list by size"
        pprevfree := block.ThreadOwner.FFreeSmallMem[iDiff];
        block.ThreadOwner.FFreeSmallMem[iDiff] := psmallfree;
        if pprevfree <> nil then
        begin
          pprevfree.PrevMem  := psmallfree;
          psmallfree.NextMem := pprevfree;
        end
        else
          psmallfree.NextMem := nil;
        psmallfree.PrevMem   := nil;
        psmallfree.ThreadFreed := False;
      end;

      {$IFDEF SCALEMM_DEBUG}
      ClearSmallSlots(iSlot);
      CheckMem;
      {$ENDIF}

      //in place resize
      Result := APointer;
    end
    else
    begin
      //alloc new mem from own thread
      Result := Scale_GetMem(ANewSize * 2);

      iOldSize := iCurrentSlots * C_SmallPageSize;
      //copy old contents
      Move(APointer^, Result^, iOldSize);
      //free old mem
      Scale_FreeMem(APointer);
    end;
  end
end;

procedure TSmallBlock.ClearSmallSlots(aSlotIndex: Integer);
var
  i, iSize: Integer;
begin
  iSize := Abs(Self.Slots[aSlotIndex]);
  for i := aSlotIndex+1 to aSlotIndex + iSize - 1 do
    Slots[i] := Low(ShortInt);
end;

procedure TSmallBlock.CheckMem;
var
  iFree,
  i: Integer;
begin
  Assert(Self.Flag = mtSmall);
  Assert(Self.FreeSlots < Length(Slots) );
  Assert(Self.FreeSlotsFromOtherThreads < Length(Slots) );

  iFree := 0;
  i := Low(Slots);
  while i <= High(Slots) do
  begin
    Assert(Slots[i] > Low(Shortint));
    Assert(Slots[i] <> 0);
    if Slots[i] > 0 then
    begin
      Inc(iFree, Slots[i]);
    end;
    Inc(i, Abs(Slots[i]))
  end;
  //check free count
  Assert(Self.FreeSlots + FreeSlotsFromOtherThreads = iFree);

  if Self.NextSmallSlot <> nil then
    Assert( @Self = Self.NextSmallSlot.PrevSmallSlot);
  if Self.PrevSmallSlot <> nil then
    Assert( @Self = Self.PrevSmallSlot.NextSmallSlot);
end;

(*=========================================*)

{$ifdef USEMEMMANAGEREX}
function Scale_RegisterMemoryLeak(P: Pointer): Boolean;
begin
  { TODO : implement memory leak checking }
//  Result := OldMM.RegisterExpectedMemoryLeak(p);
  Result := True;
end;

function Scale_UnregisterMemoryLeak(P: Pointer): Boolean;
begin
//  Result := OldMM.UnregisterExpectedMemoryLeak(p);
  Result := True;
end;
{$endif}

type
  TEndThreadProc = procedure(ExitCode: Integer);
var
  _OldEndThread: TEndThreadProc;

procedure NewEndThread(ExitCode: Integer); //register; // ensure that calling convension matches EndThread
begin
  // free all thread mem
  GlobalManager.FreeThreadManager( GetCurrentThreadManager );

  // OldEndThread(ExitCode);  todo: make trampoline with original begin etc
  // code of original EndThread;
  ExitThread(ExitCode);
end;

type
  PJump = ^TJump;
  TJump = packed record
    OpCode  : Byte;
    Distance: Integer;
  end;
var
  _NewCode: TJump = (OpCode  : $E9;
                     Distance: 0);

// redirect calls to System.EndThread to NewEndThread
procedure PatchThread;
var
  pEndThreadAddr: PJump;
  iOldProtect: DWord;
begin
  pEndThreadAddr := Pointer(@EndThread);
  Scale_VirtualProtect(pEndThreadAddr, 5, PAGE_EXECUTE_READWRITE, iOldProtect);
  // calc jump to new function
  _NewCode.Distance := Cardinal(@NewEndThread) - (Cardinal(@EndThread) + 5);
  // store old
  _OldEndThread := TEndThreadProc(pEndThreadAddr);
  // overwrite with jump to new function
  pEndThreadAddr^  := _NewCode;
  // flush CPU
  FlushInstructionCache(GetCurrentProcess, pEndThreadAddr, 5);
end;

const
{$ifdef USEMEMMANAGEREX}
  _ScaleMM: TMemoryManagerEx = (
    GetMem    : Scale_GetMem;
    FreeMem   : Scale_FreeMem;
    ReallocMem: Scale_ReallocMem;
    AllocMem  : Scale_AllocMem;
    RegisterExpectedMemoryLeak  : Scale_RegisterMemoryLeak;
    UnregisterExpectedMemoryLeak: Scale_UnregisterMemoryLeak );
{$else}
  _ScaleMM: TMemoryManager = (
    GetMem    : Scale_GetMem;
    FreeMem   : Scale_FreeMem;
    ReallocMem: Scale_ReallocMem );
{$endif}

var
{$ifdef USEMEMMANAGEREX}
  _OldMM: TMemoryManagerEx;
{$else}
  _OldMM: TMemoryManager;
{$endif}

procedure ScaleMMInstall;
begin
  // Hook memory Manager
  GetMemoryManager(_OldMM);
  if @_OldMM <> @_ScaleMM then
    SetMemoryManager(_ScaleMM);

  // init main thread manager
  GlobalManager.Init;

  // we need to patch System.EndThread to properly mark memory to be freed
  PatchThread;
end;

initialization
  ScaleMMInstall;
  Assert( SizeOf(TSmallBlock) mod 32 = 0);
  Assert( SizeOf(TSmallFreeMemHeader) <= C_SmallPageSize);

finalization
  SetMemoryManager(_OldMM);
  { TODO : check for memory leaks }
  //GlobalManager.FreeAllMemory;

end.
