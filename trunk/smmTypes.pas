unit smmTypes;

interface

type
  {$if CompilerVersion <= 20}
  // from Delphi 6 up to Delphi 2007
  // also for 2009: http://code.google.com/p/scalemm/issues/detail?id=1
  NativeUInt = Cardinal;
  NativeInt  = Integer;
  {$ifend}

  PBaseMemHeader     = ^TBaseMemHeader;
  PBaseFreeMemHeader = ^TBaseFreeMemHeader;
  PBaseBlockMemory   = ^TBaseBlockMemory;
  PBaseThreadMemory  = ^TBaseThreadMemory;
  PBaseThreadManager = ^TBaseThreadManager;

  TBaseMemHeader = object
    //small, medium and large mem can add extra stuff IN FRONT
    Size : NativeUInt;  
    //must be last of "universal" header!
    OwnerBlock: PBaseBlockMemory;
  end;

  TBaseFreeMemHeader = object
    //small, medium and large mem can add extra stuff IN FRONT
    Size  : NativeUInt;
    OwnerBlock : PBaseBlockMemory;

    //Extra data of free item:---------------------------------
    NextThreadFree: PBaseFreeMemHeader;  //linked list of interthread memory
  end;

  TSizeType = (stSmall, stMedium, stLarge);

  TBaseBlockMemory = object
    //SizeType   : TSizeType;
    OwnerThread: PBaseThreadMemory;
    //small, medium and large mem can add extra stuff BEHIND
  end;

  TBaseThreadMemory = object
    SizeType    : TSizeType;
    OwnerManager: PBaseThreadManager;
    //small, medium and large mem can add extra stuff BEHIND
  end;

  TBaseThreadManager = object
    FOtherThreadFreedMemory: PBaseFreeMemHeader;
    FOtherThreadFreeLock: Boolean;

    FThreadId: LongWord;
    FThreadTerminated: Boolean;
    //extra stuff BEHIND
  end;

  TScanDirection = (sdNone, sdPrevious, sdNext, sdBoth);

implementation

end.
