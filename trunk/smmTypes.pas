unit smmTypes;

interface

type
  {$if CompilerVersion < 19}
  // from Delphi 6 up to Delphi 2007
  NativeUInt = Cardinal;
  NativeInt  = Integer;
  {$ifend}

  PBaseMemHeader     = ^TBaseMemHeader;
  PBaseBlockMemory   = ^TBaseBlockMemory;
  PBaseThreadMemory  = ^TBaseThreadMemory;
  PBaseThreadManager = ^TBaseThreadManager;

  TBaseMemHeader = object
    //small, medium and large mem can add extra stuff IN FRONT
    //Size : NativeUInt;

    //must be last of "universal" header!
    OwnerBlock: PBaseBlockMemory;
  end;

  TBaseFreeMemHeader = object
    //small, medium and large mem can add extra stuff IN FRONT
    //Size  : NativeUInt;

    Owner : PBaseBlockMemory;

    //Extra data of free item:---------------------------------
    NextThreadFree: Pointer;  //linked list of interthread memory
  end;

  TBaseBlockMemory = object
    OwnerThread: PBaseThreadMemory;
    //small, medium and large mem can add extra stuff BEHIND
  end;

  TSizeType = (stSmall, stMedium, stLarge);

  TBaseThreadMemory = object
    SizeType    : TSizeType;
    OwnerManager: PBaseThreadManager;
    //small, medium and large mem can add extra stuff BEHIND
  end;

  TBaseThreadManager = object
    FThreadId: LongWord;
    FThreadTerminated: Boolean;
    //extra stuff BEHIND
  end;

implementation

end.
