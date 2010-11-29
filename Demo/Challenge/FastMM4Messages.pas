{

Fast Memory Manager: Messages

Change these strings to translate FastMM into the language of your choice.

}

unit FastMM4Messages;

interface

{$Include FastMM4Options.inc}

const
  {The name of the debug info support DLL}
  DebugInfoLibraryName = 'FastMM_DebugInfo.dll';
  DebugInfoDllNotAvailableMsg = #13#10#13#10'(The ' + DebugInfoLibraryName + ' library is not available, so no unit/line number debug information can be displayed in the stack traces.)';
  {Event log strings}
  LogFileExtension = '_MemoryManager_EventLog.txt'#0;
  CRLF = #13#10;
  EventSeparator = '--------------------------------';
  {Class name messages}
  UnknownClassNameMsg = 'Unknown';
  {Stack trace Message}
  CurrentStackTraceMsg = #13#10#13#10'The current stack trace leading to this error (return addresses): ';
  {Memory dump message}
  MemoryDumpMsg = #13#10#13#10'Current memory dump of 256 bytes starting at pointer address ';
  {Block Error Messages}
  ErrorMsgHeader = 'FastMM has detected an error during a ';
  GetMemMsg = 'GetMem';
  FreeMemMsg = 'FreeMem';
  ReallocMemMsg = 'ReallocMem';
  BlockCheckMsg = 'free block scan';
  OperationMsg = ' operation. ';
  BlockHeaderCorruptedMsg = 'The block header has been corrupted. ';
  BlockFooterCorruptedMsg = 'The block footer has been corrupted. ';
  FreeModifiedErrorMsg = 'FastMM detected that a block has been modified after being freed. ';
  DoubleFreeErrorMsg = 'An attempt has been made to free/reallocate an unallocated block.';
  PreviousBlockSizeMsg = #13#10#13#10'The previous block size was: ';
  CurrentBlockSizeMsg = #13#10#13#10'The block size is: ';
  StackTraceAtPrevAllocMsg = #13#10#13#10'Stack trace of when this block was previously allocated (return addresses):';
  StackTraceAtAllocMsg = #13#10#13#10'Stack trace of when this block was allocated (return addresses):';
  PreviousObjectClassMsg = #13#10#13#10'The block was previously used for an object of class: ';
  CurrentObjectClassMsg = #13#10#13#10'The block is currently used for an object of class: ';
  StackTraceAtFreeMsg = #13#10#13#10'Stack trace of when the block was previously freed (return addresses):';
  BlockErrorMsgTitle = 'FastMM: Memory Error Detected';
  {Virtual Method Called On Freed Object Errors}
  StandardVirtualMethodNames: array[1 + vmtParent div 4 .. -1] of PChar = (
    'SafeCallException',
    'AfterConstruction',
    'BeforeDestruction',
    'Dispatch',
    'DefaultHandler',
    'NewInstance',
    'FreeInstance',
    'Destroy');
  VirtualMethodErrorHeader = 'FastMM has detected an attempt to call a virtual method on a freed object. An access violation will now be raised in order to abort the current operation.';
  InterfaceErrorHeader = 'FastMM has detected an attempt to use an interface of a freed object. An access violation will now be raised in order to abort the current operation.';
  BlockHeaderCorruptedNoHistoryMsg = ' Unfortunately the block header has been corrupted so no history is available.';
  FreedObjectClassMsg = #13#10#13#10'Freed object class: ';
  VirtualMethodName = #13#10#13#10'Virtual method: ';
  VirtualMethodOffset = 'Offset +';
  VirtualMethodAddress = #13#10#13#10'Virtual method address: ';
  StackTraceAtObjectAllocMsg = #13#10#13#10'Stack trace of when the object was allocated (return addresses):';
  StackTraceAtObjectFreeMsg = #13#10#13#10'Stack trace of when the object was subsequently freed (return addresses):';
  {Installation Messages}
  AlreadyInstalledMsg = 'FastMM4 is already installed.';
  AlreadyInstalledTitle = 'Already installed.';
  OtherMMInstalledMsg = 'FastMM4 cannot be installed since another third party memory '
    + 'manager has already installed itself.'#13#10'If you want to use FastMM4, '
    + 'please make sure that FastMM4.pas is the very first unit in the "uses"'
    + #13#10'section of your project''s .dpr file.';
  OtherMMInstalledTitle = 'Cannot install FastMM4 - Another memory manager is already installed';
  MemoryAllocatedMsg = 'FastMM4 cannot install since memory has already been '
    + 'allocated through the default memory manager.'#13#10'FastMM4.pas MUST '
    + 'be the first unit in your project''s .dpr file, otherwise memory may '
    + 'be allocated'#13#10'through the default memory manager before FastMM4 '
    + 'gains control. '#13#10#13#10'If you are using an exception trapper '
    + 'like MadExcept (or any tool that modifies the unit initialization '
    + 'order),'#13#10'go into its configuration page and ensure that the '
    + 'FastMM4.pas unit is initialized before any other unit.';
  MemoryAllocatedTitle = 'Cannot install FastMM4 - Memory has already been allocated';
  {Leak checking messages}
  LeakLogHeader = 'A memory block has been leaked. The size is: ';
  LeakMessageHeader = 'This application has leaked memory. ';
  SmallLeakDetail = 'The small block leaks are:'#13#10;
  LargeLeakDetail = 'The sizes of leaked medium and large blocks are: ';
  BytesMessage = ' bytes: ';
  StringBlockMessage = 'String';
  LeakMessageFooter = #13#10
{$ifndef HideMemoryLeakHintMessage}
    + #13#10'You may use a tool like MemProof to help you track down the source of these leaks. '
    + 'Steps to use MemProof:'#13#10'  1) Remove FastMM from the project.'#13#10'  2) Enable TD32 debug info in compiler options.'#13#10
    + '  3) Build (not compile) the application.'#13#10'  4) Ensure that the MemProof search directories are configured correctly.'#13#10
    + '  5) Run the application inside MemProof.'#13#10
    + 'MemProof is freeware and can be downloaded from http://www.automatedqa.com/downloads/memproof.'#13#10#13#10
    + 'Note: '
  {$ifdef RequireIDEPresenceForLeakReporting}
    + 'This memory leak check is only performed if Delphi is currently running on the same computer. '
  {$endif}
  {$ifdef LogMemoryLeakDetailToFile}
    + 'Memory leak detail is logged to a text file in the same folder as this application. '
  {$endif}
    + 'To disable this check, undefine "EnableMemoryLeakReporting".'#13#10
{$endif}
    + #0;
  LeakMessageTitle = 'FastMM: Memory Leak Detected';
{$ifdef UseOutputDebugString}
  FastMMInstallMsg = 'FastMM has been installed.';
  FastMMInstallSharedMsg = 'Sharing an existing instance of FastMM.';
  FastMMUninstallMsg = 'FastMM has been uninstalled.';
  FastMMUninstallSharedMsg = 'Stopped sharing an existing instance of FastMM.';
{$endif}

implementation

end.
