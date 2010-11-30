program MemTestGUI_D2010;

uses
//  ShareMem,
//  SynScaleMM in '..\SynScaleMM.pas',
//  ScaleMM in '..\ScaleMM.pas',

//  BucketMem,
//  BucketMem_ASM,
//  DKC_IA32_MM_Unit,
//  EWCMM,
  MultiMM,
//  nxReplacementMemoryManager,
//  PSDMemoryManager,
//  QMemory,
//  RecyclerMM,
//  WinMem,
//  LocalHeapMM,
//  HeapMM,

  //
  _uTextThread in '_uTextThread.pas',
  Forms,
  mfMain in 'mfMain.pas' {frmMain};

{$R *.res}

begin
//  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

  TopInstall in 'TopMM\TopInstall.pas',

//  ScaleMM in '..\ScaleMM.pas',

