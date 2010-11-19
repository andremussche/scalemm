program MemTestGUI_D2010;

uses
  ScaleMM in '..\ScaleMM.pas',
//  ThreadHook in '..\ThreadHook.pas',
//  uWinApiFunctions in '..\uWinApiFunctions.pas',
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


