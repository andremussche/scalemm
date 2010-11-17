program MemTestGUI_D2010;

uses
  ScaleMM in '..\ScaleMM.pas',
  ThreadHook in '..\ThreadHook.pas',
  Forms,
  mfMain in 'mfMain.pas' {frmMain},
  _uTextThread in '_uTextThread.pas',
  uWinApiFunctions in '..\uWinApiFunctions.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
