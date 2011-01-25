program MemTestGUI_D2010;

uses
  ScaleMM2 in '..\ScaleMM2.pas',
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
