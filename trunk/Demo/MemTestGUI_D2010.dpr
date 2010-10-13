program MemTestGUI_D2010;

uses
  //ScaleMM in '..\ScaleMM.pas',
  //TopMemory in 'TopMM\TopMemory.pas',
  //FastMM4,
  Forms,
  mfMain in 'mfMain.pas' {frmMain},
  _uTextThread in '_uTextThread.pas',
  uWinApiFunctions in '..\uWinApiFunctions.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
