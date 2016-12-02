program Main;

uses
  ScaleMM2 in '..\ScaleMM2.pas',
  Vcl.Forms,
  formMain in 'formMain.pas' {frmMain},
  Shared.Types in 'Shared.Types.pas',
  Shared.Actions in 'Shared.Actions.pas';

{$ifNdef MMSharingEnabled}MMSharingEnabled must be defined!{$endif}
{$ifNdef MMSharingDLLOwnerEnabled}MMSharingDLLOwnerEnabled must be defined!{$endif}

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
