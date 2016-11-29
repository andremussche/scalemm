unit formMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmMain = class(TForm)
    btnMain: TButton;
    btnSub: TButton;
    btnSub2: TButton;
    procedure btnMainClick(Sender: TObject);
    procedure btnSubClick(Sender: TObject);
    procedure btnSub2Click(Sender: TObject);
  private
    hMain: THandle;
    hSub, hSub2: THandle;
  public
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.btnMainClick(Sender: TObject);
begin
  hMain := LoadLibrary('maindll.dll');

  btnSub.Enabled  := (hMain > 32);
  btnSub2.Enabled := (hMain > 32);
end;

procedure TfrmMain.btnSub2Click(Sender: TObject);
begin
  hSub2 := LoadLibrary('subdll2.dll');
end;

procedure TfrmMain.btnSubClick(Sender: TObject);
begin
  hSub := LoadLibrary('subdll.dll');
end;

end.
