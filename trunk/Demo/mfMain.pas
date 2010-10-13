unit mfMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TfrmMain = class(TForm)
    edtThreadCount: TLabeledEdit;
    btnStart: TButton;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
  private
    { Private declarations }
    FCsvFile: Tstrings;
    procedure ExecuteTest;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  _uTextThread;

{$R *.dfm}

procedure TfrmMain.btnStartClick(Sender: TObject);
begin
  ExecuteTest;
end;

procedure TfrmMain.ExecuteTest;
var
  iTotal, iGem,
  i, iCount: integer;
  l: TList;
  t: TDummyStringThread;
begin
  t := nil;
  if TryStrToInt(edtThreadCount.Text,iCount) then
  begin
    Memo1.Lines.Add('---------------------------');
    Memo1.Lines.Add(format('Testing with %d threads...',[iCount]));

    //create threads
    l := TList.Create;
    for i := 1 to iCount do
    begin
      t := TDummyStringThread.Create;
      l.Add(t);
    end;

    //run threads
    for i := 0 to iCount-1 do
    begin
      t := l.Items[i];
      t.Start;
    end;

    //wait...
    iTotal := 0;
    t.WaitFor;
    for i := 0 to iCount-1 do
    begin
      t := l.Items[i];
      t.Terminate;
      t.WaitFor;
      Memo1.Lines.Add(format('Thread %d: %d',[i+1, t.Duration_msec]));
      iTotal := iTotal + t.Duration_msec;
    end;
    //avg
    iGem := iTotal div iCount;
    Memo1.Lines.Add(format('Average: %d', [iGem]));
    FCsvFile.Add(Format('%d;%d;%d',[iCount, -1, iGem]));

    Memo1.Lines.Add('Done.');
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Memo1.Clear;
  FCsvFile := TStringList.Create;

//  edtThreadCount.Text := '4';
//  ExecuteTest;
//  Application.Terminate;
end;

end.
