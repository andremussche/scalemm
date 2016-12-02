unit formMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Shared.Types;

type
  TfrmMain = class(TForm)
    btnMain: TButton;
    btnSub: TButton;
    btnSub2: TButton;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    procedure btnMainClick(Sender: TObject);
    procedure btnSubClick(Sender: TObject);
    procedure btnSub2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
  private
    hMain: THandle;
    hSub, hSub2: THandle;

    FMain, FSub, FSub2: ITestActions;
  public
  end;

var
  frmMain: TfrmMain;

implementation

uses
  Shared.Actions, smmDump;

{$R *.dfm}

procedure TfrmMain.btnMainClick(Sender: TObject);
var
  p: TFuncTestActions;
begin
  hMain := LoadLibrary('maindll.dll');
  p := GetProcAddress(hMain, 'TestActions');
  if Assigned(p) then
    FMain := p();

  btnSub.Enabled  := (hMain > 32);
  btnSub2.Enabled := (hMain > 32);
end;

procedure TfrmMain.btnSub2Click(Sender: TObject);
var
  p: TFuncTestActions;
begin
  hSub2 := LoadLibrary('subdll2.dll');
  p := GetProcAddress(hSub2, 'TestActions');
  if Assigned(p) then
    FSub2 := p();
end;

procedure TfrmMain.btnSubClick(Sender: TObject);
var
  p: TFuncTestActions;
begin
  hSub := LoadLibrary('subdll.dll');
  p := GetProcAddress(hSub, 'TestActions');
  if Assigned(p) then
    FSub := p();
end;

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  FSub.FreeStoredData;
  FSub.StoreData( FMain.GetRandomData );
end;

procedure TfrmMain.Button2Click(Sender: TObject);
begin
  FSub2.FreeStoredData;
  FSub2.StoreData( FMain.GetRandomData );
end;

procedure TfrmMain.Button3Click(Sender: TObject);
begin
  FSub.CreateTestThread;
end;

procedure TfrmMain.Button4Click(Sender: TObject);
begin
  FSub2.CreateTestThread;
end;

procedure TfrmMain.Button5Click(Sender: TObject);
begin
  TThread.CreateAnonymousThread(
    procedure
    var data: TStringArray;
    begin
      data := FMain.GetRandomData;

      TThread.CreateAnonymousThread(
        procedure
        begin
          FSub.StoreData(data);
          data := nil;

          TThread.CreateAnonymousThread(
            procedure
            begin
              FSub.FreeStoredData;
              sleep(200);
            end).Start;

            sleep(200);
        end).Start;

      sleep(200);
    end).Start;
end;

procedure TfrmMain.Button6Click(Sender: TObject);
var i: ITestActions;
begin
  i := TTestActions.Create;
  i.CreateTestThread;
end;

procedure TfrmMain.Button7Click(Sender: TObject);
begin
  DumpScaleMMStateToFile('test.' + FormatDateTime('hhnnss', Now) + '.log');
end;

end.
