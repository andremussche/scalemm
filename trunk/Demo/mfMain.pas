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
  _uTextThread, KOLDetours, StrCopyFix;

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

procedure Dummy;
begin
  Copy('',0,0);
  //004AC279 E82AADF5FF       call @UStrCopy         FFF5ADAA
  //  System.pas.18091: begin
  //00406FA8 55               push ebp

  //A52D1
  //FFFFFFFF - A52D1 = FFF5AD2E - FFF5ADAA = 7C
end;

var
  iDummy: Integer = 0;

procedure CopyHack;
var
  p,p2: Pointer;
begin
  if iDummy > 0 then
    Dummy;
  p2 := nil;

  p := @Dummy;
  while PByte(p)^ <> $C3{ret} do
  begin
    inc(PByte(p));
    if PByte(p)^ = $E8{call} then
    begin
      inc(PByte(p));
      p2 := PPointer(p)^;
      Break;
    end;
  end;

  if p2 <> nil then
  begin
    p2 := Pointer( $FFFFFFFF - Cardinal(p2) );
    p2 := Pointer( Cardinal(p) - Cardinal(p2) );
    //p2 := Pointer( $FFFFFFFF - Cardinal(p)
    KOLDetours.InterceptCreate(p2, @StrCopyFix._UStrCopy)
    //test
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var s:string;
begin
  Memo1.Clear;
  FCsvFile := TStringList.Create;

//  CopyHack;
  //004AB339 E86ABCF5FF       call @UStrCopy
  //004AB3C0 C3               ret

  //Dummy;
//  KOLDetours.InterceptCreate(@System.Copy, @StrCopyFix._UStrCopy)

//  s := 'test';
//  s := _UStrCopy(s, 0, 2);
//  edtThreadCount.Text := '4';
//  ExecuteTest;
//  Application.Terminate;
end;

end.
