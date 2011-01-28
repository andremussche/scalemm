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
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure MyButton3Click(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    FCsvFile: Tstrings;
    procedure ExecuteTest;
  public
    { Public declarations }
  end;

  TMyButton = class(TButton)
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure WMWindowPosChanging(var Message: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  _uTextThread;
  //KOLDetours, StrCopyFix;

{$R *.dfm}

procedure TfrmMain.btnStartClick(Sender: TObject);
begin
  ExecuteTest;
end;

function BitScanFirst(aValue: Integer): Integer;
asm
  BSF	EAX, aValue;
end;

function BitTestReset(aValue: Integer; aBit: Integer): Integer;
asm
  BTR	aValue, aBit;
end;

procedure TfrmMain.Button1Click(Sender: TObject);
var
  i1, i2: Integer;
begin
  i1 := 1;
  i2 := BitScanFirst(i1);
  i1 := 2;
  i2 := BitScanFirst(i1);

  i1 := 9;
  i2 := BitScanFirst(i1);
  i2 := BitTestReset(i1, i2);
end;

procedure TfrmMain.Button3Click(Sender: TObject);
var
  WindowPlacement: TWindowPlacement;
begin
  with TMyButton.Create(Self) do
  begin
    Parent := Self;
    OnClick := MyButton3Click;
  end;
  Exit;

  {  WindowPlacement.Length := SizeOf(WindowPlacement);
  GetWindowPlacement(Button3.Handle, WindowPlacement);
  WindowPlacement.rcNormalPosition := Button3.BoundsRect;
  InflateRect(WindowPlacement.rcNormalPosition, 10, 10);
  SetWindowPlacement(Button3.Handle, WindowPlacement);
  }
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
      {$if CompilerVersion < 19}
      t.Resume;
      {$else}
      t.Start;
      {$ifend}
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

(*
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

//  CopyHack;
  //004AB339 E86ABCF5FF       call @UStrCopy
  //004AB3C0 C3               ret

  //Dummy;
//  KOLDetours.InterceptCreate(@System.Copy, @StrCopyFix._UStrCopy)

//  s := 'test';
//  s := _UStrCopy(s, 0, 2);
*)

const
  ABOVE_NORMAL_PRIORITY_CLASS = $00008000;

procedure TfrmMain.FormCreate(Sender: TObject);
//var s:string;
begin
  Memo1.Clear;
  FCsvFile := TStringList.Create;

  SetPriorityClass(GetCurrentProcess, ABOVE_NORMAL_PRIORITY_CLASS);

//  edtThreadCount.Text :=  '4';
//  ExecuteTest;
//  Application.Terminate;
//  GetMemory(-1)
end;

procedure TfrmMain.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  list: tlist;
  h: THandle;
  p, p2: TPoint;

  procedure __TestPos;
  var c: TControl;
      wControl: TWinControl;
  begin
    h  := WindowFromPoint(p2);
    c  := FindControl(h);

    if (c <> nil) and (c <> self) then
    begin
      mouse.CursorPos := p2;
      //MessageDlg(Format('%s - %s', [c.ClassName, c.Name]), mtWarning, [mbOK], 0);
    end;
  end;

begin
  list := TList.Create;
  p    := Mouse.CursorPos;
  try
    p2 := p;
    inc(p2.X, 15);
    __TestPos;
    inc(p2.Y, 15);
    __TestPos;
    inc(p2.Y, -30);
    __TestPos;

    p2 := p;
    inc(p2.X, -15);
    __TestPos;
    inc(p2.Y, -15);
    __TestPos;
    inc(p2.Y, 30);
    __TestPos;
  finally
    list.Free;
  end;
end;

procedure TfrmMain.MyButton3Click(Sender: TObject);
var
  WindowPlacement: TWindowPlacement;
begin
  {
  WindowPlacement.Length := SizeOf(WindowPlacement);
  GetWindowPlacement( TMyButton(Sender).Handle, WindowPlacement);
  WindowPlacement.rcNormalPosition := TMyButton(Sender).BoundsRect;
  InflateRect(WindowPlacement.rcNormalPosition, 10, 10);
  SetWindowPlacement( TMyButton(Sender).Handle, WindowPlacement);
  }
end;

{ TMyButton }

procedure TMyButton.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  //
end;

procedure TMyButton.WMWindowPosChanging(var Message: TWMWindowPosChanging);
begin
  //
end;

end.
