object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Memory manager speed tester'
  ClientHeight = 447
  ClientWidth = 482
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnMouseDown = FormMouseDown
  DesignSize = (
    482
    447)
  PixelsPerInch = 96
  TextHeight = 13
  object edtThreadCount: TLabeledEdit
    Left = 8
    Top = 24
    Width = 97
    Height = 21
    EditLabel.Width = 94
    EditLabel.Height = 13
    EditLabel.Caption = 'Number of threads:'
    TabOrder = 0
    Text = '2'
  end
  object btnStart: TButton
    Left = 8
    Top = 51
    Width = 97
    Height = 25
    Caption = 'Start!'
    TabOrder = 1
    OnClick = btnStartClick
  end
  object Memo1: TMemo
    Left = 8
    Top = 82
    Width = 467
    Height = 357
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Memo1')
    TabOrder = 2
  end
  object Button1: TButton
    Left = 232
    Top = 51
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 400
    Top = 51
    Width = 75
    Height = 25
    Caption = 'Button2'
    TabOrder = 4
  end
  object Button3: TButton
    Left = 232
    Top = 20
    Width = 75
    Height = 25
    Caption = 'Button3'
    TabOrder = 5
    OnClick = Button3Click
  end
  object Panel1: TPanel
    Left = 128
    Top = 42
    Width = 89
    Height = 41
    Caption = 'Panel1'
    TabOrder = 6
  end
end
