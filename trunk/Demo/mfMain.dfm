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
end
