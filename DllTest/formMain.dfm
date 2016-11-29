object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'frmMain'
  ClientHeight = 299
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnMain: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'load main dll'
    TabOrder = 0
    OnClick = btnMainClick
  end
  object btnSub: TButton
    Left = 8
    Top = 39
    Width = 75
    Height = 25
    Caption = 'load sub dll'
    Enabled = False
    TabOrder = 1
    OnClick = btnSubClick
  end
  object btnSub2: TButton
    Left = 89
    Top = 39
    Width = 75
    Height = 25
    Caption = 'load sub dll 2'
    Enabled = False
    TabOrder = 2
    OnClick = btnSub2Click
  end
end
