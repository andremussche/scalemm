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
  object Button1: TButton
    Left = 8
    Top = 70
    Width = 75
    Height = 25
    Caption = 'main to sub'
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 88
    Top = 70
    Width = 75
    Height = 25
    Caption = 'main to sub2'
    TabOrder = 4
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 8
    Top = 101
    Width = 75
    Height = 25
    Caption = 'sub thread'
    TabOrder = 5
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 88
    Top = 101
    Width = 75
    Height = 25
    Caption = 'sub2 thread'
    TabOrder = 6
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 169
    Top = 70
    Width = 75
    Height = 25
    Caption = 'main to sub MT'
    TabOrder = 7
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 256
    Top = 101
    Width = 75
    Height = 25
    Caption = 'app thread'
    TabOrder = 8
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 256
    Top = 132
    Width = 75
    Height = 25
    Caption = 'dump stats'
    TabOrder = 9
    OnClick = Button7Click
  end
end
