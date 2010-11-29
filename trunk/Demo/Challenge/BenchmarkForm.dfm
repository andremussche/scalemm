object fBenchmark: TfBenchmark
  Left = 174
  Top = 83
  Caption = 'Fastcode MM challenge B&V'
  ClientHeight = 562
  ClientWidth = 816
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    816
    562)
  PixelsPerInch = 96
  TextHeight = 13
  object gbBenchmarks: TGroupBox
    Left = 8
    Top = 4
    Width = 774
    Height = 323
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Benchmarks'
    TabOrder = 0
    DesignSize = (
      774
      323)
    object bRunSelectedBenchmark: TBitBtn
      Left = 393
      Top = 194
      Width = 369
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Run Selected Benchmark'
      DoubleBuffered = True
      ParentDoubleBuffered = False
      TabOrder = 0
      OnClick = bRunSelectedBenchmarkClick
    end
    object bRunAllCheckedBenchmarks: TBitBtn
      Left = 393
      Top = 226
      Width = 369
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Run All Checked Benchmarks'
      DoubleBuffered = True
      ParentDoubleBuffered = False
      TabOrder = 1
      OnClick = bRunAllCheckedBenchmarksClick
    end
    object mBenchmarkDescription: TMemo
      Left = 8
      Top = 196
      Width = 378
      Height = 115
      Anchors = [akLeft, akRight, akBottom]
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 2
    end
    object ValidateButton: TBitBtn
      Left = 393
      Top = 286
      Width = 181
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Validate'
      DoubleBuffered = True
      ParentDoubleBuffered = False
      TabOrder = 3
      OnClick = ValidateButtonClick
    end
    object RunTimeEdit: TEdit
      Left = 393
      Top = 258
      Width = 369
      Height = 21
      Anchors = [akRight, akBottom]
      ReadOnly = True
      TabOrder = 4
    end
    object ExtraValidateButton: TBitBtn
      Left = 577
      Top = 286
      Width = 185
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Extra Validate'
      DoubleBuffered = True
      ParentDoubleBuffered = False
      TabOrder = 5
      OnClick = ValidateButtonClick
    end
    object ListViewBenchmarks: TListView
      Left = 8
      Top = 16
      Width = 754
      Height = 175
      Anchors = [akLeft, akTop, akRight, akBottom]
      Checkboxes = True
      Columns = <
        item
          Caption = 'Benchmarks'
          Width = 200
        end
        item
          Caption = 'Category'
          Width = 150
        end
        item
          Alignment = taRightJustify
          Caption = 'Speed Weigth'
          Width = 80
        end
        item
          Alignment = taRightJustify
          Caption = 'Mem Usage Weigth'
          Width = 110
        end
        item
          Alignment = taRightJustify
          Caption = 'Relative Weigth'
          Width = 90
        end
        item
          Alignment = taRightJustify
          Caption = 'Global Weigth'
          Width = 80
        end>
      ColumnClick = False
      HideSelection = False
      ReadOnly = True
      RowSelect = True
      PopupMenu = PopupMenuBenchmarks
      TabOrder = 6
      ViewStyle = vsReport
      OnSelectItem = ListViewBenchmarksSelectItem
    end
  end
  object BitBtn1: TBitBtn
    Left = 683
    Top = 533
    Width = 101
    Height = 25
    Anchors = [akRight, akBottom]
    DoubleBuffered = True
    Kind = bkClose
    ParentDoubleBuffered = False
    TabOrder = 1
  end
  object bGraph: TBitBtn
    Left = 8
    Top = 531
    Width = 101
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'View Graphs'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 2
    OnClick = bGraphClick
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 332
    Width = 775
    Height = 196
    ActivePage = TabSheetScores
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 3
    object TabSheetBenchmarkResults: TTabSheet
      Caption = 'Benchmark Results'
      ImageIndex = 1
      object ListViewResults: TListView
        Left = 0
        Top = 36
        Width = 767
        Height = 132
        Align = alClient
        Columns = <
          item
            Caption = 'Benchmark'
          end
          item
            Caption = 'Memory Manager'
          end
          item
            Caption = 'Time Ran'
          end
          item
            Alignment = taRightJustify
            Caption = 'Ticks (M)'
          end
          item
            Alignment = taRightJustify
            Caption = 'Peak Addr Space (K)'
          end
          item
            Caption = 'CurrentSession'
            Width = 0
          end>
        ColumnClick = False
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnCompare = ListViewResultsCompare
      end
      object ToolBar1: TToolBar
        Left = 0
        Top = 0
        Width = 767
        Height = 36
        AutoSize = True
        ButtonHeight = 36
        ButtonWidth = 107
        Caption = 'ToolBar1'
        Images = ImageList1
        ShowCaptions = True
        TabOrder = 1
        object ToolButtonCopyResults: TToolButton
          Left = 0
          Top = 0
          Caption = 'Copy to Clipboard'
          ImageIndex = 0
          OnClick = ToolButtonCopyResultsClick
        end
        object bRenameMM: TToolButton
          Left = 107
          Top = 0
          Caption = 'Rename MM'
          ImageIndex = 3
          OnClick = bRenameMMClick
        end
        object ToolButton1: TToolButton
          Left = 214
          Top = 0
          Caption = 'Delete results for MM'
          ImageIndex = 2
          OnClick = ToolButton1Click
        end
        object ToolButtonDeleteResults: TToolButton
          Left = 321
          Top = 0
          Caption = 'Delete All Results'
          ImageIndex = 1
          OnClick = ToolButtonDeleteResultsClick
        end
      end
    end
    object TabSheetValidation: TTabSheet
      Caption = 'Validation Results'
      ImageIndex = 3
      object MemoValidation: TMemo
        Left = 0
        Top = 0
        Width = 767
        Height = 168
        Align = alClient
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
    object TabSheetProgress: TTabSheet
      Caption = 'Progress'
      ImageIndex = 2
      object mResults: TMemo
        Left = 0
        Top = 0
        Width = 767
        Height = 168
        Align = alClient
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
    object TabSheetScores: TTabSheet
      Caption = 'Scores'
      ImageIndex = 3
      object MemoScores: TMemo
        Left = 0
        Top = 0
        Width = 767
        Height = 168
        Align = alClient
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        Lines.Strings = (
          'MemoScores')
        ParentFont = False
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
    object TabSheetCPU: TTabSheet
      Caption = 'Environment'
      ImageIndex = 4
      object MemoCPU: TMemo
        Left = 0
        Top = 0
        Width = 767
        Height = 168
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
  end
  object btnCopySummary: TBitBtn
    Left = 120
    Top = 531
    Width = 161
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Copy Summary to Clipboard'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 4
    OnClick = btnCopySummaryClick
  end
  object ImageList1: TImageList
    Left = 4
    Top = 280
    Bitmap = {
      494C010107001000100010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000002000000001002000000000000020
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C6000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C6C6
      C600C6C6C600C6C6C60000000000C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C6000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C6C6
      C600C6C6C600000000000000000000000000C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C6000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C6C6
      C6000000000000000000C6C6C6000000000000000000C6C6C600C6C6C600C6C6
      C600C6C6C6000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C6C6
      C60000000000C6C6C600C6C6C600C6C6C6000000000000000000C6C6C600C6C6
      C600C6C6C6000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C6000000000000000000C6C6
      C600C6C6C6000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600000000000000
      0000C6C6C6000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C6000000
      0000C6C6C6000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C6000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C6000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000FFFF007B7B7B00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000FFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000B5000000B5000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000FFFF007B7B7B00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000FFFF0000000000000000000000B5000000B5000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000B5000000B500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      000000000000000000000000000000FFFF007B7B7B00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000FFFF0000000000000000000000B5000000B5000000
      B500000000000000000000000000000000000000000000000000000000000000
      00000000B5000000B50000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF0000000000FFFF
      FF000000000000000000FFFFFF00000000000000000000000000000000000000
      000000000000000000000000000000FFFF007B7B7B00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000FFFF0000000000000000000000B5000000B5000000
      B5000000B5000000000000000000000000000000000000000000000000000000
      B5000000B5000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFF0000000000000000000000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF0000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B7B7B007B7B
      7B0000000000000000007B7B7B000000000000000000000000000000DE000000
      BD000000B5000000B500000000000000000000000000000000000000B5000000
      B500000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFF00000000000000FFFF00FFFF
      FF0000FFFF00FFFFFF00000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000
      00007B7B7B007B7B7B0000FFFF00000000000000000000000000000000000000
      00000000B5000000B5000000B500000000000000B5000000B5000000B5000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000840000008400000000000000
      000000000000000000000000000000000000FFFF000000000000FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000000000FFFFFF000000
      000000000000FFFFFF00FFFFFF000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000
      00007B7B7B0000FFFF0000FFFF00000000000000000000000000000000000000
      0000000000000000B5000000CE000000CE000000CE000000B500000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FF000000FF000000FF00000084000000
      000000000000000000000000000000000000FFFF00000000000000FFFF00FFFF
      FF0000FFFF00FFFFFF00000000000000000000000000000000000000000000FF
      FF0000000000FFFFFF00FFFFFF000000000000000000FFFFFF00000000000000
      0000FFFFFF00000000000000000000000000FFFFFF0000000000000000000000
      00000000FF0000000000000000007B7B7B000000000000000000000000000000
      000000000000000000000000C6000000C6000000DE0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FF000000FF000000FF000000FF000000
      840000000000000000000000000000000000FFFF000000000000FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF000000
      0000FFFFFF00FFFFFF00FFFFFF000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      0000000000000000B5000000DE000000CE000000DE000000E700000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF000000FF000000FF000000
      000000848400000000000000000000000000FFFF00000000000000FFFF00FFFF
      FF0000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFFFF00000000000000
      00000000000000000000FFFFFF0000000000FFFFFF00000000000000FF000000
      FF000000FF000000FF000000FF00000000000000000000000000000000000000
      00000000E7000000DE000000D60000000000000000000000E7000000EF000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000FF000000000000FF
      FF000000000000848400000000000000000000000000000000000000000000FF
      FF00FFFFFF0000FFFF00000000000000000000FFFF0000000000FFFFFF00FFFF
      FF000000000000000000FFFFFF000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000000000000000000000000000
      F7000000DE000000EF00000000000000000000000000000000000000F7000000
      F700000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FFFF000000
      000000FFFF000000000000848400000000000000000000000000000000000000
      000000000000000000000000000000FFFF0000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFFFF00000000000000
      0000FFFFFF000000000000000000000000000000000000000000000000000000
      FF000000FF000000FF00000000000000000000000000000000000000F7000000
      F7000000F7000000000000000000000000000000000000000000000000000000
      F7000000F7000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000000FF
      FF000000000000FFFF0000848400008484000000000000000000000000000000
      0000000000000000000000FFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00FFFFFF000000000000000000000000000000
      FF000000FF000000FF000000000000000000000000000000F7000000F7000000
      F700000000000000000000000000000000000000000000000000000000000000
      0000000000000000F70000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000FFFF0000FFFF0000FFFF00008484000000000000000000000000000000
      00000000000000FFFF0000000000FFFFFF00FFFFFF000000000000000000FFFF
      FF0000000000FFFFFF00FFFFFF000000000000000000FFFFFF0000000000BDBD
      BD00FFFFFF0000000000FFFFFF000000000000000000000000007B7B7B000000
      FF000000FF000000FF0000000000000000000000F7000000F7000000F7000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FFFF0000FFFF0000FFFF000000000000000000000000000000
      000000FFFF000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF00000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000FF000000FF000000FF000000
      FF000000FF000000000000000000000000000000F7000000F700000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000FFFF0000FFFF000000000000000000000000000000
      FF00000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000FFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000200000000100010000000000000100000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFFFFFF0000FFFFFFFFFFFF0000
      C003C003C0030000DFFBC003DFFB0000DDFBC003DFFB0000D8FBC003DFFB0000
      D27BC003DFFB0000D73BC003DFFB0000DF9BC003DFFB0000DFCBC003DFFB0000
      DFEBC003DFFB0000DFFBC003DFFB0000DFFBC003DFFB0000C003C003C0030000
      FFFFFFFFFFFF0000FFFFFFFFFFFF0000FC00FFFC847FFC00FC009FF900EFFC00
      FC008FF331BF2000FC0087E739FF00000000C3CF993F00000000F11FCA1F0000
      0000F83FF40F00000000FC7F9C0700000023F83F960300000001F19FCB010000
      0000E3CFFF80E0000023C7E7F7C0F80000638FFBFFE0F00000C31FFFEFF0E001
      01073FFFFFF8C40303FFFFFFFFFCEC0700000000000000000000000000000000
      000000000000}
  end
  object PopupMenuBenchmarks: TPopupMenu
    Images = ImageList1
    Left = 28
    Top = 44
    object PopupClearAllCheckMarks: TMenuItem
      Caption = 'Clear All Check Marks'
      ImageIndex = 6
      OnClick = PopupClearAllCheckMarksClick
    end
    object PopupSelectAllCheckMarks: TMenuItem
      Caption = 'Check All Benchmarks'
      ImageIndex = 4
      OnClick = PopupSelectAllCheckMarksClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object PopupCheckAllDefaultBenchmarks: TMenuItem
      Caption = 'Check All Default Benchmarks'
      ImageIndex = 5
      OnClick = PopupCheckAllDefaultBenchmarksClick
    end
  end
end
