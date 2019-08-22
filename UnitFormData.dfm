object FormData: TFormData
  Left = 0
  Top = 0
  Caption = 'FormData'
  ClientHeight = 592
  ClientWidth = 1000
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object StringGrid2: TStringGrid
    AlignWithMargins = True
    Left = 3
    Top = 34
    Width = 994
    Height = 555
    Align = alClient
    BorderStyle = bsNone
    ColCount = 4
    DefaultRowHeight = 22
    DefaultDrawing = False
    FixedColor = clBackground
    FixedCols = 0
    RowCount = 1
    FixedRows = 0
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    GradientEndColor = clBlack
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSelect]
    ParentFont = False
    TabOrder = 0
    OnDrawCell = StringGrid2DrawCell
    ColWidths = (
      64
      64
      64
      64)
    RowHeights = (
      22)
  end
  object Panel3: TPanel
    Left = 0
    Top = 0
    Width = 1000
    Height = 31
    Align = alTop
    Alignment = taLeftJustify
    BevelOuter = bvNone
    Caption = '   '#1044#1072#1090#1072
    Color = clGradientInactiveCaption
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
    TabOrder = 1
    object ComboBox1: TComboBox
      Left = 77
      Top = 2
      Width = 132
      Height = 26
      Style = csOwnerDrawFixed
      Color = clHighlightText
      ItemHeight = 20
      ItemIndex = 0
      TabOrder = 0
      Text = '11.11.2018'
      OnChange = ComboBox1Change
      Items.Strings = (
        '11.11.2018')
    end
  end
end
