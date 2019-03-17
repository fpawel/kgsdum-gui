object FormJournal: TFormJournal
  Left = 0
  Top = 0
  Caption = 'FormJournal'
  ClientHeight = 300
  ClientWidth = 762
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 18
  object Splitter1: TSplitter
    Left = 265
    Top = 31
    Width = 5
    Height = 269
    ExplicitLeft = 249
    ExplicitTop = 0
    ExplicitHeight = 300
  end
  object Panel1: TPanel
    Left = 0
    Top = 31
    Width = 265
    Height = 269
    Align = alLeft
    BevelOuter = bvNone
    Constraints.MinWidth = 265
    TabOrder = 0
    OnResize = Panel1Resize
    object StringGrid1: TStringGrid
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 259
      Height = 263
      Align = alClient
      BorderStyle = bsNone
      ColCount = 2
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
      OnDrawCell = StringGrid1DrawCell
      OnKeyDown = StringGrid1KeyDown
      OnSelectCell = StringGrid1SelectCell
      ColWidths = (
        64
        64)
      RowHeights = (
        22)
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 0
    Width = 762
    Height = 31
    Align = alTop
    Alignment = taLeftJustify
    BevelOuter = bvNone
    Caption = '   '#1046#1091#1088#1085#1072#1083
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
