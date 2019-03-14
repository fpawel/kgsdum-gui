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
    Left = 249
    Top = 0
    Width = 5
    Height = 300
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 249
    Height = 300
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    OnResize = Panel1Resize
    object StringGrid1: TStringGrid
      AlignWithMargins = True
      Left = 3
      Top = 32
      Width = 243
      Height = 265
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
      ExplicitLeft = 0
      ExplicitTop = 35
      ColWidths = (
        64
        64)
    end
    object ComboBox1: TComboBox
      Left = 0
      Top = 0
      Width = 249
      Height = 29
      Align = alTop
      Style = csOwnerDrawFixed
      ItemHeight = 23
      ItemIndex = 0
      TabOrder = 1
      Text = '11.11.2018'
      OnChange = ComboBox1Change
      Items.Strings = (
        '11.11.2018')
      ExplicitLeft = 56
      ExplicitTop = 120
      ExplicitWidth = 129
    end
  end
end
