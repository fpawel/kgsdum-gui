object FormLastParty: TFormLastParty
  Left = 0
  Top = 0
  Caption = 'FormLastParty'
  ClientHeight = 440
  ClientWidth = 878
  Color = clWhite
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
  object StringGrid1: TStringGrid
    AlignWithMargins = True
    Left = 36
    Top = 3
    Width = 839
    Height = 434
    Align = alClient
    BorderStyle = bsNone
    ColCount = 4
    DefaultDrawing = False
    FixedColor = clBackground
    FixedCols = 0
    RowCount = 1
    FixedRows = 0
    GradientEndColor = clBlack
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
    TabOrder = 0
    OnDrawCell = StringGrid1DrawCell
    OnMouseDown = StringGrid1MouseDown
    OnSelectCell = StringGrid1SelectCell
    OnSetEditText = StringGrid1SetEditText
    ExplicitLeft = 3
    ExplicitTop = 36
    ExplicitWidth = 872
    ExplicitHeight = 401
    ColWidths = (
      64
      64
      64
      64)
    RowHeights = (
      24)
  end
  object ToolBarParty: TToolBar
    Left = 0
    Top = 0
    Width = 33
    Height = 440
    Align = alLeft
    ButtonHeight = 30
    ButtonWidth = 30
    Caption = 'ToolBar1'
    EdgeInner = esNone
    EdgeOuter = esNone
    Images = ImageList1
    TabOrder = 1
    ExplicitHeight = 878
    object ToolButtonParty: TToolButton
      Left = 0
      Top = 0
      Hint = #1057#1086#1079#1076#1072#1090#1100' '#1085#1086#1074#1091#1102' '#1087#1072#1088#1090#1080#1102
      Caption = 'ToolButtonParty'
      ImageIndex = 0
      ParentShowHint = False
      ShowHint = True
      OnClick = ToolButtonPartyClick
    end
    object ToolButton2: TToolButton
      Left = 0
      Top = 0
      Width = 8
      Caption = 'ToolButton2'
      ImageIndex = 3
      Wrap = True
      Style = tbsSeparator
    end
    object ToolButtonStop: TToolButton
      Left = 0
      Top = 38
      Hint = #1044#1086#1073#1072#1074#1080#1090#1100' '#1087#1088#1080#1073#1086#1088' '#1074' '#1087#1072#1088#1090#1080#1102
      Caption = 'ToolButtonStop'
      ImageIndex = 1
      ParentShowHint = False
      Wrap = True
      ShowHint = True
      OnClick = ToolButtonStopClick
    end
    object ToolButton1: TToolButton
      Left = 0
      Top = 68
      Hint = #1059#1076#1072#1083#1080#1090#1100' '#1087#1088#1080#1073#1086#1088' '#1080#1079' '#1087#1072#1088#1090#1080#1080
      Caption = 'ToolButton1'
      ImageIndex = 2
      ParentShowHint = False
      ShowHint = True
      OnClick = ToolButton1Click
    end
  end
  object ImageList1: TImageList
    ColorDepth = cd32Bit
    BlendColor = clWindow
    BkColor = clWhite
    DrawingStyle = dsTransparent
    Height = 20
    Width = 20
    Left = 536
    Top = 98
    Bitmap = {
      494C010103009C03C00314001400FFFFFF002110FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000500000001400000001002000000000000019
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000202011F22180D693F2D1A8D3F2D1A8D2218
      0D690202011F0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000001C140B5F84603AC8C09360EAF4C08BFFFAC994FFFAC994FFF4C0
      8BFFC09360EA84603AC81C140B5F000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000001911
      0A606F4B30C7704C31C86D492FC5060402300000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000F71512FBBDEAC79F6FDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC
      98FFFDCC98FFFDCC98FFDCAC77F5704F2EB90000000E00000000000000000000
      0000000000000000000000000000000000000000000000000000000000002D1E
      1380B67C52FBD3986BFFB5794DFF0B0705400000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000F8761
      3BCBF9C892FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC
      98FFFDCC98FFFDCC98FFFDCC98FFF9C792FF876139CB0000000F000000000000
      0000000000000000000000000000000000000000000000000000000000002D1E
      1380B68055F7F0B78BFFB5794DFF0B0705400000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000001735230BCF9C7
      93FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC
      98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFF9C792FF6E4F2EB9000000000000
      0000000000000000000000000000000000000000000000000000000000002D1E
      1380B68055F7F0B78BFFB5794DFF0B0705400000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001D140C60DDAC78F5FDCC
      98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFFFFFFFFFFFFFFFFFDCC
      98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFDBAB77F51B140B5E0000
      0000000000000000000000000000000000000000000000000000000000002D1E
      1380B68055F7F0B78BFFB5794DFF0B0705400000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000086613BC9FDCC98FFFDCC
      98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFFFFFFFFFFFFFFFFFDCC
      98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FF845F3AC70000
      0000000000000000000000000000000000000000000000000000000000002D1E
      1380B68055F7F0B78BFFB5794DFF0B0705400000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000003020120C09361EAFDCC98FFFDCC
      98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFFFFFFFFFFFFFFFFFDCC
      98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFBF9161E90201
      001E000000000000000000000000000000000000000000000000000000002D1E
      1380B68055F7F0B78BFFB5794DFF0B0705400000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000023190E6AF4C18BFFFDCC98FFFDCC
      98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFFFFFFFFFFFFFFFFFDCC
      98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFF4C18BFF2117
      0D660B0705400B0705400B0705400B0705400B0705400B0705400B0705404730
      1EA0B68055F7F0B78BFFB5794DFF22170E700B0705400B0705400B0705400B07
      05400B0705400B07054006040230000000000B0705400B0705400B0705400B07
      05400B0705400B0705400B0705400B0705400B0705400B0705400B0705400B07
      05400B0705400B0705400B0705400B0705400B0705400B070540060402300000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000412E1B8FFAC994FFFDCC98FFFDCC
      98FFFDCC98FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFDCC98FFFDCC98FFFDCC98FFFAC994FF3F2D
      1A8DB5794DFFB5794DFFB5794DFFB5794DFFB5794DFFB5794DFFB5794DFFB579
      4DFFBE855AFCF0B78BFFB5794DFFB5794DFFB5794DFFB5794DFFB5794DFFB579
      4DFFB5794DFFB5794DFF6D492FC500000000B5794DFFB5794DFFB5794DFFB579
      4DFFB5794DFFB5794DFFB5794DFFB5794DFFB5794DFFB5794DFFB5794DFFB579
      4DFFB5794DFFB5794DFFB5794DFFB5794DFFB5794DFFB5794DFF6D492FC50000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000412E1B8FFAC994FFFDCC98FFFDCC
      98FFFDCC98FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFDCC98FFFDCC98FFFDCC98FFFAC994FF3F2D
      1A8DB5794DFFE1A87BFFF0B78BFFF0B78BFFF0B78BFFF0B78BFFF0B78BFFF0B7
      8BFFF0B78BFFF0B78BFFF0B78BFFF0B78BFFF0B78BFFF0B78BFFF0B78BFFF0B7
      8BFFF0B78BFFD3986BFF704C31C800000000B5794DFFE1A87BFFF0B78BFFF0B7
      8BFFF0B78BFFF0B78BFFF0B78BFFF0B78BFFF0B78BFFF0B78BFFF0B78BFFF0B7
      8BFFF0B78BFFF0B78BFFF0B78BFFF0B78BFFF0B78BFFD3986BFF704C31C80000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000024190E6BF4C28CFFFDCC98FFFDCC
      98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFFFFFFFFFFFFFFFFFDCC
      98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFF4C18BFF2117
      0D66B5794DFFB67F53F9B68055F7B68055F7B68055F7B68055F7B68055F7B680
      55F7CC9367FEF0B78BFFBE855AFCB68055F7B68055F7B68055F7B68055F7B680
      55F7B68055F7B67C52FB6F4B30C700000000B5794DFFB67F53F9B68055F7B680
      55F7B68055F7B68055F7B68055F7B68055F7B68055F7B68055F7B68055F7B680
      55F7B68055F7B68055F7B68055F7B68055F7B68055F7B67C52FB6F4B30C70000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000003020121C09461EAFDCC98FFFDCC
      98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFFFFFFFFFFFFFFFFFDCC
      98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFC09360EA0202
      011F2D1E13802D1E13802D1E13802D1E13802D1E13802D1E13802D1E13806645
      2CC0B68055F7F0B78BFFB5794DFF47301EA02D1E13802D1E13802D1E13802D1E
      13802D1E13802D1E138019110A60000000002D1E13802D1E13802D1E13802D1E
      13802D1E13802D1E13802D1E13802D1E13802D1E13802D1E13802D1E13802D1E
      13802D1E13802D1E13802D1E13802D1E13802D1E13802D1E138019110A600000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000086613BCAFDCC98FFFDCC
      98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFFFFFFFFFFFFFFFFFDCC
      98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FF84603AC80000
      0000000000000000000000000000000000000000000000000000000000002D1E
      1380B68055F7F0B78BFFB5794DFF0B0705400000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001E150C62DEAE79F5FDCC
      98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFFFFFFFFFFFFFFFFFDCC
      98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFDCAC77F51D140C600000
      0000000000000000000000000000000000000000000000000000000000002D1E
      1380B68055F7F0B78BFFB5794DFF0B0705400000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000001765330BEFAC8
      93FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC
      98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFF9C892FF71512FBB000000000000
      0000000000000000000000000000000000000000000000000000000000002D1E
      1380B68055F7F0B78BFFB5794DFF0B0705400000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000F8963
      3CCDF9C793FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC
      98FFFDCC98FFFDCC98FFFDCC98FFF9C892FF87613BCB0000000F000000000000
      0000000000000000000000000000000000000000000000000000000000002D1E
      1380B68055F7F0B78BFFB5794DFF0B0705400000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000F735230BDDDAC79F6FDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC
      98FFFDCC98FFFDCC98FFDEAC79F6735130BC0000000F00000000000000000000
      0000000000000000000000000000000000000000000000000000000000002D1E
      1380B68055F7F0B78BFFB5794DFF0B0705400000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000011E150C6284603AC8C09461EAF4C28CFFFBC994FFFBC994FFF4C1
      8BFFC09461EA84603AC81C140B5F000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000002D1E
      1380B67F53F9E1A87BFFB5794DFF0B0705400000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000302012124190E6B3F2D1A8D3F2D1A8D2319
      0E6A030201210000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000002D1E
      1380B5794DFFB5794DFFB5794DFF0B0705400000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000050000000140000000100010000000000F00000000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000}
  end
end
