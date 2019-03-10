object FormSettings: TFormSettings
  Left = 0
  Top = 0
  Caption = 'FormSettings'
  ClientHeight = 522
  ClientWidth = 635
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 18
  object FlowPanel1: TFlowPanel
    Left = 0
    Top = 0
    Width = 635
    Height = 433
    Align = alTop
    BevelOuter = bvNone
    FlowStyle = fsTopBottomLeftRight
    TabOrder = 0
    object Label3: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 204
      Height = 18
      Align = alTop
      Caption = #1057#1054#1052' '#1087#1086#1088#1090' '#1073#1083#1086#1082#1086#1074' '#1086#1087#1090#1080#1095#1077#1089#1082#1080#1093
    end
    object ComboBoxComportKgsdum: TComboBox
      AlignWithMargins = True
      Left = 3
      Top = 28
      Width = 204
      Height = 26
      Align = alTop
      BevelInner = bvNone
      BevelOuter = bvNone
      Style = csOwnerDrawFixed
      Color = clInfoBk
      ItemHeight = 20
      ItemIndex = 0
      TabOrder = 0
      Text = '035'
      Items.Strings = (
        '035')
    end
    object Label5: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 61
      Width = 177
      Height = 18
      Align = alTop
      Caption = #1057#1054#1052' '#1087#1086#1088#1090' '#1075#1072#1079#1086#1074#1086#1075#1086' '#1073#1083#1086#1082#1072
    end
    object ComboBoxComportGas: TComboBox
      AlignWithMargins = True
      Left = 3
      Top = 86
      Width = 204
      Height = 26
      Align = alTop
      BevelInner = bvNone
      BevelOuter = bvNone
      Style = csOwnerDrawFixed
      Color = clInfoBk
      ItemHeight = 20
      ItemIndex = 0
      TabOrder = 1
      Text = #1084#1075'/'#1084'3'
      Items.Strings = (
        #1084#1075'/'#1084'3'
        'ppm')
    end
    object Label1: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 119
      Width = 167
      Height = 18
      Align = alTop
      Caption = #1057#1054#1052' '#1087#1086#1088#1090' '#1090#1077#1088#1084#1086#1082#1072#1084#1077#1088#1099
    end
    object ComboBoxComportTemperature: TComboBox
      AlignWithMargins = True
      Left = 3
      Top = 144
      Width = 204
      Height = 26
      Align = alTop
      BevelInner = bvNone
      BevelOuter = bvNone
      Style = csOwnerDrawFixed
      Color = clInfoBk
      ItemHeight = 20
      ItemIndex = 0
      TabOrder = 2
      Text = #1084#1075'/'#1084'3'
      Items.Strings = (
        #1084#1075'/'#1084'3'
        'ppm')
    end
    object Label2: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 177
      Width = 35
      Height = 18
      Align = alTop
      Caption = #1055#1043#1057'1'
    end
    object EditSens: TEdit
      AlignWithMargins = True
      Left = 3
      Top = 202
      Width = 204
      Height = 26
      Align = alTop
      Color = clInfoBk
      TabOrder = 3
      Text = '100'
    end
  end
end
