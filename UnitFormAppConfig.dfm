object FormAppConfig: TFormAppConfig
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080
  ClientHeight = 600
  ClientWidth = 387
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDeactivate = FormDeactivate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 19
  object Panel19: TPanel
    Left = 0
    Top = 0
    Width = 387
    Height = 17
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 1
    TabOrder = 0
  end
  object Panel20: TPanel
    Left = 0
    Top = 17
    Width = 387
    Height = 584
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 1
    TabOrder = 1
    object Panel1: TPanel
      Left = 1
      Top = 1
      Width = 385
      Height = 38
      Align = alTop
      BevelOuter = bvNone
      BorderWidth = 1
      TabOrder = 0
      object Shape1: TShape
        Left = 1
        Top = 36
        Width = 383
        Height = 1
        Align = alBottom
        Pen.Color = cl3DLight
        ExplicitLeft = 168
        ExplicitTop = 152
        ExplicitWidth = 65
      end
      object Panel2: TPanel
        Left = 1
        Top = 1
        Width = 240
        Height = 35
        Align = alLeft
        Alignment = taRightJustify
        BevelOuter = bvNone
        Caption = #1057#1054#1052' '#1087#1086#1088#1090' '#1073#1083#1086#1082#1086#1074' '#1086#1087#1090#1080#1095#1077#1089#1082#1080#1093
        TabOrder = 1
      end
      object ComboBoxComportProducts: TComboBox
        Left = 260
        Top = 4
        Width = 98
        Height = 26
        Style = csOwnerDrawFixed
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ItemHeight = 20
        ItemIndex = 0
        ParentFont = False
        TabOrder = 0
        Text = 'COM1'
        OnChange = ComboBoxComportProductsChange
        OnDropDown = ComboBoxComportProductsDropDown
        Items.Strings = (
          'COM1')
      end
    end
    object Panel3: TPanel
      Left = 1
      Top = 77
      Width = 385
      Height = 38
      Align = alTop
      BevelOuter = bvNone
      BorderWidth = 1
      TabOrder = 2
      object Shape2: TShape
        Left = 1
        Top = 36
        Width = 383
        Height = 1
        Align = alBottom
        Pen.Color = cl3DLight
        ExplicitLeft = 168
        ExplicitTop = 152
        ExplicitWidth = 65
      end
      object Panel4: TPanel
        Left = 1
        Top = 1
        Width = 240
        Height = 35
        Align = alLeft
        Alignment = taRightJustify
        BevelOuter = bvNone
        Caption = #1055#1043#1057'1'
        TabOrder = 0
      end
      object EditPgs1: TEdit
        Left = 260
        Top = 5
        Width = 98
        Height = 26
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        Text = 'EditPgs1'
        OnChange = EditPgs1Change
      end
    end
    object Panel5: TPanel
      Left = 1
      Top = 39
      Width = 385
      Height = 38
      Align = alTop
      BevelOuter = bvNone
      BorderWidth = 1
      TabOrder = 1
      object Shape3: TShape
        Left = 1
        Top = 36
        Width = 383
        Height = 1
        Align = alBottom
        Pen.Color = cl3DLight
        ExplicitLeft = 168
        ExplicitTop = 152
        ExplicitWidth = 65
      end
      object Panel6: TPanel
        Left = 1
        Top = 1
        Width = 240
        Height = 35
        Align = alLeft
        Alignment = taRightJustify
        BevelOuter = bvNone
        Caption = #1057#1054#1052' '#1087#1086#1088#1090' '#1090#1077#1088#1084#1086#1082#1072#1084#1077#1088#1099
        TabOrder = 0
      end
      object ComboBoxComportTemp: TComboBox
        Left = 260
        Top = 4
        Width = 98
        Height = 26
        Style = csOwnerDrawFixed
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ItemHeight = 20
        ItemIndex = 0
        ParentFont = False
        TabOrder = 1
        Text = 'COM1'
        OnChange = ComboBoxComportTempChange
        OnDropDown = ComboBoxComportProductsDropDown
        Items.Strings = (
          'COM1')
      end
    end
    object Panel7: TPanel
      Left = 1
      Top = 191
      Width = 385
      Height = 38
      Align = alTop
      BevelOuter = bvNone
      BorderWidth = 1
      TabOrder = 5
      object Shape4: TShape
        Left = 1
        Top = 36
        Width = 383
        Height = 1
        Align = alBottom
        Pen.Color = cl3DLight
        ExplicitLeft = 168
        ExplicitTop = 152
        ExplicitWidth = 65
      end
      object Panel8: TPanel
        Left = 1
        Top = 1
        Width = 240
        Height = 35
        Align = alLeft
        Alignment = taRightJustify
        BevelOuter = bvNone
        Caption = #1055#1043#1057'4'
        TabOrder = 0
      end
      object EditPgs4: TEdit
        Left = 260
        Top = 5
        Width = 98
        Height = 26
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        Text = 'Edit1'
        OnChange = EditPgs1Change
      end
    end
    object Panel9: TPanel
      Left = 1
      Top = 153
      Width = 385
      Height = 38
      Align = alTop
      BevelOuter = bvNone
      BorderWidth = 1
      TabOrder = 4
      object Shape5: TShape
        Left = 1
        Top = 36
        Width = 383
        Height = 1
        Align = alBottom
        Pen.Color = cl3DLight
        ExplicitLeft = 168
        ExplicitTop = 152
        ExplicitWidth = 65
      end
      object Panel10: TPanel
        Left = 1
        Top = 1
        Width = 240
        Height = 35
        Align = alLeft
        Alignment = taRightJustify
        BevelOuter = bvNone
        Caption = #1055#1043#1057'3'
        TabOrder = 0
      end
      object EditPgs3: TEdit
        Left = 260
        Top = 5
        Width = 98
        Height = 26
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        Text = 'Edit1'
        OnChange = EditPgs1Change
      end
    end
    object Panel11: TPanel
      Left = 1
      Top = 115
      Width = 385
      Height = 38
      Align = alTop
      BevelOuter = bvNone
      BorderWidth = 1
      TabOrder = 3
      object Shape6: TShape
        Left = 1
        Top = 36
        Width = 383
        Height = 1
        Align = alBottom
        Pen.Color = cl3DLight
        ExplicitLeft = 168
        ExplicitTop = 152
        ExplicitWidth = 65
      end
      object Panel12: TPanel
        Left = 1
        Top = 1
        Width = 240
        Height = 35
        Align = alLeft
        Alignment = taRightJustify
        BevelOuter = bvNone
        Caption = #1055#1043#1057'2'
        TabOrder = 0
      end
      object EditPgs2: TEdit
        Left = 260
        Top = 5
        Width = 98
        Height = 26
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        Text = 'Edit1'
        OnChange = EditPgs1Change
      end
    end
    object Panel13: TPanel
      Left = 1
      Top = 229
      Width = 385
      Height = 38
      Align = alTop
      BevelOuter = bvNone
      BorderWidth = 1
      TabOrder = 6
      object Shape7: TShape
        Left = 1
        Top = 36
        Width = 383
        Height = 1
        Align = alBottom
        Pen.Color = cl3DLight
        ExplicitLeft = 168
        ExplicitTop = 152
        ExplicitWidth = 65
      end
      object Panel14: TPanel
        Left = 1
        Top = 1
        Width = 240
        Height = 35
        Align = alLeft
        Alignment = taRightJustify
        BevelOuter = bvNone
        Caption = #1042#1099#1076#1077#1088#1078#1082#1072' '#1090#1077#1084#1087#1077#1088#1072#1090#1091#1088#1099', '#1084#1080#1085'.'
        TabOrder = 0
      end
      object EdTempTime: TEdit
        Left = 260
        Top = 5
        Width = 98
        Height = 26
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        Text = 'EdTemp'
        OnChange = EdTempTimeChange
      end
    end
    object Panel15: TPanel
      Left = 1
      Top = 267
      Width = 385
      Height = 38
      Align = alTop
      BevelOuter = bvNone
      BorderWidth = 1
      TabOrder = 7
      object Shape8: TShape
        Left = 1
        Top = 36
        Width = 383
        Height = 1
        Align = alBottom
        Pen.Color = cl3DLight
        ExplicitLeft = 168
        ExplicitTop = 152
        ExplicitWidth = 65
      end
      object Panel16: TPanel
        Left = 1
        Top = 1
        Width = 240
        Height = 35
        Align = alLeft
        Alignment = taRightJustify
        BevelOuter = bvNone
        Caption = #1055#1088#1086#1076#1091#1074#1082#1072' '#1075#1072#1079#1086#1084', '#1084#1080#1085'.'
        TabOrder = 0
      end
      object EdGasTime: TEdit
        Left = 260
        Top = 5
        Width = 98
        Height = 26
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        Text = 'EdGas'
        OnChange = EdGasTimeChange
      end
    end
    object Panel18: TPanel
      Left = 1
      Top = 305
      Width = 385
      Height = 47
      Align = alTop
      BevelOuter = bvNone
      BorderWidth = 1
      TabOrder = 8
      object Shape10: TShape
        Left = 1
        Top = 45
        Width = 383
        Height = 1
        Align = alBottom
        Pen.Color = cl3DLight
        ExplicitLeft = 168
        ExplicitTop = 152
        ExplicitWidth = 65
      end
      object Label2: TLabel
        Left = 1
        Top = 1
        Width = 227
        Height = 44
        Align = alLeft
        Alignment = taRightJustify
        Caption = #1055#1086#1085#1080#1078#1077#1085#1085#1072#1103' '#1090#1077#1084#1087#1077#1088#1072#1090#1091#1088#1072' '#1087#1088#1080' '#1085#1072#1089#1090#1088#1086#1081#1082#1077
        WordWrap = True
        ExplicitHeight = 38
      end
      object EdTempLow1: TEdit
        Left = 260
        Top = 5
        Width = 98
        Height = 26
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        Text = 'EdGas'
        OnChange = EdTempLow1Change
      end
    end
    object Panel17: TPanel
      Left = 1
      Top = 352
      Width = 385
      Height = 47
      Align = alTop
      BevelOuter = bvNone
      BorderWidth = 1
      TabOrder = 9
      object Shape9: TShape
        Left = 1
        Top = 45
        Width = 383
        Height = 1
        Align = alBottom
        Pen.Color = cl3DLight
        ExplicitLeft = 168
        ExplicitTop = 152
        ExplicitWidth = 65
      end
      object Label1: TLabel
        Left = 1
        Top = 1
        Width = 228
        Height = 44
        Align = alLeft
        Alignment = taRightJustify
        Caption = #1055#1086#1074#1099#1096#1077#1085#1085#1072#1103' '#1090#1077#1084#1087#1077#1088#1072#1090#1091#1088#1072' '#1087#1088#1080' '#1085#1072#1089#1090#1088#1086#1081#1082#1077
        WordWrap = True
        ExplicitHeight = 38
      end
      object EdTempHigh1: TEdit
        Left = 260
        Top = 5
        Width = 98
        Height = 26
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        Text = 'EdGas'
        OnChange = EdTempHigh1Change
      end
    end
    object Panel21: TPanel
      Left = 1
      Top = 399
      Width = 385
      Height = 47
      Align = alTop
      BevelOuter = bvNone
      BorderWidth = 1
      TabOrder = 10
      object Shape11: TShape
        Left = 1
        Top = 45
        Width = 383
        Height = 1
        Align = alBottom
        Pen.Color = cl3DLight
        ExplicitLeft = 168
        ExplicitTop = 152
        ExplicitWidth = 65
      end
      object Label3: TLabel
        Left = 1
        Top = 1
        Width = 140
        Height = 44
        Align = alLeft
        Alignment = taRightJustify
        Caption = #1058#1077#1084#1087#1077#1088#1072#1090#1091#1088#1072' '#1085'.'#1082'.'#1091'.'
        WordWrap = True
        ExplicitHeight = 19
      end
      object EdTempNku: TEdit
        Left = 260
        Top = 5
        Width = 98
        Height = 26
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        Text = 'EdGas'
        OnChange = EdTempNkuChange
      end
    end
    object Panel22: TPanel
      Left = 1
      Top = 446
      Width = 385
      Height = 47
      Align = alTop
      BevelOuter = bvNone
      BorderWidth = 1
      TabOrder = 11
      object Shape12: TShape
        Left = 1
        Top = 45
        Width = 383
        Height = 1
        Align = alBottom
        Pen.Color = cl3DLight
        ExplicitLeft = 168
        ExplicitTop = 152
        ExplicitWidth = 65
      end
      object Label4: TLabel
        Left = 1
        Top = 1
        Width = 227
        Height = 44
        Align = alLeft
        Alignment = taRightJustify
        Caption = #1055#1086#1085#1080#1078#1077#1085#1085#1072#1103' '#1090#1077#1084#1087#1077#1088#1072#1090#1091#1088#1072' '#1087#1088#1080' '#1087#1088#1086#1074#1077#1088#1082#1077
        WordWrap = True
        ExplicitHeight = 38
      end
      object EdTempLow2: TEdit
        Left = 260
        Top = 5
        Width = 98
        Height = 26
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        Text = 'EdGas'
        OnChange = EdTempLow2Change
      end
    end
    object Panel23: TPanel
      Left = 1
      Top = 493
      Width = 385
      Height = 47
      Align = alTop
      BevelOuter = bvNone
      BorderWidth = 1
      TabOrder = 12
      object Shape13: TShape
        Left = 1
        Top = 45
        Width = 383
        Height = 1
        Align = alBottom
        Pen.Color = cl3DLight
        ExplicitLeft = 168
        ExplicitTop = 152
        ExplicitWidth = 65
      end
      object Label5: TLabel
        Left = 1
        Top = 1
        Width = 228
        Height = 44
        Align = alLeft
        Alignment = taRightJustify
        Caption = #1055#1086#1074#1099#1096#1077#1085#1085#1072#1103' '#1090#1077#1084#1087#1077#1088#1072#1090#1091#1088#1072' '#1087#1088#1080' '#1087#1088#1086#1074#1077#1088#1082#1077
        WordWrap = True
        ExplicitHeight = 38
      end
      object EdTempHigh2: TEdit
        Left = 260
        Top = 5
        Width = 98
        Height = 26
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        Text = 'EdGas'
        OnChange = EdTempHigh2Change
      end
    end
    object Panel24: TPanel
      Left = 1
      Top = 540
      Width = 385
      Height = 38
      Align = alTop
      BevelOuter = bvNone
      BorderWidth = 1
      TabOrder = 13
      ExplicitLeft = -15
      ExplicitTop = 601
      object Shape14: TShape
        Left = 1
        Top = 36
        Width = 383
        Height = 1
        Align = alBottom
        Pen.Color = cl3DLight
        ExplicitLeft = 168
        ExplicitTop = 152
        ExplicitWidth = 65
      end
      object Panel25: TPanel
        Left = 1
        Top = 1
        Width = 240
        Height = 35
        Align = alLeft
        Alignment = taRightJustify
        BevelOuter = bvNone
        Caption = #1040#1076#1088#1077#1089' '#1075#1072#1079#1086#1074#1086#1075#1086' '#1073#1083#1086#1082#1072
        TabOrder = 0
      end
      object EdPneumoAddr: TEdit
        Left = 260
        Top = 5
        Width = 98
        Height = 26
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        Text = 'EdPneumoAddr'
        OnChange = EdPneumoAddrChange
      end
    end
  end
end
