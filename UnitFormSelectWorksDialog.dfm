object FormSelectWorksDialog: TFormSelectWorksDialog
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'FormSelectWorksDialog'
  ClientHeight = 260
  ClientWidth = 518
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefault
  OnCreate = FormCreate
  OnDeactivate = FormDeactivate
  PixelsPerInch = 96
  TextHeight = 18
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 513
    Height = 249
    ActivePage = TabSheet1
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = #1057#1094#1077#1085#1072#1088#1080#1081
      ExplicitLeft = 8
      ExplicitTop = 33
      object CheckListBox1: TCheckListBox
        Left = 8
        Top = 8
        Width = 195
        Height = 209
        BorderStyle = bsNone
        Color = clWhite
        ItemHeight = 25
        Items.Strings = (
          #1050#1072#1083#1080#1073#1088#1086#1074#1082#1072
          #1051#1080#1085#1077#1072#1088#1080#1079#1072#1094#1080#1103
          #1058#1077#1088#1084#1086#1082#1086#1084#1087#1077#1085#1089#1094#1080#1103' '#1053#1050#1059
          #1058#1077#1088#1084#1086#1082#1086#1084#1087#1077#1085#1089#1094#1080#1103' '#1058'-'
          #1058#1077#1088#1084#1086#1082#1086#1084#1087#1077#1085#1089#1094#1080#1103' '#1058'+'
          #1055#1088#1086#1074#1077#1088#1082#1072' '#1058'+'
          #1055#1088#1086#1074#1077#1088#1082#1072' '#1058'-'
          #1055#1088#1086#1074#1077#1088#1082#1072' '#1053#1050#1059)
        Style = lbOwnerDrawFixed
        TabOrder = 0
      end
      object Button1: TButton
        Left = 382
        Top = 8
        Width = 105
        Height = 35
        Caption = #1042#1099#1087#1086#1083#1085#1080#1090#1100
        TabOrder = 1
      end
      object Button16: TButton
        Left = 382
        Top = 49
        Width = 105
        Height = 35
        Caption = #1054#1087#1088#1086#1089
        TabOrder = 2
        OnClick = Button16Click
      end
    end
    object TabSheet2: TTabSheet
      Caption = #1059#1087#1088#1072#1074#1083#1077#1085#1080#1077
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 30
      ExplicitWidth = 529
      ExplicitHeight = 279
      object FlowPanel1: TFlowPanel
        Left = 3
        Top = 14
        Width = 317
        Height = 187
        BevelOuter = bvNone
        FlowStyle = fsTopBottomLeftRight
        TabOrder = 0
        object Label2: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 101
          Height = 18
          Align = alTop
          Caption = #1057#1077#1090#1077#1074#1086#1081' '#1072#1076#1088#1077#1089
        end
        object EditSerial: TEdit
          AlignWithMargins = True
          Left = 3
          Top = 28
          Width = 148
          Height = 26
          Align = alTop
          Color = clInfoBk
          TabOrder = 0
          Text = '100'
        end
        object Label3: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 61
          Width = 108
          Height = 18
          Align = alTop
          Caption = #1040#1076#1088#1077#1089' '#1079#1085#1072#1095#1077#1085#1080#1103
        end
        object Edit1: TEdit
          AlignWithMargins = True
          Left = 3
          Top = 86
          Width = 148
          Height = 26
          Align = alTop
          Color = clInfoBk
          TabOrder = 1
          Text = '100'
        end
        object Label4: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 119
          Width = 64
          Height = 18
          Align = alTop
          Caption = #1047#1085#1072#1095#1077#1085#1080#1077
        end
        object Edit3: TEdit
          AlignWithMargins = True
          Left = 3
          Top = 144
          Width = 148
          Height = 26
          Align = alTop
          Color = clInfoBk
          TabOrder = 2
          Text = '100'
        end
        object CheckBox1: TCheckBox
          AlignWithMargins = True
          Left = 157
          Top = 10
          Width = 151
          Height = 17
          Margins.Top = 10
          Caption = #1042#1089#1077' '#1072#1076#1088#1077#1089#1072' '#1089#1077#1090#1080
          TabOrder = 3
        end
        object RadioButton1: TRadioButton
          AlignWithMargins = True
          Left = 157
          Top = 41
          Width = 85
          Height = 17
          Margins.Top = 10
          Caption = #1056#1077#1075#1080#1089#1090#1088
          TabOrder = 4
        end
        object RadioButton2: TRadioButton
          AlignWithMargins = True
          Left = 157
          Top = 72
          Width = 126
          Height = 17
          Margins.Top = 10
          Caption = #1050#1086#1101#1092#1092#1080#1094#1080#1077#1085#1090
          TabOrder = 5
        end
      end
      object Button2: TButton
        Left = 382
        Top = 14
        Width = 105
        Height = 35
        Caption = #1047#1072#1087#1080#1089#1072#1090#1100
        TabOrder = 1
      end
      object Button3: TButton
        Left = 382
        Top = 55
        Width = 105
        Height = 35
        Caption = #1057#1095#1080#1090#1072#1090#1100
        TabOrder = 2
      end
      object Button4: TButton
        Left = 382
        Top = 96
        Width = 105
        Height = 35
        Caption = #1040#1076#1088#1077#1089
        TabOrder = 3
      end
    end
    object TabSheet3: TTabSheet
      Caption = #1055#1085#1077#1074#1084#1086#1073#1083#1086#1082
      ImageIndex = 2
      ExplicitLeft = 0
      object Button5: TButton
        Left = 386
        Top = 167
        Width = 105
        Height = 35
        Caption = #1042#1099#1082#1083#1102#1095#1080#1090#1100
        TabOrder = 0
      end
      object Button6: TButton
        Left = 274
        Top = 13
        Width = 105
        Height = 35
        Caption = #1050#1083#1072#1087#1072#1085' 1'
        TabOrder = 1
      end
      object Button7: TButton
        Left = 273
        Top = 54
        Width = 105
        Height = 35
        Caption = #1050#1083#1072#1087#1072#1085' 3'
        TabOrder = 2
      end
      object Button8: TButton
        Left = 384
        Top = 54
        Width = 105
        Height = 35
        Caption = #1050#1083#1072#1087#1072#1085' 4'
        TabOrder = 3
      end
      object Button9: TButton
        Left = 385
        Top = 13
        Width = 105
        Height = 35
        Caption = #1050#1083#1072#1087#1072#1085' 2'
        TabOrder = 4
      end
      object Button10: TButton
        Left = 273
        Top = 95
        Width = 105
        Height = 35
        Caption = #1050#1083#1072#1087#1072#1085' 5'
        TabOrder = 5
      end
      object Button15: TButton
        Left = 384
        Top = 95
        Width = 105
        Height = 35
        Caption = #1050#1083#1072#1087#1072#1085' 6'
        TabOrder = 6
      end
    end
    object TabSheet4: TTabSheet
      Caption = #1058#1077#1088#1084#1086#1082#1072#1084#1077#1088#1072
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitHeight = 248
      object Label1: TLabel
        AlignWithMargins = True
        Left = 170
        Top = 102
        Width = 64
        Height = 18
        Caption = #1047#1085#1072#1095#1077#1085#1080#1077
      end
      object Button11: TButton
        Left = 384
        Top = 13
        Width = 105
        Height = 35
        Caption = #1057#1090#1072#1088#1090
        TabOrder = 0
      end
      object Button12: TButton
        Left = 384
        Top = 54
        Width = 105
        Height = 35
        Caption = #1057#1090#1086#1087
        TabOrder = 1
      end
      object Button13: TButton
        Left = 384
        Top = 95
        Width = 105
        Height = 35
        Caption = #1059#1089#1090#1072#1074#1082#1072
        TabOrder = 2
      end
      object Button14: TButton
        Left = 384
        Top = 136
        Width = 105
        Height = 35
        Caption = #1058#1077#1084#1087#1077#1088#1072#1090#1091#1088#1072
        TabOrder = 3
      end
      object Edit2: TEdit
        AlignWithMargins = True
        Left = 240
        Top = 99
        Width = 130
        Height = 26
        Color = clInfoBk
        TabOrder = 4
        Text = '20'
      end
    end
  end
end
