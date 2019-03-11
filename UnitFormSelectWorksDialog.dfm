object FormSelectWorksDialog: TFormSelectWorksDialog
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'FormSelectWorksDialog'
  ClientHeight = 265
  ClientWidth = 391
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefault
  OnCreate = FormCreate
  OnDeactivate = FormDeactivate
  PixelsPerInch = 96
  TextHeight = 19
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 391
    Height = 265
    Align = alClient
    BevelKind = bkFlat
    BevelOuter = bvNone
    TabOrder = 0
    object CheckListBox1: TCheckListBox
      Left = 22
      Top = 16
      Width = 195
      Height = 211
      BorderStyle = bsNone
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
      Left = 237
      Top = 16
      Width = 132
      Height = 97
      Caption = #1042#1099#1087#1086#1083#1085#1080#1090#1100' '#1085#1072#1089#1090#1088#1086#1081#1082#1091' '#1087#1088#1080#1073#1086#1088#1086#1074
      TabOrder = 1
    end
    object Button2: TButton
      Left = 237
      Top = 119
      Width = 132
      Height = 97
      Caption = #1053#1072#1095#1072#1090#1100' '#1086#1087#1088#1086#1089' '#1087#1088#1080#1073#1086#1088#1086#1074
      TabOrder = 2
      OnClick = Button2Click
    end
  end
end
