object FormPopup: TFormPopup
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  ClientHeight = 163
  ClientWidth = 546
  Color = clInfoBk
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -19
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnDeactivate = FormDeactivate
  PixelsPerInch = 96
  TextHeight = 23
  object RichEdit1: TRichEdit
    Left = 0
    Top = 0
    Width = 546
    Height = 163
    Align = alClient
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    Lines.Strings = (
      #1047#1076#1077#1089#1100' '#1076#1086#1083#1078#1085#1086' '#1073#1099#1090#1100' '#1076#1083#1080#1085#1085#1086#1077#1080#1085#1092#1086#1088#1084#1072#1094#1080#1086#1085#1085#1086#1077' '#1089#1086#1086#1073#1097#1077#1085#1080#1077)
    ParentColor = True
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
    Zoom = 100
  end
end
