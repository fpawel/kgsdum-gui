object KgsdumMainForm: TKgsdumMainForm
  Left = 0
  Top = 0
  Caption = 'KgsdumMainForm'
  ClientHeight = 690
  ClientWidth = 1056
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 19
  object PageControlMain: TPageControl
    Left = 0
    Top = 0
    Width = 1056
    Height = 690
    ActivePage = TabSheetParty
    Align = alClient
    MultiLine = True
    OwnerDraw = True
    TabHeight = 100
    TabOrder = 0
    TabPosition = tpLeft
    TabWidth = 50
    OnChange = PageControlMainChange
    OnDrawTab = PageControlMainDrawTab
    object TabSheetParty: TTabSheet
      Caption = #1055#1072#1088#1090#1080#1103
      ImageIndex = 4
      ExplicitLeft = 108
      ExplicitTop = 8
    end
    object TabSheetArchive: TTabSheet
      Caption = #1040#1088#1093#1080#1074
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
  end
end
