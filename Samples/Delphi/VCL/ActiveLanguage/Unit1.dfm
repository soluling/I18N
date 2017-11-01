object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Sample'
  ClientHeight = 138
  ClientWidth = 378
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object LanguageButton: TButton
    Left = 264
    Top = 15
    Width = 105
    Height = 25
    Caption = 'L&anguage...'
    TabOrder = 0
    OnClick = LanguageButtonClick
  end
  object LanguageNameGroup: TRadioGroup
    Left = 8
    Top = 8
    Width = 250
    Height = 121
    Caption = ' Language names in Language dialog '
    Items.Strings = (
      '&Native'
      '&Localized'
      '&Both native and localized'
      '&English'
      '&System')
    TabOrder = 1
  end
end
