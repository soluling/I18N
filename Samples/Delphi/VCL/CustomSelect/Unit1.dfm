object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Sample'
  ClientHeight = 129
  ClientWidth = 427
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
  object Label1: TLabel
    Left = 8
    Top = 104
    Width = 101
    Height = 13
    Caption = 'This is a sample text.'
  end
  object LanguageGroup: TRadioGroup
    Left = 8
    Top = 8
    Width = 409
    Height = 41
    Caption = ' Select language '
    TabOrder = 0
    OnClick = LanguageGroupClick
  end
  object NameGroup: TRadioGroup
    Left = 8
    Top = 56
    Width = 409
    Height = 41
    Caption = ' Language names '
    Columns = 4
    Items.Strings = (
      '&Native'
      '&Localized'
      '&English'
      '&System')
    TabOrder = 1
    OnClick = NameGroupClick
  end
end
