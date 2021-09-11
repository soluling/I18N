object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Sample'
  ClientHeight = 142
  ClientWidth = 371
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  PixelsPerInch = 96
  TextHeight = 13
  object LanguageButton: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = '&Language...'
    TabOrder = 0
    OnClick = LanguageButtonClick
  end
  object ShowButton: TButton
    Left = 89
    Top = 8
    Width = 75
    Height = 25
    Caption = '&Show...'
    TabOrder = 1
    OnClick = ShowButtonClick
  end
end
