object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'QuickReport Sample'
  ClientHeight = 43
  ClientWidth = 306
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  PixelsPerInch = 96
  TextHeight = 15
  object LanguageButton: TButton
    Left = 88
    Top = 8
    Width = 75
    Height = 25
    Caption = '&Language...'
    TabOrder = 0
    OnClick = LanguageButtonClick
  end
  object PreviewButton: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = '&Preview...'
    TabOrder = 1
    OnClick = PreviewButtonClick
  end
end
