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
  TextHeight = 15
  object LanguageButton: TButton
    Left = 168
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
    TabOrder = 2
    OnClick = PreviewButtonClick
  end
  object PrintButton: TButton
    Left = 89
    Top = 8
    Width = 75
    Height = 25
    Caption = 'P&rint...'
    TabOrder = 1
    OnClick = PrintButtonClick
  end
end
