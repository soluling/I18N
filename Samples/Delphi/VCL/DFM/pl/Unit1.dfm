object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Próbki'
  ClientHeight = 226
  ClientWidth = 447
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 74
    Height = 13
    Caption = 'Jest to próbka'
  end
  object CloseButton: TButton
    Left = 89
    Top = 56
    Width = 75
    Height = 25
    Caption = '&Zamknij'
    TabOrder = 0
    OnClick = CloseButtonClick
  end
  object ShowButton: TButton
    Left = 8
    Top = 56
    Width = 75
    Height = 25
    Caption = 'Poka¿'
    TabOrder = 1
    OnClick = ShowButtonClick
  end
end
