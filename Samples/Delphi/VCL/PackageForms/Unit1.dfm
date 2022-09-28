object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Sample'
  ClientHeight = 81
  ClientWidth = 428
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object Label1: TLabel
    Left = 8
    Top = 48
    Width = 42
    Height = 15
    Caption = 'dummy'
  end
  object BaseButton: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = '&Base...'
    TabOrder = 0
    OnClick = BaseButtonClick
  end
  object PackageDerivedButton: TButton
    Left = 88
    Top = 8
    Width = 161
    Height = 25
    Caption = '&Derived in the package...'
    TabOrder = 1
    OnClick = PackageDerivedButtonClick
  end
  object ApplicationDerivedButton: TButton
    Left = 256
    Top = 8
    Width = 161
    Height = 25
    Caption = '&Derived in the application...'
    TabOrder = 2
    OnClick = ApplicationDerivedButtonClick
  end
end
