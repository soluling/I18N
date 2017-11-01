object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Gender Sample'
  ClientHeight = 97
  ClientWidth = 418
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
  object NeutralLabel: TLabel
    Left = 8
    Top = 40
    Width = 34
    Height = 13
    Caption = 'dummy'
  end
  object MaleLabel: TLabel
    Left = 8
    Top = 8
    Width = 34
    Height = 13
    Caption = 'dummy'
  end
  object FemaleLabel: TLabel
    Left = 8
    Top = 24
    Width = 34
    Height = 13
    Caption = 'dummy'
  end
  object NeutralInfo: TLabel
    Left = 376
    Top = 40
    Width = 34
    Height = 13
    Alignment = taRightJustify
    Caption = 'dummy'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object MaleInfo: TLabel
    Left = 376
    Top = 8
    Width = 34
    Height = 13
    Alignment = taRightJustify
    Caption = 'dummy'
  end
  object FemaleInfo: TLabel
    Left = 376
    Top = 24
    Width = 34
    Height = 13
    Alignment = taRightJustify
    Caption = 'dummy'
  end
  object LanguageButton: TButton
    Left = 312
    Top = 64
    Width = 99
    Height = 25
    Caption = '&Language...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnClick = LanguageButtonClick
  end
end
