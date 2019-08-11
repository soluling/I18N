object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Select Sample'
  ClientHeight = 96
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
  object SoccerLabel: TLabel
    Left = 8
    Top = 8
    Width = 34
    Height = 13
    Caption = 'dummy'
  end
  object HockeyLabel: TLabel
    Left = 8
    Top = 24
    Width = 34
    Height = 13
    Caption = 'dummy'
  end
  object BasketballLabel: TLabel
    Left = 8
    Top = 40
    Width = 34
    Height = 13
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
