object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Sample'
  ClientHeight = 168
  ClientWidth = 417
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
    Top = 8
    Width = 132
    Height = 13
    Caption = 'This sentence has a period.'
  end
  object Label2: TLabel
    Left = 8
    Top = 40
    Width = 250
    Height = 13
    Caption = ' This sentence has leading and trailing white spaces '
  end
  object Label3: TLabel
    Left = 8
    Top = 24
    Width = 127
    Height = 13
    Caption = 'This sentense has a colon:'
  end
  object HotkeyLabel: TLabel
    Left = 8
    Top = 56
    Width = 131
    Height = 13
    Caption = 'This sentence has a &hotkey'
  end
  object PatternLabel: TLabel
    Left = 8
    Top = 72
    Width = 34
    Height = 13
    Caption = 'dummy'
  end
end
