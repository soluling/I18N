object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Ordinal Sample'
  ClientHeight = 281
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
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 34
    Height = 13
    Caption = 'dummy'
  end
  object Label2: TLabel
    Left = 8
    Top = 24
    Width = 34
    Height = 13
    Caption = 'dummy'
  end
  object Label3: TLabel
    Left = 8
    Top = 40
    Width = 34
    Height = 13
    Caption = 'dummy'
  end
  object Label4: TLabel
    Left = 8
    Top = 56
    Width = 34
    Height = 13
    Caption = 'dummy'
  end
  object Label5: TLabel
    Left = 8
    Top = 72
    Width = 34
    Height = 13
    Caption = 'dummy'
  end
  object Label10: TLabel
    Left = 8
    Top = 88
    Width = 34
    Height = 13
    Caption = 'dummy'
  end
  object Label11: TLabel
    Left = 8
    Top = 104
    Width = 34
    Height = 13
    Caption = 'dummy'
  end
  object Label20: TLabel
    Left = 8
    Top = 120
    Width = 34
    Height = 13
    Caption = 'dummy'
  end
  object Label101: TLabel
    Left = 8
    Top = 136
    Width = 34
    Height = 13
    Caption = 'dummy'
  end
  object LanguageButton: TButton
    Left = 312
    Top = 248
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
  object FormGroup: TRadioGroup
    Left = 8
    Top = 152
    Width = 401
    Height = 41
    Caption = ' Format '
    Columns = 2
    Items.Strings = (
      'Use &short (e.g "1st")'
      'Use &long (e.g. "first")')
    TabOrder = 1
    OnClick = RadioClick
  end
  object PluralGroup: TRadioGroup
    Left = 8
    Top = 200
    Width = 401
    Height = 41
    Caption = ' Singular/Plural  '
    Columns = 2
    Items.Strings = (
      'Use &singular sample'
      'Use &plural sample')
    TabOrder = 2
    OnClick = RadioClick
  end
end
