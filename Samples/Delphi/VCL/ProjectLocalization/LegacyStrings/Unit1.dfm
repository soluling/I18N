object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Sample'
  ClientHeight = 170
  ClientWidth = 347
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 63
    Height = 15
    Caption = 'Hello World'
  end
  object Label2: TLabel
    Left = 8
    Top = 24
    Width = 42
    Height = 15
    Caption = 'dummy'
  end
  object Label3: TLabel
    Left = 8
    Top = 40
    Width = 42
    Height = 15
    Caption = 'dummy'
  end
  object Label4: TLabel
    Left = 8
    Top = 56
    Width = 42
    Height = 15
    Caption = 'dummy'
  end
  object Label5: TLabel
    Left = 8
    Top = 72
    Width = 42
    Height = 15
    Caption = 'dummy'
  end
  object Label6: TLabel
    Left = 8
    Top = 88
    Width = 42
    Height = 15
    Caption = 'dummy'
  end
  object Label7: TLabel
    Left = 8
    Top = 104
    Width = 42
    Height = 15
    Caption = 'dummy'
  end
  object Label8: TLabel
    Left = 8
    Top = 120
    Width = 42
    Height = 15
    Caption = 'dummy'
  end
  object Label9: TLabel
    Left = 8
    Top = 136
    Width = 42
    Height = 15
    Caption = 'dummy'
  end
  object SkisLabel: TLabel
    Left = 224
    Top = 120
    Width = 23
    Height = 15
    Caption = '&Skis:'
    FocusControl = SkisEdit
  end
  object CarsLabel: TLabel
    Left = 224
    Top = 72
    Width = 26
    Height = 15
    Caption = '&Cars:'
    FocusControl = CarsEdit
  end
  object LanguageButton: TButton
    Left = 224
    Top = 40
    Width = 113
    Height = 25
    Caption = '&Language...'
    TabOrder = 1
    OnClick = LanguageButtonClick
  end
  object ShowButton: TButton
    Left = 224
    Top = 8
    Width = 113
    Height = 25
    Caption = '&Show...'
    TabOrder = 0
    OnClick = ShowButtonClick
  end
  object SkisEdit: TEdit
    Left = 224
    Top = 136
    Width = 97
    Height = 23
    TabOrder = 2
    Text = '0'
    OnChange = CountEditChange
  end
  object SkisUpDown: TUpDown
    Left = 321
    Top = 136
    Width = 16
    Height = 23
    Associate = SkisEdit
    TabOrder = 3
  end
  object CarsEdit: TEdit
    Left = 224
    Top = 88
    Width = 97
    Height = 23
    TabOrder = 4
    Text = '0'
    OnChange = CountEditChange
  end
  object CarsUpDown: TUpDown
    Left = 321
    Top = 88
    Width = 16
    Height = 23
    Associate = CarsEdit
    TabOrder = 5
  end
end
