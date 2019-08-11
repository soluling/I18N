object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Sample'
  ClientHeight = 146
  ClientWidth = 338
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
    Top = 56
    Width = 34
    Height = 13
    Caption = 'dummy'
  end
  object Label2: TLabel
    Left = 8
    Top = 72
    Width = 34
    Height = 13
    Caption = 'dummy'
  end
  object Label3: TLabel
    Left = 8
    Top = 88
    Width = 34
    Height = 13
    Caption = 'dummy'
  end
  object Label4: TLabel
    Left = 8
    Top = 104
    Width = 34
    Height = 13
    Caption = 'dummy'
  end
  object Label5: TLabel
    Left = 8
    Top = 120
    Width = 34
    Height = 13
    Caption = 'dummy'
  end
  object FirstNameLabel: TLabel
    Left = 8
    Top = 8
    Width = 54
    Height = 13
    Caption = '&First name:'
  end
  object CountLabel: TLabel
    Left = 264
    Top = 8
    Width = 33
    Height = 13
    Caption = '&Count:'
  end
  object SecondNameLabel: TLabel
    Left = 136
    Top = 8
    Width = 68
    Height = 13
    Caption = '&Second name:'
  end
  object FirstNameEdit: TEdit
    Left = 8
    Top = 24
    Width = 121
    Height = 21
    TabOrder = 0
    OnChange = EditChange
  end
  object SecondNameEdit: TEdit
    Left = 136
    Top = 24
    Width = 121
    Height = 21
    TabOrder = 1
    OnChange = EditChange
  end
  object CountEdit: TEdit
    Left = 264
    Top = 24
    Width = 49
    Height = 21
    TabOrder = 2
    Text = '0'
    OnChange = EditChange
  end
  object CountUpDown: TUpDown
    Left = 313
    Top = 24
    Width = 16
    Height = 21
    Associate = CountEdit
    TabOrder = 3
  end
  object LanguageButton: TButton
    Left = 248
    Top = 112
    Width = 83
    Height = 25
    Caption = '&Language...'
    TabOrder = 4
    OnClick = LanguageButtonClick
  end
end
