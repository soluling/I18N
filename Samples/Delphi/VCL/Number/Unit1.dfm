object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Number Abbreviation Sample'
  ClientHeight = 321
  ClientWidth = 459
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
  object Abbreviated1: TLabel
    Left = 8
    Top = 8
    Width = 34
    Height = 13
    Caption = 'dummy'
  end
  object Abbreviated2: TLabel
    Left = 8
    Top = 24
    Width = 34
    Height = 13
    Caption = 'dummy'
  end
  object Abbreviated3: TLabel
    Left = 8
    Top = 40
    Width = 34
    Height = 13
    Caption = 'dummy'
  end
  object Abbreviated4: TLabel
    Left = 8
    Top = 56
    Width = 34
    Height = 13
    Caption = 'dummy'
  end
  object Abbreviated5: TLabel
    Left = 8
    Top = 72
    Width = 34
    Height = 13
    Caption = 'dummy'
  end
  object Abbreviated6: TLabel
    Left = 8
    Top = 88
    Width = 34
    Height = 13
    Caption = 'dummy'
  end
  object Abbreviated7: TLabel
    Left = 8
    Top = 104
    Width = 34
    Height = 13
    Caption = 'dummy'
  end
  object Abbreviated8: TLabel
    Left = 8
    Top = 120
    Width = 34
    Height = 13
    Caption = 'dummy'
  end
  object Abbreviated9: TLabel
    Left = 8
    Top = 136
    Width = 34
    Height = 13
    Caption = 'dummy'
  end
  object PrecisionLabel: TLabel
    Left = 352
    Top = 240
    Width = 46
    Height = 13
    Caption = '&Precision:'
  end
  object Abbreviated10: TLabel
    Left = 8
    Top = 152
    Width = 34
    Height = 13
    Caption = 'dummy'
  end
  object Original1: TLabel
    Left = 416
    Top = 8
    Width = 34
    Height = 13
    Alignment = taRightJustify
    Caption = 'dummy'
  end
  object Original2: TLabel
    Left = 416
    Top = 24
    Width = 34
    Height = 13
    Alignment = taRightJustify
    Caption = 'dummy'
  end
  object Original3: TLabel
    Left = 416
    Top = 40
    Width = 34
    Height = 13
    Alignment = taRightJustify
    Caption = 'dummy'
  end
  object Original4: TLabel
    Left = 416
    Top = 56
    Width = 34
    Height = 13
    Alignment = taRightJustify
    Caption = 'dummy'
  end
  object Original5: TLabel
    Left = 416
    Top = 72
    Width = 34
    Height = 13
    Alignment = taRightJustify
    Caption = 'dummy'
  end
  object Original6: TLabel
    Left = 416
    Top = 88
    Width = 34
    Height = 13
    Alignment = taRightJustify
    Caption = 'dummy'
  end
  object Original7: TLabel
    Left = 416
    Top = 104
    Width = 34
    Height = 13
    Alignment = taRightJustify
    Caption = 'dummy'
  end
  object Original8: TLabel
    Left = 416
    Top = 120
    Width = 34
    Height = 13
    Alignment = taRightJustify
    Caption = 'dummy'
  end
  object Original9: TLabel
    Left = 416
    Top = 136
    Width = 34
    Height = 13
    Alignment = taRightJustify
    Caption = 'dummy'
  end
  object Original10: TLabel
    Left = 416
    Top = 152
    Width = 34
    Height = 13
    Alignment = taRightJustify
    Caption = 'dummy'
  end
  object Abbreviated11: TLabel
    Left = 8
    Top = 168
    Width = 34
    Height = 13
    Caption = 'dummy'
  end
  object Original11: TLabel
    Left = 416
    Top = 168
    Width = 34
    Height = 13
    Alignment = taRightJustify
    Caption = 'dummy'
  end
  object Abbreviated12: TLabel
    Left = 8
    Top = 184
    Width = 34
    Height = 13
    Caption = 'dummy'
  end
  object Original12: TLabel
    Left = 416
    Top = 184
    Width = 34
    Height = 13
    Alignment = taRightJustify
    Caption = 'dummy'
  end
  object Abbreviated13: TLabel
    Left = 8
    Top = 200
    Width = 34
    Height = 13
    Caption = 'dummy'
  end
  object Abbreviated14: TLabel
    Left = 8
    Top = 216
    Width = 34
    Height = 13
    Caption = 'dummy'
  end
  object Original13: TLabel
    Left = 416
    Top = 200
    Width = 34
    Height = 13
    Alignment = taRightJustify
    Caption = 'dummy'
  end
  object Original14: TLabel
    Left = 416
    Top = 216
    Width = 34
    Height = 13
    Alignment = taRightJustify
    Caption = 'dummy'
  end
  object FormGroup: TRadioGroup
    Left = 8
    Top = 240
    Width = 337
    Height = 73
    Caption = ' Format '
    Items.Strings = (
      '&Long such as "1 million"'
      '&Short such as "1M"'
      '&Currency such as "$1M"')
    TabOrder = 0
    OnClick = FormGroupClick
  end
  object LanguageButton: TButton
    Left = 352
    Top = 288
    Width = 99
    Height = 25
    Caption = '&Language...'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    OnClick = LanguageButtonClick
  end
  object PrecisionEdit: TEdit
    Left = 352
    Top = 256
    Width = 25
    Height = 21
    TabOrder = 1
    Text = '2'
    OnChange = PrecisionEditChange
  end
  object PrecisionUpDown: TUpDown
    Left = 377
    Top = 256
    Width = 16
    Height = 21
    Associate = PrecisionEdit
    Min = 1
    Max = 4
    Position = 2
    TabOrder = 2
  end
end
