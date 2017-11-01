object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 516
  ClientWidth = 931
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object RzDBRadioGroup1: TRzDBRadioGroup
    Left = 200
    Top = 8
    Width = 185
    Caption = 'RzDBRadioGroup1'
    TabOrder = 0
  end
  object RzDBCheckBoxGroup1: TRzDBCheckBoxGroup
    Left = 8
    Top = 8
    Width = 185
    ValueChecked = 'True'
    ValueUnchecked = 'False'
    Caption = 'RzDBCheckBoxGroup1'
    TabOrder = 1
    TabStop = True
  end
  object RzDBEdit1: TRzDBEdit
    Left = 400
    Top = 248
    Width = 121
    Height = 21
    TabOrder = 2
  end
  object RzDBButtonEdit1: TRzDBButtonEdit
    Left = 224
    Top = 248
    Width = 121
    Height = 21
    TabOrder = 3
    AltBtnWidth = 15
    ButtonWidth = 15
  end
  object RzDBDateTimeEdit1: TRzDBDateTimeEdit
    Left = 408
    Top = 200
    Width = 121
    Height = 21
    TabOrder = 4
    EditType = etDate
  end
  object RzDBNumericEdit3: TRzDBNumericEdit
    Left = 480
    Top = 400
    Width = 65
    Height = 21
    Alignment = taLeftJustify
    TabOrder = 5
    DisplayFormat = ',0;(,0)'
  end
  object RzDBSpinEdit1: TRzDBSpinEdit
    Left = 656
    Top = 336
    Width = 47
    Height = 21
    TabOrder = 6
    Max = 100.000000000000000000
    Value = 1.000000000000000000
  end
  object RzDBExpandEdit1: TRzDBExpandEdit
    Left = 712
    Top = 256
    Width = 121
    Height = 21
    TabOrder = 7
    ExpandedWidth = 0
  end
  object RzDBMemo1: TRzDBMemo
    Left = 544
    Top = 336
    Width = 185
    Height = 89
    TabOrder = 8
  end
  object RzDBRichEdit1: TRzDBRichEdit
    Left = 232
    Top = 392
    Width = 185
    Height = 89
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    TabOrder = 9
    Zoom = 100
  end
  object RzDBSpinner1: TRzDBSpinner
    Left = 400
    Top = 48
    TabOrder = 10
  end
  object RzDBTrackBar1: TRzDBTrackBar
    Left = 608
    Top = 16
    Position = 0
    TabOrder = 11
  end
  object RzDBDateTimePicker1: TRzDBDateTimePicker
    Left = 400
    Top = 16
    Width = 186
    Height = 21
    Date = 42296.324618148150000000
    Format = ''
    Time = 42296.324618148150000000
    TabOrder = 12
  end
  object RzDBListBox1: TRzDBListBox
    Left = 24
    Top = 128
    Width = 121
    Height = 97
    ItemHeight = 13
    TabOrder = 13
  end
  object RzDBComboBox1: TRzDBComboBox
    Left = 232
    Top = 144
    Width = 145
    Height = 21
    TabOrder = 14
  end
  object RzDBComboBox2: TRzDBComboBox
    Left = 56
    Top = 288
    Width = 145
    Height = 21
    TabOrder = 15
  end
  object RzDBLookupComboBox1: TRzDBLookupComboBox
    Left = 80
    Top = 352
    Width = 145
    Height = 21
    TabOrder = 16
  end
  object RzDBGrid1: TRzDBGrid
    Left = 552
    Top = 80
    TabOrder = 17
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
end
