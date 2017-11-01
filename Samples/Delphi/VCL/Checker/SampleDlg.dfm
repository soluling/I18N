object SampleDialog: TSampleDialog
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Sample Dialog'
  ClientHeight = 185
  ClientWidth = 394
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object ComboBox1: TComboBox
    Left = 8
    Top = 8
    Width = 145
    Height = 21
    TabOrder = 0
  end
  object ListBox1: TListBox
    Left = 144
    Top = 8
    Width = 121
    Height = 97
    ItemHeight = 13
    TabOrder = 1
  end
  object Memo1: TMemo
    Left = 304
    Top = 48
    Width = 185
    Height = 89
    TabOrder = 2
  end
  object OkButton: TButton
    Left = 312
    Top = 152
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 3
  end
end
