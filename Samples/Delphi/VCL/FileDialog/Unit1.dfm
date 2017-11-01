object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'File Dialog Sample'
  ClientHeight = 209
  ClientWidth = 418
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object OpenLabel1: TLabel
    Left = 88
    Top = 13
    Width = 107
    Height = 13
    Caption = 'Filter as a single string'
  end
  object OpenLabel2: TLabel
    Left = 88
    Top = 45
    Width = 133
    Height = 13
    Caption = 'Composed filter with all files'
  end
  object OpenLabel3: TLabel
    Left = 88
    Top = 77
    Width = 149
    Height = 13
    Caption = 'Composed filter without all files'
  end
  object OpenLabel4: TLabel
    Left = 88
    Top = 109
    Width = 181
    Height = 13
    Caption = 'Composed filter all files after XML files'
  end
  object SaveLabel1: TLabel
    Left = 88
    Top = 149
    Width = 107
    Height = 13
    Caption = 'Filter as a single string'
  end
  object SaveLabel2: TLabel
    Left = 88
    Top = 181
    Width = 75
    Height = 13
    Caption = 'Composed filter'
  end
  object OpenButton1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = '&Open...'
    TabOrder = 0
    OnClick = OpenButton1Click
  end
  object SaveButton1: TButton
    Left = 8
    Top = 144
    Width = 75
    Height = 25
    Caption = '&Save...'
    TabOrder = 4
    OnClick = SaveButton1Click
  end
  object OpenButton2: TButton
    Left = 8
    Top = 40
    Width = 75
    Height = 25
    Caption = 'O&pen...'
    TabOrder = 1
    OnClick = OpenButton2Click
  end
  object OpenButton3: TButton
    Left = 8
    Top = 72
    Width = 75
    Height = 25
    Caption = 'Op&en...'
    TabOrder = 2
    OnClick = OpenButton3Click
  end
  object OpenButton4: TButton
    Left = 8
    Top = 104
    Width = 75
    Height = 25
    Caption = 'Ope&n...'
    TabOrder = 3
    OnClick = OpenButton4Click
  end
  object SaveButton2: TButton
    Left = 8
    Top = 176
    Width = 75
    Height = 25
    Caption = 'S&ave...'
    TabOrder = 5
    OnClick = SaveButton2Click
  end
end
