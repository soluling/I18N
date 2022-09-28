object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form Sample'
  ClientHeight = 171
  ClientWidth = 478
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 48
    Width = 34
    Height = 13
    Caption = 'dummy'
  end
  object Label2: TLabel
    Left = 8
    Top = 88
    Width = 34
    Height = 13
    Caption = 'dummy'
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 121
    Height = 25
    Caption = 'Show base form'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 136
    Top = 8
    Width = 121
    Height = 25
    Caption = 'Show derived form'
    TabOrder = 1
    OnClick = Button2Click
  end
end
