object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Application Form'
  ClientHeight = 169
  ClientWidth = 377
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
    Width = 85
    Height = 13
    Caption = 'Design time string'
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
  object Button1: TButton
    Left = 232
    Top = 8
    Width = 137
    Height = 25
    Caption = 'Show package form...'
    TabOrder = 0
    OnClick = Button1Click
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 80
    Width = 361
    Height = 81
    Caption = ' Package frame '
    TabOrder = 1
  end
end
