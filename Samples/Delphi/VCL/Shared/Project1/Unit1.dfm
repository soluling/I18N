object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Sample One'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 86
    Height = 15
    Caption = 'This is a sample.'
  end
  object Button1: TButton
    Left = 8
    Top = 32
    Width = 75
    Height = 25
    Caption = '&Show...'
    TabOrder = 0
    OnClick = Button1Click
  end
end
