object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Sample'
  ClientHeight = 66
  ClientWidth = 261
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 54
    Height = 13
    Caption = 'Hello World'
  end
  object Button1: TButton
    Left = 8
    Top = 32
    Width = 97
    Height = 25
    Caption = '&Language...'
    TabOrder = 0
    OnClick = Button1Click
  end
end
