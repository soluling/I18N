object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Custom Control Sample'
  ClientHeight = 217
  ClientWidth = 504
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 69
    Height = 13
    Caption = 'Standard label'
  end
  object MyLabel1: TMyLabel
    Left = 8
    Top = 24
    Width = 61
    Height = 13
    Caption = 'Custom label'
    Description = 'This is description'
    ExcludeMe = 'This should be excluded'
  end
end
