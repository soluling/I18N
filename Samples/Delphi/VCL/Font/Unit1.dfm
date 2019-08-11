object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Font Sample'
  ClientHeight = 211
  ClientWidth = 482
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
    Width = 35
    Height = 13
    Caption = 'Default'
  end
  object Label2: TLabel
    Left = 8
    Top = 32
    Width = 60
    Height = 23
    Caption = 'Default'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label3: TLabel
    Left = 8
    Top = 64
    Width = 77
    Height = 22
    Caption = 'Default'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Courier New'
    Font.Style = [fsBold, fsItalic]
    ParentFont = False
  end
  object Label4: TLabel
    Left = 8
    Top = 96
    Width = 35
    Height = 22
    Caption = 'All'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Stencil'
    Font.Style = [fsBold, fsItalic, fsUnderline, fsStrikeOut]
    ParentFont = False
  end
end
