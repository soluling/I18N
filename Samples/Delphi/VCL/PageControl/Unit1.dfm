object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Sample'
  ClientHeight = 248
  ClientWidth = 467
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 449
    Height = 233
    ActivePage = TabSheet1
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'One'
      object Label1: TLabel
        Left = 8
        Top = 8
        Width = 54
        Height = 13
        Caption = 'Hello World'
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Two'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 281
      ExplicitHeight = 165
      object CheckBox1: TCheckBox
        Left = 8
        Top = 8
        Width = 425
        Height = 17
        Caption = 'Check'
        TabOrder = 0
      end
    end
  end
end
