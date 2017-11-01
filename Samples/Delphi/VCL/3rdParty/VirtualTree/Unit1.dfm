object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Sample'
  ClientHeight = 130
  ClientWidth = 450
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object VirtualStringTree1: TVirtualStringTree
    Left = 8
    Top = 8
    Width = 321
    Height = 113
    Header.AutoSizeIndex = 0
    Header.DefaultHeight = 17
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.Height = 17
    Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
    TabOrder = 0
    Columns = <
      item
        Position = 0
        Width = 100
        WideText = 'One'
      end
      item
        Position = 1
        Width = 100
        WideText = 'Two'
      end
      item
        Position = 2
        Width = 100
        WideText = 'Three'
      end>
  end
  object Button1: TButton
    Left = 336
    Top = 8
    Width = 105
    Height = 25
    Caption = '&Language...'
    TabOrder = 1
    OnClick = Button1Click
  end
end
