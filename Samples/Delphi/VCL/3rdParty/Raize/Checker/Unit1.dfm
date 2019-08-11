object Form3: TForm3
  Left = 0
  Top = 0
  Caption = 'Raize Checker'
  ClientHeight = 248
  ClientWidth = 457
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  OnMouseDown = FormMouseDown
  PixelsPerInch = 96
  TextHeight = 13
  object RzPageControl1: TRzPageControl
    Left = 8
    Top = 12
    Width = 273
    Height = 149
    Hint = ''
    ActivePage = TabSheet1
    TabIndex = 0
    TabOrder = 0
    FixedDimension = 19
    object TabSheet1: TRzTabSheet
      Caption = 'One'
      object CheckBox1: TCheckBox
        Left = 32
        Top = 12
        Width = 97
        Height = 17
        Caption = 'This is a a long sample'
        TabOrder = 0
      end
      object CheckBox3: TCheckBox
        Left = 56
        Top = 20
        Width = 97
        Height = 17
        Caption = 'This is a a long sample'
        TabOrder = 1
      end
    end
    object TabSheet2: TRzTabSheet
      Caption = 'Two'
      object CheckBox2: TCheckBox
        Left = 64
        Top = 60
        Width = 97
        Height = 17
        Caption = 'This is a a long sample'
        TabOrder = 0
      end
    end
  end
  object RzGroupBar1: TRzGroupBar
    Left = 0
    Top = 0
    Height = 248
    GradientColorStart = clBtnFace
    GradientColorStop = clBtnShadow
    GroupBorderSize = 8
    Color = clBtnShadow
    ParentColor = False
    TabOrder = 1
  end
end
