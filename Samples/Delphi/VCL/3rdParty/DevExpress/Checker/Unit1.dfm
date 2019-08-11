object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 299
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object cxPageControl1: TcxPageControl
    Left = 72
    Top = 24
    Width = 201
    Height = 113
    TabOrder = 0
    Properties.ActivePage = cxTabSheet2
    Properties.CustomButtons.Buttons = <>
    ClientRectBottom = 109
    ClientRectLeft = 4
    ClientRectRight = 197
    ClientRectTop = 24
    object cxTabSheet1: TcxTabSheet
      Caption = 'cxTabSheet1'
      ImageIndex = 0
      object CheckBox1: TCheckBox
        Left = 48
        Top = 32
        Width = 57
        Height = 17
        Caption = 'CheckBox1'
        TabOrder = 0
      end
    end
    object cxTabSheet2: TcxTabSheet
      Caption = 'cxTabSheet2'
      ImageIndex = 1
      object CheckBox2: TCheckBox
        Left = 152
        Top = 40
        Width = 113
        Height = 17
        Caption = 'CheckBox1'
        TabOrder = 0
      end
    end
  end
end
