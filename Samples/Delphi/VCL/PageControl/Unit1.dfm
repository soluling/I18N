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
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 449
    Height = 233
    ActivePage = TabSheet3
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
      object CheckBox1: TCheckBox
        Left = 8
        Top = 8
        Width = 425
        Height = 17
        Caption = 'Check'
        TabOrder = 0
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Three'
      ImageIndex = 2
      object StackPanel1: TStackPanel
        Left = 0
        Top = 0
        Width = 441
        Height = 205
        Align = alClient
        BevelOuter = bvNone
        ControlCollection = <
          item
            Control = Label2
          end
          item
            Control = CheckBox2
          end>
        Padding.Left = 8
        Padding.Top = 8
        Padding.Right = 8
        Padding.Bottom = 8
        TabOrder = 0
        ExplicitLeft = 120
        ExplicitTop = 8
        ExplicitWidth = 185
        ExplicitHeight = 200
        object Label2: TLabel
          Left = 8
          Top = 8
          Width = 23
          Height = 13
          Caption = 'Hello'
        end
        object CheckBox2: TCheckBox
          Left = 8
          Top = 23
          Width = 97
          Height = 17
          Caption = 'Check me'
          TabOrder = 0
        end
      end
    end
  end
end
