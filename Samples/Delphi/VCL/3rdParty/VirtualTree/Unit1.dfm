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
  TextHeight = 13
  object VirtualStringTree1: TVirtualStringTree
    Left = 8
    Top = 8
    Width = 321
    Height = 113
    DefaultNodeHeight = 17
    Header.AutoSizeIndex = 0
    Header.DefaultHeight = 17
    Header.Height = 17
    Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
    TabOrder = 0
    Touch.InteractiveGestures = [igPan, igPressAndTap]
    Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
    Columns = <
      item
        Position = 0
        Text = 'One'
        Width = 100
      end
      item
        Position = 1
        Text = 'Two'
        Width = 100
      end
      item
        Position = 2
        Text = 'Three'
        Width = 100
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
