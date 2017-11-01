object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Resources'
  ClientHeight = 425
  ClientWidth = 713
  Color = clBtnFace
  Constraints.MinHeight = 200
  Constraints.MinWidth = 500
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnResize = FormResize
  DesignSize = (
    713
    425)
  PixelsPerInch = 96
  TextHeight = 13
  object TextLabel: TLabel
    Left = 8
    Top = 8
    Width = 34
    Height = 13
    Caption = 'dummy'
  end
  object TextMemo: TMemo
    Left = 8
    Top = 24
    Width = 697
    Height = 361
    TabStop = False
    Anchors = [akLeft, akTop, akRight, akBottom]
    BorderStyle = bsNone
    Color = clBtnFace
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object TextPreviousButton: TButton
    Left = 7
    Top = 392
    Width = 75
    Height = 25
    Anchors = [akBottom]
    Caption = 'P&revious'
    TabOrder = 1
    OnClick = TextPreviousButtonClick
  end
  object TextNextButton: TButton
    Left = 88
    Top = 392
    Width = 75
    Height = 25
    Anchors = [akBottom]
    Caption = 'N&ext'
    TabOrder = 2
    OnClick = TextNextButtonClick
  end
end
