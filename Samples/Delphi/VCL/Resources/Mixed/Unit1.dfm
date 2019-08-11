object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Resources'
  ClientHeight = 369
  ClientWidth = 881
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
    881
    369)
  PixelsPerInch = 96
  TextHeight = 13
  object Image: TImage
    Left = 8
    Top = 24
    Width = 201
    Height = 305
    Anchors = [akLeft, akTop, akBottom]
    ExplicitHeight = 201
  end
  object ImageLabel: TLabel
    Left = 8
    Top = 8
    Width = 34
    Height = 13
    Caption = 'dummy'
  end
  object TextLabel: TLabel
    Left = 224
    Top = 8
    Width = 34
    Height = 13
    Caption = 'dummy'
  end
  object TextMemo: TMemo
    Left = 224
    Top = 24
    Width = 649
    Height = 305
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
    TabOrder = 2
  end
  object ImagePreviousButton: TButton
    Left = 8
    Top = 336
    Width = 75
    Height = 25
    Anchors = [akBottom]
    Caption = '&Previous'
    TabOrder = 0
    OnClick = ImagePreviousButtonClick
  end
  object ImageNextButton: TButton
    Left = 88
    Top = 336
    Width = 75
    Height = 25
    Anchors = [akBottom]
    Caption = '&Next'
    TabOrder = 1
    OnClick = ImageNextButtonClick
  end
  object TextPreviousButton: TButton
    Left = 224
    Top = 336
    Width = 75
    Height = 25
    Anchors = [akBottom]
    Caption = 'P&revious'
    TabOrder = 3
    OnClick = TextPreviousButtonClick
  end
  object TextNextButton: TButton
    Left = 304
    Top = 336
    Width = 75
    Height = 25
    Anchors = [akBottom]
    Caption = 'N&ext'
    TabOrder = 4
    OnClick = TextNextButtonClick
  end
end
