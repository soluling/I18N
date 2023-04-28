object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Move'
  ClientHeight = 106
  ClientWidth = 275
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object RedLabel: TLabel
    Left = 8
    Top = 8
    Width = 23
    Height = 15
    Caption = '&Red:'
    FocusControl = RedEdit
  end
  object GreenLabel: TLabel
    Left = 48
    Top = 8
    Width = 34
    Height = 15
    Caption = '&Green:'
    FocusControl = GreenEdit
  end
  object BlueLabel: TLabel
    Left = 88
    Top = 8
    Width = 26
    Height = 15
    Caption = '&Blue:'
    FocusControl = BlueEdit
  end
  object ValueLabel: TLabel
    Left = 8
    Top = 48
    Width = 42
    Height = 15
    Caption = 'dummy'
  end
  object RedEdit: TEdit
    Left = 8
    Top = 24
    Width = 33
    Height = 23
    TabOrder = 0
    OnChange = EditChange
  end
  object CalculateButton: TButton
    Left = 8
    Top = 72
    Width = 97
    Height = 25
    Caption = 'Calculate colors'
    TabOrder = 1
    OnClick = CalculateButtonClick
  end
  object ResetButton: TButton
    Left = 112
    Top = 72
    Width = 75
    Height = 25
    Caption = 'Reset values'
    TabOrder = 2
    OnClick = ResetButtonClick
  end
  object GreenEdit: TEdit
    Left = 48
    Top = 24
    Width = 33
    Height = 23
    TabOrder = 3
    OnChange = EditChange
  end
  object BlueEdit: TEdit
    Left = 88
    Top = 24
    Width = 33
    Height = 23
    TabOrder = 4
    OnChange = EditChange
  end
  object CloseButton: TButton
    Left = 192
    Top = 72
    Width = 75
    Height = 25
    Caption = '&Close'
    TabOrder = 5
    OnClick = CloseButtonClick
  end
end
