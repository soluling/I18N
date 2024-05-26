object SampleForm: TSampleForm
  Left = 0
  Top = 0
  Caption = 'Package Form'
  ClientHeight = 226
  ClientWidth = 685
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 85
    Height = 13
    Caption = 'Design time string'
  end
  object Label2: TLabel
    Left = 8
    Top = 24
    Width = 34
    Height = 13
    Caption = 'dummy'
  end
  object Label3: TLabel
    Left = 8
    Top = 40
    Width = 34
    Height = 13
    Caption = 'dummy'
  end
  object Label4: TLabel
    Left = 8
    Top = 56
    Width = 34
    Height = 13
    Caption = 'dummy'
  end
  inline SampleFrame1: TSampleFrame
    Left = 24
    Top = 104
    Width = 347
    Height = 53
    TabOrder = 0
    ExplicitLeft = 24
    ExplicitTop = 104
    inherited Label1: TLabel
      Width = 46
      Height = 13
      ExplicitWidth = 46
      ExplicitHeight = 13
    end
    inherited Label2: TLabel
      Width = 34
      Height = 13
      ExplicitWidth = 34
      ExplicitHeight = 13
    end
    inherited Edit1: TEdit
      Height = 21
      ExplicitHeight = 21
    end
  end
end
