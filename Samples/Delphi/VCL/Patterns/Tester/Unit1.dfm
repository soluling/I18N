object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Pattern Tester'
  ClientHeight = 218
  ClientWidth = 746
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    746
    218)
  PixelsPerInch = 96
  TextHeight = 13
  object PatternLabel: TLabel
    Left = 8
    Top = 8
    Width = 40
    Height = 13
    Caption = '&Pattern:'
    FocusControl = PatternEdit
  end
  object LanguageLabel: TLabel
    Left = 632
    Top = 56
    Width = 51
    Height = 13
    Anchors = [akTop, akRight]
    Caption = '&Language:'
    FocusControl = LanguageCombo
  end
  object ResultLabel: TLabel
    Left = 8
    Top = 48
    Width = 34
    Height = 13
    Caption = 'dummy'
  end
  object ParameterLabel: TLabel
    Left = 632
    Top = 104
    Width = 95
    Height = 13
    Anchors = [akTop, akRight]
    Caption = '&Numeric parameter:'
    FocusControl = ParameterEdit
  end
  object StatusLabel: TLabel
    Left = 592
    Top = 48
    Width = 34
    Height = 13
    Alignment = taRightJustify
    Caption = 'dummy'
  end
  object PatternEdit: TComboBox
    Left = 8
    Top = 24
    Width = 617
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = PatternEditChange
    OnDblClick = DefaultButtonClick
  end
  object ClearButton: TButton
    Left = 632
    Top = 148
    Width = 105
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Clear'
    TabOrder = 6
    OnClick = ClearButtonClick
  end
  object ParseButton: TButton
    Left = 632
    Top = 22
    Width = 105
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Parse'
    TabOrder = 2
    OnClick = ParseButtonClick
  end
  object LanguageCombo: TComboBox
    Left = 632
    Top = 72
    Width = 105
    Height = 21
    Style = csDropDownList
    Anchors = [akTop, akRight]
    DropDownCount = 30
    TabOrder = 3
    OnChange = LanguageComboChange
  end
  object ItemsList: TListView
    Left = 8
    Top = 72
    Width = 617
    Height = 137
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Type'
        Width = 100
      end
      item
        Caption = 'Value'
        Width = 495
      end>
    ReadOnly = True
    RowSelect = True
    TabOrder = 1
    ViewStyle = vsReport
  end
  object ParameterEdit: TEdit
    Left = 632
    Top = 120
    Width = 89
    Height = 21
    Anchors = [akTop, akRight]
    TabOrder = 4
    Text = '1'
    OnChange = ParameterEditChange
  end
  object ParameterUpDown: TUpDown
    Left = 721
    Top = 120
    Width = 16
    Height = 21
    Anchors = [akTop, akRight]
    Associate = ParameterEdit
    Max = 1000
    Position = 1
    TabOrder = 5
  end
  object DefaultButton: TButton
    Left = 632
    Top = 180
    Width = 105
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Default'
    TabOrder = 7
    OnClick = DefaultButtonClick
  end
end
