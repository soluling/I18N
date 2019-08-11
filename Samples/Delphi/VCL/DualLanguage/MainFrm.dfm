object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Dual Language Sample'
  ClientHeight = 249
  ClientWidth = 539
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object MainLabel: TLabel
    Left = 8
    Top = 8
    Width = 73
    Height = 13
    Caption = '&Main language:'
  end
  object ClientLabel: TLabel
    Left = 272
    Top = 8
    Width = 78
    Height = 13
    Caption = '&Client language:'
  end
  object MainCombo: TComboBox
    Left = 8
    Top = 24
    Width = 257
    Height = 21
    Style = csDropDownList
    Sorted = True
    TabOrder = 0
    OnChange = MainComboChange
  end
  object ClientCombo: TComboBox
    Left = 272
    Top = 24
    Width = 257
    Height = 21
    Style = csDropDownList
    Sorted = True
    TabOrder = 1
    OnChange = ClientComboChange
  end
  object MainGroup: TGroupBox
    Left = 8
    Top = 48
    Width = 257
    Height = 161
    Caption = ' Main '
    TabOrder = 2
    object PlanLabel: TLabel
      Left = 8
      Top = 16
      Width = 24
      Height = 13
      Caption = '&Plan:'
      FocusControl = PlanList
    end
    object PlanList: TListBox
      Left = 8
      Top = 32
      Width = 241
      Height = 97
      ItemHeight = 13
      Items.Strings = (
        'Saver'
        'Standard'
        'Premium'
        'Ultimate')
      TabOrder = 0
    end
    object NowCheck: TCheckBox
      Left = 8
      Top = 136
      Width = 241
      Height = 17
      Caption = '&Start today'
      TabOrder = 1
    end
  end
  object ClientGroup: TGroupBox
    Left = 272
    Top = 48
    Width = 257
    Height = 161
    Caption = ' Client '
    TabOrder = 3
    object NameLabel: TLabel
      Left = 8
      Top = 16
      Width = 31
      Height = 13
      Caption = '&Name:'
      FocusControl = NameEdit
    end
    object AgeLabel: TLabel
      Left = 8
      Top = 56
      Width = 23
      Height = 13
      Caption = '&Age:'
    end
    object NameEdit: TEdit
      Left = 8
      Top = 32
      Width = 241
      Height = 21
      TabOrder = 0
    end
    object AgeEdit: TEdit
      Left = 8
      Top = 72
      Width = 57
      Height = 21
      TabOrder = 1
      Text = '35'
    end
    object AgeUpDown: TUpDown
      Left = 65
      Top = 72
      Width = 16
      Height = 21
      Associate = AgeEdit
      Min = 18
      Max = 130
      Position = 35
      TabOrder = 2
    end
    object NewCheck: TCheckBox
      Left = 8
      Top = 96
      Width = 241
      Height = 17
      Caption = '&New client'
      TabOrder = 3
    end
  end
  object MainButton: TButton
    Left = 128
    Top = 216
    Width = 137
    Height = 25
    Caption = '&Main options...'
    TabOrder = 4
    OnClick = MainButtonClick
  end
  object ClientButton: TButton
    Left = 392
    Top = 216
    Width = 137
    Height = 25
    Caption = 'C&lient options...'
    TabOrder = 5
    OnClick = ClientButtonClick
  end
end
