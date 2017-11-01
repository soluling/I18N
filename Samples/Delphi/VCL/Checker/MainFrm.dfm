inherited MainForm: TMainForm
  Caption = 'Sample'
  ClientHeight = 281
  ClientWidth = 483
  OnCreate = FormCreate
  ExplicitWidth = 499
  ExplicitHeight = 320
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 120
    Top = 96
    Width = 34
    Height = 13
    AutoSize = False
    Caption = 'dummy'
  end
  object TruncationGroup: TGroupBox
    Left = 8
    Top = 8
    Width = 145
    Height = 73
    Caption = ' Truncation '
    TabOrder = 0
    object TruncationLabel1: TLabel
      Left = 8
      Top = 16
      Width = 126
      Height = 13
      Caption = 'This is a sample label text.'
    end
    object TruncationLabel2: TLabel
      Left = 8
      Top = 32
      Width = 34
      Height = 13
      Caption = 'dummy'
    end
    object TruncationLabel3: TLabel
      Left = 7
      Top = 48
      Width = 34
      Height = 13
      AutoSize = False
      Caption = 'dummy'
    end
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 88
    Width = 97
    Height = 17
    Caption = 'This is a sample'
    TabOrder = 1
  end
  object CheckBox2: TCheckBox
    Left = 8
    Top = 120
    Width = 97
    Height = 17
    Caption = 'dummy'
    TabOrder = 2
  end
  object Button2: TButton
    Left = 168
    Top = 152
    Width = 75
    Height = 25
    Caption = 'dummy'
    TabOrder = 3
  end
  object OverlappingGroup: TGroupBox
    Left = 160
    Top = 8
    Width = 185
    Height = 41
    Caption = ' Overlapping '
    TabOrder = 4
    object OverlappingLabel1: TLabel
      Left = 8
      Top = 16
      Width = 53
      Height = 13
      Caption = 'String one.'
    end
    object OverlappingLabel2: TLabel
      Left = 72
      Top = 16
      Width = 34
      Height = 13
      Caption = 'dummy'
    end
    object OverlappingLabel3: TLabel
      Left = 120
      Top = 16
      Width = 54
      Height = 13
      Caption = 'Last string.'
    end
  end
  object LanguageButton: TButton
    Left = 400
    Top = 248
    Width = 75
    Height = 25
    Caption = '&Language...'
    TabOrder = 5
    OnClick = LanguageButtonClick
  end
  inline Frame1: TFrame1
    Left = 358
    Top = 8
    Width = 116
    Height = 67
    TabOrder = 6
    ExplicitLeft = 358
    ExplicitTop = 8
    inherited CheckBox1: TCheckBox
      Caption = 'Check box'
    end
    inherited CheckBox2: TCheckBox
      Caption = 'Check box'
    end
  end
  object Panel1: TPanel
    Left = 8
    Top = 184
    Width = 89
    Height = 57
    TabOrder = 7
    object CheckBox3: TCheckBox
      Left = 8
      Top = 8
      Width = 97
      Height = 17
      Caption = 'Check box'
      TabOrder = 0
    end
  end
  object ShowButton: TButton
    Left = 8
    Top = 152
    Width = 75
    Height = 25
    Caption = 'Click to show'
    TabOrder = 8
    OnClick = ShowButtonClick
  end
  object Button1: TButton
    Left = 88
    Top = 152
    Width = 75
    Height = 25
    Caption = 'This text is too.'
    TabOrder = 9
    OnClick = ShowButtonClick
  end
  object CheckBox4: TCheckBox
    Left = 8
    Top = 104
    Width = 97
    Height = 17
    Caption = 'This is a a long sample'
    TabOrder = 10
  end
  object RadioButton1: TRadioButton
    Left = 104
    Top = 184
    Width = 97
    Height = 17
    Caption = 'This is a sample'
    TabOrder = 11
  end
  object RadioButton2: TRadioButton
    Left = 104
    Top = 200
    Width = 97
    Height = 17
    Caption = 'This is a a long sample'
    TabOrder = 12
  end
  object ComboBox1: TComboBox
    Left = 160
    Top = 56
    Width = 73
    Height = 21
    Style = csDropDownList
    TabOrder = 13
    Items.Strings = (
      'One'
      'Two'
      'This is too long')
  end
  object PageControl1: TPageControl
    Left = 280
    Top = 96
    Width = 185
    Height = 97
    ActivePage = TabSheet2
    TabOrder = 14
    object TabSheet1: TTabSheet
      Caption = 'TabSheet1'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object CheckBox5: TCheckBox
        Left = 8
        Top = 20
        Width = 97
        Height = 17
        Caption = 'This is a a long sample'
        TabOrder = 0
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'TabSheet2'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object CheckBox6: TCheckBox
        Left = 48
        Top = 4
        Width = 97
        Height = 17
        Caption = 'This is a a long sample'
        TabOrder = 0
      end
    end
  end
  object SimpleButton: TButton
    Left = 8
    Top = 248
    Width = 97
    Height = 25
    Caption = 'Show simple...'
    TabOrder = 15
    OnClick = SimpleButtonClick
  end
end
