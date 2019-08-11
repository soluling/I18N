inherited SimpleForm: TSimpleForm
  Caption = 'Sample'
  ClientHeight = 90
  ClientWidth = 218
  ExplicitWidth = 234
  ExplicitHeight = 129
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 121
    Height = 73
    Caption = ' Options '
    TabOrder = 0
    object CheckBox1: TCheckBox
      Left = 8
      Top = 16
      Width = 105
      Height = 17
      Caption = '&This is a read only file'
      TabOrder = 0
    end
    object CheckBox2: TCheckBox
      Left = 8
      Top = 32
      Width = 161
      Height = 17
      Caption = '&Encrypt data before sending'
      TabOrder = 1
    end
    object CheckBox3: TCheckBox
      Left = 8
      Top = 48
      Width = 65
      Height = 17
      Caption = '&Mark'
      TabOrder = 2
    end
    object CheckBox4: TCheckBox
      Left = 56
      Top = 48
      Width = 57
      Height = 17
      Caption = 'Release'
      TabOrder = 3
    end
  end
  object OkButton: TButton
    Left = 136
    Top = 16
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 1
  end
end
