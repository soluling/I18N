inherited SharedInheritedForm: TSharedInheritedForm
  Caption = 'Shared and Inherited'
  StyleElements = [seFont, seClient, seBorder]
  TextHeight = 15
  inherited Label1: TLabel
    StyleElements = [seFont, seClient, seBorder]
  end
  object CheckBox1: TCheckBox
    Left = 56
    Top = 56
    Width = 97
    Height = 17
    Caption = 'Check this'
    TabOrder = 0
  end
end
