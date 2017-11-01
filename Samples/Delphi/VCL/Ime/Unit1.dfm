object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Sample'
  ClientHeight = 203
  ClientWidth = 330
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object DefaultLabel: TLabel
    Left = 8
    Top = 8
    Width = 105
    Height = 13
    Caption = '&Default input method:'
  end
  object OpenLabel: TLabel
    Left = 8
    Top = 104
    Width = 96
    Height = 13
    Caption = '&Open input method:'
  end
  object OpenDescriptionLabel: TLabel
    Left = 8
    Top = 144
    Width = 313
    Height = 49
    AutoSize = False
    Caption = 
      'The above edit component has ImeMode property set to imOpen. Thi' +
      's will add the property to DFM, it will be scanned and it can be' +
      ' localized.'
    WordWrap = True
  end
  object DefaultDescriptionLabel: TLabel
    Left = 8
    Top = 48
    Width = 313
    Height = 49
    AutoSize = False
    Caption = 
      'The above edit component has the default ImeMode: imDontCare. Th' +
      'e default value is not stored to DFM, so it will not be scanned ' +
      'and it cannot be localized.'
    WordWrap = True
  end
  object DefaultEdit: TEdit
    Left = 8
    Top = 24
    Width = 313
    Height = 21
    TabOrder = 0
  end
  object OpenEdit: TEdit
    Left = 8
    Top = 120
    Width = 313
    Height = 21
    ImeMode = imOpen
    TabOrder = 1
  end
end
