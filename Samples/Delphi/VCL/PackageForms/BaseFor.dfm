object BaseForm: TBaseForm
  Left = 0
  Top = 0
  Caption = 'Base Form'
  ClientHeight = 82
  ClientWidth = 288
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 182
    Height = 15
    Caption = 'This is a base form from a package'
  end
  object Label2: TLabel
    Left = 8
    Top = 24
    Width = 42
    Height = 15
    Caption = 'dummy'
  end
end
