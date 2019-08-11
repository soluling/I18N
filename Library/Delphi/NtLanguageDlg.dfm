object NtLanguageDialog: TNtLanguageDialog
  Left = 256
  Top = 347
  BorderStyle = bsDialog
  Caption = 'Select Language'
  ClientHeight = 193
  ClientWidth = 291
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object LanguageList: TListBox
    Left = 8
    Top = 8
    Width = 193
    Height = 177
    ItemHeight = 13
    Sorted = True
    TabOrder = 0
    OnDblClick = LanguageListDblClick
  end
  object OkButton: TButton
    Left = 208
    Top = 8
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object CancelButton: TButton
    Left = 208
    Top = 40
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
