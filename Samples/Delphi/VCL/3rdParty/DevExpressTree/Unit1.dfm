object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 323
  ClientWidth = 557
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object cxTreeList1: TcxTreeList
    Left = 96
    Top = 56
    Width = 250
    Height = 150
    Bands = <
      item
      end>
    Navigator.Buttons.CustomButtons = <>
    TabOrder = 0
    Data = {
      00000500080100000F00000044617461436F6E74726F6C6C6572310200000012
      000000546378537472696E6756616C7565547970651200000054637853747269
      6E6756616C75655479706503000000445855464D540000030000004F006E0065
      000003000000540077006F00445855464D540000030000004F006E0065000005
      00000054006800720065006500445855464D54000003000000540077006F0000
      0400000046006F0075007200020000000000000008080000000000000000FFFF
      FFFFFFFFFFFFFFFFFFFF010000000208010000000000000000000000FFFFFFFF
      FFFFFFFFFFFFFFFF0200000008080000000000000000FFFFFFFFFFFFFFFFFFFF
      FFFF1A0802000000}
    object cxTreeList1Column1: TcxTreeListColumn
      DataBinding.ValueType = 'String'
      Width = 100
      Position.ColIndex = 0
      Position.RowIndex = 0
      Position.BandIndex = 0
      SortOrder = soAscending
      SortIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxTreeList1Column2: TcxTreeListColumn
      DataBinding.ValueType = 'String'
      Width = 100
      Position.ColIndex = 1
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
  end
end
