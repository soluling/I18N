object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 1218
  ClientWidth = 1181
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 15
  object QuickRep1: TQuickRep
    Left = 0
    Top = 0
    Width = 794
    Height = 1123
    ShowingPreview = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    Functions.Strings = (
      'PAGENUMBER'
      'COLUMNNUMBER'
      'REPORTTITLE'
      'QRLOOPBAND1'
      'QRSTRINGSBAND1')
    Functions.DATA = (
      '0'
      '0'
      #39#39
      '0'
      #39#39)
    Options = [FirstPageHeader, LastPageFooter]
    Page.Columns = 1
    Page.Orientation = poPortrait
    Page.PaperSize = A4
    Page.Continuous = False
    Page.Values = (
      100.000000000000000000
      2970.000000000000000000
      100.000000000000000000
      2100.000000000000000000
      100.000000000000000000
      100.000000000000000000
      0.000000000000000000)
    PrinterSettings.Copies = 1
    PrinterSettings.OutputBin = Auto
    PrinterSettings.Duplex = False
    PrinterSettings.FirstPage = 0
    PrinterSettings.LastPage = 0
    PrinterSettings.UseStandardprinter = False
    PrinterSettings.UseCustomBinCode = False
    PrinterSettings.CustomBinCode = 0
    PrinterSettings.ExtendedDuplex = 0
    PrinterSettings.UseCustomPaperCode = False
    PrinterSettings.CustomPaperCode = 0
    PrinterSettings.PrintMetaFile = False
    PrinterSettings.MemoryLimit = 1000000
    PrinterSettings.Collate = 0
    PrinterSettings.ColorOption = 2
    PrintIfEmpty = True
    SnapToGrid = True
    Units = MM
    Zoom = 100
    PrevFormStyle = fsNormal
    PreviewInitialState = wsNormal
    PreviewWidth = 500
    PreviewHeight = 500
    PrevInitialZoom = qrZoomToFit
    PreviewDefaultSaveType = stPDF
    PreviewLeft = 0
    PreviewTop = 0
    object QRSubDetail1: TQRSubDetail
      Left = 38
      Top = 278
      Width = 718
      Height = 40
      AlignToBottom = False
      TransparentBand = False
      ForceNewColumn = False
      ForceNewPage = False
      Size.Values = (
        105.833333333333300000
        1899.708333333333000000)
      PreCaluculateBandHeight = False
      KeepOnOnePage = False
      Master = QuickRep1
      PrintBefore = False
      PrintIfEmpty = True
    end
    object ColumnHeaderBand1: TQRBand
      Left = 38
      Top = 158
      Width = 718
      Height = 40
      AlignToBottom = False
      TransparentBand = False
      ForceNewColumn = False
      ForceNewPage = False
      Size.Values = (
        105.833333333333300000
        1899.708333333333000000)
      PreCaluculateBandHeight = False
      KeepOnOnePage = False
      BandType = rbColumnHeader
    end
    object DetailBand1: TQRBand
      Left = 38
      Top = 238
      Width = 718
      Height = 40
      AlignToBottom = False
      TransparentBand = False
      ForceNewColumn = False
      ForceNewPage = False
      Size.Values = (
        105.833333333333300000
        1899.708333333333000000)
      PreCaluculateBandHeight = False
      KeepOnOnePage = False
      BandType = rbDetail
    end
    object PageFooterBand1: TQRBand
      Left = 38
      Top = 478
      Width = 718
      Height = 40
      AlignToBottom = False
      TransparentBand = False
      ForceNewColumn = False
      ForceNewPage = False
      Size.Values = (
        105.833333333333300000
        1899.708333333333000000)
      PreCaluculateBandHeight = False
      KeepOnOnePage = False
      BandType = rbPageFooter
    end
    object PageHeaderBand1: TQRBand
      Left = 38
      Top = 38
      Width = 718
      Height = 40
      AlignToBottom = False
      TransparentBand = False
      ForceNewColumn = False
      ForceNewPage = False
      Size.Values = (
        105.833333333333300000
        1899.708333333333000000)
      PreCaluculateBandHeight = False
      KeepOnOnePage = False
      BandType = rbPageHeader
    end
    object SummaryBand1: TQRBand
      Left = 38
      Top = 438
      Width = 718
      Height = 40
      AlignToBottom = False
      TransparentBand = False
      ForceNewColumn = False
      ForceNewPage = False
      Size.Values = (
        105.833333333333300000
        1899.708333333333000000)
      PreCaluculateBandHeight = False
      KeepOnOnePage = False
      BandType = rbSummary
    end
    object TitleBand1: TQRBand
      Left = 38
      Top = 78
      Width = 718
      Height = 40
      AlignToBottom = False
      TransparentBand = False
      ForceNewColumn = False
      ForceNewPage = False
      Size.Values = (
        105.833333333333300000
        1899.708333333333000000)
      PreCaluculateBandHeight = False
      KeepOnOnePage = False
      BandType = rbTitle
    end
    object QRSubDetail2: TQRSubDetail
      Left = 38
      Top = 318
      Width = 718
      Height = 40
      AlignToBottom = False
      TransparentBand = False
      ForceNewColumn = False
      ForceNewPage = False
      Size.Values = (
        105.833333333333300000
        1899.708333333333000000)
      PreCaluculateBandHeight = False
      KeepOnOnePage = False
      Master = QuickRep1
      PrintBefore = False
      PrintIfEmpty = True
    end
    object QRLoopBand1: TQRLoopBand
      Left = 38
      Top = 358
      Width = 718
      Height = 40
      AlignToBottom = False
      TransparentBand = False
      ForceNewColumn = False
      ForceNewPage = False
      Size.Values = (
        105.833333333333300000
        1899.708333333333000000)
      PreCaluculateBandHeight = False
      KeepOnOnePage = False
      Master = QuickRep1
      PrintCount = 0
      PrintBefore = False
    end
    object QRStringsBand1: TQRStringsBand
      Left = 38
      Top = 398
      Width = 718
      Height = 40
      AlignToBottom = False
      TransparentBand = False
      ForceNewColumn = False
      ForceNewPage = False
      Size.Values = (
        105.833333333333300000
        1899.708333333333000000)
      PreCaluculateBandHeight = False
      KeepOnOnePage = False
      Master = QuickRep1
      PrintBefore = False
    end
    object QRBand1: TQRBand
      Left = 38
      Top = 118
      Width = 718
      Height = 40
      AlignToBottom = False
      TransparentBand = False
      ForceNewColumn = False
      ForceNewPage = False
      Size.Values = (
        105.833333333333300000
        1899.708333333333000000)
      PreCaluculateBandHeight = False
      KeepOnOnePage = False
      BandType = rbTitle
    end
    object QRWildBand1: TQRWildBand
      Left = 38
      Top = 518
      Width = 718
      Height = 40
      AlignToBottom = False
      TransparentBand = False
      ForceNewColumn = False
      ForceNewPage = False
      Size.Values = (
        105.833333333333300000
        1899.708333333333000000)
      PreCaluculateBandHeight = False
      KeepOnOnePage = False
    end
    object QRChildBand1: TQRChildBand
      Left = 38
      Top = 558
      Width = 718
      Height = 40
      AlignToBottom = False
      TransparentBand = False
      ForceNewColumn = False
      ForceNewPage = False
      Size.Values = (
        105.833333333333300000
        1899.708333333333000000)
      PreCaluculateBandHeight = False
      KeepOnOnePage = False
      PrintOrder = cboAfterParent
    end
    object QRGroup1: TQRGroup
      Left = 38
      Top = 198
      Width = 718
      Height = 40
      AlignToBottom = False
      TransparentBand = False
      ForceNewColumn = False
      ForceNewPage = False
      Size.Values = (
        105.833333333333300000
        1899.708333333333000000)
      PreCaluculateBandHeight = False
      KeepOnOnePage = False
      Master = QuickRep1
      ReprintOnNewPage = False
    end
    object QRLabel1: TQRLabel
      Left = 272
      Top = 688
      Width = 58
      Height = 17
      Size.Values = (
        44.979166666666670000
        719.666666666666700000
        1820.333333333333000000
        153.458333333333300000)
      XLColumn = 0
      XLNumFormat = nfGeneral
      ActiveInPreview = False
      Alignment = taLeftJustify
      AlignToBand = False
      Caption = 'QRLabel1'
      Color = clWhite
      Transparent = False
      ExportAs = exptText
      WrapStyle = BreakOnSpaces
      VerticalAlignment = tlTop
      FontSize = 10
    end
    object QRDBText1: TQRDBText
      Left = 432
      Top = 656
      Width = 70
      Height = 17
      Size.Values = (
        44.979166666666670000
        1143.000000000000000000
        1735.666666666667000000
        185.208333333333300000)
      XLColumn = 0
      XLNumFormat = nfGeneral
      ActiveInPreview = False
      Alignment = taLeftJustify
      AlignToBand = False
      Color = clWhite
      Transparent = False
      ExportAs = exptText
      WrapStyle = BreakOnSpaces
      FullJustify = False
      MaxBreakChars = 0
      VerticalAlignment = tlTop
      FontSize = 10
    end
    object QRExpr1: TQRExpr
      Left = 488
      Top = 736
      Width = 54
      Height = 17
      Size.Values = (
        44.979166666666670000
        1291.166666666667000000
        1947.333333333333000000
        142.875000000000000000)
      XLColumn = 0
      XLNumFormat = nfGeneral
      ActiveInPreview = False
      Alignment = taLeftJustify
      AlignToBand = False
      Color = clWhite
      ResetAfterPrint = False
      Transparent = False
      ExportAs = exptText
      WrapStyle = BreakOnSpaces
      FontSize = 10
    end
    object QRSysData1: TQRSysData
      Left = 328
      Top = 816
      Width = 37
      Height = 17
      Size.Values = (
        44.979166666666670000
        867.833333333333300000
        2159.000000000000000000
        97.895833333333330000)
      XLColumn = 0
      XLNumFormat = nfGeneral
      ActiveInPreview = False
      Alignment = taLeftJustify
      AlignToBand = False
      Color = clWhite
      Data = qrsTime
      Text = ''
      Transparent = False
      ExportAs = exptText
      VerticalAlignment = tlTop
      FontSize = 10
    end
  end
  object QuickAbstractRep1: TQuickAbstractRep
    Left = 856
    Top = 368
    Width = 233
    Height = 249
    ShowingPreview = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = []
    Functions.Strings = (
      'PAGENUMBER'
      'COLUMNNUMBER'
      'REPORTTITLE')
    Functions.DATA = (
      '0'
      '0'
      #39#39)
    Options = [FirstPageHeader, LastPageFooter]
    Page.Columns = 1
    Page.Orientation = poPortrait
    Page.PaperSize = A4
    Page.Continuous = False
    Page.Values = (
      100.000000000000000000
      2970.000000000000000000
      100.000000000000000000
      2100.000000000000000000
      100.000000000000000000
      100.000000000000000000
      0.000000000000000000)
    PrinterSettings.Copies = 1
    PrinterSettings.OutputBin = Auto
    PrinterSettings.Duplex = False
    PrinterSettings.FirstPage = 0
    PrinterSettings.LastPage = 0
    PrinterSettings.UseStandardprinter = False
    PrinterSettings.UseCustomBinCode = False
    PrinterSettings.CustomBinCode = 0
    PrinterSettings.ExtendedDuplex = 0
    PrinterSettings.UseCustomPaperCode = False
    PrinterSettings.CustomPaperCode = 0
    PrinterSettings.PrintMetaFile = False
    PrinterSettings.MemoryLimit = 1000000
    PrinterSettings.Collate = 0
    PrinterSettings.ColorOption = 2
    PrintIfEmpty = True
    Units = MM
    Zoom = 100
  end
  object QRDBImage1: TQRDBImage
    Left = 544
    Top = 576
    Width = 105
    Height = 105
    Size.Values = (
      277.812500000000000000
      1439.333333333333000000
      1524.000000000000000000
      277.812500000000000000)
    XLColumn = 0
    XLNumFormat = nfGeneral
    ActiveInPreview = False
  end
  object QRPreviewController1: TQRPreviewController
    PagesInPreview = 0
    prevInitHeight = 640
    PrevInitLeft = 0
    PrevInitTop = 0
    PrevDefaultSaveType = stQRP
    PrevWindowStyle = fsNormal
    PrevInitState = wsNormal
    PrevInitZoom = qrZoomToFit
    Left = 496
    Top = 232
  end
  object QRXMLSSFilter1: TQRXMLSSFilter
    OpenWidth = 1000
    OpenHeight = 600
    Creator = 'QRXLSXDocumentFilter'
    WorkSheetname = 'Sheet 1'
    Left = 584
    Top = 616
  end
  object QRCompositeReport1: TQRCompositeReport
    Options = []
    PrinterSettings.Copies = 1
    PrinterSettings.OutputBin = Auto
    PrinterSettings.Duplex = False
    PrinterSettings.FirstPage = 0
    PrinterSettings.LastPage = 0
    PrinterSettings.UseStandardprinter = False
    PrinterSettings.UseCustomBinCode = False
    PrinterSettings.CustomBinCode = 0
    PrinterSettings.ExtendedDuplex = 0
    PrinterSettings.UseCustomPaperCode = False
    PrinterSettings.CustomPaperCode = 0
    PrinterSettings.PrintMetaFile = False
    PrinterSettings.MemoryLimit = 1000000
    PrinterSettings.Collate = 0
    PrinterSettings.ColorOption = 2
    PrinterSettings.Orientation = poPortrait
    PrinterSettings.PaperSize = Letter
    PageCount = 0
    Left = 592
    Top = 624
  end
end
