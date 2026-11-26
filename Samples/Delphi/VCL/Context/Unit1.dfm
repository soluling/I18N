object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Context Sample'
  ClientHeight = 452
  ClientWidth = 640
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Menu = MainMenu1
  Position = poScreenCenter
  OnCreate = FormCreate
  TextHeight = 15
  object ThemeLabel: TLabel
    Left = 8
    Top = 420
    Width = 3
    Height = 15
  end
  object TicketGroupBox: TGroupBox
    Left = 8
    Top = 8
    Width = 305
    Height = 241
    Caption = 'Ticket'
    TabOrder = 0
    object StatusCaptionLabel: TLabel
      Left = 12
      Top = 24
      Width = 35
      Height = 15
      Caption = 'Status:'
    end
    object StatusLabel: TLabel
      Left = 80
      Top = 24
      Width = 3
      Height = 15
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object StatusTextLabel: TLabel
      Left = 12
      Top = 44
      Width = 3
      Height = 15
    end
    object ReplyLabel: TLabel
      Left = 12
      Top = 104
      Width = 32
      Height = 15
      Caption = '&Reply:'
      FocusControl = ReplyEdit
    end
    object PostsLabel: TLabel
      Left = 12
      Top = 152
      Width = 3
      Height = 15
    end
    object AssignedLabel: TLabel
      Left = 12
      Top = 172
      Width = 3
      Height = 15
    end
    object OpenCloseButton: TButton
      Left = 12
      Top = 68
      Width = 90
      Height = 25
      TabOrder = 0
      OnClick = OpenCloseButtonClick
    end
    object PendingButton: TButton
      Left = 108
      Top = 68
      Width = 90
      Height = 25
      Caption = 'Pe&nding'
      TabOrder = 1
      OnClick = PendingButtonClick
    end
    object ReplyEdit: TEdit
      Left = 12
      Top = 122
      Width = 186
      Height = 23
      TabOrder = 2
    end
    object PostButton: TButton
      Left = 204
      Top = 121
      Width = 90
      Height = 25
      Caption = '&Post'
      TabOrder = 3
      OnClick = PostButtonClick
    end
    object DeleteButton: TButton
      Left = 12
      Top = 200
      Width = 90
      Height = 25
      Caption = '&Delete...'
      TabOrder = 4
      OnClick = DeleteButtonClick
    end
  end
  object QueueGroupBox: TGroupBox
    Left = 8
    Top = 256
    Width = 305
    Height = 153
    Caption = 'Queue'
    TabOrder = 1
    object CountCaptionLabel: TLabel
      Left = 12
      Top = 28
      Width = 71
      Height = 15
      Caption = 'Open &tickets:'
      FocusControl = CountEdit
    end
    object QueueCaptionLabel: TLabel
      Left = 12
      Top = 60
      Width = 45
      Height = 15
      Caption = '&Queues:'
      FocusControl = QueueEdit
    end
    object OpenTicketsLabel: TLabel
      Left = 12
      Top = 96
      Width = 3
      Height = 15
    end
    object SummaryLabel: TLabel
      Left = 12
      Top = 120
      Width = 3
      Height = 15
    end
    object CountEdit: TEdit
      Left = 120
      Top = 24
      Width = 50
      Height = 23
      TabOrder = 0
      Text = '3'
      OnChange = CountEditChange
    end
    object CountUpDown: TUpDown
      Left = 170
      Top = 24
      Width = 16
      Height = 23
      Associate = CountEdit
      Max = 1000
      Position = 3
      TabOrder = 1
    end
    object QueueEdit: TEdit
      Left = 120
      Top = 56
      Width = 50
      Height = 23
      TabOrder = 2
      Text = '1'
      OnChange = CountEditChange
    end
    object QueueUpDown: TUpDown
      Left = 170
      Top = 56
      Width = 16
      Height = 23
      Associate = QueueEdit
      Min = 1
      Max = 100
      Position = 1
      TabOrder = 3
    end
  end
  object ShopGroupBox: TGroupBox
    Left = 324
    Top = 8
    Width = 305
    Height = 241
    Caption = 'Shop'
    TabOrder = 2
    object PriceCaptionLabel: TLabel
      Left = 12
      Top = 24
      Width = 29
      Height = 15
      Caption = 'Price:'
    end
    object PriceLabel: TLabel
      Left = 120
      Top = 24
      Width = 3
      Height = 15
    end
    object DistanceCaptionLabel: TLabel
      Left = 12
      Top = 44
      Width = 48
      Height = 15
      Caption = 'Distance:'
    end
    object DistanceLabel: TLabel
      Left = 120
      Top = 44
      Width = 3
      Height = 15
    end
    object DistanceNoCommentLabel: TLabel
      Left = 200
      Top = 44
      Width = 3
      Height = 15
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGrayText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
    end
    object SortLabel: TLabel
      Left = 12
      Top = 108
      Width = 33
      Height = 15
      Caption = 'O&rder:'
      FocusControl = SortComboBox
    end
    object DiskLabel: TLabel
      Left = 12
      Top = 148
      Width = 3
      Height = 15
    end
    object BookButton: TButton
      Left = 12
      Top = 68
      Width = 90
      Height = 25
      Caption = '&Book'
      TabOrder = 0
      OnClick = BookButtonClick
    end
    object OrderButton: TButton
      Left = 108
      Top = 68
      Width = 90
      Height = 25
      Caption = '&Order'
      TabOrder = 1
      OnClick = OrderButtonClick
    end
    object SortComboBox: TComboBox
      Left = 120
      Top = 104
      Width = 145
      Height = 23
      Style = csDropDownList
      TabOrder = 2
      Items.Strings = (
        'Ascending'
        'Descending'
        'Newest first'
        'Oldest first')
    end
  end
  object LanguageButton: TButton
    Left = 539
    Top = 415
    Width = 90
    Height = 25
    Caption = '&Language...'
    TabOrder = 3
    OnClick = LanguageButtonClick
  end
  object MainMenu1: TMainMenu
    Left = 456
    Top = 296
    object FileMenu: TMenuItem
      Caption = '&File'
      object OpenMenu: TMenuItem
        Caption = '&Open...'
        ShortCut = 16463
      end
      object SaveMenu: TMenuItem
        Caption = '&Save'
        ShortCut = 16467
      end
      object CloseMenu: TMenuItem
        Caption = '&Close'
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object ExitMenu: TMenuItem
        Caption = 'E&xit'
        OnClick = ExitMenuClick
      end
    end
    object EditMenu: TMenuItem
      Caption = '&Edit'
      object ClearMenu: TMenuItem
        Caption = 'C&lear'
      end
      object FindMenu: TMenuItem
        Caption = '&Find...'
        ShortCut = 16454
      end
    end
    object ViewMenu: TMenuItem
      Caption = '&View'
      object ThemeMenu: TMenuItem
        Caption = '&Theme'
        object LightMenu: TMenuItem
          Caption = '&Light'
          RadioItem = True
          OnClick = ThemeMenuClick
        end
        object DarkMenu: TMenuItem
          Caption = '&Dark'
          RadioItem = True
          OnClick = ThemeMenuClick
        end
      end
    end
  end
end
