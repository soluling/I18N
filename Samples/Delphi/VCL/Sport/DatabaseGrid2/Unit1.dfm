object MainForm: TMainForm
  Left = 205
  Top = 126
  Caption = 'Data Sample'
  ClientHeight = 102
  ClientWidth = 729
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu
  OnShow = FormShow
  TextHeight = 13
  object DBGrid1: TDBGrid
    Left = 0
    Top = 0
    Width = 729
    Height = 102
    Align = alClient
    DataSource = DataSource1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'Id'
        Title.Caption = 'ID'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Lang'
        Title.Caption = 'Language'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Name'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'FieldPlayers'
        Title.Caption = 'Field players'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Goalie'
        Title.Caption = 'Goalkeeper'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Origin'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Description'
        Visible = True
      end>
  end
  object MainMenu: TMainMenu
    Left = 16
    Top = 48
    object DatabaseMenu: TMenuItem
      Caption = 'Database'
      OnClick = DatabaseMenuClick
      object OpenMenu: TMenuItem
        Caption = 'Open'
        OnClick = OpenMenuClick
      end
      object CloseMenu: TMenuItem
        Caption = 'Close'
        OnClick = CloseMenuClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object LanguageMenu: TMenuItem
        Caption = 'Language...'
        OnClick = LanguageMenuClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object ExitMenu: TMenuItem
        Caption = 'Exit'
        OnClick = ExitMenuClick
      end
    end
    object HelpMenu: TMenuItem
      Caption = 'Help'
      object AboutMenu: TMenuItem
        Caption = 'About'
        OnClick = AboutMenuClick
      end
    end
  end
  object DataSource1: TDataSource
    DataSet = Query1
    Left = 216
    Top = 48
  end
  object Query1: TADOQuery
    Active = True
    Connection = Connection1
    CursorType = ctStatic
    Parameters = <>
    SQL.Strings = (
      'SELECT * FROM Sport')
    Left = 152
    Top = 48
  end
  object Connection1: TADOConnection
    Connected = True
    ConnectionString = 
      'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=D:\NT\Deploy\Sample' +
      's\Delphi\VCL\Sport\Sport.mdb;Persist Security Info=False'
    LoginPrompt = False
    Mode = cmShareDenyNone
    Provider = 'Microsoft.Jet.OLEDB.4.0'
    Left = 88
    Top = 48
  end
end
