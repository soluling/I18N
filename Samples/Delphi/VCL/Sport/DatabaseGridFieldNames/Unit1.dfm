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
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
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
    Parameters = <
      item
        Name = 'Sample'
        DataType = ftString
        Size = -1
        Value = Null
      end
      item
        Name = 'Sample2'
        DataType = ftFloat
        Value = Null
      end>
    SQL.Strings = (
      'SELECT * FROM Sport')
    Left = 152
    Top = 48
    object QueryId: TIntegerField
      DisplayLabel = 'Id2'
      FieldName = 'Id'
    end
    object QueryName: TWideStringField
      DisplayLabel = 'Name2'
      FieldName = 'Name'
      Size = 30
    end
    object Query1FieldPlayers: TIntegerField
      DisplayLabel = 'Field players'
      FieldName = 'FieldPlayers'
    end
    object Query1Goalie: TBooleanField
      DisplayLabel = 'Goalkeeper'
      FieldName = 'Goalie'
    end
    object Query1Origin: TWideStringField
      DisplayLabel = 'Origin2'
      FieldName = 'Origin'
      Size = 30
    end
    object Query1Description: TWideStringField
      DisplayLabel = 'Description2'
      FieldName = 'Description'
      Size = 200
    end
  end
  object Connection1: TADOConnection
    Connected = True
    ConnectionString = 
      'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=D:\NT\Deploy\Sample' +
      's\Delphi\VCL\Sport\DatabaseGridFieldNames\Sport.mdb;Persist Secu' +
      'rity Info=False;'
    LoginPrompt = False
    Mode = cmShareDenyNone
    Provider = 'Microsoft.Jet.OLEDB.4.0'
    Left = 88
    Top = 48
  end
end
