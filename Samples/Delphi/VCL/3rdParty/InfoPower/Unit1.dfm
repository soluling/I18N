object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Sample'
  ClientHeight = 596
  ClientWidth = 1332
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object wwDBGrid1: TwwDBGrid
    Left = 8
    Top = 8
    Width = 1313
    Height = 521
    Selected.Strings = (
      'Id'#9'10'#9'ID'#9'F'
      'Name'#9'30'#9'Name'#9#9
      'FieldPlayers'#9'10'#9'Field Players'#9'F'
      'Goalie'#9'5'#9'Goalie'#9#9
      'Origin'#9'30'#9'Origin'#9#9
      'Description'#9'200'#9'Description'#9#9)
    IniAttributes.Delimiter = ';;'
    IniAttributes.UnicodeIniFile = False
    TitleColor = clBtnFace
    FixedCols = 0
    ShowHorzScrollBar = True
    DataSource = DataSource1
    TabOrder = 0
    TitleAlignment = taLeftJustify
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -12
    TitleFont.Name = 'Segoe UI'
    TitleFont.Style = []
    TitleLines = 1
    TitleButtons = False
  end
  object DataSource1: TDataSource
    DataSet = ADOTable1
    Left = 24
    Top = 536
  end
  object ADOConnection1: TADOConnection
    Connected = True
    ConnectionString = 
      'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=D:\NT\Deploy\Sample' +
      's\Delphi\VCL\3rdParty\InfoPower\Sport.mdb;Persist Security Info=' +
      'False'
    LoginPrompt = False
    Mode = cmShareDenyNone
    Provider = 'Microsoft.Jet.OLEDB.4.0'
    Left = 128
    Top = 536
  end
  object ADOTable1: TADOTable
    Active = True
    Connection = ADOConnection1
    CursorType = ctStatic
    TableName = 'Sport'
    Left = 232
    Top = 536
  end
end
