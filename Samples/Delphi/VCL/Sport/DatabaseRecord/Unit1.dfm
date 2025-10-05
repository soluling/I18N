inherited Form1: TForm1
  StyleElements = [seFont, seClient, seBorder]
  TextHeight = 13
  inherited NameLabel: TLabel
    StyleElements = [seFont, seClient, seBorder]
  end
  inherited DescriptionLabel: TLabel
    StyleElements = [seFont, seClient, seBorder]
  end
  inherited OriginLabel: TLabel
    StyleElements = [seFont, seClient, seBorder]
  end
  inherited PlayersLabel: TLabel
    StyleElements = [seFont, seClient, seBorder]
  end
  inherited GoalieLabel: TLabel
    StyleElements = [seFont, seClient, seBorder]
  end
  object Connection1: TADOConnection
    ConnectionString = 
      'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=..\..\..\Sport.mdb;' +
      'Persist Security Info=False'
    LoginPrompt = False
    Provider = 'Microsoft.Jet.OLEDB.4.0'
    Left = 152
    Top = 80
  end
  object Query1: TADOQuery
    Connection = Connection1
    Parameters = <>
    Left = 216
    Top = 80
  end
end
