inherited Form1: TForm1
  PixelsPerInch = 96
  TextHeight = 13
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
