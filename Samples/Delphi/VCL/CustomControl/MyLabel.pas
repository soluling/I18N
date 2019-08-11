unit MyLabel;

interface

uses
  Classes,
  StdCtrls;

type
  TMyLabel = class(TLabel)
  private
    FDescription: String;
    FExcludeMe: String;

  published
    property Description: String read FDescription write FDescription;
    property ExcludeMe: String read FExcludeMe write FExcludeMe;
  end;

  procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('My label', [TMyLabel]);
end;

end.
