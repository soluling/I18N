unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    SoccerLabel: TLabel;
    HockeyLabel: TLabel;
    BasketballLabel: TLabel;
    LanguageButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure LanguageButtonClick(Sender: TObject);

  private
    procedure UpdateValues;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  NtPattern, NtLanguageDlg;

procedure TForm1.UpdateValues;

  procedure Process(const select: String; const name: String; messageLabel: TLabel);
  resourcestring
    // Contains three patterns: soccer, hockey and basketball.
    SSportSelect = '{select, soccer {%s is the best soccer player.} hockey {%s is the best ice hockey player.} basketball {%s is the best basketball player.}}'; //loc 0: Name of the player
  begin
    messageLabel.Caption := TMultiPattern.Format(SSportSelect, select, [name]);
  end;

begin
  Process('soccer', 'Pelé', SoccerLabel);
  Process('hockey', 'Wayne Gretzky', HockeyLabel);
  Process('basketball', 'Michael Jordan', BasketBallLabel);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  UpdateValues;
end;

procedure TForm1.LanguageButtonClick(Sender: TObject);
begin
  if TNtLanguageDialog.Select then
    UpdateValues;
end;

end.
