unit SportFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.Buttons, Vcl.StdCtrls, Sport;

type
  TSportForm = class(TForm)
    NameLabel: TLabel;
    DescriptionLabel: TLabel;
    OriginLabel: TLabel;
    PlayersLabel: TLabel;
    GoalieLabel: TLabel;
    FirstButton: TSpeedButton;
    PreviousButton: TSpeedButton;
    NextButton: TSpeedButton;
    LastButton: TSpeedButton;
    LanguageButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FirstButtonClick(Sender: TObject);
    procedure PreviousButtonClick(Sender: TObject);
    procedure NextButtonClick(Sender: TObject);
    procedure LastButtonClick(Sender: TObject);
    procedure LanguageButtonClick(Sender: TObject);

  private
    FItemIndex: Integer;

    procedure SetItemIndex(value: Integer);

  protected
    FSports: TSports;

    procedure LoadSports; virtual; abstract;

  public
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
  end;

implementation

{$R *.dfm}

uses
  NtLanguageDlg;

procedure TSportForm.SetItemIndex(value: Integer);
resourcestring
  SOrigin = 'Origin: %s';
  SPlayers = '%d players';
  SHasGoalie = 'Has a goalie';
  SNoGoalie = 'No goalie';
var
  selected: TSport;
begin
  // Get the selected sport
  FItemIndex := value;
  selected := FSports[FItemIndex];

  // Update labels
  NameLabel.Caption := selected.Name;
  DescriptionLabel.Caption := selected.Description;
  OriginLabel.Caption := Format(SOrigin, [selected.Origin]);
  PlayersLabel.Caption := Format(SPlayers, [selected.Players]);

  if selected.Goalie then
    GoalieLabel.Caption := SHasGoalie
  else
    GoalieLabel.Caption := SNoGoalie;

  // Show only those buttons that can be clicked
  FirstButton.Visible := FItemIndex > 0;
  PreviousButton.Visible := FirstButton.Visible;
  NextButton.Visible := FItemIndex < FSports.Count - 1;
  LastButton.Visible := NextButton.Visible;
end;

procedure TSportForm.FormCreate(Sender: TObject);
begin
  // Load spoart data from the resources
  FSports := TSports.Create;
  LoadSports;

  // Show first sport
  FirstButtonClick(Self);
end;

procedure TSportForm.FirstButtonClick(Sender: TObject);
begin
  // Show first sport
  ItemIndex := 0;
end;

procedure TSportForm.PreviousButtonClick(Sender: TObject);
begin
  // Show previous sport
  ItemIndex := ItemIndex - 1;
end;

procedure TSportForm.NextButtonClick(Sender: TObject);
begin
  // Show next sport
  ItemIndex := ItemIndex + 1;
end;

procedure TSportForm.LastButtonClick(Sender: TObject);
begin
  // Show last sport
  ItemIndex := FSports.Count - 1;
end;

procedure TSportForm.LanguageButtonClick(Sender: TObject);
begin
  // Show a language select dialog and turn on the selected language
  if TNtLanguageDialog.Select('en') then
  begin
    // Language has been changed.
    // Sport data must be reloaded.
    LoadSports;
    SetItemIndex(ItemIndex);
  end;
end;

end.
