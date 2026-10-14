unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    MaleLabel: TLabel;
    FemaleLabel: TLabel;
    NeutralLabel: TLabel;
    MaleInfo: TLabel;
    FemaleInfo: TLabel;
    NeutralInfo: TLabel;
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
resourcestring
  // Contains two patterns: male and female.
  SMessageGender = '{gender, male {%s will bring his bicycle} female {%s will bring her bicycle}}';  //loc %s: Name of a person
  // Contains three patterns: neutral, male and female.
  SOtherMessageGender = '{gender, male {%s will bring his bicycle} female {%s will bring her bicycle} other {%s will bring a bicycle}}';  //loc %s: Name of a person

  procedure Process(msg: String; gender: TGender; const name: String; messageLabel, infoLabel: TLabel);
  var
    actualGender: TGender;
  begin
    // Update message label
    messageLabel.Caption := TMultiPattern.Format(msg, gender, [name]);

    // Update info label
    actualGender := TMultiPattern.GetGender(msg, gender);

    if actualGender <> gender then
      infoLabel.Font.Style := [fsBold]
    else
      infoLabel.Font.Style := [];

    infoLabel.Caption := TMultiPattern.GetGenderName(actualGender);
  end;

begin
  // Both patterns use the same labels on purpose. The second set of calls overwrites
  // the results of the first one, so the form shows SOtherMessageGender. To see how
  // SMessageGender, which has no other form, handles the neutral gender, comment
  // out the last three calls.
  Process(SMessageGender, geMale, 'John', MaleLabel, MaleInfo);
  Process(SMessageGender, geFemale, 'Jill', FemaleLabel, FemaleInfo);
  Process(SMessageGender, geNeutral, 'Amazon', NeutralLabel, NeutralInfo);

  Process(SOtherMessageGender, geMale, 'John', MaleLabel, MaleInfo);
  Process(SOtherMessageGender, geFemale, 'Jill', FemaleLabel, FemaleInfo);
  Process(SOtherMessageGender, geNeutral, 'Amazon', NeutralLabel, NeutralInfo);
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
