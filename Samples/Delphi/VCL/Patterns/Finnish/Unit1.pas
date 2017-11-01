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
  NtBase, NtPattern, NtLanguageDlg;

procedure TForm1.UpdateValues;
resourcestring
  // Contains two patterns: male and female.
  SMessageGender = 'neutral;%s tuo polkupyöränsä';  //loc 0: Name of the person
  // Note! Name the gender enabled resource strings as XXXGender. This will automatically turn on plural/gender parsing in NewTool's editor.

  procedure Process(gender: TGender; const name: String; messageLabel, infoLabel: TLabel);
  var
    actualGender: TGender;
  begin
    // Update message label
    messageLabel.Caption := TMultiPattern.Format(SMessageGender, gender, [name]);

    // Update info label
    actualGender := TMultiPattern.GetGender(SMessageGender, gender);

    if actualGender <> gender then
      infoLabel.Font.Style := [fsBold]
    else
      infoLabel.Font.Style := [];

    infoLabel.Caption := TMultiPattern.GetGenderName(actualGender);
  end;

resourcestring
  SJoku = 'Joku';
begin
  Process(geMale, 'Jussi', MaleLabel, MaleInfo);
  Process(geFemale, 'Jaana', FemaleLabel, FemaleInfo);
  Process(geNeutral, SJoku, NeutralLabel, NeutralInfo);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  UpdateValues;
end;

procedure TForm1.LanguageButtonClick(Sender: TObject);
begin
  if TNtLanguageDialog.Select('') then
    UpdateValues;
end;

initialization
  DefaultLocale := 'fi';
end.
