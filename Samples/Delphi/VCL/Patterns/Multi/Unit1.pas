unit Unit1;

{$DEFINE ZERO}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Label0_0: TLabel;
    Label0_1: TLabel;
    Label1_1: TLabel;
    Label0_2: TLabel;
    Label1_2: TLabel;
    Label2_2: TLabel;
    Label1_3: TLabel;
    Label2_3: TLabel;
    Label1_10: TLabel;
    Label5_10: TLabel;
    ZeroCheckBox: TCheckBox;
    StartCheckBox: TCheckBox;
    LanguageButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure CheckBoxClick(Sender: TObject);
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
  // Contains two parts:
  // - Part 1 handles completed steps.
  // - Part 2 handles the total amount of steps.
  SMessagePlural = '{plural, one {I have completed one step} other {I have completed %d steps}} {plural one {out of one step} other {out of %d steps}}';
  // Same as above but with top level pattern. This makes pattern shorter and help eliminating
  // repeating the same text on multiple patterns. Contains three parts:
  // - Top level pattern that has two placeholders for plural parts.
  // - Part 1 handles completed steps.
  // - Part 2 handles the total amount of steps.
  SStartMessagePlural = 'I have completed {plural, one {one step} other {%d steps}} {plural, one {out of one step} other {out of %d steps}}';
  // Same as above but with special zero case.
  SZeroMessagePlural = 'I have completed {plural, zero {no steps} one {one step} other {%d steps}} {plural, zero {out of none steps} one {out of one step} other {out of %d steps}}';

  function Process(completed, total: Integer): String;
  var
    pattern: String;
  begin
    if ZeroCheckBox.Checked then
      pattern := SZeroMessagePlural
    else if StartCheckBox.Checked then
      pattern := SStartMessagePlural
    else
      pattern := SMessagePlural;

    Result := TMultiPattern.Format(pattern, [completed, total]);
  end;

begin
  Label0_0.Caption := Process(0, 0);
  Label0_1.Caption := Process(0, 1);
  Label1_1.Caption := Process(1, 1);
  Label0_2.Caption := Process(0, 2);
  Label1_2.Caption := Process(1, 2);
  Label2_2.Caption := Process(2, 2);
  Label1_3.Caption := Process(1, 3);
  Label2_3.Caption := Process(2, 3);
  Label1_10.Caption := Process(1, 10);
  Label5_10.Caption := Process(5, 10);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  UpdateValues;
end;

procedure TForm1.CheckBoxClick(Sender: TObject);
begin
  UpdateValues;
end;

procedure TForm1.LanguageButtonClick(Sender: TObject);
begin
  if TNtLanguageDialog.Select then
    UpdateValues;
end;

// If your original language is not English remove comments and set the default language here.
{
initialization
  DefaultLocale := 'de';
}
end.
