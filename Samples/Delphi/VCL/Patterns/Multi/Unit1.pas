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
  // Contains four patterns:
  // - Patterns 0-1 are one and other for completed parameter.
  //   These contain additional placeholder (%s or %1:s) for the rest of the message
  // - Patterns 2-3 are one and other for total parameter.
  SMessagePlural = 'one;I have completed one step %1:s;other;I have completed %d steps %s;next;one;out of one step;other;out of %d steps';
  // Same as above but with start pattern. This makes pattern shorter and help eliminating
  // repeating the same text on multiple patterns. Contains five patterns:
  // - Pattern 0 is the start pattern without pluralized parameters.
  // - Patterns 1-2 are one and other for completed parameter.
  //   These contain additional placeholder (%s or %1:s) for the rest of the message
  // - Patterns 3-4 are one and other for total parameter.
  SStartMessagePlural = 'I have completed %s;one;one step %1:s;other;%d steps %s;next;one;out of one step;other;out of %d steps';
  // Same as above but with special zero case. Contains seven patterns:
  // - Pattern 0 is the start pattern without pluralized parameters.
  // - Patterns 1-3 are zero, one and other for completed parameter.
  //   These contain additional placeholder (%s or %1:s) for the rest of the message
  // - Patterns 4-6 are zero, one and other for total parameter.
  SZeroMessagePlural = 'I have completed %s;zero;no steps %1:s;one;one step %1:s;other;%d steps %s;next;zero;out of none steps;one;out of one step;other;out of %d steps';
  // Note! Name the resource string as XXXPlural. This will automatically turn on plural parsing in the localization tool.

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
