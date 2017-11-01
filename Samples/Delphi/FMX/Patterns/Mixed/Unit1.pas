unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation;

type
  TForm1 = class(TForm)
    NoPluralLabel: TLabel;
    NoPluralLabel0: TLabel;
    NoPluralLabel1: TLabel;
    NoPluralLabel2: TLabel;
    HomeLabel: TLabel;
    HomeLabel0: TLabel;
    HomeLabel1: TLabel;
    HomeLabel2: TLabel;
    PluralLabel: TLabel;
    PluralLabel0: TLabel;
    PluralLabel1: TLabel;
    PluralLabel2: TLabel;
    MultiPluralLabel: TLabel;
    MultiPluralLabel1: TLabel;
    MultiPluralLabel2: TLabel;
    MultiPluralLabel3: TLabel;
    MultiPluralLabel4: TLabel;
    LanguageButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure LanguageButtonClick(Sender: TObject);

  private
    procedure UpdateStrings;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  NtBase,
  NtPattern,
  NtResource,
  FMX.NtLanguageDlg,
  FMX.NtTranslator;

procedure TForm1.UpdateStrings;

  procedure ProcessNoPlural(count: Integer; lab: TLabel);
  begin
    // On most languages this does not work except when count is 1
    // Do not use code like this!
    lab.Text := Format(_T('%d file'), [count]);

    if TMultiPattern.IsSingleFormLanguage or (count = 1) or ((count = 0) and TMultiPattern.IsZeroLikeOne) then
      lab.TextSettings.FontColor := TAlphaColorRec.DarkGreen
    else
      lab.TextSettings.FontColor := TAlphaColorRec.Crimson;
  end;

  procedure ProcessHomeBrewed(count: Integer; lab: TLabel);
  begin
    // This works on some Western languages (those that use similar plural rules as English)
    // but would fail for example in French.
    // Do not use code like this!
    if count = 1 then
      lab.Text := Format(_T('%d file'), [count])
    else
      lab.Text := Format(_T('%d files'), [count]);

    if not TMultiPattern.IsSingleFormLanguage and (count = 0) and TMultiPattern.IsZeroLikeOne then
      lab.TextSettings.FontColor := TAlphaColorRec.Crimson
    else
      lab.TextSettings.FontColor := TAlphaColorRec.DarkGreen
  end;

  // The following two samples handle plural forms correctly. Use this kind of code in your applications.
  procedure ProcessPluralAware(count: Integer; lab: TLabel);
  begin
    // This works on every count and language (expecting SFiles translated correctly with right patterns)
    // This message contains two patterns: one and other.
    // Localized strings will contain the patterns used by the target languages:
    // - German is like English: one and other
    // - Polish: one, paucal, other
    // - Japanese: other
    lab.Text := TMultiPattern.Format(_T('one;%d file;other;%d files', 'FilesPlural'), count, [count]);
    // Note! Name the resource string as XXXPlural. This will automatically turn on plural parsing in the localization tool.
    lab.TextSettings.FontColor := TAlphaColorRec.DarkGreen;
  end;

  procedure ProcessMultiPlural(completed, total: Integer; lab: TLabel);
  begin
    // This message contains two pluralized parameters. Contains three parts and seven patterns:
    // - Pattern 0 is the top level pattern without pluralized parameters.
    // - Patterns 1-3 are zero, one and other for completed parameter.
    //   These contain additional second placeholder (%s or %1:s) for the rest of the message
    // - Patterns 4-6 are zero, one and other for total parameter.
    //
    // On runtime part 3 is firstly created by choosing the right pattern and injecting total there.
    // Then part 2 is created by choosing the right pattern and injecting completed and part 3 there.
    // Finally part 1 is created by injecting part 2 there resulting the final string.
    lab.Text := TMultiPattern.Format(_T('I have completed %s;zero;none %1:s;one;one %1:s;other;%d %s;next;zero;out of none steps;one;out of one step;other;out of %d steps', 'MessagePlural'), [completed, total]);
    // Note! Name the resource string as XXXPlural. This will automatically turn on plural parsing.
    lab.TextSettings.FontColor := TAlphaColorRec.DarkGreen;
  end;

begin
  ProcessNoPlural(0, NoPluralLabel0);
  ProcessNoPlural(1, NoPluralLabel1);
  ProcessNoPlural(2, NoPluralLabel2);

  ProcessHomeBrewed(0, HomeLabel0);
  ProcessHomeBrewed(1, HomeLabel1);
  ProcessHomeBrewed(2, HomeLabel2);

  ProcessPluralAware(0, PluralLabel0);
  ProcessPluralAware(1, PluralLabel1);
  ProcessPluralAware(2, PluralLabel2);

  ProcessMultiPlural(0, 1, MultiPluralLabel1);
  ProcessMultiPlural(1, 1, MultiPluralLabel2);
  ProcessMultiPlural(1, 3, MultiPluralLabel3);
  ProcessMultiPlural(2, 3, MultiPluralLabel4);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  NtResources._T('English', 'en');
  NtResources._T('Finnish', 'fi');
  NtResources._T('German', 'de');
  NtResources._T('French', 'fr');
  NtResources._T('Japanese', 'ja');

  _T(Self);
  UpdateStrings;
end;

procedure TForm1.LanguageButtonClick(Sender: TObject);
begin
  if TNtLanguageDialog.Select then
    UpdateStrings;
end;

end.
