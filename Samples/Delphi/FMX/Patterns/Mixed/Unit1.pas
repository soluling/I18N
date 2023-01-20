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
  NtResourceString,  // Turns on resource string translation
  FMX.NtLanguageDlg,
  FMX.NtTranslator;

procedure TForm1.UpdateStrings;

  procedure ProcessNoPlural(count: Integer; lab: TLabel);
  resourcestring
    SFile = '%d file';
  begin
    // On most languages this does not work except when count is 1
    // Do not use code like this!
    lab.Text := Format(SFile, [count]);

    if TMultiPattern.IsSingleFormLanguage or (count = 1) or ((count = 0) and TMultiPattern.IsZeroLikeOne) then
      lab.TextSettings.FontColor := TAlphaColorRec.DarkGreen
    else
      lab.TextSettings.FontColor := TAlphaColorRec.Crimson;
  end;

  procedure ProcessHomeBrewed(count: Integer; lab: TLabel);
  resourcestring
    SFile1 = '%d file';
    SFile2 = '%d files';
  begin
    // This works on some Western languages (those that use similar plural rules as English)
    // but would fail for example in French.
    // Do not use code like this!
    if count = 1 then
      lab.Text := Format(SFile1, [count])
    else
      lab.Text := Format(SFile2, [count]);

    if not TMultiPattern.IsSingleFormLanguage and (count = 0) and TMultiPattern.IsZeroLikeOne then
      lab.TextSettings.FontColor := TAlphaColorRec.Crimson
    else
      lab.TextSettings.FontColor := TAlphaColorRec.DarkGreen
  end;

  // The following two samples handle plural forms correctly. Use this kind of code in your applications.
  procedure ProcessPluralAware(count: Integer; lab: TLabel);
  resourcestring
    // This works on every count and language (expecting SFiles translated correctly with right patterns)
    // This message contains two patterns: one and other.
    // Localized strings will contain the patterns used by the target languages:
    // - German is like English: one and other
    // - Polish: one, paucal, other
    // - Japanese: other
    SFilesPlural = '{plural, one {%d file} other {%d files}}';  //loc 0: file count
  begin
    lab.Text := TMultiPattern.Format(SFilesPlural, count, [count]);
    lab.TextSettings.FontColor := TAlphaColorRec.DarkGreen;
  end;

  procedure ProcessMultiPlural(completed, total: Integer; lab: TLabel);
  resourcestring
    // This message contains two pluralized parameters. Contains three parts and seven patterns:
    // - The pattern start with a static text.
    // - Then it follows the first ICU part that has zero, one and other for completed parameter.
    // - Finally there is another ICU part for total steps.
    SMessagePlural = 'I have completed {plural, zero {none} one {one} other {%d}} {plural, zero {out of none steps} one {out of one step} other {out of %d steps}}';
  begin
    lab.Text := TMultiPattern.Format(SMessagePlural, [completed, total]);
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
resourcestring
  SEnglish = 'English';
  SFinnish = 'Finnish';
  SGerman = 'German';
  SFrench = 'French';
  SJapanese = 'Japanese';
begin
  NtResources.Add('English', 'English', SEnglish, 'en');
  NtResources.Add('Finnish', 'suomi', SFinnish, 'fi');
  NtResources.Add('German', 'Deutsch', SGerman, 'de');
  NtResources.Add('French', 'français', SFrench, 'fr');
  NtResources.Add('Japanese', '日本語', SJapanese, 'ja');

  _T(Self);
  UpdateStrings;
end;

procedure TForm1.LanguageButtonClick(Sender: TObject);
begin
  if TNtLanguageDialog.Select then
    UpdateStrings;
end;

end.
