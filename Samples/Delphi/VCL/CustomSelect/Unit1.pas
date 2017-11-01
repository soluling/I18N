unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, NtBase;

type
  TForm1 = class(TForm)
    LanguageGroup: TRadioGroup;
    NameGroup: TRadioGroup;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure LanguageGroupClick(Sender: TObject);
    procedure NameGroupClick(Sender: TObject);

  private
    FUpdating: Boolean;
    FLanguages: TNtLanguages;

    procedure UpdateLanguages;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  NtLocalization, NtTranslator;

procedure TForm1.UpdateLanguages;
var
  i: Integer;
begin
  // Update the language names and the language selection.
  FUpdating := True;

  for i := 0 to FLanguages.Count - 1 do
  begin
    LanguageGroup.Items[i] := FLanguages[i].Names[TNtLanguageName(NameGroup.ItemIndex)];

    if FLanguages[i].Code = TNtBase.GetActiveLocale then
      LanguageGroup.ItemIndex := i;
  end;

  if LanguageGroup.ItemIndex = -1 then
    LanguageGroup.ItemIndex := 0;

  FUpdating := False;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  // Get available languages
  FLanguages := TNtLanguages.Create;
  FLanguages.AddDefault;
  TNtBase.GetAvailable(FLanguages);

  // Initialize components
  LanguageGroup.Columns := FLanguages.Count;

  for i := 0 to FLanguages.Count - 1 do
    LanguageGroup.Items.AddObject('', TObject(i));

  NameGroup.ItemIndex := 0;
end;

procedure TForm1.LanguageGroupClick(Sender: TObject);
begin
  if FUpdating then
    Exit;

  // User clicked a language button. Turn that language on.
  TNtTranslator.SetNew(FLanguages[LanguageGroup.ItemIndex].Code);
  UpdateLanguages;
end;

procedure TForm1.NameGroupClick(Sender: TObject);
begin
  // Naming mode has been changed.
  // Update the list to contain correct language names.
  UpdateLanguages;
end;

initialization
  DefaultLocale := 'en';
end.
