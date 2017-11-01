unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, NtBase;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    LanguageGroup: TRadioGroup;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure LanguageGroupClick(Sender: TObject);

  private
    FLanguages: TNtLanguages;
    FUpdating: Boolean;

    procedure UpdateLanguages;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  NtLocalization,
  NtTranslator;

procedure TForm1.UpdateLanguages;
resourcestring
  SStr = 'これはサンプルです。';
var
  i: Integer;
begin
  Label2.Caption := SStr;
  FUpdating := True;

  for i := 0 to FLanguages.Count - 1 do
  begin
    if FLanguages[i].Code = TNtBase.GetActiveLocale then
    begin
      LanguageGroup.ItemIndex := i;
      Break;
    end;
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
    LanguageGroup.Items.AddObject(FLanguages[i].Names[lnNative], TObject(i));

  UpdateLanguages;
end;

procedure TForm1.LanguageGroupClick(Sender: TObject);
begin
  if FUpdating then
    Exit;

  // User clicked a language button. Turn that language on.
  TNtTranslator.SetNew(FLanguages[LanguageGroup.ItemIndex].Code, [roSaveLocale]);
  UpdateLanguages;
end;

initialization
  DefaultLocale := 'ja';
end.
