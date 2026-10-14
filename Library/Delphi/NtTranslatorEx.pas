unit NtTranslatorEx;

interface

uses
  Forms;

{ Translates the form.
  @param form Form to be translated. }
procedure _T(form: TCustomForm); overload;

implementation

uses
  NtTranslator;

procedure _T(form: TCustomForm);
var
  translator: TNtTranslator;
begin
  if form = nil then
    Exit;

  translator := TNtTranslator.Create;
  try
    translator.TranslateForm(form);
  finally
    translator.Free;
  end;
end;

end.
