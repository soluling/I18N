{
  @abstract Implements @link(TNtTranslator) class that perform runtime language switch.

  Normally you don't use this unit. Instead you use @link(NtLanguageDlg.TNtLanguageDialog)
  to perform runtime language switch. If you have your own user interface to
  select a new language then you use @link(TNtTranslator.SetNew). You can use
  lower lever routines to select the language and to @link(TNtBase.LoadNew load)
  a new resource DLL, and to @link(TNtTranslator.TranslateForms translate) all
  forms into the new language.
}

unit NtTranslator;

{$I NtVer.inc}

interface

uses
  Classes, Forms, NtBase, NtBaseTranslator;

type
  { @abstract Class that translates VCL forms, frames and data modules.
    This class performs runtime language switch by going through each component and
    property on the host component (form, frame or data module).
    Call @link(TNtTranslator.TranslateForms) to translate all forms. }
  TNtTranslator = class(TNtBaseTranslator)

  protected
    { Called after a component has been translated.
      @param component Component that has been translated. }
    procedure AfterProcessComponent(component: TComponent); override;

    procedure Translate(component: TComponent); override;

  public
    procedure TranslateForm(form: TCustomForm);

    { Call this function in the initialization section of your main form.
      @param layout The original layout of the application. }
    class procedure InitializeApplication(layout: TNtLayout = laLeftToRight);

    { Flips the form layout if needed.
      @param form Form to be initialized. }
    class procedure InitializeForm(form: TCustomForm);

    { Load a new resource DLL file, initialize the language depend values, and
      finally translate the forms.
      @param code         Specifies the file extension without period of the new resource DLL.
      @param options      Language change options.
      @param originalCode Language used in the original application.
      @param fileName     The file to be loaded. If empty then function looks the standard resource file name and location.
      @return @true if succesful, @false if failed. }
    class function SetNew(
      const code: String = '';
      options: TNtResourceOptions = [];
      const originalCode: String = '';
      const fileName: String = ''): Boolean;

    { Translate all created forms, frames and data modules. }
    class procedure TranslateForms;
  end;

// _T has been moved to NtTranslatorEx.pas

implementation

uses
  Windows, Controls, NtLocalization, NtPattern;

class function TNtTranslator.SetNew(
  const code: String;
  options: TNtResourceOptions;
  const originalCode: String;
  const fileName: String): Boolean;
var
  locale: Integer;
begin
  ResourceOptions := options;
  Result := TNtBase.LoadNew(code, fileName) <> 0;

  if Result then
  begin
    if code = '' then
      locale := TNtLocale.ExtensionToLocale(originalCode)
    else
      locale := TNtLocale.ExtensionToLocale(code);

    // Updates thred's locale, format settings and bidi mode to match
    if not (roNoThreadLocale in options) then
      SetThreadLocale(locale);

    if not (roNoLocaleVariables in options) then
      TNtLocale.UpdateFormatSettings(locale);

    if not (roNoUpdateBidiMode in options) then
    begin
      if TNtLocale.IsLocaleBiDi(TNtBase.LocaleToExtension(locale)) then
        Application.BiDiMode := bdRightToLeft
      else
        Application.BiDiMode := bdLeftToRight;
    end;

    // Translate forms
    TranslateForms;
  end;

  if roSaveLocale in options then
    TNtLocaleRegistry.SetCurrentDefaultLocale;
end;

procedure TNtTranslator.AfterProcessComponent(component: TComponent);
begin
  if component is TControl then
    TControl(component).Perform(CM_TEXTCHANGED, 0, 0);
end;

procedure TNtTranslator.Translate(component: TComponent);
var
  i: Integer;
begin
  for i := 0 to component.ComponentCount - 1 do
    if component.Components[i] is TFrame then
      Translate(component.Components[i]);

  inherited;
end;

procedure TNtTranslator.TranslateForm(form: TCustomForm);
begin
  if (roFlipChildren in ResourceOptions) and TNtLocale.IsPreviousLocaleBidi then
    form.FlipChildren(True);

{$IFDEF DELPHIDX4}
  FCurrentPpi := form.CurrentPPI;
  FScreenPpi := Screen.DefaultPixelsPerInch;
{$ENDIF}

  Translate(form);

  if (roFlipChildren in ResourceOptions) and TNtLocale.IsActiveLocaleBidi then
    form.FlipChildren(True);
end;

class procedure TNtTranslator.TranslateForms;
var
  translator: TNtTranslator;

  procedure SetNewLayout(value: TNtLayout);
  begin
    translator.TranslateLayout := UiLayout <> value;
    UiLayout := value;
  end;

var
  i: Integer;
begin
  translator := TNtTranslator.Create;
  try
    if TNtLocale.IsActiveLocaleBidi and (UiLayout = laLeftToRight) then
      SetNewLayout(laRightToLeft)
    else if not TNtLocale.IsActiveLocaleBidi and (UiLayout = laRightToLeft) then
      SetNewLayout(laLeftToRight);

    if NtTranslateDataModules then
      for i := 0 to Screen.DataModuleCount - 1 do
        translator.Translate(Screen.DataModules[i]);

    for i := 0 to Screen.FormCount - 1 do
      translator.TranslateForm(Screen.Forms[i]);
  finally
    translator.Free;
  end;
end;

class procedure TNtTranslator.InitializeApplication(layout: TNtLayout);
begin
  // Sets the bidi mode of the application
  if TNtLocale.IsActiveLocaleBidi then
    Application.BiDiMode := bdRightToLeft
  else
    Application.BiDiMode := bdLeftToRight;

  // Sets the initial layout of the application
  if TNtResource.GetActiveLocale = '' then
    UiLayout := layout
  else if TNtLocale.IsActiveLocaleBidi then
    UiLayout := laRightToLeft
  else
    UiLayout := laLeftToRight;
end;

class procedure TNtTranslator.InitializeForm(form: TCustomForm);
begin
  if TNtLocale.IsActiveLocaleBidi then
    form.FlipChildren(True);
end;

end.
