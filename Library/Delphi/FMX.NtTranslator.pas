{
  @abstract Contains @link(TNtFmxTranslator) class that perform runtime language switch for a FireMonkey application.
}
unit FMX.NtTranslator;

{$I NtVer.inc}

interface

uses
  System.Classes, FMX.Forms, FMX.Types, NtBase, NtBaseTranslator;

type
  { @abstract Class that translates FMX forms.
    This class performs runtime language switch by going through each component and
    property on the host form.
    Call @link(TNtFmxTranslator.TranslateForms) to translate all forms. }
  TNtTranslator = class(TNtBaseTranslator)
  private
    procedure TranslateForm(form: TCommonCustomForm);

    procedure TranslateDelta(component: TFmxObject);

  protected
    procedure AfterProcessComponent(component: TComponent); override;

    procedure Translate(component: TComponent); override;

  public
    { Load a new resource DLL file, initialize the language depend values, and
      finally translate the forms.
      @param code         Specifies the file extension without period of the new resource DLL.
      @param options      Language change options.
      @param originalCode Language used in the original application.
      @return @true if succesful, @false if failed. }
    class function SetNew(
      const code: String = '';
      options: TNtResourceOptions = [];
      const originalCode: String = ''): Boolean;

    { Translate all created forms, frames and data modules. }
    class procedure TranslateForms;

    class function GetDevices(component: TFmxObject; devices: TStrings): Integer;
    class function GetDevice(component: TFmxObject): String;
  end;

{ Makes the initial translation and flips the form layout if needed.
  @param form Form to be initialized. }
procedure _T(form: TCustomForm); overload;

implementation

uses
  System.Devices,
  System.SysUtils,
  System.Types,
  NtResource,
  FMX.BehaviorManager,
  FMX.Controls,
  FMX.NtLocalization,
  FMX.Menus;

procedure _T(form: TCustomForm);
var
  translator: TNtTranslator;
begin
  translator := TNtTranslator.Create;
  try
    translator.TranslateForm(form);
  finally
    translator.Free;
  end;
end;

procedure TNtTranslator.AfterProcessComponent(component: TComponent);
begin
  if component is TControl then
    TControl(component).Repaint;
end;

class function TNtTranslator.GetDevice(component: TFmxObject): String;
var
  devices: TStrings;
begin
  devices := TStringList.Create;
  try
    GetDevices(component, devices);

    if devices.Count > 0 then
      Result := devices[0]
    else
      Result := '';
  finally
    devices.Free;
  end;
end;

class function TNtTranslator.GetDevices(component: TFmxObject; devices: TStrings): Integer;
var
  i: Integer;
  deviceBehavior: IDeviceBehavior;
  deviceClass: TDeviceInfo.TDeviceClass;
  displayMetrics: TDeviceDisplayMetrics;
  logicalScreenSize, PhysicalScreenSize: TSize;
  deviceInfos: TArray<TDeviceInfo>;
begin
  if TBehaviorServices.Current.SupportsBehaviorService(IDeviceBehavior, deviceBehavior, component) then
  begin
    deviceClass := deviceBehavior.GetDeviceClass(component);
    displayMetrics := deviceBehavior.GetDisplayMetrics(component);

    if displayMetrics.LogicalScreenSize.Height > displayMetrics.LogicalScreenSize.Width then
    begin
      logicalScreenSize := TSize.Create(displayMetrics.LogicalScreenSize.Height, displayMetrics.LogicalScreenSize.Width);
      physicalScreenSize := TSize.Create(displayMetrics.RawScreenSize.Height, displayMetrics.RawScreenSize.Width);
    end
    else
    begin
      logicalScreenSize := displayMetrics.LogicalScreenSize;
      physicalScreenSize := displayMetrics.RawScreenSize;
    end;

    deviceInfos := TDeviceInfo.SelectDevices(
      deviceClass,
      physicalScreenSize,
      logicalScreenSize,
      TOSVersion.Platform,
      displayMetrics.PixelsPerInch);

    Result := Length(deviceInfos);

    for i := Low(deviceInfos) to High(deviceInfos) do
      devices.Add(deviceInfos[i].ID);
  end
  else
    Result := 0;
end;

procedure TNtTranslator.TranslateDelta(component: TFmxObject);
var
  devices: TStrings;
  device, rootName, resName: String;
begin
  rootName := component.ClassType.ClassName;

  devices := TStringList.Create;
  try
    GetDevices(component, devices);

    for device in devices do
    begin
      resName := String.Join('_', [rootName, device]);

      if DoTranslate(component, resName) then
        Break;
    end;
  finally
    devices.Free;
  end;
end;

procedure TNtTranslator.Translate(component: TComponent);
var
  i: Integer;
begin
  // Translate frame
  for i := 0 to component.ComponentCount - 1 do
    if component.Components[i] is TFrame then
      Translate(component.Components[i]);

  // Translate component
  inherited;

  // Translate delta (e.g. device depend data)
  if component is TFmxObject then
    TranslateDelta(TFmxObject(component));
end;

procedure TNtTranslator.TranslateForm(form: TCommonCustomForm);
begin
  //if (roFlipChildren in ResourceOptions) and TNtLocale.IsPreviousLocaleBidi then
  //  form.FlipChildren(True);

  Translate(form);

  //if (roFlipChildren in ResourceOptions) and TNtLocale.IsActiveLocaleBidi then
  //  form.FlipChildren(True);
end;

class procedure TNtTranslator.TranslateForms;
var
  translator: TNtTranslator;
{
  procedure SetNewLayout(value: TNtLayout);
  begin
    translator.FTranslateLayout := UiLayout <> value;
    UiLayout := value;
  end;
}
var
  i: Integer;
begin
  translator := TNtTranslator.Create;
  try
{
    if TNtLocale.IsActiveLocaleBidi and (UiLayout = laLeftToRight) then
      SetNewLayout(laRightToLeft)
    else if not TNtLocale.IsActiveLocaleBidi and (UiLayout = laRightToLeft) then
      SetNewLayout(laLeftToRight);
}
    for i := 0 to Screen.DataModuleCount - 1 do
      translator.Translate(Screen.DataModules[i]);

    for i := 0 to Screen.FormCount - 1 do
      translator.TranslateForm(Screen.Forms[i]);
  finally
    translator.Free;
  end;
end;

class function TNtTranslator.SetNew(
  const code: String;
  options: TNtResourceOptions;
  const originalCode: String): Boolean;
begin
  ResourceOptions := options;

  if NtResources.Count > 0 then
  begin
    NtResources.LanguageId := code;
    LoadedResourceLocale := NtResources.LanguageId;
    Result := True;
  end
  else
    Result := TNtBase.LoadNew(code) <> 0;

  if Result then
  begin
    TranslateForms;
{
    if code = '' then
      locale := TNtLocale.ExtensionToLocale(originalCode)
    else
      locale := TNtLocale.ExtensionToLocale(code);

    // Updates thred's locale, format settings and bidi mode to match
    if not (roNoThreadLocale in options) then
      SetThreadLocale(locale);

    if not (roNoLocaleVariables in options) then
      TNtSystem.UpdateFormatSettings(locale);

    if not (roNoUpdateBidiMode in options) then
    begin
      if TNtLocale.IsLocaleBiDi(TNtLocale.LocaleToExtension(locale)) then
        Application.BiDiMode := bdRightToLeft
      else
        Application.BiDiMode := bdLeftToRight;
    end;

    // Translate forms
    TNtTranslator.TranslateForms;
}
  end;

  //if roSaveLocale in options then
  //  TNtRegistry.SetCurrentDefaultLocale;
end;

end.
