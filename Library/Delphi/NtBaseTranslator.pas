{
  @abstract Implements classes that are used when performing runtime language switch.
}
unit NtBaseTranslator;

{$I NtVer.inc}

interface

uses
  SysUtils, Classes, TypInfo, NtBase;

const
  { All string types. }
  STRING_TYPES = [  //FI:O803
    tkString,
    tkLString,
    tkWString
{$IFDEF UNICODE}
    , tkUString
{$ENDIF}
  ];

type
  TNtBaseTranslator = class;

  { This event is called before translating a property value.
    @param host          Either form or data module where the object belongs to.
    @param obj           Component or a sub component that is currenty being translated.
    @param propertyInfo  Property info.
    @param currentValue  Current value of the property.
    @param newValue      New value of the property. Change it if you want to use different value.
    @param cancel        Set True if you want to cancel the translation. }
  TNtBeforeTranslateEvent = procedure(
    host: TComponent;
    obj: TObject;
    propertyInfo: PPropInfo;
    const currentValue: Variant;
    var newValue: Variant;
    var cancel: Boolean);

  { This event is called before translating a property value.
    @param translator    Instance of translator that is performing the translation.
    @param host          Either form or data module where the object belongs to.
    @param component     Component that is current being translated.
    @param obj           Component or a sub component that is currenty being translated.
    @param propertyInfo  Property info.
    @param currentValue  Current value of the property.
    @param newValue      New value of the property. Change it if you want to use different value.
    @param cancel        Set True if you want to cancel the translation. }
  TNtBeforeTranslateEventEx = procedure(
    translator: TNtBaseTranslator;
    host: TComponent;
    component: TComponent;
    obj: TObject;
    propertyInfo: PPropInfo;
    const currentValue: Variant;
    var newValue: Variant;
    var cancel: Boolean);

  { This event is called after translating a property value.
    @param host          Either form or data module where the object belongs to.
    @param obj           Component or a sub component that was translated.
    @param propertyInfo  Property info. }
  TNtAfterTranslateEvent = procedure(
    host: TComponent;
    obj: TObject;
    propertyInfo: PPropInfo);

  { @abstract Abstract translator class. }
  TNtBaseTranslator = class(TObject)
  private
    FCurrent: TComponent;
    FHost: TComponent;
    FIndex: Integer;
    FName: String;
    FObj: TObject;
    FPropertyName: String;
    FPropInfo: PPropInfo;
    FReader: TReader;
    FSubName: String;
    FUnnamedTypes: TStringList;
    FTranslateLayout: Boolean;

    function GetTypeInfo: PTypeInfo;

{$IFDEF DELPHIXE}
    function ProcessBinary: TBytes;
{$ELSE}
    function ProcessBinary: AnsiString;
{$ENDIF}
    procedure ProcessBytes(count: Integer);
    procedure ProcessCollection;
    procedure ProcessComponent(parent: TComponent; root: Boolean);
    procedure ProcessList;
    procedure ProcessProperty(obj: TObject);
    procedure ProcessSet;
    procedure ProcessPropertyValue;

    function GetPropValue(instance: TObject): Variant;
    procedure SetPropValue(instance: TObject; const value: Variant);

    property TypeInfo: PTypeInfo read GetTypeInfo;

  protected
    procedure AfterProcessComponent(component: TComponent); virtual;

    function DoTranslate(component: TComponent; resourceName: String = ''): Boolean;

    procedure Translate(component: TComponent); virtual;

    constructor Create;

    property TranslateLayout: Boolean read FTranslateLayout write FTranslateLayout;

  public
    destructor Destroy; override;

    class function IsString(varType: TVarType): Boolean;
  end;

  { @abstract Class that extends @link(TNtTranslator).
    @link(TNtTranslator) can translate all normal properties but can not translate
    complex properties. A complex property is either a property that contains
    a string list, defined property or defined binary property. Defined properties
    can contain anything and the format can either be the same as normal properties
    (defined property) or propiertary binary format (defined binary property).
    In either case @link(TNtTranslator) does not know the format can not either read
    it or map the data into the real properties of the object. This is where
    translator extensions are used. An translator extension is derived from
    TNtTranslatorExtension class and it implements translations of a defined property
    of a component. For example @link(TNtPictureTranslator) translation the binary
    image data of the TPicture. If you have a 3rd party component that does not
    translate you may have to implement an extension for it. Derive your extension
    from this class and implement @link(TNtTranslatorExtension.Translate) function. You may
    also have to implement @link(TNtTranslatorExtension.GetActualObject) and/or
    @link(TNtTranslatorExtension.GetActualName) functions.
    @seealso(TNtStringsTranslator)
    @seealso(TNtPictureTranslator)
    @seealso(TNtTreeViewTranslator)
    @seealso(TNtListViewTranslator)
    @seealso(TNtShortcutItemsTranslator)
    @seealso(TNtVirtualTreeViewTranslator) }
  TNtTranslatorExtension = class(TNtExtension)
  public
    { Checks if the extension can translate the object.
      @param obj The object to be transalted.
      @return @true if the object can be translated, @false if not. }
    function CanTranslate(obj: TObject): Boolean; virtual; abstract;

    { An abstract function that translate the comple property. Each extension
      class has to implement this function.
      @param component  The component where the property belongs to.
      @param obj        The object where the property belongs to.
      @param name       The name of the property.
      @param value      The value of the property.
      @param index      The index of the item if the value is an array value. }
    procedure Translate(
      component: TComponent;
      obj: TObject;
      const name: String;
      value: Variant;
      index: Integer); virtual; abstract;

    { Get the runtime object of a resource property.
      @param obj      Object that contains the property in the form file.
      @param propName Name of the property in the form file.
      @return Object. }
    function GetActualObject(obj: TObject; const propName: String): TObject; virtual;

    { Get the runtime property name of a resource property.
      @param obj      Object that contains the property in the form file.
      @param propName Name of the property in the form file.
      @return Property name. }
    function GetActualName(obj: TObject; const propName: String): String; virtual;
  end;

  { Class type of the translator extension class. }
  TNtTranslatorExtensionClass = class of TNtTranslatorExtension;

  { @abstract Extension class that translates TStrings objects. }
  TNtStringsTranslator = class(TNtTranslatorExtension)
  public
    { @seealso(TNtTranslatorExtension.CanTranslate) }
    function CanTranslate(obj: TObject): Boolean; override;

    { @seealso(TNtTranslatorExtension.Translate) }
    procedure Translate(
      component: TComponent;
      obj: TObject;
      const name: String;
      value: Variant;
      index: Integer); override;
  end;

  { @abstract Class that stores installed extension.

    If you derive a new extension from @link(TNtExtension) you have to register
    it in order to take it in use. A good place to register an extension is to
    call the @link(TNtExtensions.Register) in the initialization block of your
    extension class unit.

    @longCode(#
initialization
  NtExtensions.Register(TYourExtension);
end.
#) }
  TNtTranslatorExtensions = class(TNtExtensions)
  private
    function GetItem(i: Integer): TNtTranslatorExtension;

    function CanTranslate(obj: TObject; var extension: TNtTranslatorExtension): Boolean;
    function GetActualObject(obj: TObject; const propName: String): TObject;
    function GetActualName(obj: TObject; const propName: String): String;

  public
    { Array of the registered extension. }
    property Items[i: Integer]: TNtTranslatorExtension read GetItem; default;
  end;

  { @abstract Binary stream class that read integer and strings values.
    This class extends TMemoryStream to provide function to read integer,
    pointer and strings values. }
  TNtStream = class(TMemoryStream)
  public
    { Creates a binary stream. }
{$IFDEF DELPHIXE}
    constructor Create(value: TBytes);
{$ELSE}
    constructor Create(value: AnsiString);
{$ENDIF}

    { Read one byte.
      @return The byte value that was read. }
    function ReadByte: Byte;

    { Read one word.
      @return The word value that was read. }
    function ReadWord: Word;

    { Read one integer.
      @return The integer value that was read. }
    function ReadInteger: Integer;

    { Read one pointer.
      @return The pointer value that was read. }
    function ReadPointer: Pointer;

    { Read string Ansi string.
      @return The string that was read. }
{$IFDEF DELPHIXE}
    function ReadShortString: TBytes;
{$ELSE}
    function ReadShortString: AnsiString;
{$ENDIF}

    { Read string Unicode string.
      @return The string that was read. }
    function ReadShortUnicodeString: UnicodeString;
  end;

var
  { Set of types that are translated. If empty then all properties are translated.
    If not empty then only those properties whose type is include in this set are translated.

    The following sample shows how to translate only string properties.
    @longCode(#
unit Unit1;
...
initialization
  NtEnabledProperties := STRING_TYPES;
end.#) }
  NtEnabledProperties: TTypeKinds;

  { If @true the form position (Left and Top properties) is also translated
    (e.g. changed if the new resource file contains different Left or Top properties).
    If @false postion is not changed. }
  NtFormPositionTranslationEnabled: Boolean;

  { If @true data modiles are also translated
    If @false data modules are not translated. }
  NtTranslateDataModules: Boolean;

  { An event that is called before translating a property value.
    Use this to disable or change the translation process.
    If you assign this value, make sure that the event is as fast as possible because
    this event is called on every single property of the application.

    See @italic(Samples\Delphi\VCL\DualLanguage) sample to see how to use the event.
    @seealso(NtAfterTranslate) }
  NtBeforeTranslate: TNtBeforeTranslateEvent;

  { An event that is called before translating a property value.
    Use this to disable or change the translation process.
    If you assign this value, make sure that the event is as fast as possible because
    this event is called on every single property of the application.

    See @italic(Samples\Delphi\VCL\DualLanguage) sample to see how to use the event.
    @seealso(NtAfterTranslate) }
  NtBeforeTranslateEx: TNtBeforeTranslateEventEx;

  { Event that is called after a property value has been translated.
    If you assing this value make sure that the event is as fast as possible because
    this event is called on every single property of the application.
    @seealso(NtBeforeTranslate) }
  NtAfterTranslate: TNtAfterTranslateEvent;

  { Collections of registered extensions. Use it to register your custom extension.
    @longCode(#
unit MyExtension;
...
initialization
  NtTranslatorExtensions.Register(TMyExtension);
end.#) }
  NtTranslatorExtensions: TNtTranslatorExtensions;

implementation

uses
{$IFDEF DELPHI_FMX}
  Types,
{$ELSE}
  Windows,
  ActnList,
{$ENDIF}
{$IFDEF DELPHIXE}
  NtResource,
{$ENDIF}
  SysConst,
  Variants;


// TNtBaseTranslator

constructor TNtBaseTranslator.Create;
begin
  inherited;
  FUnnamedTypes := TStringList.Create;
end;

destructor TNtBaseTranslator.Destroy;
begin
  FUnnamedTypes.Free;
  inherited;
end;

function TNtBaseTranslator.GetTypeInfo: PTypeInfo;
begin
  if (FPropInfo <> nil) and (FPropInfo.PropType <> nil) then
    Result := FPropInfo.PropType^
  else
    Result := nil;
end;

{$IFDEF DELPHIXE}
function TNtBaseTranslator.ProcessBinary: TBytes;
var
  size: Integer;
begin
  FReader.ReadValue;
  FReader.Read(size, SizeOf(size));

  if size > 0 then
  begin
    SetLength(Result, size);
    FReader.Read(Result[0], Length(Result));
  end
  else
    SetLength(Result, 0);
end;
{$ELSE}
function TNtBaseTranslator.ProcessBinary: AnsiString;
var
  size: Integer;
begin
  FReader.ReadValue;
  FReader.Read(size, SizeOf(size));

  if size > 0 then
  begin
    SetLength(Result, size);
    FReader.Read(PAnsiChar(Result)^, Length(Result));
  end
  else
    Result := '';
end;
{$ENDIF}

procedure TNtBaseTranslator.ProcessBytes(count: Integer);
{$IFDEF DELPHIXE}
var
  data: TBytes;
begin
  FReader.ReadValue;

  if count > 0 then
  begin
    SetLength(data, count);
    FReader.Read(data[0], count);
  end;
{$ELSE}
var
  data: AnsiString;
begin
  FReader.ReadValue;

  if count > 0 then
  begin
    SetLength(data, count);
    FReader.Read(PAnsiChar(data)^, count);
  end;
{$ENDIF}
end;

procedure TNtBaseTranslator.ProcessCollection;
var
  i: Integer;
  thisObj, actualObj: TObject;
  propInfo: PPropInfo;
  item: TCollectionItem;
  collection: TCollection;
begin  //FI:C101
  if (TypeInfo <> nil) and (TypeInfo.Kind <> tkClass) then
    raise Exception.Create('Error');

  actualObj := FObj;

  if actualObj <> nil then
  begin
    propInfo := GetPropInfo(actualObj, FName);

    if (propInfo = nil) and (FSubName <> '') then
      propInfo := GetPropInfo(actualObj, FSubName);

    if (propInfo = nil) then
    begin
      thisObj := NtTranslatorExtensions.GetActualObject(actualObj, FName);

      if thisObj <> nil then
      begin
        actualObj := thisObj;
        propInfo := GetPropInfo(actualObj, FName);
      end;
    end;
  end
  else
    propInfo := nil;

  if propInfo <> nil then
    collection := TObject(GetOrdProp(actualObj, propInfo)) as TCollection
  else
    collection := nil;

  FReader.ReadValue;
  i := 0;

  while not FReader.EndOfList do
  begin
    if (collection <> nil) and (i < collection.Count) then
      item := collection.Items[i]
    else
      item := nil;

    if FReader.NextValue in [vaInt8, vaInt16, vaInt32] then
      FReader.ReadInteger;

    FReader.ReadListBegin;

    while not FReader.EndOfList do
      ProcessProperty(item);

    FReader.ReadListEnd;

    Inc(i);
  end;

  FReader.ReadListEnd;
end;

procedure TNtBaseTranslator.ProcessList;
begin
  FReader.ReadListBegin;
  FIndex := 0;

  while not FReader.EndOfList do
  begin
    ProcessPropertyValue;
    Inc(FIndex);
  end;

  FReader.ReadListEnd;
end;

procedure TNtBaseTranslator.ProcessSet;
var
  ident: String;
begin
  FReader.ReadValue;

  while True do
  begin
    ident := FReader.ReadStr;

    if ident = '' then
      Break;
  end;
end;

{$IFDEF DELPHIXE3}
function InternalGetPropInfo(typeInfo: PTypeInfo; const propName: String): PPropInfo;

  function AfterString(const p: PByte): Pointer; inline;
  begin
    Result := p + p^ + 1;
  end;

var
  i: Integer;
  utf8Length: Integer;
  s1, s2: string;
  typeData: PTypeData;
  propData: PPropData;
begin
  if typeInfo = nil then
    Exit(nil);

  s1 := PropName;
  utf8Length := Length(UTF8Encode(propName));

  typeData := GetTypeData(typeInfo);

  while typeData <> nil do
  begin
    propData := typeData.PropData;
    Result := PPropInfo(@propData^.PropList);

    for i := 1 to propData^.PropCount do  //FI:W528
    begin

      if Result.NameFld.UTF8Length = UTF8Length then
      begin
        S2 := Result.NameFld.ToString;;

        if SameText(S1, S2) then
          Exit;
      end;
      Result := AfterString(@(Result^.Name));
    end;

    if TypeData^.ParentInfo = nil then
      TypeData := nil
    else
      TypeData := GetTypeData(TypeData^.ParentInfo^);
  end;

  Result := nil;
end;

function DoesPropertyExists(Instance: TObject; const PropName: string): Boolean;
begin
  Result := InternalGetPropInfo(PTypeInfo(Instance.ClassInfo), PropName) <> nil;
end;
{$ENDIF}

procedure TNtBaseTranslator.ProcessProperty(obj: TObject);
var
  p: Integer;
  propName, actualName: String;
  objNames: TStringList;
  i: Integer;
begin  //FI:C101
  FPropertyName := FReader.ReadStr;
  FName := FPropertyName;
  FSubName := '';
  FObj := obj;

  if FObj <> nil then
  begin
    FPropInfo := GetPropInfo(FObj, FName);

    if FPropInfo = nil then
    begin
      p := Pos('.', FName);

      if p > 0 then
      begin
        objNames := TStringList.Create;
        try
          propName := FName;

          while p > 0 do
          begin
            objNames.Add(Copy(propName, 1, p - 1));
            Delete(propName, 1, p);
            p := Pos('.', propName);
          end;

          for i := 0 to objNames.Count - 1 do
          begin
{$IFDEF DELPHIXE3}
            if DoesPropertyExists(FObj, objNames[i]) then
{$ENDIF}
            begin
              FObj := TObject(GetOrdProp(FObj, objNames[i]));

              if FObj = nil then
                Break;
            end;
          end;

          if FObj <> nil then
          begin
            FSubName := propName;
            FPropInfo := GetPropInfo(FObj, propName);
          end;
        finally
          objNames.Free;
        end;
      end
      else
      begin
        actualName := NtTranslatorExtensions.GetActualName(FObj, FName);

        if (actualName <> '') and (actualName <> FName) then
          FPropInfo := GetPropInfo(FObj, actualName);
      end;
    end;
  end
  else
    FPropInfo := nil;

  ProcessPropertyValue;
end;

function TNtBaseTranslator.GetPropValue(instance: TObject): Variant;
{$IFDEF DELPHI2005}
var
  dynArray: Pointer;
{$ENDIF}
begin  //FI:C101
  Result := Null;

  case FPropInfo^.PropType^^.Kind of
    tkInteger, tkClass:
      Result := GetOrdProp(instance, FPropInfo);

    tkChar:
      Result := Char(GetOrdProp(instance, FPropInfo));

    tkWChar:
      Result := WideChar(GetOrdProp(instance, FPropInfo));

    tkEnumeration:
      if GetTypeData(FPropInfo^.PropType^)^.BaseType^ = System.TypeInfo(Boolean) then
        Result := Boolean(GetOrdProp(instance, FPropInfo))
      else
        Result := GetOrdProp(instance, FPropInfo);

    tkSet:
      Result := GetOrdProp(Instance, FPropInfo);

    tkFloat:
      Result := GetFloatProp(Instance, FPropInfo);

    tkMethod:
      Result := FPropInfo^.PropType^.Name;

    tkString, tkLString:
      Result := GetStrProp(instance, FPropInfo);

    tkWString:
{$IFNDEF NEXTGEN}
      Result := GetWideStrProp(instance, FPropInfo);
{$ELSE}
      Result := GetStrProp(instance, FPropInfo);
{$ENDIF}

{$IFDEF UNICODE}
    tkUString:
  {$IFNDEF NEXTGEN}
    {$IFDEF DELPHIDX2}
      Result := GetStrProp(instance, FPropInfo);
    {$ELSE}
      Result := GetUnicodeStrProp(instance, FPropInfo);
    {$ENDIF}
  {$ELSE}
      Result := GetStrProp(instance, FPropInfo);
  {$ENDIF}
{$ENDIF}

    tkVariant:
      Result := GetVariantProp(instance, FPropInfo);

    tkInt64:
      Result := GetInt64Prop(instance, FPropInfo);

    tkDynArray:
      begin
{$IFDEF DELPHI2005}
        DynArray := GetDynArrayProp(instance, FPropInfo);
        DynArrayToVariant(Result, dynArray, FPropInfo^.PropType^);
{$ELSE}
        DynArrayToVariant(Result, Pointer(GetOrdProp(instance, FPropInfo)), FPropInfo^.PropType^);
{$ENDIF}
      end;
  end;
end;

class function TNtBaseTranslator.IsString(varType: TVarType): Boolean;
begin
  Result :=
    (varType = varString) or
{$IFDEF UNICODE}
    (varType = varUString) or
{$ENDIF}
    (varType = varOleStr);
end;

procedure TNtBaseTranslator.SetPropValue(
  instance: TObject;
  const value: Variant);

  procedure RangeError;
  begin
    raise ERangeError.CreateRes(@SRangeError);
  end;

  function RangedValue(const aMin, aMax: Int64): Int64;
  begin
    Result := Trunc(value);

    if (Result < aMin) or (Result > aMax) then
      RangeError;
  end;

  function RangedCharValue(const aMin, aMax: Int64): Int64;
  var
    ansi: String;
    wide: String;
  begin
    case VarType(value) of
      varString:
        begin
          ansi := value;

          if Length(ansi) = 1 then
{$IFDEF DELPHIXE3}
            Result := Ord(ansi[Low(String)])
{$ELSE}
            Result := Ord(ansi[1])
{$ENDIF}
          else
            Result := aMin - 1;
       end;

{$IFDEF UNICODE}
      varUString,
{$ENDIF}
      varOleStr:
        begin
          wide := value;

          if Length(wide) = 1 then
{$IFDEF DELPHIXE3}
            Result := Integer(wide[Low(String)])
{$ELSE}
            Result := Integer(wide[1])
{$ENDIF}
          else
            Result := aMin - 1;
        end;
    else
      Result := Trunc(value);
    end;

    if (Result < aMin) or (Result > aMax) then
      RangeError;
  end;

var
  typeData: PTypeData;
  dynArray: Pointer;
{$IFNDEF DELPHI2006}
  oldFarEast: Boolean;
{$ENDIF}
begin  //FI:C101
  typeData := GetTypeData(FPropInfo^.PropType^);

{$IFNDEF DELPHI2006}
  oldFarEast := SysLocale.FarEast;

  if instance is TAction then
    SysLocale.FarEast := True;
{$ENDIF}

  case FPropInfo.PropType^^.Kind of
    tkString,
    tkLString:
      SetStrProp(instance, FPropInfo, VarToStr(value));

    tkWString:
{$IFNDEF NEXTGEN}
      SetWideStrProp(instance, FPropInfo, VarToWideStr(value));
{$ELSE}
      SetStrProp(instance, FPropInfo, VarToStr(value));
{$ENDIF}

{$IFDEF UNICODE}
    tkUString:
  {$IFNDEF NEXTGEN}
    {$IFDEF DELPHIDX2}
      SetStrProp(instance, FPropInfo, VarToStr(value));
    {$ELSE}
      SetUnicodeStrProp(instance, FPropInfo, VarToWideStr(value));
    {$ENDIF}
  {$ELSE}
      SetStrProp(instance, FPropInfo, VarToStr(value));
  {$ENDIF}
{$ENDIF}

    tkChar, tkWChar:
      SetOrdProp(
        instance,
        FPropInfo,
        RangedCharValue(typeData^.MinValue, typeData^.MaxValue));

    tkInteger:
      if typeData^.MinValue < typeData^.MaxValue then
        SetOrdProp(
          instance,
          FPropInfo,
          RangedValue(typeData^.MinValue, typeData^.MaxValue))
      else
        SetOrdProp(
          instance,
          FPropInfo,
          RangedValue(LongWord(typeData^.MinValue), LongWord(typeData^.MaxValue)));

    tkInt64:
      SetInt64Prop(
        instance,
        FPropInfo,
        RangedValue(typeData^.MinInt64Value, typeData^.MaxInt64Value));

    tkFloat:
      SetFloatProp(instance, FPropInfo, value);

    tkEnumeration:
      if IsString(value) then
        SetEnumProp(instance, FPropInfo, VarToStr(value))
      else if VarType(value) = varBoolean then
        SetOrdProp(instance, FPropInfo, Abs(Trunc(value)))
      else
        SetOrdProp(instance, FPropInfo, RangedValue(typeData^.MinValue, typeData^.MaxValue));

    tkSet:
      if VarType(value) = varInteger then
        SetOrdProp(instance, FPropInfo, value)
      else
        SetSetProp(instance, FPropInfo, VarToStr(value));

    tkVariant:
      SetVariantProp(instance, FPropInfo, value);

    tkDynArray:
    begin
      dynArray := nil;
      DynArrayFromVariant(dynArray, value, FPropInfo^.PropType^);
      SetOrdProp(instance, FPropInfo, NativeInt(dynArray));
    end;
  end;

{$IFNDEF DELPHI2006}
  if instance is TAction then
    SysLocale.FarEast := oldFarEast;
{$ENDIF}
end;

procedure TNtBaseTranslator.ProcessPropertyValue;
var
  current, value: Variant;

  function TypesEqual: Boolean;
  var
    currentType, newType: TVarType;
  begin
    currentType := VarType(current);
    newType := VarType(value);

    Result :=
      (currentType = newType) or
      (IsString(currentType) and IsString(newType));
  end;

  function IgnoreByEvent: Boolean;
  begin
    Result := False;

    if Assigned(NtBeforeTranslateEx) then
      NtBeforeTranslateEx(Self, FHost, FCurrent, FObj, FPropInfo, current, value, Result);

    if Assigned(NtBeforeTranslate) then
      NtBeforeTranslate(FHost, FObj, FPropInfo, current, value, Result);
  end;

  procedure AfterTranslate;
  begin
    if Assigned(NtAfterTranslate) then
      NtAfterTranslate(FHost, FObj, FPropInfo);
  end;

  function IgnoreProperty: Boolean;
  var
    typeKind: TypInfo.TTypeKind;
  begin
    if (FName = 'Font.Charset') or (NtEnabledProperties = []) then
      Result := False
    else
    begin
      if FPropInfo <> nil then
        typeKind := FPropInfo.PropType^.Kind
      else
        typeKind := tkLString;

      if FTranslateLayout and (typeKind in [tkInteger]) then
        Result := False
      else
        Result := not (typeKind in NtEnabledProperties);
    end;

    if Result then
      Exit;

    if FObj = FHost then
      Result :=
        (FName = 'Visible') or
        (
          not NtFormPositionTranslationEnabled and
          ((FName = 'Position') or (FName = 'Left') or (FName = 'Top'))
        )
    else
      Result := (FName = 'ConnectionString');

    if Result then
      Exit;

    Result := IgnoreByEvent;
  end;

var
  int: Integer;
  str: String;
  identToInt: TIdentToInt;
  extension: TNtTranslatorExtension;
begin  //FI:C101
  case FReader.NextValue of
    vaNull:
      FReader.ReadValue;

    vaList:
      ProcessList;

    vaCollection:
      ProcessCollection;

    vaBinary:
      value := ProcessBinary;

    vaSet:
      ProcessSet;

    vaIdent:
    begin
      str := FReader.ReadIdent;

      if TypeInfo <> nil then
      begin
        case TypeInfo.Kind of
          tkEnumeration:
            value := GetEnumValue(TypeInfo, str);

          tkInteger:
          begin
           identToInt := FindIdentToInt(TypeInfo);

           if Assigned(identToInt) and IdentToInt(str, int) then
             value := int;
          end;
        end;
      end;
    end;

    vaInt8,
    vaInt16,
    vaInt32:
      value := FReader.ReadInteger;

    vaExtended:
      value := FReader.ReadFloat;

    vaSingle:
      value := FReader.ReadSingle;

    vaString,
    vaLString:
      value := FReader.ReadString;

    vaWString:
{$IFDEF DELPHIXE3}
      value := FReader.ReadString;
{$ELSE}
      value := FReader.ReadWideString;
{$ENDIF}

    vaFalse,
    vaTrue:
      value := FReader.ReadBoolean;

    vaNil:
      ProcessBytes(0);

    vaCurrency:
      value := FReader.ReadCurrency;

    vaDate:
      ProcessBytes(Sizeof(TDateTime));

    vaInt64:
      ProcessBytes(Sizeof(Int64));

    vaUTF8String:
{$IFDEF DELPHIXE3}
      value := FReader.ReadString;
{$ELSE}
      value := FReader.ReadWideString;
{$ENDIF}

{$IFDEF DELPHI2005}
    vaDouble:
      value := FReader.ReadDouble;
{$ENDIF}
  else
    raise Exception.Create('Unknown TValueType');
  end;

  if not VarIsNull(value) and not VarIsEmpty(value) then
  begin
    if FPropInfo <> nil then
    begin
      try
        current := GetPropValue(FObj);

        if FObj.ClassName = 'TFieldDef' then
        begin
          FObj := FObj;
        end;

        if TypesEqual and (current <> value) and not IgnoreProperty then
        begin
          SetPropValue(FObj, value);
          AfterTranslate;
        end;
      except
        on e: Exception do
          raise Exception.CreateFmt('Could not translate %s.%s from "%s" to "%s": %s', [FObj.ClassName, FName, current, value, e.Message]);
      end;
    end
    else if NtTranslatorExtensions.CanTranslate(FObj, extension) then
    begin
      if not IgnoreByEvent then
      begin
        extension.Translate(FCurrent, FObj, FPropertyName, value, FIndex);
        AfterTranslate;
      end;
    end;
  end;
end;

procedure TNtBaseTranslator.AfterProcessComponent(component: TComponent);
begin
end;

procedure TNtBaseTranslator.ProcessComponent(parent: TComponent; root: Boolean);

  function FindUnnamedComponent(
    parent: TComponent;
    const typeName: String;
    index: Integer): TComponent;
  var
    i, thisIndex: Integer;
    component: TComponent;
  begin
    Result := nil;
    thisIndex := 0;

    for i := 0 to parent.ComponentCount - 1 do
    begin
      component := parent.Components[i];

      if (component.Name = '') and (component.ClassName = typeName) then
      begin
        if thisIndex = index then
        begin
          Result := component;
          Break;
        end;

        Inc(thisIndex);
      end;
    end;
  end;

var
  i, childPos, index: Integer;
  typeName, componentName: String;
  flags: TFilerFlags;
  thisComponent: TComponent;
begin
  FReader.ReadPrefix(flags, childPos);
  typeName := FReader.ReadStr;
  componentName := FReader.ReadStr;

  if root then
    thisComponent := FHost
  else if parent = nil then
    thisComponent := nil
  else if componentName = '' then
  begin
    index := 0;

    for i := 0 to FUnnamedTypes.Count - 1 do
      if FUnnamedTypes[i] = typeName then
        Inc(index);

    FUnnamedTypes.Add(typeName);

    thisComponent := FindUnnamedComponent(parent, typeName, index);

    if thisComponent = nil then
      thisComponent := FindUnnamedComponent(FHost, typeName, index);
  end
  else
  begin
    thisComponent := parent.FindComponent(componentName);

    if thisComponent = nil then
      thisComponent := FHost.FindComponent(componentName);
  end;

  FCurrent := thisComponent;

  while not FReader.EndOfList do
    ProcessProperty(thisComponent);

  FReader.ReadListEnd;

  while not FReader.EndOfList do
    ProcessComponent(thisComponent, False);

  FReader.ReadListEnd;

  AfterProcessComponent(thisComponent);
end;

function FindInstance(classType: TClass): THandle;
begin
  Result := FindResourceHInstance(FindClassHInstance(classType));
end;

function TNtBaseTranslator.DoTranslate(component: TComponent; resourceName: String): Boolean;
var
  instance: THandle;
  header: array[0..3] of Byte;
  stream: TStream;
begin
  Result := False;
  FHost := component;

  if resourceName = '' then
    resourceName := component.ClassName;

  instance := FindInstance(component.ClassType);

  if FindResource(instance, PChar(resourceName), RT_RCDATA) <> 0 then
  begin
{$IFDEF DELPHIXE}
    stream := NtResources.FindForm(resourceName);
{$ELSE}
    stream := nil;
{$ENDIF}

    if stream = nil then
      stream := TResourceStream.Create(instance, PChar(resourceName), RT_RCDATA);

    FReader := TReader.Create(stream, stream.Size);
    try
      FReader.Read(header, 4);

      if CompareMem(@VCL_FORM_HEADER, @header[0], Length(VCL_FORM_HEADER)) then
      begin
        ProcessComponent(nil, True);
        Result := True;
      end;
    finally
      FReader.Free;
      stream.Free;
    end;
  end;
end;

procedure TNtBaseTranslator.Translate(component: TComponent);

  function DoesResourceExists(classType: TClass): Boolean;
  var
    resourceName: String;
  begin
    resourceName := classType.ClassName;

{$IFDEF DELPHIXE}
    if NtResources.Count > 0 then
    begin
      Result := NtResources.FormExists(resourceName);

      if Result then
        Exit;
    end;
{$ENDIF}

    Result := FindResource(FindInstance(classType), PChar(resourceName), RT_RCDATA) <> 0;
  end;

var
  i: Integer;
  thisClass: TClass;
  names: TStringList;
begin
  names := TStringList.Create;
  try
    thisClass := component.ClassType;

    while DoesResourceExists(thisClass) do
    begin
      names.Insert(0, thisClass.ClassName);
      thisClass := thisClass.ClassParent;
    end;

    for i := 0 to names.Count - 1 do
      DoTranslate(component, names[i]);
  finally
    names.Free;
  end;
end;


// TNtTranslatorExtension

function TNtTranslatorExtension.GetActualObject(obj: TObject; const propName: String): TObject;
begin
  Result := nil;
end;

function TNtTranslatorExtension.GetActualName(obj: TObject; const propName: String): String;
begin
  Result := '';
end;


// TNtStringsTranslator

function TNtStringsTranslator.CanTranslate(obj: TObject): Boolean;
begin
  Result := obj is TStrings;
end;

procedure TNtStringsTranslator.Translate(
  component: TComponent;
  obj: TObject;
  const name: String;
  value: Variant;
  index: Integer);
const
  ITEM_INDEX = 'ItemIndex';
var
  oldItemIndex: Integer;
  strings: TStrings;
begin
  strings := obj as TStrings;

  if index >= strings.Count then
    Exit;

  if IsPublishedProp(component, ITEM_INDEX) then
    oldItemIndex := GetOrdProp(component, ITEM_INDEX)
  else
    oldItemIndex := -2;

  strings[index] := value;

  if oldItemIndex >= -1 then
    SetOrdProp(component, ITEM_INDEX, oldItemIndex);
end;


// TNtTranslatorExtensions

function TNtTranslatorExtensions.GetItem(i: Integer): TNtTranslatorExtension;
begin
  Result := inherited Items[i] as TNtTranslatorExtension;
end;

function TNtTranslatorExtensions.CanTranslate(obj: TObject; var extension: TNtTranslatorExtension): Boolean;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    extension := Items[i];
    Result := extension.CanTranslate(obj);

    if Result then
      Exit;
  end;

  extension := nil;
  Result := False;
end;

function TNtTranslatorExtensions.GetActualObject(
  obj: TObject;
  const propName: String): TObject;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := Items[i].GetActualObject(obj, propName);

    if Result <> nil then
      Exit;
  end;

  Result := nil;
end;

function TNtTranslatorExtensions.GetActualName(obj: TObject; const propName: String): String;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := Items[i].GetActualName(obj, propName);

    if Result <> '' then
      Exit;
  end;

  Result := '';
end;


// TNtStream

{$IFDEF DELPHIXE}
constructor TNtStream.Create(value: TBytes);
begin
  inherited Create;

  if Length(value) > 0 then
  begin
    Write(value[0], Length(value));
    Seek(0, TSeekOrigin.soBeginning);
  end;
end;
{$ELSE}
constructor TNtStream.Create(value: AnsiString);
begin
  inherited Create;

  if Length(value) > 0 then
  begin
    Write(PAnsiChar(value)^, Length(value));
    Seek(0, soFromBeginning);
  end;
end;
{$ENDIF}

function TNtStream.ReadByte: Byte;
begin
  Read(Result, SizeOf(Result));
end;

function TNtStream.ReadWord: Word;
begin
  Read(Result, SizeOf(Result));
end;

function TNtStream.ReadInteger: Integer;
begin
  Read(Result, SizeOf(Result));
end;

function TNtStream.ReadPointer: Pointer;
begin
  Read(Result, SizeOf(Result));
end;

{$IFDEF DELPHIXE}
function TNtStream.ReadShortString: TBytes;
var
  size: Byte;
begin
  Read(size, SizeOf(size));
  SetLength(Result, size);
  Read(Result[0], size);
end;
{$ELSE}
function TNtStream.ReadShortString: AnsiString;
var
  size: Byte;
begin
  Read(size, SizeOf(size));
  SetLength(Result, size);
  Read(PAnsiChar(Result)^, size);
end;
{$ENDIF}

function TNtStream.ReadShortUnicodeString: UnicodeString;
var
  size: Byte;
begin
  Read(size, SizeOf(size));
  SetLength(Result, size);
  Read(PWideChar(Result)^, 2*size);
end;


initialization
  NtEnabledProperties := [];
  NtFormPositionTranslationEnabled := False;
  NtTranslateDataModules := True;
  NtBeforeTranslate := nil;
  NtAfterTranslate := nil;

  NtTranslatorExtensions := TNtTranslatorExtensions.Create;
  NtTranslatorExtensions.Register(TNtStringsTranslator);
finalization
  NtTranslatorExtensions.Free;
end.
