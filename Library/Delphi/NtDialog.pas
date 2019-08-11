{
  @abstract Implements class that formats filter string for TOpenDialog and TSaveDialog.

  If you set the whole filter string into one single resource string the format of string
  is quite complicated. For example.

  @longCode(#
  All supported files (*.xml;*.ini)|*.xml;*.ini|XML files (*.xml)|*.xml|Ini files (*.ini)|*.ini
#)

  It is likely that translator will break the above format and your localized application
  will crash when the incorrect filter string is assigned to dialog's Filter property.
  Use this class to break the complicated string into plain string that are easy and safe
  to translate. Instead of one complex string we will have three simple strings to be translated.

  @longCode(#
  All supported files
  XML files
  Ini files
#)

  There is no possibility to break the filter format my entering invalid translations.
  See the following code about how ti use the class.

  @longCode(#
resourcestring
  SAllSupportedFiles = 'All supported files';
  SXmlFiles = 'XML files';
  SIniFiles = 'Ini files';
var
  dialog: TOpenDialog;
  filter: TNtDialogFilter;
begin
  dialog := TOpenDialog.Create(nil);
  try
    dialog.Title := SOpenTitle;

    filter := TNtDialogFilter.Create;
    try
      filter.Supported(SAllSupportedFiles);
      filter.Add(SXmlFiles, 'xml');
      filter.Add(SIniFiles, 'ini');
      filter.All(SAllFiles);

      dialog.Filter := filter.Value;
    finally
      filter.Free;
    end;

    if dialog.Execute then
      ShowMessage(dialog.FileName);
  finally
    dialog.Free;
  end;
#)

  When you create @link(TNtDialogFilter) you have to call @link(TNtDialogFilter.Add)
  method as least once. @link(TNtDialogFilter.Supported) and @link(TNtDialogFilter.All)
  are optional.

  See @italic(Samples\Delphi\VCL\FileDialog) sample to see how to use the unit.
}

unit NtDialog;

interface

uses
  Classes;

type
  TNtDialogFilterPosition = (fpNone, fpFirst, fpLast);

  { @abstract Dialog filter formatter class. }
  TNtDialogFilter = class(TObject)
  private
    FAllSupportedPosition: TNtDialogFilterPosition;
    FFirstPart: String;
    FSecondPart: String;
    FAllSupportedPattern: String;
    FMasks: TStringList;

    function GetValue: String;

    procedure DoAdd(const value: String);

  public
    constructor Create;
    destructor Destroy; override;

    { Add All files part to the filter.
      @param pattern All files pattern. It can contain an optional %s placeholder where mask is injected. }
    procedure All(const pattern: String);

    { Add All supported files part to the filter.
      @param pattern All supported files pattern. It can contain an optional %s placeholder where mask is injected. }
    procedure Supported(const pattern: String);

    { Add one file format.
      @param pattern File type description. It can contain an optional %s placeholder where mask is injected.
      @param mask    Mask or extension of the file type. }
    procedure Add(const pattern: String; mask: String);

    property Value: String read GetValue;
  end;

implementation

uses
  SysUtils;

function AppendSeparator(const filter: String): String;
begin
  // Append separator character, |, if needed.
  Result := filter;

  if Result <> '' then
    Result := Result + '|';
end;

function GetFilter(const pattern, mask: String): String;
begin
  Result := pattern;

  if Pos('%s', Result) = 0 then
    Result := Result + ' (%s)';

  Result := Format(Result, [mask]) + '|' + mask;
end;

constructor TNtDialogFilter.Create;
begin
  inherited;
  FMasks := TStringList.Create;
end;

destructor TNtDialogFilter.Destroy;
begin
  FMasks.Free;
  inherited;
end;

procedure TNtDialogFilter.DoAdd(const value: String);
begin
  if FAllSupportedPosition = fpLast then
    FSecondPart := AppendSeparator(FSecondPart) + value
  else
    FFirstPart := AppendSeparator(FFirstPart) + value;
end;

procedure TNtDialogFilter.All(const pattern: String);
begin
  DoAdd(GetFilter(pattern, '*.*'));
end;

procedure TNtDialogFilter.Supported(const pattern: String);
begin
  if FFirstPart = '' then
    FAllSupportedPosition := fpFirst
  else
    FAllSupportedPosition := fpLast;

  FAllSupportedPattern := pattern;
end;

procedure TNtDialogFilter.Add(const pattern: String; mask: String);
begin
  // "" -> *.*
  // ext -> *.ext
  // .ext -> *.ext
  if mask = '' then
    mask := '*.*'
  else if mask[1] = '.' then
    mask := '*' + mask
  else if mask[1] <> '*' then
    mask := '*.' + mask;

  FMasks.Add(mask);

  DoAdd(GetFilter(pattern, mask));
end;

function TNtDialogFilter.GetValue: String;
var
  i: Integer;
  mask, masksStr: String;
begin
  masksStr := '';

  if FAllSupportedPosition <> fpNone then
  begin
    for i := 0 to FMasks.Count - 1 do
    begin
      mask := FMasks[i];

      if masksStr <> '' then
        masksStr := masksStr + ';';

      masksStr := masksStr + mask;
    end;
  end;

  Result := '';

  if (FAllSupportedPosition = fpFirst) and (masksStr <> '') then
    Result := AppendSeparator(Result) + GetFilter(FAllSupportedPattern, masksStr);

  Result := AppendSeparator(Result) + FFirstPart;

  if (FAllSupportedPosition = fpLast) and (masksStr <> '') then
    Result := AppendSeparator(result) + GetFilter(FAllSupportedPattern, masksStr);

  if FSecondPart <> '' then
    Result := AppendSeparator(Result) + FSecondPart;
end;

end.
