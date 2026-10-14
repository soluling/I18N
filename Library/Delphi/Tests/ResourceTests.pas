unit ResourceTests;

{
  Tests for NtResource.pas.

  The tests use the .ntres and .ntlang files of the FMX samples. The sample
  directory is searched from the directory of the test application and from the
  current directory and from their parent directories.

  Tests.dpr embeds Image\NtLangRes.ntres as NtTestRes resource. A different
  resource name than the default NtLangRes is used so that the global
  NtResources object is not enabled in the tests.
}

interface

uses
  System.Classes,
  TestFramework,
  NtResource;

type
  TResourceTests = class(TTestCase)
  private
    FResources: TNtDelphiResources;

    function SampleFile(const fileName: String): String;
    procedure LoadSample(const fileName: String);

  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure LoadLanguages;
    procedure LoadTwice;
    procedure SelectLanguage;
    procedure FindForm;
    procedure FormExists;
    procedure FindStringInGroup;
    procedure FindStringInLanguage;
    procedure WinStrings;
    procedure EmbeddedResource;
    procedure MissingResourceRaisesException;
    procedure LanguageFiles;
    procedure ConcurrentLookups;
    procedure ParseKey;
    procedure ParseKeyUnknownFormat;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  System.SyncObjs,
  System.Generics.Collections,
  NtBase;

const
  DRIVING_C = 'Driving\Localized\NtLangRes.ntres';   // Version 2: de, es, fi, fr, it, nl
  COMMENTS_C = 'Version1\Comments\NtLangRes.ntres';  // Version 1: fi, ja
  ACTIVE_C = 'Version1\ActiveLanguage\NtLangRes.ntres';  // Version 1: fi
  NTLANG_C = 'ExternalFile\fi.ntlang';
  TEST_RESOURCE_C = 'NtTestRes';

function TResourceTests.SampleFile(const fileName: String): String;

  function Find(dir: String): String;
  begin
    while dir <> '' do
    begin
      Result := TPath.Combine(dir, 'Samples\Delphi\FMX\' + fileName);

      if FileExists(Result) then
        Exit;

      if ExtractFileDir(dir) = dir then
        Break;

      dir := ExtractFileDir(dir);
    end;

    Result := '';
  end;

begin
  Result := Find(ExtractFileDir(ParamStr(0)));

  if Result = '' then
    Result := Find(GetCurrentDir);

  if Result = '' then
    Fail('Sample file not found: ' + fileName);
end;

procedure TResourceTests.LoadSample(const fileName: String);
begin
  FResources.Load(SampleFile(fileName));
end;

procedure TResourceTests.SetUp;
begin
  inherited;
  FResources := TNtDelphiResources.Create;
end;

procedure TResourceTests.TearDown;
begin
  FResources.Free;
  inherited;
end;

procedure TResourceTests.LoadLanguages;
const
  IDS: array[0..5] of String = ('de', 'es', 'fi', 'fr', 'it', 'nl');
var
  i: Integer;
begin
  LoadSample(DRIVING_C);

  CheckEquals(Length(IDS), FResources.Count);
  CheckTrue(FResources.TranslationSource = tsFile);

  for i := 0 to FResources.Count - 1 do
  begin
    CheckEquals(IDS[i], FResources[i].Id);
    CheckTrue(FResources[i].Version = nr2, FResources[i].Id);
    CheckEquals(2, FResources[i].FormCount, FResources[i].Id);
  end;
end;

procedure TResourceTests.LoadTwice;
var
  stream: TStream;
begin
  // Loading again used to keep the old languages that pointed to a freed stream
  LoadSample(DRIVING_C);
  LoadSample(DRIVING_C);

  CheckEquals(6, FResources.Count);

  FResources.LanguageId := 'fi';
  stream := FResources.FindForm('TForm1');
  try
    CheckNotNull(stream);
  finally
    stream.Free;
  end;
end;

procedure TResourceTests.SelectLanguage;
begin
  LoadSample(DRIVING_C);

  FResources.LanguageId := 'fi';
  CheckEquals('fi', FResources.LanguageId);

  // Country specific id falls back to the language
  FResources.LanguageId := 'de-AT';
  CheckEquals('de', FResources.LanguageId);

  FResources.LanguageId := 'xx';
  CheckEquals('', FResources.LanguageId);
  CheckNull(FResources.Current);
  CheckEquals('Hello', FResources.GetString('Hello', '', ''), 'No language uses the original');
end;

procedure TResourceTests.FindForm;
var
  header: array[0..3] of Byte;
  stream: TStream;
begin
  LoadSample(DRIVING_C);
  FResources.LanguageId := 'fi';

  stream := FResources.FindForm('TForm1');
  try
    CheckNotNull(stream);
    CheckEquals(0, stream.Position);
    CheckTrue(stream.Size > Length(header));

    stream.ReadBuffer(header, Length(header));
    CheckTrue(CompareMem(@header, @NtBase.VCL_FORM_HEADER, Length(header)), 'Form data must start with TPF0');
  finally
    stream.Free;
  end;

  CheckNull(FResources.FindForm('NoSuchForm'));
end;

procedure TResourceTests.FormExists;
var
  i, j: Integer;
  names: TStringList;
  stream: TStream;
  language: TNtDelphiResource;
begin
  LoadSample(DRIVING_C);

  names := TStringList.Create;
  try
    for i := 0 to FResources.Count - 1 do
    begin
      language := FResources[i];
      names.Clear;
      language.GetFormNames(names);
      CheckEquals(language.FormCount, names.Count);

      for j := 0 to names.Count - 1 do
      begin
        CheckTrue(language.FormExists(names[j]), names[j]);

        stream := language.FindForm(names[j]);
        try
          CheckNotNull(stream, names[j]);
        finally
          stream.Free;
        end;
      end;

      CheckFalse(language.FormExists('NoSuchForm'));
    end;
  finally
    names.Free;
  end;

  FResources.LanguageId := 'fi';
  CheckTrue(FResources.FormExists('TForm1'));
  CheckFalse(FResources.FormExists('NoSuchForm'));
end;

procedure TResourceTests.FindStringInGroup;
begin
  LoadSample(COMMENTS_C);
  FResources.LanguageId := 'fi';

  CheckEquals('Moi maailma', FResources.GetString('Hello World', '', ''));
  CheckEquals('Moi %s, tervetuloa %s:een', FResources.GetString('Hello %s, welcome to %s', '', ''));
  CheckEquals('Missing', FResources.GetString('Missing', '', ''), 'Missing string uses the original');

  CheckEquals('Moi maailma', FResources.GetStringInLanguage('fi', 'Hello World', '', ''));
  CheckEquals('Hello World', FResources.GetStringInLanguage('xx', 'Hello World', '', ''), 'Missing language uses the original');
end;

procedure TResourceTests.FindStringInLanguage;
begin
  // Language names are stored by their ids
  LoadSample(ACTIVE_C);

  CheckEquals('englanti', FResources.GetStringInLanguage('fi', 'English', 'en', ''));
  CheckEquals('suomi', FResources.GetStringInLanguage('fi', 'Finnish', 'fi', ''));
end;

procedure TResourceTests.WinStrings;
var
  i: Integer;
  values: TStringList;
  language: TNtDelphiResource;
begin
  LoadSample(DRIVING_C);

  values := TStringList.Create;
  try
    for i := 0 to FResources.Count - 1 do
    begin
      language := FResources[i];

      values.Clear;
      language.GetWin32Strings(values);
      CheckEquals(language.Win32StringCount, values.Count, language.Id);

      values.Clear;
      language.GetWin64Strings(values);
      CheckEquals(language.Win64StringCount, values.Count, language.Id);
    end;
  finally
    values.Free;
  end;
end;

procedure TResourceTests.EmbeddedResource;
var
  stream: TStream;
begin
  FResources.ResourceName := TEST_RESOURCE_C;

  CheckEquals(1, FResources.Count);
  CheckTrue(FResources.TranslationSource = tsResource);
  CheckEquals('fi', FResources[0].Id);
  CheckEquals(6, FResources[0].ResourceCount);

  FResources.LanguageId := 'fi';
  CheckTrue(FResources.FormExists('TForm1'));

  stream := FResources.GetResource('Image1');
  try
    CheckNotNull(stream);
    CheckEquals(6166, stream.Size);
  finally
    stream.Free;
  end;
end;

procedure TResourceTests.MissingResourceRaisesException;
begin
  FResources.ResourceName := 'NoSuchResource';

  StartExpectingException(EReadError);
  FResources.Load;
  StopExpectingException;
end;

procedure TResourceTests.LanguageFiles;
var
  dir: String;
begin
  // A directory that contains only a .ntlang file
  dir := TPath.Combine(TPath.GetTempPath, 'NtResourceTests' + IntToStr(TThread.CurrentThread.ThreadID));
  ForceDirectories(dir);
  try
    TFile.Copy(SampleFile(NTLANG_C), TPath.Combine(dir, 'fi.ntlang'), True);

    FResources.ResourcePath := dir;

    CheckEquals(1, FResources.Count);
    CheckTrue(FResources.TranslationSource = tsDirectory);
    CheckEquals('fi', FResources[0].Id);

    // Language data is read when it is used for the first time
    CheckEquals(2, FResources[0].FormCount);
    CheckTrue(FResources[0].FormExists('TForm1'));
  finally
    // Close the file before deleting it
    FreeAndNil(FResources);
    FResources := TNtDelphiResources.Create;
    TDirectory.Delete(dir, True);
  end;
end;

type
  TExpectedString = record
    Language: Integer;
    Id: Integer;
    Value: String;
  end;

  TLookupThread = class(TThread)
  private
    FResources: TNtDelphiResources;
    FExpected: TList<TExpectedString>;
    FSeed: Cardinal;
    FMismatches: Integer;
    FErrors: Integer;
  protected
    procedure Execute; override;
  end;

function FindStringById(language: TNtDelphiResource; id: Integer): String;
var
  rec: TResStringRec;
begin
  FillChar(rec, SizeOf(rec), 0);
  rec.Identifier := id;
  Result := language.FindString(@rec);
end;

procedure TLookupThread.Execute;
var
  i: Integer;
  stream: TStream;
  expected: TExpectedString;
begin
  for i := 1 to 5000 do
  begin
    // xorshift32. Random would make the threads share the global RandSeed.
    FSeed := FSeed xor (FSeed shl 13);
    FSeed := FSeed xor (FSeed shr 17);
    FSeed := FSeed xor (FSeed shl 5);

    try
      expected := FExpected[FSeed mod Cardinal(FExpected.Count)];

      if FindStringById(FResources[expected.Language], expected.Id) <> expected.Value then
        Inc(FMismatches);

      // Read form data at the same time
      if i mod 10 = 0 then
      begin
        stream := FResources[expected.Language].FindForm('TForm1');

        if stream = nil then
          Inc(FMismatches);

        stream.Free;
      end;
    except
      Inc(FErrors);
    end;
  end;
end;

procedure TResourceTests.ConcurrentLookups;
const
  THREAD_COUNT = 8;
var
  i, id, mismatches, errors: Integer;
  expected: TList<TExpectedString>;
  item: TExpectedString;
  threads: array[0..THREAD_COUNT - 1] of TLookupThread;
begin
  // Lookups used to share the stream position of the resource file
  LoadSample(DRIVING_C);

  expected := TList<TExpectedString>.Create;
  try
    // Single threaded baseline
    for i := 0 to FResources.Count - 1 do
      for id := 0 to 65535 do
      begin
        item.Language := i;
        item.Id := id;
        item.Value := FindStringById(FResources[i], id);

        if item.Value <> '' then
          expected.Add(item);
      end;

    CheckTrue(expected.Count > 0, 'No resource strings found');

    for i := 0 to THREAD_COUNT - 1 do
    begin
      threads[i] := TLookupThread.Create(True);
      threads[i].FResources := FResources;
      threads[i].FExpected := expected;
      threads[i].FSeed := 2463534242 + Cardinal(i)*7919;
    end;

    for i := 0 to THREAD_COUNT - 1 do
      threads[i].Start;

    mismatches := 0;
    errors := 0;

    for i := 0 to THREAD_COUNT - 1 do
    begin
      threads[i].WaitFor;
      Inc(mismatches, threads[i].FMismatches);
      Inc(errors, threads[i].FErrors);
      threads[i].Free;
    end;

    CheckEquals(0, mismatches, 'Wrong results');
    CheckEquals(0, errors, 'Exceptions');
  finally
    expected.Free;
  end;
end;

procedure TResourceTests.ParseKey;

  procedure Process(const key, unitName, itemName: String);
  var
    thisUnitName, thisItemName: String;
  begin
    CheckTrue(TNtDelphiResource.ParseKey(key, thisUnitName, thisItemName), key);
    CheckEquals(unitName, thisUnitName, key);
    CheckEquals(itemName, thisItemName, key);
  end;

begin
  Process('___rstr__ZN3Fmx6Consts10_SEditCopyE', 'Fmx.Consts', 'SEditCopy');
  Process('___rstr__ZN6System9Rtlconsts24_SCentimetersDescriptionE', 'System.Rtlconsts', 'SCentimetersDescription');
  Process('___rstr__ZN5Unit16_SStr1E', 'Unit1', 'SStr1');
  Process('___rstr__ZZN5Unit13OneEiE5SStr1', 'Unit1', 'SStr1');
  Process('___rstr__ZZN5Unit13OneEvE5SStr1', 'Unit1', 'SStr1');
  Process('___rstr__ZZN5Unit13OneEiN6System13UnicodeStringEE5SStr1', 'Unit1', 'SStr1');
  Process('___rstr__ZZN5Unit16TForm110FormCreateEPN6System7TObjectEE4SStr', 'Unit1', 'SStr');
  Process('___rstr__ZZN5Unit16TForm110FormCreateEPN6System7TObjectEE13SThisIsSample', 'Unit1', 'SThisIsSample');
end;

procedure TResourceTests.ParseKeyUnknownFormat;

  procedure Process(const key: String);
  var
    unitName, itemName: String;
  begin
    CheckFalse(TNtDelphiResource.ParseKey(key, unitName, itemName), key);
    CheckEquals('', itemName, key);
  end;

begin
  // These used to crash or hang
  Process('___rstr__ZN5Unit16_SStr1');       // No E after the item
  Process('___rstr__ZN5Unit1X6_SStr1E');     // Not a length
  Process('___rstr__ZZN5Unit13OneEiE');      // Nothing after E
  Process('___rstr__ZZN5Unit1X3OneEiE5SStr1');
  Process('___rstr__ZN99Unit1E');            // Length too long
  Process('___rstr__ZN');
  Process('NotAKey');
end;

initialization
  RegisterTest(TResourceTests.Suite);
end.
