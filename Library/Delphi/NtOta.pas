{
  @abstract Over-the-air (OTA) translation updates from Soluling Cloud for applications that
  use resource DLLs.

  Publish the project's localized files in Soluling Cloud (Publish artifacts on the project's
  Manage page). Then a translation fix or a new language reaches the users of the application the
  next time it starts, without a new release: the application downloads the changed resource DLLs
  in the background, keeps them in the user's local application data folder, and uses them
  instead of the ones installed with the application. Without a connection, or before the first
  download, the installed resource DLLs are used as before.

  Call Initialize in the project file before the forms are created, and then UpdateAsync:

  @longCode(#
uses
  NtOta;

begin
  TNtOta.Initialize('1b6e2f3a-...');  // The project's GUID, shown on its Manage page
  TNtOta.UpdateAsync;
  Application.Initialize;
  ...
#)

  Initialize points the application to the downloaded resource DLLs and selects the language again,
  so they are used from the start. UpdateAsync checks for changed files in a background thread and
  downloads them - once a day (CheckInterval), as translations change seldom; a failed check is tried
  again at the next start. A downloaded file is used right away for a language that isn't loaded, and from
  the next start for the language in use. Give UpdateAsync a procedure to know when it finished:

  @longCode(#
  TNtOta.UpdateAsync(
    procedure(status: TNtOtaStatus)
    begin
      if status = osUpdated then
        ShowMessage('New translations are used the next time you start the application.');
    end);
#)

  A resource DLL contains the forms of the application it was built for. A downloaded resource DLL
  is used only when its version is the same as the application's, so a build for a newer version
  of the application, with changed forms, is never used by an older one. Give the application a
  version number and update it when the forms change.

  The unit needs Delphi XE8 or later, and works on Windows.
}
unit NtOta;

{$I NtVer.inc}

interface

uses
  SysUtils, Classes, NtBase;

const
  NT_OTA_API_URL = 'https://www.soluling.com/api';        //< The default address of Soluling's API
  NT_OTA_CDN_URL = 'https://www.soluling.com/artifacts';  //< The default address of the published files on Soluling's CDN

type
  { The result of an update check. }
  TNtOtaStatus =
  (
    osUpToDate,      //< The downloaded files are the newest ones.
    osUpdated,       //< Changed files were downloaded. See TNtOta.UpdatedLanguages.
    osNotPublished,  //< The project has no published files, or there is no project with the GUID.
    osFailed,        //< The check or a download failed. See TNtOta.LastError.
    osNotChecked     //< Not checked: the last check was less than TNtOta.CheckInterval ago.
  );

  { Called when UpdateAsync is finished, in the main thread. }
  TNtOtaDoneProc = reference to procedure(status: TNtOtaStatus);

  { @abstract Downloads the changed resource DLLs of the application from Soluling Cloud. }
  TNtOta = class
  private
    class var FProjectGuid: String;
    class var FApiUrl: String;
    class var FCdnUrl: String;
    class var FCheckInterval: Integer;
    class var FCacheDir: String;
    class var FLastError: String;
    class var FUpdatedLanguages: TStringList;

    class function GetExeBaseName: String;
    class function GetIndexFileName: String;
    class function IsResourceFile(const name: String): Boolean;
    class function IsUsable(const fileName: String): Boolean;
    class procedure ApplyPendingFiles;
    class procedure RemoveUnusableFiles;
    class function CheckedRecently: Boolean;
    class procedure SaveLastCheck;
    class function Download(const url, fileName: String): Boolean;

  public
    class constructor Create;
    class destructor Destroy;

    { Uses the downloaded resource DLLs of the application: finishes the downloads that were waiting
      for the application to start again, removes downloaded files that don't match the version of
      the application, and selects the language again from both the downloaded and the installed
      resource DLLs. Call it in the project file before the forms are created.
      @param projectGuid   The GUID of the project in Soluling Cloud.
      @param apiUrl        The address of Soluling's API. Empty for the default.
      @param cacheDir      The folder of the downloaded files. Empty for a folder in the user's local application data.
      @param localeSelect  How the language is selected when the user hasn't chosen one. }
    class procedure Initialize(
      const projectGuid: String;
      const apiUrl: String = '';
      const cacheDir: String = '';
      localeSelect: TLocaleSelect = lsUI);

    { Checks for changed resource DLLs and downloads them, unless the last check was less than
      CheckInterval ago. Takes a moment, so call UpdateAsync from the main thread instead.
      @param force  Check even when the last check was less than CheckInterval ago.
      @return The result. }
    class function Update(force: Boolean = False): TNtOtaStatus;

    { Runs Update in a background thread.
      @param onDone  Called with the result in the main thread when finished. Can be nil.
      @param force   Check even when the last check was less than CheckInterval ago. }
    class procedure UpdateAsync(onDone: TNtOtaDoneProc = nil; force: Boolean = False);

    { Removes the downloaded files, so the installed resource DLLs are used from the next start. }
    class procedure Clear;

    class property ProjectGuid: String read FProjectGuid;                  //< The GUID of the project.
    class property ApiUrl: String read FApiUrl;                            //< The address of Soluling's API.
    class property CdnUrl: String read FCdnUrl write FCdnUrl;              //< The address of the published files on Soluling's CDN, where the manifest is read first.
    class property CheckInterval: Integer read FCheckInterval write FCheckInterval;  //< Minutes between checks. Default 1440 (a day). 0 checks every time.
    class property CacheDir: String read FCacheDir;                        //< The folder of the downloaded files.
    class property LastError: String read FLastError;                      //< Why the last update failed.
    class property UpdatedLanguages: TStringList read FUpdatedLanguages;   //< The languages the last update downloaded.
  end;

implementation

{$IFNDEF DELPHIXE8}
  {$MESSAGE ERROR 'NtOta needs Delphi XE8 or later'}
{$ENDIF}

uses
  Windows, IOUtils, DateUtils, System.JSON, System.Net.HttpClient, System.Net.URLClient, NtLocalization;

const
  INDEX_FILE = 'ota.ini';           // File name = hash of each downloaded file; -hash for a rejected one
  PENDING_EXT = '.pending';         // A download waiting for the file it replaces to be free
  DOWNLOAD_EXT = '.download';       // A download in progress
  CLEAR_FILE = 'ota.clear';         // Clear couldn't remove all: Initialize removes them
  LAST_CHECK = '#LastCheck';        // The index's key of the last successful check, UTC (not a file name)
  DEFAULT_CHECK_INTERVAL = 24 * 60; // Translations change a few times a year: a check every start would only cost
  USER_AGENT = 'Soluling-OTA-Delphi/1.0';
  TIMEOUT = 15000;

class constructor TNtOta.Create;
begin
  FUpdatedLanguages := TStringList.Create;
  FCdnUrl := NT_OTA_CDN_URL;
  FCheckInterval := DEFAULT_CHECK_INTERVAL;
end;

class destructor TNtOta.Destroy;
begin
  FUpdatedLanguages.Free;
end;

class function TNtOta.GetExeBaseName: String;
begin
  Result := ChangeFileExt(ExtractFileName(TNtBase.GetRunningFileName), '');
end;

class function TNtOta.GetIndexFileName: String;
begin
  Result := TPath.Combine(FCacheDir, INDEX_FILE);
end;

// A resource DLL of the application: the application's name and a language extension, such as
// MyApp.fi. Not the application itself, nor the files of this unit.
class function TNtOta.IsResourceFile(const name: String): Boolean;
var
  ext: String;
begin
  ext := ExtractFileExt(name);

  Result :=
    SameText(ChangeFileExt(name, ''), GetExeBaseName) and
    (ext <> '') and
    not SameText(ext, '.exe') and
    not SameText(ext, '.dll') and
    not SameText(ext, PENDING_EXT) and
    not SameText(ext, DOWNLOAD_EXT);
end;

// A file is a resource DLL of this version of the application. A file that Windows can't load is
// not: TNtResource.DoesVersionMatch would read the application's own version for it.
class function TNtOta.IsUsable(const fileName: String): Boolean;
var
  instance: THandle;
begin
  instance := LoadLibraryEx(PChar(fileName), 0, LOAD_LIBRARY_AS_DATAFILE);

  if instance = 0 then
    Exit(False);

  try
    Result := TNtResource.DoesVersionMatch(instance);
  finally
    FreeLibrary(instance);
  end;
end;

// Downloads that replace a file that was in use: the file is free now, before it's loaded
class procedure TNtOta.ApplyPendingFiles;
var
  pending, target: String;
begin
  for pending in TDirectory.GetFiles(FCacheDir, '*' + PENDING_EXT) do
  begin
    target := ChangeFileExt(pending, '');

    if FileExists(target) then
      SysUtils.DeleteFile(target);

    if not RenameFile(pending, target) then
      SysUtils.DeleteFile(pending);
  end;

  for pending in TDirectory.GetFiles(FCacheDir, '*' + DOWNLOAD_EXT) do
    SysUtils.DeleteFile(pending);
end;

// Downloaded files that don't match this version of the application, for example after the
// application was upgraded. The installed resource DLLs are used instead, and the next update
// downloads the files of this version when they are published.
class procedure TNtOta.RemoveUnusableFiles;
var
  fileName, name: String;
  index: TStringList;
  changed: Boolean;
begin
  index := TStringList.Create;
  try
    if FileExists(GetIndexFileName) then
      index.LoadFromFile(GetIndexFileName, TEncoding.UTF8);

    changed := False;

    for fileName in TDirectory.GetFiles(FCacheDir) do
    begin
      name := ExtractFileName(fileName);

      if IsResourceFile(name) and not IsUsable(fileName) then
      begin
        SysUtils.DeleteFile(fileName);

        if index.IndexOfName(name) >= 0 then
        begin
          index.Delete(index.IndexOfName(name));
          changed := True;
        end;
      end;
    end;

    if changed then
      index.SaveToFile(GetIndexFileName, TEncoding.UTF8);
  finally
    index.Free;
  end;
end;

class procedure TNtOta.Initialize(
  const projectGuid: String;
  const apiUrl: String;
  const cacheDir: String;
  localeSelect: TLocaleSelect);
var
  fileName: String;
begin
  FProjectGuid := Trim(projectGuid);

  if apiUrl <> '' then
    FApiUrl := Trim(apiUrl)
  else
    FApiUrl := NT_OTA_API_URL;

  while (FApiUrl <> '') and (FApiUrl[Length(FApiUrl)] = '/') do
    Delete(FApiUrl, Length(FApiUrl), 1);

  if cacheDir <> '' then
    FCacheDir := cacheDir
  else
    FCacheDir := TPath.Combine(TPath.Combine(TPath.Combine(GetEnvironmentVariable('LOCALAPPDATA'), 'Soluling\Ota'), GetExeBaseName), FProjectGuid);

  try
    ForceDirectories(FCacheDir);

    // Clear couldn't remove a file in use: remove them all now, before they are loaded
    if FileExists(TPath.Combine(FCacheDir, CLEAR_FILE)) then
      for fileName in TDirectory.GetFiles(FCacheDir) do
        SysUtils.DeleteFile(fileName);

    ApplyPendingFiles;
    RemoveUnusableFiles;
  except
    // A folder that can't be written only means no updates - the installed files still work
  end;

  // The downloaded files first, then the installed ones - see TNtBase.GetResourceFile. Selects
  // the language again: Windows loaded the first one before this, from the installed files only.
  TNtBase.SetResourceDllDir(FCacheDir, localeSelect);
end;

class function TNtOta.Download(const url, fileName: String): Boolean;
var
  client: THTTPClient;
  stream: TFileStream;
  response: IHTTPResponse;
begin
  client := THTTPClient.Create;
  try
    client.UserAgent := USER_AGENT;
    client.ConnectionTimeout := TIMEOUT;
    client.ResponseTimeout := TIMEOUT;

    stream := TFileStream.Create(fileName, fmCreate);
    try
      response := client.Get(url, stream);
      Result := response.StatusCode = 200;

      if not Result then
        FLastError := Format('%s: %d %s', [url, response.StatusCode, response.StatusText]);
    finally
      stream.Free;
    end;
  finally
    client.Free;
  end;

  if not Result then
    SysUtils.DeleteFile(fileName);
end;

// Whether the last successful check was less than CheckInterval ago
class function TNtOta.CheckedRecently: Boolean;
var
  index: TStringList;
  value: String;
  last: Int64;
begin
  Result := False;

  if (FCheckInterval <= 0) or not FileExists(GetIndexFileName) then
    Exit;

  index := TStringList.Create;
  try
    index.LoadFromFile(GetIndexFileName, TEncoding.UTF8);
    value := index.Values[LAST_CHECK];
  finally
    index.Free;
  end;

  if TryStrToInt64(value, last) then
    Result := (DateTimeToUnix(TTimeZone.Local.ToUniversalTime(Now)) - last) < Int64(FCheckInterval) * 60;
end;

class function TNtOta.Update(force: Boolean): TNtOtaStatus;
var
  client: THTTPClient;
  response: IHTTPResponse;
  manifest: TJSONValue;
  item: TJSONValue;
  index: TStringList;
  fileName, name, hash, url, language, known, temp, target: String;
  failed: Boolean;
begin
  FUpdatedLanguages.Clear;
  FLastError := '';

  if (FProjectGuid = '') or (FCacheDir = '') then
  begin
    FLastError := 'Call TNtOta.Initialize first';
    Exit(osFailed);
  end;

  if not force and CheckedRecently then
    Exit(osNotChecked);

  try
    client := THTTPClient.Create;
    try
      client.UserAgent := USER_AGENT;
      client.ConnectionTimeout := TIMEOUT;
      client.ResponseTimeout := TIMEOUT;

      // The static manifest on the CDN first: it costs nothing on Soluling's servers. The API's
      // when the CDN doesn't have one - a project not built since it started to be published there.
      response := nil;

      if FCdnUrl <> '' then
        response := client.Get(FCdnUrl + '/' + FProjectGuid + '/manifest.json');

      if (response = nil) or (response.StatusCode <> 200) then
        response := client.Get(FApiUrl + '/artifacts/' + FProjectGuid + '/manifest');
    finally
      client.Free;
    end;

    if response.StatusCode = 404 then
    begin
      SaveLastCheck;
      Exit(osNotPublished);
    end;

    if response.StatusCode <> 200 then
    begin
      FLastError := Format('%d %s', [response.StatusCode, response.StatusText]);
      Exit(osFailed);
    end;

    manifest := TJSONObject.ParseJSONValue(response.ContentAsString(TEncoding.UTF8));

    if not (manifest is TJSONArray) then
    begin
      manifest.Free;
      FLastError := 'The manifest is not valid';
      Exit(osFailed);
    end;

    index := TStringList.Create;
    try
      if FileExists(GetIndexFileName) then
        index.LoadFromFile(GetIndexFileName, TEncoding.UTF8);

      failed := False;

      for item in TJSONArray(manifest) do
      begin
        if not item.TryGetValue<String>('fileName', fileName) or
          not item.TryGetValue<String>('hash', hash) or
          not item.TryGetValue<String>('url', url) then
        begin
          Continue;
        end;

        if not item.TryGetValue<String>('language', language) then
          language := '';

        name := ExtractFileName(StringReplace(fileName, '/', '\', [rfReplaceAll]));

        if not IsResourceFile(name) then
          Continue;

        known := index.Values[name];
        target := TPath.Combine(FCacheDir, name);

        // The same file as downloaded, or as rejected for another version of the application
        if ((known = hash) and (FileExists(target) or FileExists(target + PENDING_EXT))) or (known = '-' + hash) then
          Continue;

        temp := target + DOWNLOAD_EXT;

        if not Download(url, temp) then
        begin
          failed := True;
          Continue;
        end;

        if not IsUsable(temp) then
        begin
          SysUtils.DeleteFile(temp);
          index.Values[name] := '-' + hash;
          Continue;
        end;

        // A file in use can't be replaced: it waits for the next start (ApplyPendingFiles)
        if FileExists(target) and not SysUtils.DeleteFile(target) then
        begin
          SysUtils.DeleteFile(target + PENDING_EXT);
          RenameFile(temp, target + PENDING_EXT);
        end
        else
          RenameFile(temp, target);

        index.Values[name] := hash;

        if language = '' then
        begin
          language := ExtractFileExt(name);
          Delete(language, 1, 1);
        end;

        FUpdatedLanguages.Add(language);
      end;

      // A check with a failed download is checked again next time
      if not failed then
        index.Values[LAST_CHECK] := IntToStr(DateTimeToUnix(TTimeZone.Local.ToUniversalTime(Now)));

      index.SaveToFile(GetIndexFileName, TEncoding.UTF8);
    finally
      index.Free;
      manifest.Free;
    end;

    if FUpdatedLanguages.Count > 0 then
      Result := osUpdated
    else if failed then
      Result := osFailed
    else
      Result := osUpToDate;
  except
    on e: Exception do
    begin
      FLastError := e.Message;
      Result := osFailed;
    end;
  end;
end;

// Records a successful check without downloads (no published files)
class procedure TNtOta.SaveLastCheck;
var
  index: TStringList;
begin
  index := TStringList.Create;
  try
    try
      if FileExists(GetIndexFileName) then
        index.LoadFromFile(GetIndexFileName, TEncoding.UTF8);

      index.Values[LAST_CHECK] := IntToStr(DateTimeToUnix(TTimeZone.Local.ToUniversalTime(Now)));
      index.SaveToFile(GetIndexFileName, TEncoding.UTF8);
    except
      // Not saved: checked again next time
    end;
  finally
    index.Free;
  end;
end;

class procedure TNtOta.UpdateAsync(onDone: TNtOtaDoneProc; force: Boolean);
begin
  TThread.CreateAnonymousThread(
    procedure
    var
      status: TNtOtaStatus;
    begin
      status := Update(force);

      if Assigned(onDone) then
        TThread.Queue(nil,
          procedure
          begin
            onDone(status);
          end);
    end).Start;
end;

class procedure TNtOta.Clear;
var
  fileName: String;
  inUse: Boolean;
begin
  if (FCacheDir = '') or not TDirectory.Exists(FCacheDir) then
    Exit;

  inUse := False;

  for fileName in TDirectory.GetFiles(FCacheDir) do
    if not SysUtils.DeleteFile(fileName) then
      inUse := True;

  // A file in use is removed at the next start, before it's loaded - see Initialize
  if inUse then
    TFile.WriteAllText(TPath.Combine(FCacheDir, CLEAR_FILE), '');
end;

end.
