#Requires -Version 5.1
<#
.SYNOPSIS
  Creates a new Soluling cloud project seeded from a source file.
.DESCRIPTION
  POST /users/authenticate to get a JWT, then POST /projects/source with the
  source file's bytes as the request body (see ProjectController.cs in the
  Soluling Backend repo). Unlike POST /projects, this starts the project with
  the source file's strings already imported instead of empty.
#>

[CmdletBinding()]
param(
    [string] $ApiUrl           = 'https://www.soluling.com/api',
    # [string] $ApiUrl           = 'http://localhost:52104',
    [string] $User             = '',
    [string] $ProjectName      = '',
    [string] $OriginalLanguage = '',
    [string] $TargetLanguages  = '',
    # Relative paths resolve against the caller's current directory.
    [string] $SourceFile       = '',
    # Soluling type code ("ini" for an INI file); '' lets the server auto-detect.
    [string] $SourceType       = '',
    # '' uses the caller's default account.
    [string] $AccountKey       = ''
)

$ErrorActionPreference = 'Stop'

# --- Ask for values not passed in. ----------------------------------------
if (-not $SourceFile) {
    $answer = Read-Host 'Source file [Sport.ini]'
    $SourceFile = if ($answer) { $answer } else { 'Sport.ini' }
}
if (-not $ProjectName) {
    $default = [System.IO.Path]::GetFileNameWithoutExtension($SourceFile)
    $answer = Read-Host "Project name [$default]"
    $ProjectName = if ($answer) { $answer } else { $default }
}
if (-not $OriginalLanguage) {
    $answer = Read-Host 'Original language [en]'
    $OriginalLanguage = if ($answer) { $answer } else { 'en' }
}
if (-not $TargetLanguages) {
    $answer = Read-Host 'Target languages [de;fr]'
    $TargetLanguages = if ($answer) { $answer } else { 'de;fr' }
}
if (-not $SourceType) {
    $SourceType = Read-Host 'Source type (blank = auto-detect)'
}
if (-not $AccountKey) {
    $AccountKey = Read-Host 'Account key (blank = default account)'
}

# Resolve a relative source path against the caller's current directory.
if (-not [System.IO.Path]::IsPathRooted($SourceFile)) {
    $SourceFile = Join-Path (Get-Location -PSProvider FileSystem).ProviderPath $SourceFile
}

if (-not (Test-Path -LiteralPath $SourceFile)) {
    throw "Source file not found: $SourceFile"
}

# --- 1. Authenticate. SOLULING_USER / SOLULING_PASSWORD are used when set (CI);
#        otherwise you are prompted for whichever is missing. -----------------
if (-not $User) {
    $User = if ($env:SOLULING_USER) { $env:SOLULING_USER } else { Read-Host 'Soluling user' }
}
$cred = if ($env:SOLULING_PASSWORD) {
    [PSCredential]::new($User, (ConvertTo-SecureString $env:SOLULING_PASSWORD -AsPlainText -Force))
} else {
    Get-Credential -UserName $User -Message 'Soluling password'
}

Write-Host "Authenticating as $User ..."
$auth = Invoke-RestMethod -Method Post -Uri "$ApiUrl/users/authenticate" `
    -ContentType 'application/json' `
    -Body (@{ username = $cred.UserName
              password = $cred.GetNetworkCredential().Password } | ConvertTo-Json)
Write-Host 'Authenticated.'

# --- 2. Create the project from the source file. -----------------------------
$params = [ordered]@{
    name             = [System.IO.Path]::GetFileName($SourceFile)
    projectName      = $ProjectName
    originalLanguage = $OriginalLanguage
    targetLanguages  = $TargetLanguages
}
if ($SourceType) { $params.sourceType = $SourceType }
if ($AccountKey) { $params.account    = $AccountKey }

$query = ($params.GetEnumerator() | ForEach-Object {
    '{0}={1}' -f [Uri]::EscapeDataString($_.Key), [Uri]::EscapeDataString($_.Value)
}) -join '&'

Write-Host "Creating project `"$ProjectName`" from `"$SourceFile`" ..."
$project = Invoke-RestMethod -Method Post -Uri "$ApiUrl/projects/source?$query" `
    -Headers @{ Authorization = "Bearer $($auth.token)" } `
    -ContentType 'application/octet-stream' `
    -InFile $SourceFile

Write-Host "Created project `"$ProjectName`" - id=$($project.id) guid=$($project.guid)"
