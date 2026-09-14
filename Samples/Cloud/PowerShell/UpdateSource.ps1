#Requires -Version 5.1
<#
.SYNOPSIS
  Uploads a changed source file to an existing Soluling cloud project so its
  strings are re-imported.
.DESCRIPTION
  POST /users/authenticate to get a JWT, then:
    * GET /projects/{name}                    - resolve the project id
    * GET /projects/{id}/sources              - list the top-level source files
    * PUT /projects/{projectId}/files/{fileName} - replace the matching source's
                                     content with the local file's bytes (see
                                     ProjectController.cs in the Soluling Backend repo).
  The source to update is picked by file name (-SourceName, defaults to the leaf
  name of -SourceFile); if the project has exactly one source that one is used.
  This is the counterpart of CreateProject.ps1, which seeds a *new* project from
  a source file.
#>

[CmdletBinding()]
param(
    [string] $ApiUrl           = 'https://www.soluling.com/api',
    # [string] $ApiUrl           = 'http://localhost:52104',
    [string] $User             = '',
    [string] $ProjectName      = '',
    # Relative paths resolve against the caller's current directory.
    [string] $SourceFile       = '',
    # Name of the project source to replace; '' matches by the source file's leaf name.
    [string] $SourceName       = ''
)

$ErrorActionPreference = 'Stop'
[Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12 -bor `
    [Net.ServicePointManager]::SecurityProtocol

# --- Ask for values not passed in. --------------------------------------
if (-not $SourceFile) {
    $answer = Read-Host 'Source file [Version2\Sport.ini]'
    $SourceFile = if ($answer) { $answer } else { 'Version2\Sport.ini' }
}
if (-not $ProjectName) {
    $answer = Read-Host 'Project name or guid [Sport]'
    $ProjectName = if ($answer) { $answer } else { 'Sport' }
}

# Resolve a relative source path against the caller's current directory.
if (-not [System.IO.Path]::IsPathRooted($SourceFile)) {
    $SourceFile = Join-Path (Get-Location -PSProvider FileSystem).ProviderPath $SourceFile
}

if (-not (Test-Path -LiteralPath $SourceFile)) {
    throw "Source file not found: $SourceFile"
}
if (-not $SourceName) { $SourceName = [IO.Path]::GetFileName($SourceFile) }

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

$headers = @{ Authorization = "Bearer $($auth.token)" }

# --- 2. Resolve the project and the source to update. -----------------------
Write-Host "Looking up project `"$ProjectName`" ..."
$project = Invoke-RestMethod -Method Get -Uri "$ApiUrl/projects/$([Uri]::EscapeDataString($ProjectName))" `
    -Headers $headers
Write-Host "Found project id=$($project.id) guid=$($project.guid)"

$sources = @(Invoke-RestMethod -Method Get -Uri "$ApiUrl/projects/$($project.id)/sources" -Headers $headers)
if (-not $sources) { throw "Project `"$ProjectName`" has no source files." }

if ($sources.Count -eq 1) {
    $source = $sources[0]
} else {
    $source = $sources | Where-Object { $_.name -eq $SourceName } | Select-Object -First 1
}
if (-not $source) {
    throw "Project `"$ProjectName`" has no source named `"$SourceName`". Sources: $(($sources | ForEach-Object { $_.name }) -join ', ')"
}
Write-Host "Updating source id=$($source.id) name=`"$($source.name)`" from `"$SourceFile`" ..."

# --- 3. Upload the changed file bytes. ------------------------------------
$uri = "$ApiUrl/projects/$($project.id)/files/$([Uri]::EscapeDataString($source.name))"
$updated = Invoke-RestMethod -Method Put -Uri $uri `
    -Headers $headers `
    -ContentType 'application/octet-stream' `
    -InFile $SourceFile

$rows = if ($null -ne $updated.rowCount) { $updated.rowCount } else { $updated.count }
Write-Host "Updated source `"$($source.name)`"$(if ($null -ne $rows) { " - $rows row(s)" })."
