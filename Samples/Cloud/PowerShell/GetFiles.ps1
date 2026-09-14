#Requires -Version 5.1
<#
.SYNOPSIS
  Downloads a Soluling cloud project's built localized files and saves them locally.
.DESCRIPTION
  POST /users/authenticate to get a JWT, then:
    * GET /projects/{name}                     - resolve the project id
    * GET /projects/{id}/languages             - list target languages (unless -Languages given)
    * POST /projects/{id}/build                - optional, with -Build: queue a fresh build and wait
    * GET /projects/{id}/sources               - list the top-level source files
    * GET /sources/{id}/artifacts/{language}   - download each source's localized file per language
  Each localized file is written to <OutDir>\<language>\<file> (see ProjectController.cs
  GetLocalizedFile in the Soluling Backend repo).
#>

[CmdletBinding()]
param(
    [string] $ApiUrl           = 'https://www.soluling.com/api',
    # [string] $ApiUrl           = 'http://localhost:52104',
    [string] $User             = '',
    # Project name or guid. '' prompts for it (default 'Sport').
    [string] $ProjectName      = '',
    # Semicolon-separated language codes to fetch; '' fetches every target language of the project.
    [string] $Languages        = '',
    # Relative paths resolve against the caller's current directory.
    [string] $OutDir           = 'Localized',
    # Queue a fresh build first and wait for it before downloading.
    # On by default; pass -Build:$false to download the existing build as-is.
    [bool]   $Build            = $true,
    [int]    $BuildTimeoutSec  = 300
)

$ErrorActionPreference = 'Stop'
[Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12 -bor `
    [Net.ServicePointManager]::SecurityProtocol

# --- Ask for the project unless it was passed in. ------------------------
if (-not $ProjectName) {
    $answer = Read-Host 'Project name or guid [Sport]'
    $ProjectName = if ($answer) { $answer } else { 'Sport' }
}

# Resolve a relative output path against the caller's current directory.
if (-not [System.IO.Path]::IsPathRooted($OutDir)) {
    $OutDir = Join-Path (Get-Location -PSProvider FileSystem).ProviderPath $OutDir
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

$headers = @{ Authorization = "Bearer $($auth.token)" }

# --- 2. Resolve the project and the languages to fetch. ----------------------
Write-Host "Looking up project `"$ProjectName`" ..."
$project = Invoke-RestMethod -Method Get -Uri "$ApiUrl/projects/$([Uri]::EscapeDataString($ProjectName))" `
    -Headers $headers
Write-Host "Found project id=$($project.id) guid=$($project.guid)"

if ($Languages) {
    $langs = $Languages.Split(';', [StringSplitOptions]::RemoveEmptyEntries) |
        ForEach-Object { $_.Trim() } | Where-Object { $_ }
} else {
    $langs = Invoke-RestMethod -Method Get -Uri "$ApiUrl/projects/$($project.id)/languages" -Headers $headers |
        ForEach-Object { $_.code } |
        Where-Object { $_ -and ($_ -ne $project.originalLanguage) }
}

if (-not $langs) { throw "No languages to fetch for project `"$ProjectName`"." }
Write-Host "Languages: $($langs -join ', ')"

# --- 3. Optionally queue a build and wait for it. ---------------------------
if ($Build) {
    $langQuery = [Uri]::EscapeDataString($langs -join ';')
    Write-Host "Queuing build ..."
    $action = Invoke-RestMethod -Method Post -Headers $headers `
        -Uri "$ApiUrl/projects/$($project.id)/build?languages=$langQuery&publish=true"

    $deadline = (Get-Date).AddSeconds($BuildTimeoutSec)
    $percent  = 0
    do {
        Start-Sleep -Seconds 2
        try {
            $percent = Invoke-RestMethod -Method Get -Headers $headers `
                -Uri "$ApiUrl/fileactions/$($action.id)/progress/percent"
        } catch {
            $percent = 0   # 404 = no progress recorded yet; keep waiting.
        }
        Write-Host ("  build {0:N0}%" -f [double]$percent)
    } while (([double]$percent -lt 100) -and ((Get-Date) -lt $deadline))

    if ([double]$percent -lt 100) {
        throw "Build did not finish within $BuildTimeoutSec seconds."
    }
    Write-Host 'Build complete.'
}

# --- 4. Download each source's localized file per language. ----------------
$sources = Invoke-RestMethod -Method Get -Uri "$ApiUrl/projects/$($project.id)/sources" -Headers $headers
if (-not $sources) { throw "Project `"$ProjectName`" has no source files." }

$null  = New-Item -ItemType Directory -Path $OutDir -Force
$saved = 0

foreach ($source in $sources) {
    foreach ($lang in $langs) {
        $uri     = "$ApiUrl/sources/$($source.id)/artifacts/$([Uri]::EscapeDataString($lang))"
        $langDir = Join-Path $OutDir $lang
        $null    = New-Item -ItemType Directory -Path $langDir -Force
        $tmp     = Join-Path $langDir ('.{0}.download' -f [Guid]::NewGuid())

        try {
            $resp = Invoke-WebRequest -Method Get -Uri $uri -Headers $headers -OutFile $tmp -PassThru
        } catch {
            $status = $_.Exception.Response.StatusCode.value__
            if ($status -eq 404) {
                Write-Warning "No build for '$($source.name)' [$lang] - run with -Build."
            } else {
                Write-Warning "Failed to download '$($source.name)' [$lang]: $($_.Exception.Message)"
            }
            if (Test-Path -LiteralPath $tmp) { Remove-Item -LiteralPath $tmp -Force }
            continue
        }

        # Prefer the server-supplied file name (Content-Disposition), else fall back
        # to "<source base>.<lang><ext>".
        $fileName = $null
        $cd = $resp.Headers['Content-Disposition']
        if ($cd -is [array]) { $cd = $cd[0] }
        if ($cd -and ($cd -match "filename\*?=(?:UTF-8'')?`"?([^`";]+)`"?")) {
            $fileName = [Uri]::UnescapeDataString($Matches[1]).Trim()
        }
        if (-not $fileName) {
            $base = [IO.Path]::GetFileNameWithoutExtension($source.name)
            $ext  = [IO.Path]::GetExtension($source.name)
            $fileName = "$base.$lang$ext"
        }

        $dest = Join-Path $langDir $fileName
        Move-Item -LiteralPath $tmp -Destination $dest -Force
        Write-Host "  saved $dest"
        $saved++
    }
}

Write-Host "Done. $saved localized file(s) written under `"$OutDir`"."
