#Requires -Version 5.1
<#
.SYNOPSIS
  Deletes a Soluling cloud project.
.DESCRIPTION
  POST /users/authenticate to get a JWT, then:
    * GET    /projects/{name} - resolve the project id
    * DELETE /projects/{id}   - delete the project and everything in it
      (see ProjectController.cs in the Soluling Backend repo).
  This is the counterpart of CreateProject.ps1. The delete is irreversible, so
  the script asks for confirmation unless -Force is given.
.EXAMPLE
  .\DeleteProject.ps1
.EXAMPLE
  .\DeleteProject.ps1 -ProjectName Sport -Force
#>

[CmdletBinding(SupportsShouldProcess = $true, ConfirmImpact = 'High')]
param(
    [string] $ApiUrl           = 'https://www.soluling.com/api',
    # [string] $ApiUrl           = 'http://localhost:52104',
    [string] $User             = '',
    # Project name or guid. '' prompts for it (default 'Sport').
    [string] $ProjectName      = '',
    # Skip the confirmation prompt.
    [switch] $Force
)

$ErrorActionPreference = 'Stop'
[Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12 -bor `
    [Net.ServicePointManager]::SecurityProtocol

# --- Ask for the project unless it was passed in. ------------------------
if (-not $ProjectName) {
    $answer = Read-Host 'Project name or guid [Sport]'
    $ProjectName = if ($answer) { $answer } else { 'Sport' }
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

# --- 2. Resolve the project. -----------------------------------------------
Write-Host "Looking up project `"$ProjectName`" ..."
$project = Invoke-RestMethod -Method Get -Uri "$ApiUrl/projects/$([Uri]::EscapeDataString($ProjectName))" `
    -Headers $headers
Write-Host "Found project id=$($project.id) guid=$($project.guid)"

# --- 3. Delete it. -------------------------------------------------------
$target = "project `"$ProjectName`" (id=$($project.id))"
# if (-not $Force -and -not $PSCmdlet.ShouldProcess($target, 'DELETE')) { return }
if (-not $Force -and -not $PSCmdlet.ShouldContinue("Permanently delete $target and all its translations?", 'Confirm delete')) {
    return
}

Invoke-RestMethod -Method Delete -Uri "$ApiUrl/projects/$($project.id)" -Headers $headers | Out-Null
Write-Host "Deleted $target."
