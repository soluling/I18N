# Cloud Automation Using the REST API

This folder contains sample files to automate cloud operation using the REST API. This folder contains two ways to automate. First are PowerShell scripts and another are GitHub actions.

## PowerShell scripts and GitHubActions

`PowerShell` folder contains PowerShell scripts and `GitHub` contains GitHub actions to create, update and delete Soluling cloud projects.

| PowerShell script file | GitHub action file   | Description                                                  |
| ---------------------- | -------------------- | ------------------------------------------------------------ |
| `CreateProject.ps1`    | `create-project.yml` | Creates a new Soluling project and uploads a source file. You can select what file but the default is `Sport.ini`. |
| `GetFiles.ps1`         | `get-files.yml`      | Get localized files from Soluling cloud to `Localized` folder. |
| `UpdateSource.ps1`     | `update-source.yml`  | Updates the source file in Soluling cloud project.           |
| `DeleteProject.ps1`    | `delete-project.yml` | Deletes a Soluling cloud project.                            |

## Ini

A sample INI file that contains sports. `Version2` folder contains a modified file that adds water polo to the sport list. `Localized` folder contains the localized Ini file. `GetFiles.ps1` saves them there.