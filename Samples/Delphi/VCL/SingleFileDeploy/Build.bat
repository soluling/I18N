cd Win32\Debug
del /Q Deploy\*.*
copy *.exe Deploy
AddResource.exe Deploy\SingleFileDeploy.exe SingleFileDeploy.fi FI
AddResource.exe Deploy\SingleFileDeploy.exe SingleFileDeploy.ja JA
cd ..\..