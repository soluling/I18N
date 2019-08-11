cd Win32\Debug
del /Q Deploy\*.*
copy *.exe Deploy
AddResource.exe Deploy\SingleFileDeploy.exe SingleFileDeploy.fi
AddResource.exe Deploy\SingleFileDeploy.exe SingleFileDeploy.ja
..\..