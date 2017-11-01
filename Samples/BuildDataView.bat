cd ..\DataView
\Tools\DXBerlin\bin\dcc32.exe -NSWinapi;System;System.Win;Vcl;Vcl.Imaging;Data DataView.dpr
cd ..\Samples

del *.zip
copy ..\DataView\*.exe .
zip Samples *.exe