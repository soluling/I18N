PasDoc -X --title "Soluling's VCL Classes" --define VER230 --output VclDoc --format htmlhelp --introduction=VclIntro.txt --visible-members protected,public --implicit-visibility=implicit  Nt*.pas

cd VclDoc

"C:\Program Files (x86)\HTML Help Workshop\hhc.exe" docs.hhp

copy docs.chm ..\Vcl.chm

cd..