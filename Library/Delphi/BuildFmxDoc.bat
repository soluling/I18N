PasDoc -X --define VER230 --output FmxDoc --format htmlhelp --visible-members protected,public --implicit-visibility=implicit  Fmx*.pas NtBase.pas NtPlural.pas NtInitialLocale.pas NtTranslator.pas

cd FmxDoc

"C:\Program Files (x86)\HTML Help Workshop\hhc.exe" docs.hhp

copy docs.chm ..\Fmx.chm

cd..
