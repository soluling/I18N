del *.zip
zip -R ASPNET *.md *.ntp *.pdf *.sln *.csproj *.txt *.aspx *.cshtml *.cs *.resx *.js *.json *.ico *.css *.config *.png *.bat -x */bin/* -x */obj/* -x */packages/* -x */Scripts/* -x /Build.bat -x */appsettings.Production.json -x */web.config
