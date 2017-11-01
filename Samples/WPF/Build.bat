del *.zip
del /S TemporaryGenerated*.cs
del /S *.g.cs
del /S *.g.i.cs
del /S *.vshost.exe
zip -R Wpf *.ntp *.sln *.csproj *.cs *.resx *.xaml *.config *.snk *.pfx *.png *.ico bin\Release\*.exe bin\Release\*.dll