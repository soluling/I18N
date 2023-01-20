rem https://docs.microsoft.com/en-us/nuget/schema/target-frameworks#supported-frameworks

rem Build assemblies and XML document
rem C:\Windows\Microsoft.NET\Framework\v4.0.30319\msbuild.exe NET.sln /t:Rebuild /p:Configuration=Release
"C:\Program Files\Microsoft Visual Studio\2022\Professional\MSBuild\Current\Bin\msbuild.exe" NET.sln /t:Rebuild /p:Configuration=Release

rem Build help file
C:\Windows\Microsoft.NET\Framework\v4.0.30319\msbuild.exe NET.shfbproj

rem Make zip
del /S Tempo*.cs
del Net.zip
zip -R Net *.chm *.sln *.csproj *.cs *.resx *.xaml

rem Inc the NuGet package version
..\..\..\Desktop\Tools\IncXmlVer\IncXmlVer Soluling.nuspec Upload.bat

rem Make NuGet package
nuget pack Soluling.nuspec