rem Visual Studio 2026 (18): the projects target .NET 10, which the MSBuild of Visual Studio 2022 can't build
set MSBUILD=C:\Program Files\Microsoft Visual Studio\18\Insiders\MSBuild\Current\Bin\MSBuild.exe

rem https://docs.microsoft.com/en-us/nuget/schema/target-frameworks#supported-frameworks

rem Build assemblies and XML document
rem C:\Windows\Microsoft.NET\Framework\v4.0.30319\msbuild.exe NET.sln /t:Rebuild /p:Configuration=Release
"%MSBUILD%" NET.sln /t:Rebuild /p:Configuration=Release

rem Build help file
rme C:\Windows\Microsoft.NET\Framework\v4.0.30319\msbuild.exe NET.shfbproj
"%MSBUILD%" NET.shfbproj
copy /Y Help\NET.chm .

rem Make zip
del /S Tempo*.cs
del Net.zip
zip -R Net *.chm *.sln *.csproj *.cs *.resx *.xaml

rem Inc the NuGet package version
..\..\..\Desktop\Tools\IncXmlVer\IncXmlVer Soluling.nuspec Upload.bat

rem Make NuGet package
nuget pack Soluling.nuspec

rem Make the ASP.NET Core package, with the same version as Soluling.nuspec
for /f %%v in ('powershell -NoProfile -Command "([xml](Get-Content Soluling.nuspec)).package.metadata.version"') do set VERSION=%%v
nuget pack Soluling.AspNet.nuspec -Properties version=%VERSION%