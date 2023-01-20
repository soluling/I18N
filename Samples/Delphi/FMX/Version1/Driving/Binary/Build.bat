rem "c:\program files (x86)\embarcadero\studio\22.0\bin\paclient.exe" -u8 --Clean="Binary.app,D:\NT\Deploy\Samples\Delphi\FMX\Driving\Binary\Clean.txt" 


rem "c:\program files (x86)\embarcadero\studio\22.0\bin\paclient.exe" -u8 --put="c:\program files (x86)\embarcadero\studio\22.0\bin\delphi_PROJECTICNS.icns,Binary.app\Contents\Resources\,1,Binary.icns" Minimac 
rem "c:\program files (x86)\embarcadero\studio\22.0\bin\paclient.exe" -u8 --put="OSX64\Release\Binary.entitlements,Binary.app\..\,1,Binary.entitlements" Minimac 
rem "c:\program files (x86)\embarcadero\studio\22.0\bin\paclient.exe" -u8 --put="OSX64\Release\Binary,Binary.app\Contents\MacOS\,1,Binary" Minimac 
rem "c:\program files (x86)\embarcadero\studio\22.0\bin\paclient.exe" -u8 --put="OSX64\Release\Binary.info.plist,Binary.app\Contents\,1,Info.plist" Minimac 
rem "c:\program files (x86)\embarcadero\studio\22.0\bin\paclient.exe" -u8 --codesign="Binary.app,'-',Binary.app\..\Binary.entitlements" Minimac 


"c:\program files (x86)\embarcadero\studio\22.0\bin\paclient.exe" -u8 --put="c:\program files (x86)\embarcadero\studio\22.0\bin\delphi_PROJECTICNS.icns,fi\Binary.app\Contents\Resources\,1,Binary.icns" Minimac 
rem "c:\program files (x86)\embarcadero\studio\22.0\bin\paclient.exe" -u8 --put="OSX64\Release\Binary.entitlements,fi\Binary.app\..\,1,Binary.entitlements" Minimac 
rem "c:\program files (x86)\embarcadero\studio\22.0\bin\paclient.exe" -u8 --put="OSX64\Release\fi\Binary,fi\Binary.app\Contents\MacOS\,1,Binary" Minimac 
rem "c:\program files (x86)\embarcadero\studio\22.0\bin\paclient.exe" -u8 --put="OSX64\Release\Binary.info.plist,fi\Binary.app\Contents\,1,Info.plist" Minimac 
rem "c:\program files (x86)\embarcadero\studio\22.0\bin\paclient.exe" -u8 --codesign="Binary.app,'-',fi\Binary.entitlements" Minimac 
