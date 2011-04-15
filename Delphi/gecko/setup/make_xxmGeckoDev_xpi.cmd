@echo off
rem copy "c:\Program Files\Mozilla Firefox\components\xxmGeckoDev.dll" "xxmGeckoDev\platform\WINNT\components\" /y
del ..\..\bin\xxmGeckoDev.xpi
xcopy ..\..\bin\proto\*.* xxmGeckoDev\platform\WINNT\components\proto\ /y
cd xxmGeckoDev
"C:\Program Files\7-Zip\7z.exe" a -r -tzip -x!.svn -x!_svn ..\..\..\bin\xxmGeckoDev.xpi *.*
pause