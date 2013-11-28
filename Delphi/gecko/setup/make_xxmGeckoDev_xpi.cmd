@echo off
rem copy "c:\Program Files\Mozilla Firefox\components\xxmGeckoDev.dll" "xxmGeckoDev\components\" /y
copy "..\..\bin\xxmGeckoDev.dll" "xxmGeckoDev\components\" /y
del ..\..\bin\xxmGeckoDev.xpi
xcopy ..\..\bin\proto\*.* xxmGeckoDev\components\proto\ /y
xcopy ..\..\bin\public\*.* xxmGeckoDev\components\public\ /y
cd xxmGeckoDev
"C:\Program Files\7-Zip\7z.exe" a -r -tzip -x!.svn -x!_svn ..\..\..\bin\xxmGeckoDev.xpi *.*
pause