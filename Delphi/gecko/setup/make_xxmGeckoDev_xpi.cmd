@echo off
copy "c:\Program Files\Mozilla Firefox\components\xxmGeckoDev.dll" "xxmGeckoDev\platform\WINNT_x86-msvc\components\" /y
del ..\..\bin\xxmGeckoDev.xpi
cd xxmGeckoDev
"C:\Program Files\7-Zip\7z.exe" a -r -tzip -x!.svn -x!_svn ..\..\..\bin\xxmGeckoDev.xpi *.*
pause