@echo off
rem regxpcom -x "C:\Program Files\Mozilla Firefox" "C:\Program Files\Mozilla Firefox\components\xxmGeckoDev.dll"
copy xxmGeckoDev.xpt "C:\Program Files\Mozilla Firefox\components"
cd %APPDATA%\Mozilla\Firefox\Profiles
del /s xpti.dat
del /s compreg.dat
pause