Though a xxmGeckoDev.xpi file is available for install,
when debugging the xxmGeckoDev.dpr project, it's
advised to install xxmGeckoDev.dll 'the hard way'
to ensure firefox.exe loads the same DLL file
as the debugger is listening for:

- Put "xxmGeckoDev.dll" and "xxmGeckoDev.xpt" in the
  "components" folder of the FireFox folder, typically
  "C:\Program Files\Mozilla FireFox\components".
  (The project's output path is already set to this folder,
  so a build or compile should do).
  (The xpt is actually an empty XPCOM type library, since
  xxmGecko doesn't introduce new interfaces.)

- Add "xxmGeckoDev.dll" and  to the
  "C:\Program Files\Mozilla Firefox\components\components.list"
  file.

- Delete "xpti.dat" and "compreg.dat" from your FireFox profile
  folder. (Keep a backup copy to be on the safe side.)
  It's typically something like this:
  "C:\Documents and Settings\UserName\Application Data\Mozilla\Firefox\Profiles\xxxxxxxx.default\"
  (Replace UserName with your own account's username, and 
  xxxxxxxx is typically some code with seemingly random 
  characters.)

- Start FireFox, it will re-create "xpti.dat" and "compreg.dat"
  files, containing xxmGeckoDev.dll XPCOM registry.