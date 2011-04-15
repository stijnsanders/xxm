Though a xxmGeckoDev.xpi file is available for install,
when debugging the xxmGeckoDev.dpr project, it's
advised to install xxmGeckoDev.dll with a registry entry.

Open "regedit", locate this key:

HKEY_LOCAL_MACHINE\SOFTWARE\Mozilla\Firefox\Extensions

And add an item of type string, with this name:

xxmGeckoDev@xxm.sourceforge.net

and the directory that holds the files that are in the xpi package as well, for example:

C:\MyCurrentProjects\xxm\gecko\setup\xxmGeckoDev


