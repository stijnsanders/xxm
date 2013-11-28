Though a xxmGeckoDev.xpi file is available for install,
when debugging the xxmGeckoDev.dpr project, it's
advised to install xxmGeckoDev from the source folder as an extension:

Open "%APPDATA%\Mozilla\Firefox\Profiles"
Open your profile directory
Open directory "extensions"

Create a file named "xxmGeckoDev@xxm.sourceforge.net"
and edit it to contain the full path, including a trailing backslash, for example:

C:\MyCurrentProjects\xxm\gecko\setup\xxmGeckoDev\

See also
https://developer.mozilla.org/en-US/docs/Building_an_Extension#Test