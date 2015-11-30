# xxm

xxm enables you to create dynamic websites in Delphi, combining both Delphi and HTML in the source files. The project is compiled into a module, ready for use by a pluggable protocol handler in Internet Explorer, an ISAPI Extension, an Apache module, with HTTPAPI, with CGI or SCGI, or a stand-alone HTTP server. (Perhaps more in the future or by other parties.)

## How does it work?

<img src="Delphi/docs/overview1.png" align="right" hspace="8" width="397" height="424" />
An xxm project contains source-files that hold both HTML and Object Pascal source code. These are converted (e.g. by the auto-compile on refresh feature) into a plain Delphi project, which is compiled into a xxm Library (with the `.xxl` file extension). This xxm Library can be loaded by one of the xxm handlers:

* an IInternetProtocol implementation that plugs into Internet Explorer
* an ISAPI extension that plugs into IIS
* an Apache 2 module
* a process that uses HTTPAPI
* a stand-alone basic HTTP server
* an old-fashioned CGI application
* an SCGI server process
* ... (perhaps more later)

Most handlers are available in these forms:

* no extra's: the handler loads one or more xxl's and keeps them loaded until shutdown
* auto-compile: the handler checks if source-files are modified, and parses and compiles the project (great for development!)
* auto-update: the handler checks for a _`name`_`.xxu` file, if present requests are stalled until the xxl can get overwritten by the xxu file (great for live environments!)

To get started, [download](Delphi/docs/download.html) the binaries or follow the [get started screenshot tutorial](Delphi/docs/tutorial01/index.html).

[![bitcoin accepted](Delphi/docs/bitcoin.png)](bitcoin:1HuQtV2WY4PqSmuWyhmYDiqJbx3QX2mWX3)

[![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/stijnsanders/xxm?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)