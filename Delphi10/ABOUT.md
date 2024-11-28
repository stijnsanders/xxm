xxm 2.0
=======

**IN PROGRESS**

This is the second iteration of the xxm project. It may replace the main project when the first milestones are met.

Motivation
----------

The xxm project started in 2007. Having worked with PHP, Cold Fusion and (ancient) ASP; having worked with IIS’ ISAPI, CGI, SCGI and Apache httpd modules; having (succesfully!) implemented Internet Explorer’s IInternetProtocol, and FireFox’ nsIInternetProtocol; the main idea was to create a central common interface so binaries/libraries using it could run in all of these environments, be built by coding server-side logic and client-side HTML in the same files by hitting refresh to run and test — replacing a scripting engine with the raw speed of the Delphi compiler — and have a quick and silent auto-update on live servers when you publish new binaries/libraries to update your websites.

A lot has changed. The idea that Internet Explorer would keep it’s 90%+ market share seems ludacrous now. FireFox moved well away from XPCOM interfaces. The idea of having adresses that start with "xxm:" above your web-pages are a distant memory now. (And a security no-no.) Even worse is that I once thought to get those from auto-run when you enter a CD-ROM on a Windows machine!

What has proven its worth in the mean time is the core idea. Several projects show that building elaborate dynamic websites on xxm is possible, and offers a reliable solution that is somewhat comfortable to develop on. The focus on security has strengthened — though it still is a shared responsability of all involved developers — but you should find xxm a helping hand, not a loaded gun ready to shoot you in the foot. (I count on people not to string concatenate raw user input into SQL, HTML or JSON. You know who you are.)

It is time to let go of XML. JSON is as versatile without the baggage of layers of protocols that got in the way anyway. It is time to let go of COM. It served its purpose, but in case of xxm it offers no benefits and sits in the way of really pushing for maximum performance. The xxm interface itself has seen little change over the years, so it’s safe to take a step back and fall back to raw procedure and function pointers. (If I understood correctly, also the weapon of choice of file system implementations in the Linux ecosphere.) It should allow xxm to avoid some overhead, and also support binaries build in other programming languages.

The ‘DLL sandwich’ idea
-----------------------

A first idea was to have a simple as-small-as-possible `xxm.dll` that would export procedures you would call from your xxm projects. Since xxm handlers need to load your xxm libraries, `xxm.dll` would need to ‘look two layers up’ to make things work. I played around with a proof of concept of this, and a ‘DLL sandwich’ setup _could_ work, but a dependency on this central DLL could get ugly quick, and I hope by passing _direct_ function/procedure pointers on module startup xxm can avoid the dll export mapping mechanic the system does behind the scene for you. I hope to get just a tiny performance gain there, and also hope any additional pointers added with newer versions can get added to the end gracefully.

Milestones
----------

First step: create a new xxmHttp as a ‘reference implementation’ of the interface, have it ready to respond to basic HTTP requests.

Then — from scratch — create a new xxmProto as a first _consumer_ of this new interface. Use it to build and test the new project registry and module loader in xxmHttp.

Next step is to start the new xxmConv that parses xxm files into Delphi project files, based on `proto` files modeled after the xxmProto project.

(Maybe it’s also time to do something fancy and new with `xxmFReg.pas` and have it work based on something better than a modest TStringList...)

Then it’s time to create xxmHttpDev and xxmHttpAU versions that auto-compile and auto-update.

Putting those to work on the demo projects, should see all features get enabled and give some experience with more projects on this new version.

Following those are creating the other handlers: xxmIsapi, xxmAhttpd, xxmSCGI, xxmHSys. (Mayby finally try xxmFastCGI _again_ if I happen to find out what named pipe it’s supposed to use. And who knows a modern style [IIS Module](https://learn.microsoft.com/en-us/iis/get-started/introduction-to-iis/iis-modules-overview).) Also give xxmProject a do-over, though it probably won’t need much updating.

Last but not least, a ‘compatibility layer’ is needed, that still implements the venerable `IXxmContext` interface and patches all calls through to the new interface, to allow existing xxm ‘1.0’ modules to work out of the box.

With all that in place it may be time to replace the 'main' xxm project with this new xxm 2.0, and let the world know.

But it’ll take some time and a long period of slow progress before we get there. Wish me luck.