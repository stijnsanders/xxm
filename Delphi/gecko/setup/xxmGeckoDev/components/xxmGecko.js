"use strict";
let Cc = Components.classes;
let Ci = Components.interfaces;
let Cu = Components.utils;
Cu.import("resource://gre/modules/XPCOMUtils.jsm");
Cu.import("resource://gre/modules/AddonManager.jsm");
Cu.import("resource://gre/modules/FileUtils.jsm");
let console = (Cu.import("resource://gre/modules/devtools/Console.jsm",{})).console;

function xxmGeckoListener(l){
	this._listener=l;
}
xxmGeckoListener.prototype={
	classDescription:"xxmGeckoListener",
	QueryInterface:XPCOMUtils.generateQI(
		[Ci.nsIRequestObserver
		,Ci.nsIStreamListener
		]),
	//nsIRequestObserver
	onStartRequest:function(r,c){},
	onStopRequest:function(r,c,s){},
	//nsIStreamListener
	onDataAvailable:function(r,c,s,o,n){this._listener.onDataAvailable(r,c,s,o,n);}
};

var xxmGeckoLibPath=null;
var xxmUserAgent=null;

function xxmGeckoChannel(u){
	if(!xxmGeckoLibPath){
		AddonManager.getAddonByID("xxmGeckoDev@xxm.sourceforge.net",function(a){
			xxmGeckoLibPath=a.getResourceURI("components/xxmGeckoDev.dll").QueryInterface(Ci.nsIFileURL).file.path;
		});
		xxmUserAgent=Components.classes["@mozilla.org/network/protocol;1?name=http"].createInstance(Components.interfaces.nsIHttpProtocolHandler).userAgent;
		var t=Cc["@mozilla.org/thread-manager;1"].getService().currentThread;
		while(!xxmGeckoLibPath)t.processNextEvent(false);
	}
	this.URI=u;
	this.originalURI=u;
	this.name=u.spec;
	this._headers={};
	this._done=false;
}
xxmGeckoChannel.prototype={
	classDescription:"xxmGeckoChannel",
	classID:Components.ID("{78786B00-0002-0001-C000-000000000001}"),
	contractID:"@xxm.sourceforge.net/xxmGeckoChannel;1",
	QueryInterface:XPCOMUtils.generateQI(
		[Ci.nsIRequest
		,Ci.nsIChannel
		,Ci.nsIHttpChannel
		//,Ci.nsIHttpChannelInternal
		//,Ci.nsIUploadChannel
		//,Ci.nsIUploadChannel2?
		//,Ci.nsIAsyncVerifyRedirectCallback
		]),
	//nsIRequest
	loadFlags:null,
	loadGroup:null,
	name:null,
	status:0,//NS_OK,
	cancel:function(aStatus){
console.log(this.name+" cancel "+aStatus);	
		this._done=true;
		//if(this._pump)this._pump.cancel(aStatus);
		//this._pump=null;
		if(this._pipe)this._pipe.close();
		this._pipe=null;
		//TOOD: interrupt worker?
	},
	isPending:function(){
console.log(this.name+" pending:"+this._done);	
		return !this._done;
	},
	resume:function(){},
	suspend:function(){},
	//nsIChannel
	contentCharset:null,
	contentLength:null,
	contentType:null,
	notificationCallbacks:null,
	originalURI:null,
	owner:null,
	securityInfo:null,
	URI:null,
	asyncOpen:function(aListener,aContext){
try{
		if(this.loadGroup)this.loadGroup.addRequest(this,null);
		this._listener=aListener;
		this._context=aContext;
		this._worker=new ChromeWorker("chrome://xxm/content/xxmGeckoW.js");
		this._worker.onmessage=function(e){
console.log(this.name+":xxm_"+e.data.toSource());try{
			(this["xxm_"+e.data.x]||function(xx){
				throw new Error("xxm: unknown message \""+xx.x+"\"");
			}).bind(this)(e.data);
}catch(ex){console.log(this.name+":xxm_"+e.data.toSource()+":"+ex);}
		}.bind(this);
		this._worker.postMessage({
			lib:xxmGeckoLibPath,
			uri:this.URI.spec,
			verb:this.requestMethod,
			referer:this.referer&&this.referer.spec,
			userAgent:xxmUserAgent
		});
}catch(ex){console.log(this.name+" "+ex);}
	},
	open:function(){},
	//nsIHttpChannel
	requestMethod:null,
	referer:null,
	getRequestHeader:function(header){
		if((header.toLowerCase()=="referer")&&this.referer)return this.referer.spec;
console.log(this.name+" getRequestHeader "+header);	
		//TODO:
	},
	setRequestHeader:function(header,value,merge){
console.log(this.name+" setRequestHeader "+header+"="+value);	
	},
	visitRequestHeaders:function(visitor){
	},
	allowPipelining:false,//?
	redirectionLimit:32,//?
	responseStatus:200,
	responseStatusText:"OK",
	requestSucceeded:true,
	getResponseHeader:function(header){
console.log(this.name+" getResponseHeader "+header);
		return this._headers[header.toLowerCase()];
	},
	setResponseHeader:function(header,value,merge){
		//TODO: if(!merge)
		this._headers[header.toLowerCase()]=value;
console.log(this.name+" setResponseHeader "+header(merge?"":"+")+"="+value);
	},
	isNoStoreResponse:function(){return true;},
	isNoCacheResponse:function(){return true;},
	redirectTo:function(uri){
console.log(this.name+" redirectTo "+uri.spec);	
	},
	//nsIHttpChannelInternal
	//nsIUploadChannel
	//xxmGeckoW postMessage feedback
	xxm_open:function(x){
		this._pipe=Cc["@mozilla.org/network/file-input-stream;1"].createInstance(Ci.nsIFileInputStream);
		this._pipe.init(new FileUtils.File("\\\\.\\pipe\\"+x.prefix+"_B"),-1,-1,0);
		this._pump=Cc["@mozilla.org/network/input-stream-pump;1"].createInstance(Ci.nsIInputStreamPump);
		this._pump.init(this._pipe,-1,-1,0,0,true);
		this._pump.asyncRead(new xxmGeckoListener(this._listener),null);
	},
	xxm_start:function(x){
		this.responseStatus=x.status;
		this.responseStatusText=x.statusText;
		//this._headers=x.headers; //TODO merge!
		this.contentLength=x.headers["content-length"];
		this.contentType=x.headers["content-type"];//TODO: split "; charset="
		this.contentCharset=x.headers["content-charset"];
		this._listener.onStartRequest(this,this._context);
	},
	xxm_data:function(x){
		// throws failures here, moved to xxmGeckoListener
		//this._listener.onDataAvailable(this,this._context,this._pipe,0,x.n);
	},
	xxm_redir:function(x){
		//x.url if(x.rel)
		//NS_BINDING_REDIRECTED=$804B000A;//1 shl 31 or ($45+6) shl 16 or 10
		//NS_ERROR_REDIRECT_LOOP=$804B0020;//1 shl 31 or ($45+6) shl 16 or 32
		//TODO: PromptTempRedirect?
		//FRedirectionLimit
		/*
  x:=NewCString;
  u:=FURI.Clone;
  if Relative then
    FURI.Resolve(NewCString(RedirectURL).ACString,x.ACString)
  else
    x.Assign(RedirectURL);
  u.SetSpec(x.ACString);
  FRedirectChannel:=NS_GetIOService.NewChannelFromURI(u);
  FStatus:=integer(NS_BINDING_REDIRECTED);
  FReports.State:=csRedirect;
  
var
  h:nsIHttpChannel;
  hi:nsIHttpChannelInternal;
  uc:nsIUploadChannel;
  inst:nsIInputStream;
begin
{
  try
    //http://mxr.mozilla.org/mozilla2.0/source/netwerk/base/src/nsBaseChannel.cpp#107
    FRedirectChannel.OriginalURI:=nil;//FOrigURI;
//    FRedirectChannel.LoadGroup:=FLoadGroup;
//    FRedirectChannel.NotificationCallbacks:=FCallBacks;
//    FRedirectChannel.LoadFlags:=FLoadFlags or LOAD_REPLACE;
    if FRedirectChannel.QueryInterface(nsIHttpChannel,h)=S_OK then
     begin
      //h.SetRequestMethod(NewCString('GET').ACString);//FVerb?
      //if FReferer<>nil then h.Referrer:=FReferer;
      h.AllowPipelining:=FAllowPipelining;
      h.RedirectionLimit:=FRedirectionLimit-1;
      h:=nil;
     end;

    //if FVerb='POST'?
    if FRedirectChannel.QueryInterface(nsIUploadChannel,uc)=S_OK then
     begin
      if FPostData<>nil then
       begin
        inst:=(FPostData as TxxmGeckoUploadStream).InputStream;
        (inst as nsISeekableStream).seek(NS_SEEK_SET,0);
        uc.SetUploadStream(inst,NewCString('').ACString,-1);
       end;
      uc:=nil;
     end;

    if FRedirectChannel.QueryInterface(nsIHttpChannelInternal,hi)=S_OK then
     begin
      hi.SetDocumentURI(nil);//FDocURI);
      hi:=nil;
     end;

    //nsIEncodedChannel?
    //nsIResumableChannel?
    //nsIWritablePropertyBag, CopyProperties?

    try
//      if FCallBacks<>nil then
//        (FCallBacks as nsIChannelEventSink).onChannelRedirect(Self,
//          FRedirectChannel,REDIRECT_PERMANENT,Self);//temporary?
    except
      //silent?
    end;

    FRedirectChannel.AsyncOpen(Listener,Context);
  finally
    FRedirectChannel:=nil;
  end;
}
  
		*/
	},
	xxm_error:function(x){
		this.xxm_final(x);
		throw x.ex;
	},
	xxm_final:function(x){
		//TODO: redirect?
		this._listener.onStopRequest(this,this._context,this.status);
		/*
try{
		this._listener.onStopRequest(this,this._context,this.status);
		this._done=true;
		this._listener=null;
		this._context=null;
		if(this.loadGroup)this.loadGroup.removeRequest(this,null,0);
console.log(this.name+" final-");}catch(ex){console.log(this.name+" final!"+ex);}		
		*/
	}
};

function xxmGecko(){}
xxmGecko.prototype={
	classDescription:"xxmGecko",
	classID:Components.ID("{78786D00-0000-0010-C000-000000000010}"),
	contractID:"@mozilla.org/network/protocol;1?name=xxm",
	QueryInterface:XPCOMUtils.generateQI(
		[Ci.nsIProtocolHandler
		]),
	//nsIProtocolHandler
	scheme:"xxm",
	defaultPort:80,//?
	protocolFlags:
		Ci.nsIProtocolHandler.URI_LOADABLE_BY_ANYONE|
		Ci.nsIProtocolHandler.URI_NON_PERSISTABLE,
	allowPort:function(p,s){return true;},
	newURI:function(s,c,b){
		var u=Cc["@mozilla.org/network/standard-url;1"].createInstance(Ci.nsIStandardURL);
		u.init(Ci.nsIStandardURL.URLTYPE_STANDARD,this.defaultPort,s,c,b);
		return u.QueryInterface(Ci.nsIURI);
	},
	newChannel:function(u){
		return new xxmGeckoChannel(u);//.QueryInterface(Ci.nsIChannel);//nsIHttpChannel?
	}
};

var NSGetFactory=XPCOMUtils.generateNSGetFactory([xxmGecko,xxmGeckoChannel]);