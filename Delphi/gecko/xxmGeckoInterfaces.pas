unit xxmGeckoInterfaces;

interface

uses nsXPCOM, nsTypes, nsGeckoStrings;

const
  NS_OK = 0;
  NS_ERR = 1;
  NS_NOENT = 2;

  NS_ISTANDARDURL_CONTRACT='@mozilla.org/network/standard-url;1';
  NS_IHTTPPROTOCOLHANDLER_CONTRACT='@mozilla.org/network/protocol;1?name=http';
  NS_IINPUTSTREAMPUMP_CONTRACT='@mozilla.org/network/input-stream-pump;1';

  URLTYPE_STANDARD     = 1;
  URLTYPE_AUTHORITY    = 2;
  URLTYPE_NO_AUTHORITY = 3;

  NS_SEEK_SET = 0;
  NS_SEEK_CUR = 1;
  NS_SEEK_END = 2;

  REDIRECT_TEMPORARY  = $1;
  REDIRECT_PERMANENT  = $2;
  REDIRECT_INTERNAL   = $4;

  //load flags
  //from nsIRequest.idl
  LOAD_NORMAL = $00000000;
  LOAD_BACKGROUND = $00000001;//1 shl 0
  INHIBIT_CACHING = $00000080;//1 shl 7
  INHIBIT_PERSISTENT_CACHING = $00000100;//1 shl 8
  LOAD_BYPASS_CACHE = $00000200;//1 shl 9
  LOAD_FROM_CACHE = $00000400;//1 shl 10
  VALIDATE_ALWAYS = $00000800;//1 shl 11
  VALIDATE_NEVER = $00001000;//1 shl 12
  VALIDATE_ONCE_PER_SESSION = $00002000;//1 shl 13
  LOAD_ANONYMOUS = $00004000;//1 shl 14
  LOAD_FRESH_CONNECTION = $00008000;//1 shl 15
  //from nsIChannel.idl
  LOAD_DOCUMENT_URI = $00010000;//1 shl 16;
  LOAD_RETARGETED_DOCUMENT_URI = $00020000;//1 shl 17
  LOAD_REPLACE = $00040000;//1 shl 18
  LOAD_INITIAL_DOCUMENT_URI = $00080000;//1 shl 19
  LOAD_TARGETED = $00100000;//1 shl 20
  LOAD_CALL_CONTENT_SNIFFERS = $00200000;//1 shl 21
  LOAD_CLASSIFY_URI = $00400000;//1 shl 22

  DISPOSITION_INLINE = 0;
  DISPOSITION_ATTACHMENT = 1;

  //protocol flags, from nsIProtocolHandler.idl
  URI_STD = $00000000;
  URI_NORELATIVE = $00000001;//1 shl 0
  URI_NOAUTH = $00000002;//1 shl 1
  URI_INHERITS_SECURITY_CONTEXT = $00000010;//1 shl 4
  URI_FORBIDS_AUTOMATIC_DOCUMENT_REPLACEMENT = $00000020;//1 shl 5

  URI_LOADABLE_BY_ANYONE = $00000040;//1 shl 6
  URI_DANGEROUS_TO_LOAD = $00000080;//1 shl 7
  URI_IS_UI_RESOURCE = $00000100;//1 shl 8
  URI_IS_LOCAL_FILE = $00000200;//1 shl 9
  URI_LOADABLE_BY_SUBSUMERS = $00004000;//1 shl 14

  URI_NON_PERSISTABLE = $00000400;//1 shl 10
  URI_DOES_NOT_RETURN_DATA = $00000800;//1 shl 11
  URI_IS_LOCAL_RESOURCE = $00001000;//1 shl 12
  URI_OPENING_EXECUTES_SCRIPT = $00002000;//1 shl 13
  ALLOWS_PROXY = $00000004;//1 shl 2
  ALLOWS_PROXY_HTTP = $00000008;//1 shl 3
  URI_FORBIDS_COOKIE_ACCESS = $00008000;//1 shl 15

  NS_NETWORK_PROTOCOL_CONTRACTID_PREFIX='@mozilla.org/network/protocol;1?name=';

type
  nsIMutable = interface(nsISupports)
  ['{321578d0-03c1-4d95-8821-021ac612d18d}']
    function GetMutable(): PRBool; safecall;
    procedure SetMutable(aMutable: PRBool); safecall;
    property Mutable: PRBool read GetMutable write SetMutable;
  end;

  nsIStandardURL = interface(nsIMutable)
  ['{babd6cca-ebe7-4329-967c-d6b9e33caa81}']
    //const URLTYPE_STANDARD        = 1;
    //const URLTYPE_AUTHORITY       = 2;
    //const URLTYPE_NO_AUTHORITY    = 3;
    procedure Init(aUrlType, aDefaultPort: PRInt32; aSpec: nsACString;
      aOriginCharset: PAnsiChar; aBaseURI: nsIURI); safecall;
  end;

  nsISeekableStream = interface(nsISupports)
  ['{8429d350-1040-4661-8b71-f2a6ba455980}']
    //const NS_SEEK_* see above
    procedure seek(whence:PRUint32;offset:PRUint64); safecall;
    function tell:PRUint64; safecall;
    procedure setEOF(); safecall;
  end;

  nsIProgressEventSink = interface(nsISupports)
  ['{d974c99e-4148-4df9-8d98-de834a2f6462}']
    procedure onProgress(aRequest: nsIRequest; aContext: nsISupports;
      aProgress, aProgressMax: PRUint64); safecall;
    procedure onStatus(aRequest: nsIRequest; aContext: nsISupports;
      aStatus: NSRESULT; aStatusArg: PWideChar); safecall;//wstring?
  end;

  nsIAsyncVerifyRedirectCallback = interface(nsISupports)
  ['{8d171460-a716-41f1-92be-8c659db39b45}']
    procedure OnRedirectVerifyCallback(aResult: nsresult); safecall;
  end;

  nsIChannelEventSink = interface(nsISupports)
  ['{a430d870-df77-4502-9570-d46a8de33154}']
    //const REDIRECT_* see above
    procedure onChannelRedirect(oldChannel: nsIChannel;
      newChannel: nsIChannel; flags: PRUint32; callback: nsIAsyncVerifyRedirectCallback); safecall;
  end;

  nsIHttpChannelInternal = interface(nsISupports)
  ['{4b967b6d-cd1c-49ae-a457-23ff76f5a2e8}']
    function GetDocumentURI: nsIURI; safecall;
    procedure SetDocumentURI(aDocumentURI: nsIURI); safecall;
    procedure getRequestVersion(var major:PRUint32; var minor:PRUint32); safecall;
    procedure getResponseVersion(var major:PRUint32; var minor:PRUint32); safecall;
    procedure setCookie(aCookieHeader:PAnsiChar); safecall;//string?
    procedure setupFallbackChannel(aFallbackKey:PAnsiChar); safecall;//string?
    function GetForceAllowThirdPartyCookie: PRBool; safecall;
    procedure SetForceAllowThirdPartyCookie(aForceAllowThirdPartyCookie: PRBool); safecall;
    function GetCancelled: PRBool; safecall;
    function GetChannelIsForDownload: PRBool; safecall;
    procedure SetChannelIsForDownload(aChannelIsForDownload: PRBool); safecall;
    procedure GetLocalAddress(aLocalAddress: nsAUTF8String); safecall;
    function GetLocalPort: PRUint32; safecall;
    procedure GetRemoteAddress(aRemoteAddress: nsAUTF8String); safecall;
    function GetRemotePort: PRUint32; safecall;
    procedure setCacheKeysRedirectChain(cacheKeys:pointer); safecall;//StringArray:nsTArray<nsCString>
    procedure HTTPUpgrade(aProtocolName: nsACString; aListener: nsISupports); safecall; //nsIHttpUpgradeListener
    function GetAllowSpdy: PRBool; safecall;
    procedure SetAllowSpdy(aAllowSpdy: PRBool); safecall;
    property AllowSpdy: PRBool read GetAllowSpdy write SetAllowSpdy;
  end;

  nsIProtocolHandler = interface;
  nsIProxiedProtocolHandler = interface;
  nsIHttpProtocolHandler = interface;
  nsIProxyInfo = interface(nsISupports) end;
  nsIProtocolHandler = interface(nsISupports)
  ['{15fd6940-8ea7-11d3-93ad-00104ba0fd40}']
    procedure GetScheme(aScheme: nsACString); safecall;
    function GetDefaultPort(): PRInt32; safecall;
    property DefaultPort: PRInt32 read GetDefaultPort;
    function GetProtocolFlags(): PRUint32; safecall;
    property ProtocolFlags: PRUint32 read GetProtocolFlags;
    function NewURI(const aSpec: nsACString; const aOriginCharset: PAnsiChar; aBaseURI: nsIURI): nsIURI; safecall;
    function NewChannel(aURI: nsIURI): nsIChannel; safecall;
    function AllowPort(port: PRInt32; const scheme: PAnsiChar): PRBool; safecall;
  end;

  nsIProxiedProtocolHandler = interface(nsIProtocolHandler)
  ['{0a24fed4-1dd2-11b2-a75c-9f8b9a8f9ba7}']
    function NewProxiedChannel(uri: nsIURI; proxyInfo: nsIProxyInfo): nsIChannel; safecall;
  end;

  nsIHttpProtocolHandler = interface(nsIProxiedProtocolHandler)
  ['{9814fdf0-5ac3-11e0-80e3-0800200c9a66}']
    procedure GetUserAgent(aUserAgent: nsACString); safecall;
    procedure GetAppName(aAppName: nsACString); safecall;
    procedure GetAppVersion(aAppVersion: nsACString); safecall;
    procedure GetProduct(aProduct: nsACString); safecall;
    procedure GetProductSub(aProductSub: nsACString); safecall;
    procedure GetPlatform(aPlatform: nsACString); safecall;
    procedure GetOscpu(aOscpu: nsACString); safecall;
    procedure GetMisc(aMisc: nsACString); safecall;
  end;

procedure SetCString(x:nsACString;v:AnsiString);
function GetCString(const x:nsACString):AnsiString;

implementation

uses
  nsInit;

procedure SetCString(x:nsACString;v:AnsiString);
begin
  NS_CStringSetData(x,PAnsiChar(v),Length(v));
end;

function GetCString(const x:nsACString):AnsiString;
var
  l: Longword;
  p: PAnsiChar;
begin
  l:=NS_CStringGetData(x,p);
  SetLength(Result,l);
  Move(p^,PAnsiChar(Result)^,l);
end;

end.
