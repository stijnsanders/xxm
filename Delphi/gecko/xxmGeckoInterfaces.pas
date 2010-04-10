unit xxmGeckoInterfaces;

interface

uses nsXPCOM, nsTypes, nsGeckoStrings;

const
  NS_OK = 0;
  NS_ERR = 1;
  NS_NOENT = 2;

  NS_IMUTABLE_IID:TGUID='{321578d0-03c1-4d95-8821-021ac612d18d}';
  NS_ISTANDARDURL_IID:TGUID='{babd6cca-ebe7-4329-967c-d6b9e33caa81}';
  NS_IHTTPCHANNELINTERNAL_IID:TGUID='{3ce040fb-3933-462a-8d62-80b78fbd0809}';

  URLTYPE_STANDARD        = 1;
  URLTYPE_AUTHORITY       = 2;
  URLTYPE_NO_AUTHORITY    = 3;

  NS_SEEK_SET = 0;
  NS_SEEK_CUR = 1;
  NS_SEEK_END = 2;

  REDIRECT_TEMPORARY  = $1;
  REDIRECT_PERMANENT  = $2;
  REDIRECT_INTERNAL   = $4;

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
    procedure Init(aUrlType, aDefaultPort: PRInt32;aSpec: nsACString;
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

  nsIChannelEventSink = interface(nsISupports)
  ['{6757d790-2916-498e-aaca-6b668a956875}']
    //const REDIRECT_* see above
    procedure onChannelRedirect(oldChannel: nsIChannel;
      newChannel: nsIChannel; flags: PRUint32); safecall;
  end;

  nsIHttpChannelInternal = interface(nsISupports)
  ['{3ce040fb-3933-462a-8d62-80b78fbd0809}']
    function GetDocumentURI: nsIURI; safecall;
    procedure SetDocumentURI(aDocumentURI: nsIURI); safecall;
    procedure getRequestVersion(var major:PRUint32; var minor:PRUint32); safecall;
    procedure getResponseVersion(var major:PRUint32; var minor:PRUint32); safecall;
    procedure setCookie(aCookieHeader:PAnsiChar); safecall;//string?
    procedure setupFallbackChannel(aFallbackKey:PAnsiChar); safecall;//string?
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
