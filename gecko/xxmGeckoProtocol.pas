unit xxmGeckoProtocol;

interface

uses nsXPCOM, xxmGeckoModule, nsTypes, nsGeckoStrings;

type
  TxxmProtocolHandler=class(TxxmGeckoComponent, nsIProtocolHandler)
  public
    procedure GetScheme(aScheme: nsACString); safecall;
    function AllowPort(port: Integer; const scheme: PAnsiChar): LongBool;
      safecall;
    function GetDefaultPort: Integer; safecall;
    function GetProtocolFlags: Cardinal; safecall;
    function NewChannel(aURI: nsIURI): nsIChannel; safecall;
    function NewURI(const aSpec: nsACString;
      const aOriginCharset: PAnsiChar; aBaseURI: nsIURI): nsIURI; safecall;
  end;

  //TODO: nsIFileProtocolHandler?

const
  CID_xxmProtocolHandler: TGUID = '{78786D00-0000-000C-C000-00000000000C}';

implementation

uses
  xxmGeckoChannel, xxmGeckoInterfaces;

{ TxxmProtocolHandler }

procedure TxxmProtocolHandler.GetScheme(aScheme: nsACString);
begin
  NewCString(aScheme).Assign('xxm');
end;

function TxxmProtocolHandler.GetDefaultPort: Integer;
begin
  Result:=-1;
end;

function TxxmProtocolHandler.GetProtocolFlags: Cardinal;
const
  URI_LOADABLE_BY_ANYONE=$40;//1 shl 6
begin
  Result:=URI_LOADABLE_BY_ANYONE;
end;

function TxxmProtocolHandler.AllowPort(port: Integer;
  const scheme: PAnsiChar): LongBool;
begin
  Result:=true;
end;

function TxxmProtocolHandler.NewURI(const aSpec: nsACString;
  const aOriginCharset: PAnsiChar; aBaseURI: nsIURI): nsIURI;
var
  u:nsIStandardURL;
begin
  Module.CompMgr.CreateInstanceByContractID('@mozilla.org/network/standard-url;1',nil,NS_ISTANDARDURL_IID,u);
  u.Init(URLTYPE_STANDARD,80,aSpec,aOriginCharset,aBaseURI);
  Result:=u as nsIURI;
end;

function TxxmProtocolHandler.NewChannel(aURI: nsIURI): nsIChannel;
begin
  Result:=TxxmChannel.Create(aURI) as nsIChannel;
end;

initialization
  RegisterComponent('xxm','@mozilla.org/network/protocol;1?name=xxm',CID_xxmProtocolHandler,TxxmProtocolHandler);
end.
