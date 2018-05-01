unit xxmSock;

interface

uses SysUtils, Classes;

{$D-}
{$L-}

type
  PSocketAddress=^TSocketAddress;
  TSocketAddress=record
    family: word;
    port: word;
    data: array[0..11] of word;
  end;

  THostEntry=record
    h_name:PAnsiChar;
    h_aliases:^PAnsiChar;
    h_addrtype:word;
    h_length:word;
    h_addr:^PAnsiChar;
    //TODO: IPv6
  end;
  PHostEntry = ^THostEntry;

  TFDSet = record
    fd_count: cardinal;
    fd_array: array[0..63] of THandle;
  end;
  PFDSet = ^TFDSet;

  TTimeVal = record
    tv_sec: cardinal;
    tv_usec: cardinal;
  end;
  PTimeVal = ^TTimeVal;

const
  INVALID_SOCKET = THandle(not(0));
  AF_INET = 2;
  AF_INET6 = 23;
  SOCKET_ERROR = -1;
  SOCK_STREAM = 1;
  IPPROTO_IP = 0;
  SOMAXCONN = 5;
  SOL_SOCKET = $FFFF;
  SO_REUSEADDR = $0004;
  SO_SNDBUF = $1001;
  SO_RCVBUF = $1002;
  SO_SNDTIMEO = $1005;
  SO_RCVTIMEO = $1006;
  SD_BOTH = 2;
  IPPROTO_TCP = 6;
  TCP_NODELAY = 1;

type
  TCredHandle=record
    dwLower:pointer;
    dwUpper:pointer;
  end;
  PCredHandle=^TCredHandle;

  TCtxtHandle=type TCredHandle;
  PCtxtHandle=^TCtxtHandle;

type
  TTcpSocket=class(TObject)
  private
    FSocket:THandle;
    FAddr:TSocketAddress;
    FConnected:boolean;
  protected
    constructor Create(family: word; ASocket:THandle); overload;
    function GetPort:word;
    function GetAddress:string;
    function GetHostName:string;
  public
    Cred:TCredHandle;
    Ctxt:TCtxtHandle;
    constructor Create(family: word= AF_INET); overload;
    destructor Destroy; override;
    procedure Connect(const Address:AnsiString;Port:word);
    procedure Disconnect;
    function ReceiveBuf(var Buf; Count: Integer): Integer;
    function SendBuf(const Buf; Count: LongInt): LongInt;
    property Handle:THandle read FSocket;
    property Connected:boolean read FConnected;
    property Port:word read GetPort;
    property Address:string read GetAddress;
    property HostName:string read GetHostName;
  end;

  TTcpServer=class(TObject)
  private
    FFamily: word;
    FSocket: THandle;
  public
    constructor Create(family: word= AF_INET);
    destructor Destroy; override;
    procedure Bind(const Address:AnsiString;Port:word);
    procedure Listen;
    procedure WaitForConnection;
    function Accept:TTcpSocket;
    property Handle:THandle read FSocket;
  end;

  ETcpSocketError=class(Exception);

function WSAStartup(wVersionRequired: word; WSData: pointer): integer; stdcall;
function WSACleanup: integer; stdcall;
function WSAGetLastError: integer; stdcall;
function htons(hostshort: word): word; stdcall;
function inet_addr(cp: PAnsiChar): cardinal; stdcall;
function inet_ntoa(inaddr: cardinal): PAnsiChar; stdcall;
function gethostbyaddr(addr: pointer; len, Struct: integer): PHostEntry; stdcall;
function gethostbyname(name: PAnsiChar): PHostEntry; stdcall;
//TODO: getaddrinfo
function socket(af, Struct, protocol: integer): THandle; stdcall;
function setsockopt(s: THandle; level, optname: integer; optval: PAnsiChar;
  optlen: integer): integer; stdcall;
function listen(socket: THandle; backlog: integer): integer; stdcall;
function bind(s: THandle; var addr: TSocketAddress; namelen: integer): integer; stdcall;
function accept(s: THandle; addr: PSocketAddress; addrlen: PInteger): THandle; stdcall;
function connect(s: THandle; var name: TSocketAddress; namelen: integer): integer; stdcall;
function recv(s: THandle; var Buf; len, flags: integer): integer; stdcall;
function select(nfds: integer; readfds, writefds, exceptfds: PFDSet;
  timeout: PTimeVal): integer; stdcall;
function send(s: THandle; var Buf; len, flags: integer): integer; stdcall;
function shutdown(s: THandle; how: integer): integer; stdcall;
function closesocket(s: THandle): integer; stdcall;
//function __WSAFDIsSet(s: THandle; var FDSet: TFDSet): Boolean; stdcall;

type
  TSChannelCred=record
    dwVersion: cardinal;
    cCreds: cardinal;
    paCred: pointer;//PCertContext;
    hRootStore: THandle;
    cMappers: cardinal;
    aphMappers: pointer;
    cSupportedAlgs: cardinal;
    palgSupportedAlgs: PCardinal;
    grbitEnabledProtocols: cardinal;
    dwMinimumCipherStrength: cardinal;
    dwMaximumCipherStrength: cardinal;
    dwSessionLifespan: cardinal;
    dwFlags: cardinal;
    dwCredFormat: cardinal;
  end;
  PSChannelCred=^TSChannelCred;

  TSecBuffer=record
    cbBuffer: cardinal;
    BufferType: cardinal;
    pvBuffer: pointer;
  end;
  PSecBuffer=^TSecBuffer;

  TSecBufferDesc=record
    ulVersion: cardinal;
    cBuffers: cardinal;
    pBuffers: PSecBuffer;
  end;
  PSecBufferDesc=^TSecBufferDesc;

  TTimeStamp=record
    dwLowDateTime: cardinal;
    dwHighDateTime: cardinal;
  end;
  PTimeStamp=^TTimeStamp;

  TSecPkgContextNames=record
    sUserName:PAnsiChar;
  end;
  PSecPkgContextNames=^TSecPkgContextNames;

  TSecPkgContextStreamSizes=record
    cbHeader: cardinal;
    cbTrailer: cardinal;
    cbMaximumMessage: cardinal;
    cBuffers: cardinal;
    cbBlockSize: cardinal;
  end;
  PSecPkgContextStreamSizes=^TSecPkgContextStreamSizes;

  
function AcquireCredentialsHandle(pszPrincipal: PAnsiChar;
    pszPackage: PAnsiChar; fCredentialUse: cardinal; pvLogonID: PInt64;
    pAuthData: PSChannelCred; pGetKeyFn: pointer; pvGetKeyArgument: pointer;
    phCredential: PCredHandle; ptsExpiry: PTimeStamp): cardinal; stdcall;

function FreeCredentialsHandle (phCredential: PCredHandle): cardinal; stdcall;

function InitializeSecurityContext(phCredential: PCredHandle;
    phContext: PCtxtHandle; pszTargetName: PAnsiChar; fContextReq: cardinal;
    Reserved1: cardinal; TargetDataRep: cardinal; pInput: PSecBufferDesc;
    Reserved2: cardinal; phNewContext: PCtxtHandle; pOutput: PSecBufferDesc;
    pfContextAttr: PCardinal; ptsExpiry: PTimeStamp): cardinal; stdcall;

function AcceptSecurityContext(phCredential: PCredHandle;
    phContext: PCtxtHandle; pInput: PSecBufferDesc; fContextReq: cardinal;
    TargetDataRep: cardinal; phNewContext: PCtxtHandle;
    pOutput: PSecBufferDesc; pfContextAttr: PCardinal;
    ptsTimeStamp: PTimeStamp): cardinal; stdcall;

function DeleteSecurityContext(phContext: PCtxtHandle): cardinal; stdcall;

function ApplyControlToken(phContext: PCtxtHandle;
    pInput: PSecBufferDesc): cardinal; stdcall;

function QueryContextAttributes(phContext: PCtxtHandle;
    ulAttribute: cardinal; pBuffer: pointer): cardinal; stdcall;

function FreeContextBuffer(pvContextBuffer: pointer): cardinal; stdcall;

function EncryptMessage(phContext: PCtxtHandle; fQOP: cardinal;
    pMessage: PSecBufferDesc; MessageSeqNo: cardinal): cardinal; stdcall;

function DecryptMessage(phContext: PCtxtHandle; pMessage: PSecBufferDesc;
    MessageSeqNo: cardinal; pfQOP: PCardinal): cardinal; stdcall;

const
  SP_PROT_TLS1          = $0C0;
  SP_PROT_TLS1_SERVER   = $040;
  SP_PROT_TLS1_CLIENT   = $080;
  SP_PROT_TLS1_1        = $300;
  SP_PROT_TLS1_1_SERVER = $100;
  SP_PROT_TLS1_1_CLIENT = $200;
  SP_PROT_TLS1_2        = $C00;
  SP_PROT_TLS1_2_SERVER = $400;
  SP_PROT_TLS1_2_CLIENT = $800;

  SECPKG_CRED_INBOUND  = 1;
  SECPKG_CRED_OUTBOUND = 2;

  ISC_REQ_DELEGATE               = $00000001;
  ISC_REQ_MUTUAL_AUTH            = $00000002;
  ISC_REQ_REPLAY_DETECT          = $00000004;
  ISC_REQ_SEQUENCE_DETECT        = $00000008;
  ISC_REQ_CONFIDENTIALITY        = $00000010;
  ISC_REQ_USE_SESSION_KEY        = $00000020;
  ISC_REQ_PROMPT_FOR_CREDS       = $00000040;
  ISC_REQ_USE_SUPPLIED_CREDS     = $00000080;
  ISC_REQ_ALLOCATE_MEMORY        = $00000100;
  ISC_REQ_USE_DCE_STYLE          = $00000200;
  ISC_REQ_DATAGRAM               = $00000400;
  ISC_REQ_CONNECTION             = $00000800;
  ISC_REQ_CALL_LEVEL             = $00001000;
  ISC_REQ_FRAGMENT_SUPPLIED      = $00002000;
  ISC_REQ_EXTENDED_ERROR         = $00004000;
  ISC_REQ_STREAM                 = $00008000;
  ISC_REQ_INTEGRITY              = $00010000;
  ISC_REQ_IDENTIFY               = $00020000;
  ISC_REQ_NULL_SESSION           = $00040000;
  ISC_REQ_MANUAL_CRED_VALIDATION = $00080000;
  ISC_REQ_RESERVED1              = $00100000;
  ISC_REQ_FRAGMENT_TO_FIT        = $00200000;

  ASC_REQ_DELEGATE                = $00000001;
  ASC_REQ_MUTUAL_AUTH             = $00000002;
  ASC_REQ_REPLAY_DETECT           = $00000004;
  ASC_REQ_SEQUENCE_DETECT         = $00000008;
  ASC_REQ_CONFIDENTIALITY         = $00000010;
  ASC_REQ_USE_SESSION_KEY         = $00000020;
  ASC_REQ_ALLOCATE_MEMORY         = $00000100;
  ASC_REQ_USE_DCE_STYLE           = $00000200;
  ASC_REQ_DATAGRAM                = $00000400;
  ASC_REQ_CONNECTION              = $00000800;
  ASC_REQ_CALL_LEVEL              = $00001000;
  ASC_REQ_EXTENDED_ERROR          = $00008000;
  ASC_REQ_STREAM                  = $00010000;
  ASC_REQ_INTEGRITY               = $00020000;
  ASC_REQ_LICENSING               = $00040000;
  ASC_REQ_IDENTIFY                = $00080000;
  ASC_REQ_ALLOW_NULL_SESSION      = $00100000;
  ASC_REQ_ALLOW_NON_USER_LOGONS   = $00200000;
  ASC_REQ_ALLOW_CONTEXT_REPLAY    = $00400000;
  ASC_REQ_FRAGMENT_TO_FIT         = $00800000;
  ASC_REQ_FRAGMENT_SUPPLIED       = $00002000;
  ASC_REQ_NO_TOKEN                = $01000000;
  ASC_REQ_HTTP                    = $10000000;

  SECURITY_NATIVE_DREP = $10;

  SECBUFFER_VERSION = 0;
  SECBUFFER_EMPTY   = 0;
  SECBUFFER_DATA    = 1;
  SECBUFFER_TOKEN   = 2;
  SECBUFFER_EXTRA   = 5;
  SECBUFFER_STREAM_TRAILER = 6;
  SECBUFFER_STREAM_HEADER  = 7;

  SEC_E_OK = 0;
  SEC_I_CONTINUE_NEEDED        = $00090312;
  SEC_E_INCOMPLETE_MESSAGE     = $80090318;

  SECPKG_ATTR_NAMES        = 1;
  SECPKG_ATTR_STREAM_SIZES = 4;

  SCHANNEL_SHUTDOWN = 1;

implementation

var
  WSAData:record // !!! also WSDATA
    wVersion:word;
    wHighVersion:word;
    szDescription:array[0..256] of AnsiChar;
    szSystemStatus:array[0..128] of AnsiChar;
    iMaxSockets:word;
    iMaxUdpDg:word;
    lpVendorInfo:PAnsiChar;
  end;

procedure RaiseLastWSAError;
var
  r:integer;
begin
  r:=WSAGetLastError;
  raise ETcpSocketError.Create(IntToStr(r)+' '+SysErrorMessage(r));
end;

procedure PrepareSockAddr(var addr: TSocketAddress; family, port: word;
  const host: AnsiString);
var
  e:PHostEntry;
  i:integer;
begin
  addr.family:=family;//AF_INET
  addr.port:=htons(port);
  for i:=0 to 11 do addr.data[i]:=0;
  if host<>'' then
    if host[1] in ['0'..'9'] then
      PCardinal(@addr.data[0])^:=inet_addr(PAnsiChar(host))
    else
     begin
      //TODO: getaddrinfo
      e:=gethostbyname(PAnsiChar(host));
      if e=nil then RaiseLastWSAError;
      addr.family:=e.h_addrtype;
      Move(e.h_addr^[0],addr.data[0],e.h_length);
     end;
end;

{ TTcpSocket }

procedure TTcpSocket.Connect(const Address: AnsiString; Port: word);
begin
  PrepareSockAddr(FAddr,FAddr.family,Port,Address);
  if xxmSock.connect(FSocket,FAddr,SizeOf(TSocketAddress))=SOCKET_ERROR then
    RaiseLastWSAError
  else
    FConnected:=true;
end;

constructor TTcpSocket.Create(family: word);
begin
  inherited Create;
  FConnected:=false;
  FillChar(FAddr,SizeOf(TSocketAddress),#0);
  FAddr.family:=family;//AF_INET
  FSocket:=socket(family,SOCK_STREAM,IPPROTO_IP);
  if FSocket=INVALID_SOCKET then RaiseLastWSAError;
  Cred.dwLower:=nil;
  Cred.dwUpper:=nil;
  Ctxt.dwLower:=nil;
  Ctxt.dwUpper:=nil;
end;

constructor TTcpSocket.Create(family: word; ASocket: THandle);
var
  i:integer;
begin
  inherited Create;
  FAddr.family:=family;
  FSocket:=ASocket;
  if FSocket=INVALID_SOCKET then RaiseLastWSAError;
  i:=1;
  if setsockopt(FSocket,IPPROTO_TCP,TCP_NODELAY,@i,4)<>0 then
    RaiseLastWSAError;
  FConnected:=true;//?
  Cred.dwLower:=nil;
  Cred.dwUpper:=nil;
  Ctxt.dwLower:=nil;
  Ctxt.dwUpper:=nil;
end;

destructor TTcpSocket.Destroy;
begin
  //Disconnect;?
  closesocket(FSocket);
  if (Ctxt.dwLower<>nil) or (Ctxt.dwUpper<>nil) then
    DeleteSecurityContext(@Ctxt);
  if (Cred.dwLower<>nil) or (Cred.dwUpper<>nil) then
    FreeCredentialsHandle(@Cred);
  inherited;
end;

procedure TTcpSocket.Disconnect;
begin
  if FConnected then
   begin
    FConnected:=false;
    shutdown(FSocket,SD_BOTH);
   end;
end;

function TTcpSocket.GetPort: word;
begin
  Result:=FAddr.port;
end;

function TTcpSocket.GetAddress: string;
begin
  Result:=string(inet_ntoa(PCardinal(@FAddr.data[0])^));
end;

function TTcpSocket.GetHostName: string;
var
  e:PHostEntry;
  i:integer;
begin
  e:=gethostbyaddr(@FAddr.data[0],SizeOf(TSocketAddress),FAddr.family);
  if e=nil then
    //inet_ntop?
    if FAddr.family=AF_INET6 then
     begin
      i:=3;
      if FAddr.data[i]=0 then Result:=':' else
        Result:=Result+IntToHex(FAddr.data[i],4)+':';
      while (i<10) do
       begin
        while (i<10) and (FAddr.data[i]=0) do inc(i);
        if i=10 then Result:=Result+':' else
          Result:=Result+':'+IntToHex(FAddr.data[i],4);
        inc(i);
       end;
     end
    else
      Result:=string(inet_ntoa(PCardinal(@FAddr.data[0])^))
  else
    Result:=string(e.h_name);
end;

function TTcpSocket.ReceiveBuf(var Buf; Count: Integer): Integer;
begin
  Result:=recv(FSocket,Buf,Count,0);
  if Result=SOCKET_ERROR then
    try
      RaiseLastWSAError;
    finally
      Disconnect;
    end;
end;

function TTcpSocket.SendBuf(const Buf; Count: LongInt): LongInt;
var
  p:pointer;
begin
  p:=@Buf;
  Result:=send(FSocket,p^,Count,0);
  if Result=SOCKET_ERROR then
    try
      RaiseLastWSAError;
    finally
      Disconnect;
    end;
end;

{ TTcpServer }

constructor TTcpServer.Create(family: word);
begin
  inherited Create;
  FFamily:=family;//AF_INET
  FSocket:=socket(FFamily,SOCK_STREAM,IPPROTO_IP);
end;

destructor TTcpServer.Destroy;
begin
  closesocket(FSocket);
  inherited;
end;

procedure TTcpServer.Bind(const Address: AnsiString; Port: word);
var
  a:TSocketAddress;
begin
  if FSocket=INVALID_SOCKET then RaiseLastWSAError;
  PrepareSockAddr(a,FFamily,Port,Address);
  if xxmSock.bind(FSocket,a,SizeOf(TSocketAddress))=SOCKET_ERROR then
    RaiseLastWSAError;
end;

procedure TTcpServer.Listen;
begin
  //call bind first!
  if xxmSock.listen(FSocket,SOMAXCONN)=SOCKET_ERROR then
    RaiseLastWSAError;
end;

procedure TTcpServer.WaitForConnection;
var
  r,x:TFDSet;
begin
  r.fd_count:=1;
  r.fd_array[0]:=FSocket;
  x.fd_count:=1;
  x.fd_array[0]:=FSocket;
  if select(0,@r,nil,@x,nil)=SOCKET_ERROR then RaiseLastWSAError;
  if x.fd_count=1 then //if __WSAFDIsSet(FSocket,x) then
    raise ETcpSocketError.Create('Socket in error state');//?
  if r.fd_count=0 then //if not __WSAFDIsSet(FSocket,r) then
    raise ETcpSocketError.Create('Select without error nor result');//??
end;

function TTcpServer.Accept: TTcpSocket;
var
  a:TSocketAddress;
  l:integer;
begin
  l:=SizeOf(TSocketAddress);
  FillChar(a,l,#0);
  Result:=TTcpSocket.Create(FFamily,xxmSock.accept(FSocket,@a,@l));
  Result.FAddr:=a;
end;

const
  winsockdll='wsock32.dll';

function WSAStartup; external winsockdll;
function WSACleanup; external winsockdll;
function WSAGetLastError; external winsockdll;
function htons; external winsockdll;
function inet_addr; external winsockdll;
function inet_ntoa; external winsockdll;
function gethostbyaddr; external winsockdll;
function gethostbyname; external winsockdll;
function socket; external winsockdll;
function setsockopt; external winsockdll;
function listen; external winsockdll;
function bind; external winsockdll;
function accept; external winsockdll;
function connect; external winsockdll;
function recv; external winsockdll;
function select; external winsockdll;
function send; external winsockdll;
function shutdown; external winsockdll;
function closesocket; external winsockdll;
//function __WSAFDIsSet; external winsockdll;

const
  SecurityDLL='secur32.dll'; //'sspicli.dll'

function AcquireCredentialsHandle; external SecurityDLL name 'AcquireCredentialsHandleA';
function FreeCredentialsHandle; external SecurityDLL name 'FreeCredentialsHandle';
function InitializeSecurityContext; external SecurityDLL name 'InitializeSecurityContextA';
function AcceptSecurityContext; external SecurityDLL name 'AcceptSecurityContext';
function DeleteSecurityContext; external SecurityDLL name 'DeleteSecurityContext';
function ApplyControlToken; external SecurityDLL name 'ApplyControlToken';
function QueryContextAttributes; external SecurityDLL name 'QueryContextAttributesA';
function FreeContextBuffer; external SecurityDLL name 'FreeContextBuffer';
function EncryptMessage; external SecurityDLL name 'EncryptMessage';
function DecryptMessage; external SecurityDLL name 'DecryptMessage';

initialization
  WSAStartup($0101,@WSAData);
finalization
  WSACleanup;
end.
