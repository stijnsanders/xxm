unit xxmSock;

{$D-}
{$L-}
{$Y-}

interface

uses SysUtils, Classes;

type
  //more convenient alternative to TSockAddrIn
  PSocketAddress=^TSocketAddress;
  TSocketAddress=record
    family: word;
    port: word;
    data: array[0..11] of word;
  end;

  TTcpSocket=class(TObject)
  private
    FSocket:THandle;
    FAddr:TSocketAddress;
    FConnected:boolean;
  protected
    constructor Create(AFamily: word; ASocket:THandle); overload;
    function GetPort:word;
    function GetAddress:string;
    function GetHostName:string;
  public
    constructor Create(AFamily: word); overload;
    destructor Destroy; override;
    procedure Connect(const Address:AnsiString;Port:word); virtual;
    procedure Disconnect; virtual;
    function ReceiveBuf(var Buf; Count: Integer): Integer; virtual;
    function SendBuf(const Buf; Count: LongInt): LongInt; virtual;
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
    constructor Create(AFamily:word);
    destructor Destroy; override;
    procedure Bind(const Address:AnsiString;Port:word);
    procedure Listen;
    procedure WaitForConnection;
    function Accept:TTcpSocket;
    property Handle:THandle read FSocket;
  end;

  ETcpSocketError=class(Exception);

  THandlerReadStreamAdapter=class(TStream)
  private
    FSize:Int64;
    FSocket:TTcpSocket;
    FStore:TStream;
    FStorePosition,FPosition:int64;
  protected
    function GetSize: Int64; override;
    procedure SetSize(NewSize: Longint); overload; override;
    procedure SetSize(const NewSize: Int64); overload; override;
  public
    constructor Create(Socket: TTcpSocket; Size: Int64; StoreStream: TStream;
      StartData: PUTF8Char; StartDataSize: integer);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; overload; override;
  end;

implementation

uses WinSock2;

procedure RaiseLastWSAError;
var
  r:integer;
begin
  r:=WSAGetLastError;
  raise ETcpSocketError.Create(IntToStr(r)+' '+SysErrorMessage(r)) at ReturnAddress;
end;

procedure PrepareSockAddr(var addr: TSocketAddress; family, port: word;
  const host: AnsiString);
var
  e:PHostEnt;
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

{ TTcpServer }

constructor TTcpServer.Create(AFamily:word);
begin
  inherited Create;
  FFamily:=AFamily;//AF_INET
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
  p:PSockAddr;
begin
  if FSocket=INVALID_SOCKET then RaiseLastWSAError;
  PrepareSockAddr(a,FFamily,Port,Address);
  p:=@a;
  if WinSock2.bind(FSocket,p^,SizeOf(TSocketAddress))=SOCKET_ERROR then
    RaiseLastWSAError;
end;

procedure TTcpServer.Listen;
begin
  //call bind first!
  if WinSock2.listen(FSocket,SOMAXCONN)=SOCKET_ERROR then
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
  if WinSock2.select(0,@r,nil,@x,nil)=SOCKET_ERROR then RaiseLastWSAError;
  if x.fd_count=1 then //if __WSAFDIsSet(FSocket,x) then
    raise ETcpSocketError.Create('Socket in error state');//?
  if r.fd_count=0 then //if not __WSAFDIsSet(FSocket,r) then
    raise ETcpSocketError.Create('Select without error nor result');//??
end;

function TTcpServer.Accept: TTcpSocket;
var
  a:TSocketAddress;
  i,l:integer;
begin
  l:=SizeOf(TSocketAddress);
  FillChar(a,l,#0);
  Result:=TTcpSocket.Create(FFamily,WinSock2.accept(FSocket,@a,@l));
  Result.FAddr:=a;

  //?
  i:=1;
  setsockopt(Result.Handle,SOL_SOCKET,SO_REUSEADDR,@i,4);
end;

{ TTcpSocket }

constructor TTcpSocket.Create(AFamily: word);
begin
  inherited Create;
  FConnected:=false;
  FillChar(FAddr,SizeOf(TSocketAddress),#0);
  FAddr.family:=AFamily;//AF_INET
  FSocket:=socket(AFamily,SOCK_STREAM,IPPROTO_IP);
  if FSocket=INVALID_SOCKET then RaiseLastWSAError;
end;

constructor TTcpSocket.Create(AFamily: word; ASocket: THandle);
var
  i:integer;
begin
  inherited Create;
  FillChar(FAddr,SizeOf(TSocketAddress),#0);
  FAddr.family:=AFamily;
  FSocket:=ASocket;
  if FSocket=INVALID_SOCKET then RaiseLastWSAError;
  i:=1;
  if setsockopt(FSocket,IPPROTO_TCP,TCP_NODELAY,@i,4)<>0 then
    RaiseLastWSAError;
  FConnected:=true;//?
end;

destructor TTcpSocket.Destroy;
begin
  //Disconnect;?
  closesocket(FSocket);
  inherited;
end;

procedure TTcpSocket.Connect(const Address: AnsiString; Port: word);
var
  p:PSockAddr;
begin
  PrepareSockAddr(FAddr,FAddr.family,Port,Address);
  p:=@FAddr;
  if WinSock2.connect(FSocket,p^,SizeOf(TSocketAddress))=SOCKET_ERROR then
    RaiseLastWSAError
  else
    FConnected:=true;
end;

procedure TTcpSocket.Disconnect;
begin
  if FConnected then
   begin
    FConnected:=false;
    shutdown(FSocket,SD_BOTH);
   end;
end;

function TTcpSocket.GetAddress: string;
var
  i:integer;
begin
  //inet_ntop?
  if FAddr.family=AF_INET6 then
   begin
    i:=3;
    if FAddr.data[i]=0 then Result:=':' else
      Result:=Result+IntToHex(
        (FAddr.data[i] and $FF00 shr 8) or
        (FAddr.data[i] and $00FF shl 8),4)+':';
    while (i<10) do
     begin
      while (i<10) and (FAddr.data[i]=0) do inc(i);
      if i=10 then Result:=Result+':' else
        Result:=Result+':'+IntToHex(
          (FAddr.data[i] and $FF00 shr 8) or
          (FAddr.data[i] and $00FF shl 8),4);
      inc(i);
     end;
   end
  else
    Result:=string(inet_ntoa(PInAddr(@FAddr.data[0])^));
end;

function TTcpSocket.GetPort: word;
begin
  Result:=FAddr.port;
end;

function TTcpSocket.GetHostName: string;
var
  e:PHostEnt;
begin
  e:=gethostbyaddr(@FAddr.data[0],SizeOf(TSocketAddress),FAddr.family);
  if e=nil then
    Result:=GetAddress
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

{ THandlerReadStreamAdapter }

constructor THandlerReadStreamAdapter.Create(Socket: TTcpSocket; Size: Int64;
  StoreStream: TStream; StartData: PUTF8Char; StartDataSize: integer);
begin
  inherited Create;
  FSize:=Size;
  FSocket:=Socket;
  //FSocket.OnError:=//see caller
  FStore:=StoreStream;
  //FStore.Size:=FSize;//done by caller? (don't care really)
  //assert FStore.Position:=0;
  if StartDataSize<>0 then FStore.Write(StartData^,StartDataSize);
  FStorePosition:=StartDataSize;
  FPosition:=0;
end;

function THandlerReadStreamAdapter.GetSize: Int64;
begin
  Result:=FSize;//from HTTP request header
end;

function THandlerReadStreamAdapter.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
begin
  case Origin of
    soBeginning:Result:=Offset;
    soCurrent:Result:=FPosition+Offset;
    soEnd:Result:=FSize+Offset;
    else Result:=FPosition+Offset;//raise?
  end;
  if (Result<0) or (Result>FSize) then
    raise Exception.Create('THandlerReadStreamAdapter.Seek past end not allowed');
  if (Result>FStorePosition) then
    raise Exception.Create('THandlerReadStreamAdapter.Seek past current incoming position not allowed');//TODO: force read?
  FStore.Position:=Result;
  FPosition:=Result;
end;

function THandlerReadStreamAdapter.Read(var Buffer; Count: Integer): Longint;
begin
  if FPosition=FStorePosition then
   begin
    if FPosition+Count>FSize then
      Result:=FSize-FPosition
    else
      Result:=Count;
    if Result<>0 then
     begin
      Result:=FSocket.ReceiveBuf(Buffer,Result);
      if Result=-1 then RaiseLastOSError;
      FStore.Write(Buffer,Result);
      inc(FPosition,Result);
      inc(FStorePosition,Result);
     end;
   end
  else
    if FPosition<FStorePosition then
     begin
      if FPosition+Count>FStorePosition then
        Result:=FStorePosition-FPosition
      else
        Result:=Count;
      FStore.Position:=FPosition;
      Result:=FStore.Read(Buffer,Result);
      inc(FPosition,Result);
      FStore.Position:=FStorePosition;
      //TODO: read FPosition+Count-FStorePosition
     end
    else
      raise Exception.Create('THandlerReadStreamAdapter.Read past current incoming position not allowed');//TODO: force read?
end;

procedure THandlerReadStreamAdapter.SetSize(NewSize: Integer);
begin
  raise Exception.Create('THandlerReadStreamAdapter.SetSize not supported');
end;

procedure THandlerReadStreamAdapter.SetSize(const NewSize: Int64);
begin
  raise Exception.Create('THandlerReadStreamAdapter.SetSize not supported');
end;

function THandlerReadStreamAdapter.Write(const Buffer;
  Count: Integer): Longint;
begin
  raise Exception.Create('THandlerReadStreamAdapter.Write not supported');
end;

destructor THandlerReadStreamAdapter.Destroy;
begin
  FStore.Free;
  inherited;
end;

end.
