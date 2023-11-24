unit xxmReadHandler;

interface

uses Classes, Types, xxm, xxmSock, ActiveX;

type
  THandlerReadStreamAdapter=class(TStream)
  private
    FSize:Int64;
    FSocket:TTcpSocket;
    FStore:TStream;
    FStorePosition,FPosition:int64;
  protected
    function GetSize: Int64; override;
    procedure SetSize(NewSize: LongInt); overload; override;
    procedure SetSize(const NewSize: Int64); overload; override;
  public
    constructor Create(Socket: TTcpSocket; Size: Int64; StoreStream: TStream;
      const StartData: AnsiString; StartIndex, StartLength: integer);
    destructor Destroy; override;
    function Read(var Buffer; Count: LongInt): LongInt; override;
    function Write(const Buffer; Count: LongInt): LongInt; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; overload; override;
  end;

  {$IF not(Declared(FixedUInt))}
  FixedUInt=LongInt;
  PFixedUInt=PLongInt;
  LargeInt=LongLongInt;
  LargeUInt=LargeInt;
  {$IFEND}

  TRawSocketData=class(TInterfacedObject, IStream, IXxmRawSocket)
  private
    FSocket: TTcpSocket;
  public
    constructor Create(Socket: TTcpSocket);
    destructor Destroy; override;
    { IStream }
{$IF CompilerVersion<20}
    function Seek(dlibMove: LargeInt; dwOrigin: LongInt;
      out libNewPosition: LargeInt): HResult; stdcall;
    function SetSize(libNewSize: LargeInt): HResult; stdcall;
    function CopyTo(stm: IStream; cb: LargeInt; out cbRead: LargeInt;
      out cbWritten: LargeInt): HResult; stdcall;
    function Commit(grfCommitFlags: LongInt): HResult; stdcall;
    function Revert: HResult; stdcall;
    function LockRegion(libOffset: LargeInt; cb: LargeInt;
      dwLockType: LongInt): HResult; stdcall;
    function UnlockRegion(libOffset: LargeInt; cb: LargeInt;
      dwLockType: LongInt): HResult; stdcall;
    function Stat(out statstg: TStatStg; grfStatFlag: LongInt): HResult;
      stdcall;
    function Clone(out stm: IStream): HResult; stdcall;
{$ELSE}
    function Seek(dlibMove: LargeInt; dwOrigin: DWORD;
      out libNewPosition: LargeUInt): HResult; stdcall;
    function SetSize(libNewSize: LargeUInt): HResult; stdcall;
    function CopyTo(stm: IStream; cb: LargeUInt; out cbRead: LargeUInt;
      out cbWritten: LargeUInt): HResult; stdcall;
    function Commit(grfCommitFlags: DWORD): HResult; stdcall;
    function Revert: HResult; stdcall;
    function LockRegion(libOffset: LargeUInt; cb: LargeUInt;
      dwLockType: DWORD): HResult; stdcall;
    function UnlockRegion(libOffset: LargeUInt; cb: LargeUInt;
      dwLockType: DWORD): HResult; stdcall;
    function Stat(out statstg: TStatStg; grfStatFlag: DWORD): HResult;
      stdcall;
    function Clone(out stm: IStream): HResult; stdcall;
{$IFEND}
    { ISequentialStream }
    function Read(pv: Pointer; cb: FixedUInt; pcbRead: PFixedUInt): HResult;
      stdcall;
    function Write(pv: Pointer; cb: FixedUInt; pcbWritten: PFixedUInt): HResult;
      stdcall;
    { IXxmRawSocket }
    function DataReady(TimeoutMS: cardinal): boolean;
    procedure Disconnect;
  end;

implementation

uses SysUtils;

{ THandlerReadStreamAdapter }

constructor THandlerReadStreamAdapter.Create(Socket: TTcpSocket;
  Size: Int64; StoreStream: TStream; const StartData: AnsiString;
  StartIndex, StartLength: integer);
var
  l:integer;  
begin
  inherited Create;
  FSize:=Size;
  FSocket:=Socket;
  //FSocket.OnError:=//see caller
  FStore:=StoreStream;
  //FStore.Size:=FSize;//done by caller? (don't care really)
  //assert FStore.Position:=0;
  //assert Length(StartData)>=StartLength
  l:=StartLength-StartIndex+1;
  if l<0 then l:=0;//raise ?
  if l>0 then FStore.Write(StartData[StartIndex],l);
  FStorePosition:=l;
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

function THandlerReadStreamAdapter.Read(var Buffer; Count: Integer): LongInt;
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
  Count: Integer): LongInt;
begin
  raise Exception.Create('THandlerReadStreamAdapter.Write not supported');
end;

destructor THandlerReadStreamAdapter.Destroy;
begin
  FStore.Free;
  inherited;
end;

{ TRawSocketData }

constructor TRawSocketData.Create(Socket: TTcpSocket);
begin
  inherited Create;
  FSocket:=Socket;
end;

destructor TRawSocketData.Destroy;
begin
  FSocket:=nil;
  inherited;
end;

function TRawSocketData.Clone(out stm: IStream): HResult;
begin
  raise Exception.Create('TRawSocketData.Clone not supported');
end;

{$IF CompilerVersion<20}
function TRawSocketData.Commit(grfCommitFlags: LongInt): HResult;
{$ELSE}
function TRawSocketData.Commit(grfCommitFlags: DWORD): HResult;
{$IFEND}
begin
  raise Exception.Create('TRawSocketData.Commit not supported');
end;

function TRawSocketData.CopyTo(stm: IStream; cb: LargeUInt; out cbRead,
  cbWritten: LargeUInt): HResult;
begin
  raise Exception.Create('TRawSocketData.CopyTo not supported');
end;

{$IF CompilerVersion<20}
function TRawSocketData.LockRegion(libOffset: LargeInt; cb: LargeInt;
  dwLockType: LongInt): HResult;
{$ELSE}
function TRawSocketData.LockRegion(libOffset, cb: LargeUInt;
  dwLockType: DWORD): HResult;
{$IFEND}
begin
  raise Exception.Create('TRawSocketData.LockRegion not supported');
end;

function TRawSocketData.Revert: HResult;
begin
  raise Exception.Create('TRawSocketData.Revert not supported');
end;

{$IF CompilerVersion<20}
function TRawSocketData.Seek(dlibMove: LargeInt; dwOrigin: LongInt;
      out libNewPosition: LargeInt): HResult;
{$ELSE}
function TRawSocketData.Seek(dlibMove: LargeInt; dwOrigin: DWORD;
  out libNewPosition: LargeUInt): HResult;
{$IFEND}
begin
  raise Exception.Create('TRawSocketData.Seek not supported');
end;

function TRawSocketData.SetSize(libNewSize: LargeUInt): HResult;
begin
  raise Exception.Create('TRawSocketData.SetSize not supported');
end;

{$IF CompilerVersion<20}
function TRawSocketData.Stat(out statstg: TStatStg;
  grfStatFlag: LongInt): HResult;
{$ELSE}
function TRawSocketData.Stat(out statstg: TStatStg;
  grfStatFlag: DWORD): HResult;
{$IFEND}
begin
  raise Exception.Create('TRawSocketData.Stat not supported');
end;

{$IF CompilerVersion<20}
function TRawSocketData.UnlockRegion(libOffset: LargeInt; cb: LargeInt;
  dwLockType: LongInt): HResult;
{$ELSE}
function TRawSocketData.UnlockRegion(libOffset, cb: LargeUInt;
  dwLockType: DWORD): HResult;
{$IFEND}
begin
  raise Exception.Create('TRawSocketData.UnlockRegion not supported');
end;

function TRawSocketData.Read(pv: Pointer; cb: FixedUInt;
  pcbRead: PFixedUInt): HResult;
var
  l:integer;
begin
  //assert DataReady called, returned true
  l:=FSocket.ReceiveBuf(pv^,cb);
  if l<=0 then
    raise ETcpSocketError.Create('Connection Lost');//Result:=E?
  if pcbRead<>nil then pcbRead^:=l;
  Result:=S_OK;
end;

function TRawSocketData.Write(pv: Pointer; cb: FixedUInt;
  pcbWritten: PFixedUInt): HResult;
var
  l:integer;
begin
  l:=FSocket.SendBuf(pv^,cb);
  if FixedUInt(l)<>cb then
    raise ETcpSocketError.Create('Connection Lost');//Result:=E?
  if pcbWritten<>nil then pcbWritten^:=l;
  Result:=S_OK;
end;

function TRawSocketData.DataReady(TimeoutMS: cardinal): boolean;
var
  r,x:TFDSet;
  t:TTimeVal;
begin
  r.fd_count:=1;
  r.fd_array[0]:=FSocket.Handle;
  x.fd_count:=1;
  x.fd_array[0]:=FSocket.Handle;
  t.tv_sec:=TimeoutMS div 1000;
  t.tv_usec:=(TimeoutMS mod 1000)*1000;//microseconds
  if select(0,@r,nil,@x,@t)=SOCKET_ERROR then
    raise ETcpSocketError.Create(SysErrorMessage(WSAGetLastError));
  if x.fd_count=1 then //if __WSAFDIsSet(FSocket,x) then
    raise ETcpSocketError.Create('Socket in error state');//?
  Result:=r.fd_count=1;//__WSAFDIsSet(FSocket,r)
end;

procedure TRawSocketData.Disconnect;
var
  i:integer;
begin
  if FSocket<>nil then
   begin
    i:=1;
    setsockopt(FSocket.Handle,SOL_SOCKET,SO_REUSEADDR,@i,4);
    FSocket.Disconnect;
   end;
end;

end.
