unit xxmReadHandler;

interface

uses Classes, Sockets;

type
  THandlerReadStreamAdapter=class(TStream)
  private
    FSize:Int64;
    FSocket:TCustomIpClient;
    FStore:TStream;
    FStorePosition,FPosition:int64;
  protected
    function GetSize: Int64; override;
    procedure SetSize(NewSize: Longint); overload; override;
    procedure SetSize(const NewSize: Int64); overload; override;
  public
    constructor Create(Socket:TCustomIpClient;Size:Int64;StoreStream:TStream;
      const StartData: AnsiString);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; overload; override;
  end;

implementation

uses SysUtils;

{ THandlerReadStreamAdapter }

constructor THandlerReadStreamAdapter.Create(Socket: TCustomIpClient;
  Size: Int64; StoreStream: TStream; const StartData: AnsiString);
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
  l:=Length(StartData);
  if l<>0 then FStore.Write(StartData[1],l);
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
