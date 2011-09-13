unit xxmIsapiStream;

interface

uses Classes, isapi4;

type
  TXxmIsapiStreamAdapter=class(TStream)
  private
    ecb:PEXTENSION_CONTROL_BLOCK;
    FStore:TStream;
    FStoreSize,FStorePosition,FPosition:int64;
  protected
    function GetSize: Int64; override;
    procedure SetSize(NewSize: Longint); overload; override;
    procedure SetSize(const NewSize: Int64); overload; override;
  public
    constructor Create(pecb:PEXTENSION_CONTROL_BLOCK;StoreStream:TStream);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; overload; override;
  end;

implementation

uses SysUtils;

{ TXxmIsapiStreamAdapter }

constructor TXxmIsapiStreamAdapter.Create(pecb: PEXTENSION_CONTROL_BLOCK; StoreStream: TStream);
begin
  inherited Create;
  ecb:=pecb;
  FStoreSize:=ecb.cbTotalBytes;
  FStore:=StoreStream;
  FStore.Size:=FStoreSize;
  FStore.Position:=0;
  FStorePosition:=FStore.Write(ecb.lpbData^,ecb.cbAvailable);
  FPosition:=FStorePosition;
end;

destructor TXxmIsapiStreamAdapter.Destroy;
begin
  FStore.Free;
  inherited;
end;

function TXxmIsapiStreamAdapter.GetSize: Int64;
begin
  Result:=FStoreSize;//ecb.cbTotalBytes;
end;

function TXxmIsapiStreamAdapter.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
begin
  case Origin of
    soBeginning:Result:=Offset;
    soCurrent:Result:=FPosition+Offset;
    soEnd:Result:=FStoreSize+Offset;
    else Result:=FPosition+Offset;//raise?
  end;
  if (Result<0) or (Result>FStoreSize) then
    raise Exception.Create('TXxmIsapiStreamAdapter.Seek past end not allowed');
  if (Result>FStorePosition) then
    raise Exception.Create('TXxmIsapiStreamAdapter.Seek past current incoming position not allowed');//TODO: force read?
  FStore.Position:=Result;
  FPosition:=Result;
end;

function TXxmIsapiStreamAdapter.Read(var Buffer; Count: Integer): Longint;
begin
  if FPosition=FStorePosition then
   begin
    if FPosition+Count>FStoreSize then
      Result:=FStoreSize-FPosition
    else
      Result:=Count;
    if Result<>0 then
     begin
      if not(ecb.ReadClient(ecb.ConnID,@Buffer,cardinal(Result))) then RaiseLastOSError;
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
     if Result<>0 then
      begin
       FStore.Position:=FPosition;
       Result:=FStore.Read(Buffer,Result);
       inc(FPosition,Result);
       FStore.Position:=FStorePosition;
      end;
     //TODO: read FPosition+Count-FStorePosition
    end
   else
     raise Exception.Create('TXxmIsapiStreamAdapter.Read past current incoming position not allowed');//TODO: force read?
end;

procedure TXxmIsapiStreamAdapter.SetSize(NewSize: Integer);
begin
  raise Exception.Create('TXxmIsapiStreamAdapter.SetSize not supported');
end;

procedure TXxmIsapiStreamAdapter.SetSize(const NewSize: Int64);
begin
  raise Exception.Create('TXxmIsapiStreamAdapter.SetSize not supported');
end;

function TXxmIsapiStreamAdapter.Write(const Buffer;
  Count: Integer): Longint;
begin
  raise Exception.Create('TXxmIsapiStreamAdapter.Write not supported');
end;

end.
