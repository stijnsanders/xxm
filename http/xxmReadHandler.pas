unit xxmReadHandler;

interface

uses Classes, IdIOHandler;

type
  THandlerReadStreamAdapter=class(TStream)
  private
    FSize,FLeft:Int64;
    FHandler:TIdIOHandler;
  protected
    function GetSize: Int64; override;
    procedure SetSize(NewSize: Longint); overload; override;
    procedure SetSize(const NewSize: Int64); overload; override;
  public
    constructor Create(Handler:TIdIOHandler;Size:Int64);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; overload; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; overload; override;
  end;

implementation

uses SysUtils, IdGlobal;

{ THandlerReadStreamAdapter }

constructor THandlerReadStreamAdapter.Create(Handler: TIdIOHandler;
  Size: Int64);
begin
  inherited Create;
  FSize:=Size;
  FLeft:=Size;
  FHandler:=Handler;
end;

function THandlerReadStreamAdapter.GetSize: Int64;
begin
  Result:=FSize;//from HTTP request header
end;

function THandlerReadStreamAdapter.Seek(Offset: Integer;
  Origin: Word): Longint;
begin
  //TXxmReqPars.Fill seeks to beginning only for convenience
  if (Offset=soFromBeginning) and (Origin=0) then
    Result:=0
  else
    raise Exception.Create('THandlerReadStreamAdapter.Seek not supported');
end;

function THandlerReadStreamAdapter.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
begin
  Result:=Seek(integer(Offset),word(Origin));
end;

function THandlerReadStreamAdapter.Read(var Buffer;
  Count: Integer): Longint;
var
  d:TIdBytes;
begin
  if Count>FLeft then Result:=FLeft else Result:=Count;
  FHandler.ReadBytes(d,Result,false);
  Result:=Length(d);
  Move(d[0],Buffer,Result);
  dec(FLeft,Result);
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

end.
