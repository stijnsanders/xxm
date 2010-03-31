unit xxmApacheClientStream;

interface

uses SysUtils, Classes, HTTPD2;

type
  TxxmApacheClientStream=class(TStream)
  private
    rq:Prequest_rec;
    FStarted:boolean;
  protected
    procedure SetSize(NewSize: Integer); override;
  public
    constructor Create(r:Prequest_rec);
    destructor Destroy; override;
    function Read(var Buffer; Count: Integer): Integer; override;
    function Write(const Buffer; Count: Integer): Integer; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

  EXxmPostDataReadOnly=class(Exception);

implementation

resourcestring
  SXxmPostDataReadOnly='Post-data is read-only';

{ TxxmApacheClientStream }

constructor TxxmApacheClientStream.Create(r: Prequest_rec);
begin
  inherited Create;
  rq:=r;
  FStarted:=false;//more init code, see read
end;

destructor TxxmApacheClientStream.Destroy;
begin
  rq:=nil;
  inherited;
end;

function TxxmApacheClientStream.Read(var Buffer; Count: Integer): Integer;
var
  i:integer;
begin
  if not(FStarted) then
   begin
    i:=ap_setup_client_block(rq,REQUEST_CHUNKED_DECHUNK);
    if not(i=AP_OK) then raise Exception.Create('ap_setup_client_block:'+IntToStr(i));
    i:=ap_should_client_block(rq);
    if not(i=1) then raise Exception.Create('ap_should_client_block:'+IntToStr(i));
   end;
  Result:=ap_get_client_block(rq,PAnsiChar(@Buffer),Count);
end;

function TxxmApacheClientStream.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
begin
  //TODO;
  raise EXxmPostDataReadOnly.Create(SXxmPostDataReadOnly);
end;

procedure TxxmApacheClientStream.SetSize(NewSize: Integer);
begin
  raise EXxmPostDataReadOnly.Create(SXxmPostDataReadOnly);
end;

function TxxmApacheClientStream.Write(const Buffer;
  Count: Integer): Integer;
begin
  raise EXxmPostDataReadOnly.Create(SXxmPostDataReadOnly);
end;

end.
