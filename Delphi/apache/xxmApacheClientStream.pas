unit xxmApacheClientStream;

interface

uses SysUtils, Classes, HTTPD2;

type
  TxxmApacheClientStream=class(TStream)
  private
    rq:Prequest_rec;
    FStarted:boolean;
    FData:TStream;
    FDataPos,FDataSize:Int64;
    FDataFile:AnsiString;
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

uses
  Windows;

resourcestring
  SXxmPostDataReadOnly='Post-data is read-only';

const
  SwitchToFileTreshold=$100000;//TODO: setting?

{ TxxmApacheClientStream }

constructor TxxmApacheClientStream.Create(r: Prequest_rec);
begin
  inherited Create;
  rq:=r;
  FStarted:=false;//more init code, see read
  FData:=TMemoryStream.Create;
  FDataPos:=0;
  FDataSize:=0;
  //TODO: check header Content-length, if larger than SwitchToFileTreshold start with TFileStream
  FDataFile:='';
end;

destructor TxxmApacheClientStream.Destroy;
begin
  rq:=nil;
  FData.Free;
  if FDataFile<>'' then SysUtils.DeleteFile(FDataFile);
  inherited;
end;

function TxxmApacheClientStream.Read(var Buffer; Count: Integer): Integer;
var
  c:integer;
  f:TFileStream;
begin
  if not(FStarted) then
   begin
    c:=ap_setup_client_block(rq,REQUEST_CHUNKED_DECHUNK);
    if c<>AP_OK then raise Exception.Create('ap_setup_client_block:'+IntToStr(c));
    ap_should_client_block(rq);
    //if <>1 then raise Exception.Create('ap_should_client_block:'+IntToStr(c));
   end;
  Result:=0;
  if FDataPos=FDataSize then c:=Count else
   begin
    if FDataPos+Count>FDataSize then c:=FDataPos+Count-FDataSize else c:=Count;
    Result:=FData.Read(Buffer,c);
    //assert c=Result
    inc(FDataPos,c);
    c:=Count-c;
   end;
  if c<>0 then
   begin
    //assert FDataPos=FDataLen
    inc(Result,ap_get_client_block(rq,PAnsiChar(@Buffer),c));
    //assert c=Result
    if (FDataFile='') and (FDataSize+Result>=SwitchToFileTreshold) then
     begin
      SetLength(FDataFile,$400);
      SetLength(FDataFile,GetTempPathA($400,PAnsiChar(FDataFile)));//TODO: setting
      FDataFile:=FDataFile+'xxm_'+IntToStr(GetCurrentProcessId)+'_'+IntToStr(GetCurrentThreadId)+'.dat';
      f:=TFileStream.Create(FDataFile,fmCreate);
      f.Write((FData as TMemoryStream).Memory^,FDataSize);
      FData.Free;
      FData:=f;
     end;
    FData.Write(Buffer,Result);
    inc(FDataSize,Result);
    FDataPos:=FDataSize;
   end;
end;

function TxxmApacheClientStream.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
begin
  FDataPos:=FData.Seek(Offset,Origin);
  Result:=FDataPos;
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
