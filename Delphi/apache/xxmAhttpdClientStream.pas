unit xxmAhttpdClientStream;

interface

uses Windows, SysUtils, Classes, httpd24, ActiveX, xxm;

type
  TxxmAhttpdClientStream=class(TStream)
  private
    rq:PRequest;
    FStarted:boolean;
    FData:TStream;
    FDataPos,FDataSize:Int64;
    FDataFile:string;
  protected
    procedure SetSize(NewSize: Integer); override;
  public
    constructor Create(r:PRequest);
    destructor Destroy; override;
    function Read(var Buffer; Count: Integer): Integer; override;
    function Write(const Buffer; Count: Integer): Integer; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

  EXxmPostDataReadOnly=class(Exception);

  {$IF not(Declared(FixedUInt))}
  FixedUInt=LongInt;
  PFixedUInt=PLongInt;
  LargeInt=LongLongInt;
  LargeUInt=LargeInt;
  XDWORD=LongInt;
  {$ELSE}
  XDWORD=DWORD;
  {$IFEND}

  TRawSocketData=class(TInterfacedObject, IStream, IXxmRawSocket)
  private
    rq:PRequest;
    PeekData:AnsiString;
  public
    constructor Create(Request:PRequest);
    destructor Destroy; override;
    { IStream }
    function Seek(dlibMove: LargeInt; dwOrigin: XDWORD;
      out libNewPosition: LargeUInt): HResult; stdcall;
    function SetSize(libNewSize: LargeUInt): HResult; stdcall;
    function CopyTo(stm: IStream; cb: LargeUInt; out cbRead: LargeUInt;
      out cbWritten: LargeUInt): HResult; stdcall;
    function Commit(grfCommitFlags: XDWORD): HResult; stdcall;
    function Revert: HResult; stdcall;
    function LockRegion(libOffset, cb: LargeUInt; dwLockType: XDWORD): HResult; stdcall;
    function UnlockRegion(libOffset, cb: LargeUInt; dwLockType: XDWORD): HResult; stdcall;
    function Stat(out statstg: TStatStg; grfStatFlag: XDWORD): HResult; stdcall;
    function Clone(out stm: IStream): HResult; stdcall;
    { ISequentialStream }
    function Read(pv: Pointer; cb: FixedUInt;
      pcbRead: PFixedUInt): HResult; stdcall;
    function Write(pv: Pointer; cb: FixedUInt;
      pcbWritten: PFixedUInt): HResult; stdcall;
    { IXxmRawSocket }
    function DataReady(TimeoutMS: cardinal): boolean;
    procedure Disconnect;
  end;

implementation

resourcestring
  SXxmPostDataReadOnly='Post-data is read-only';

const
  SwitchToFileThreshold=$100000;//TODO: setting?

{ TxxmAhttpdClientStream }

constructor TxxmAhttpdClientStream.Create(r: PRequest);
begin
  inherited Create;
  rq:=r;
  FStarted:=false;//more init code, see read
  FData:=TMemoryStream.Create;
  FDataPos:=0;
  FDataSize:=0;
  //TODO: check header Content-length, if larger than SwitchToFileThreshold start with TFileStream
  FDataFile:='';
end;

destructor TxxmAhttpdClientStream.Destroy;
begin
  rq:=nil;
  FData.Free;
  if FDataFile<>'' then SysUtils.DeleteFile(FDataFile);
  inherited;
end;

function TxxmAhttpdClientStream.Read(var Buffer; Count: Integer): Integer;
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
    FStarted:=true;
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
    if (FDataFile='') and (FDataSize+Result>=SwitchToFileThreshold) then
     begin
      SetLength(FDataFile,$400);
      SetLength(FDataFile,GetTempPath($400,PChar(FDataFile)));//TODO: setting
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

function TxxmAhttpdClientStream.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
begin
  FDataPos:=FData.Seek(Offset,Origin);
  Result:=FDataPos;
end;

procedure TxxmAhttpdClientStream.SetSize(NewSize: Integer);
begin
  raise EXxmPostDataReadOnly.Create(SXxmPostDataReadOnly);
end;

function TxxmAhttpdClientStream.Write(const Buffer;
  Count: Integer): Integer;
begin
  raise EXxmPostDataReadOnly.Create(SXxmPostDataReadOnly);
end;

{ TRawSocketData }

constructor TRawSocketData.Create(Request: PRequest);
begin
  inherited Create;
  rq:=Request;
  PeekData:='';
end;

destructor TRawSocketData.Destroy;
begin
  rq:=nil;
  inherited;
end;

function TRawSocketData.Clone(out stm: IStream): HResult;
begin
  raise Exception.Create('TRawSocketData.Clone not supported');
end;

function TRawSocketData.Commit(grfCommitFlags: XDWORD): HResult;
begin
  raise Exception.Create('TRawSocketData.Commit not supported');
end;

function TRawSocketData.CopyTo(stm: IStream; cb: LargeUInt; out cbRead,
  cbWritten: LargeUInt): HResult;
begin
  raise Exception.Create('TRawSocketData.CopyTo not supported');
end;

function TRawSocketData.LockRegion(libOffset, cb: LargeUInt;
  dwLockType: XDWORD): HResult;
begin
  raise Exception.Create('TRawSocketData.LockRegion not supported');
end;

function TRawSocketData.Revert: HResult;
begin
  raise Exception.Create('TRawSocketData.Revert not supported');
end;

function TRawSocketData.Seek(dlibMove: LargeInt; dwOrigin: XDWORD;
  out libNewPosition: LargeUInt): HResult;
begin
  raise Exception.Create('TRawSocketData.Seek not supported');
end;

function TRawSocketData.SetSize(libNewSize: LargeUInt): HResult;
begin
  raise Exception.Create('TRawSocketData.SetSize not supported');
end;

function TRawSocketData.Stat(out statstg: TStatStg;
  grfStatFlag: XDWORD): HResult;
begin
  raise Exception.Create('TRawSocketData.Stat not supported');
end;

function TRawSocketData.UnlockRegion(libOffset, cb: LargeUInt;
  dwLockType: XDWORD): HResult;
begin
  raise Exception.Create('TRawSocketData.UnlockRegion not supported');
end;

function TRawSocketData.Read(pv: Pointer; cb: FixedUInt;
  pcbRead: PFixedUInt): HResult;
var
  l:integer;
  bb:PBucketBrigade;
begin
  if PeekData<>'' then
   begin
    l:=Length(PeekData);
    Move(PeekData[1],pv^,l);
    PeekData:='';
   end
  else
   begin
    //l:=ap_get_client_block(rq,pv,cb);//doesn't work?
    l:=0;//default
    bb:=apr_brigade_create(rq.pool,rq.connection.bucket_alloc);
    if bb<>nil then
     begin
      if ap_get_brigade(rq.input_filters,bb,AP_MODE_READBYTES,
        APR_BLOCK_READ,cb)=APR_SUCCESS then
       begin
        l:=cb;
        if apr_brigade_flatten(bb,pv,@l)<>APR_SUCCESS then l:=0;
       end;
      apr_brigade_destroy(bb);
     end;

   end;
  if pcbRead<>nil then pcbRead^:=l;
  Result:=S_OK;
end;

function rws_flush(bb: PBucketBrigade; ctx: Pointer): TStatus; cdecl;
begin
  //ap_pass_brigade?
  //apr_brigade_cleanup?
  Result:=APR_SUCCESS;
end;

function TRawSocketData.Write(pv: Pointer; cb: FixedUInt;
  pcbWritten: PFixedUInt): HResult;
var
  l:integer;
  bb:PBucketBrigade;
  f:PFilter;
begin
  //l:=ap_rwrite(pv^,cb,rq);
  //ap_rflush(rq);
  l:=0;//default
  bb:=apr_brigade_create(rq.pool,rq.connection.bucket_alloc);
  if bb<>nil then
   begin
    f:=rq.connection.output_filters;
    if apr_brigade_write(bb,rws_flush,f,pv,cb)=APR_SUCCESS then
      if f.frec.filter_func.out_func(f,bb)=APR_SUCCESS then
        l:=cb;
    ap_fflush(f,bb);
    apr_brigade_destroy(bb);
   end;

  if pcbWritten<>nil then pcbWritten^:=l;
  Result:=S_OK;
end;

function TRawSocketData.DataReady(TimeoutMS: cardinal): boolean;
var
  l:integer;
begin
  l:=$100;//?
  SetLength(PeekData,l);
  l:=ap_get_client_block(rq,@PeekData[1],l);
  SetLength(PeekData,l);
  Result:=l<>0;
end;

procedure TRawSocketData.Disconnect;
begin
  //TODO:
  ap_lingering_close(rq.connection);
  //FContext.Disconnect;?
end;

end.
