unit xxmAhttpdContext;

interface

uses SysUtils, Classes, ActiveX, httpd24, xxm, xxmContext,
  xxmHeaders, xxmParams, xxmPReg, xxmParUtils;

type
  TxxmAhttpdContext=class(TXxmGeneralContext
    ,IXxmHttpHeaders
    //,IXxmContextSuspend //TODO:
    //,IXxmSocketSuspend
    )
  private
    rq: PRequest;
    FConnected: boolean;
    FRedirectPrefix: WideString;
    FCookieParsed: boolean;
    FCookie: AnsiString;
    FCookieIdx: TParamIndexes;
  protected
    function ContextString(cs:TXxmContextString):WideString; override;
    function Connected:boolean; override;
    procedure BeginRequest; override;
    procedure HandleRequest; override;
    procedure EndRequest; override;
    procedure Redirect(const RedirectURL:WideString; Relative:boolean); override;
    function GetCookie(const Name:WideString):WideString; override;
    procedure SendHeader; override;
    function SendData(const Buffer; Count: LongInt): LongInt;
    function GetRequestHeader(const Name: WideString): WideString; override;
    procedure AddResponseHeader(const Name, Value: WideString); override;
    procedure SetStatus(Code:integer;const Text:WideString); override;
    function GetRawSocket: IStream; override;
    { IxxmHttpHeaders }
    function GetRequestHeaders:IxxmDictionaryEx;
    function GetResponseHeaders:IxxmDictionaryEx;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    procedure Perform(r: PRequest);
    property XConnected: boolean read FConnected;
  end;

implementation

uses Windows, Variants, ComObj, xxmCommonUtils,
  xxmAhttpdClientStream, xxmAhttpdPars;

resourcestring
  SXxmRWriteFailed='ap_rwrite failed';

{ TxxmAhttpdContext }

procedure TxxmAhttpdContext.AfterConstruction;
begin
  SendDirect:=SendData;
  FCookieIdx.ParsIndex:=0;
  FCookieIdx.ParsSize:=0;
  inherited;
end;

destructor TxxmAhttpdContext.Destroy;
begin
  rq:=nil;
  inherited;
end;

procedure TxxmAhttpdContext.Perform(r: PRequest);
begin
  State:=ctHeaderNotSent;
  FURL:=UTF8ToWideString(ap_construct_url(r.pool,r.unparsed_uri,r));
  rq:=r;

  BeginRequest;
  HandleRequest;
  //TODO if State then?
  EndRequest;
  Recycle;
end;

procedure TxxmAhttpdContext.BeginRequest;
begin
  inherited;
  FConnected:=true;
  FRedirectPrefix:='';//see Execute
  FCookieParsed:=false;
end;

procedure TxxmAhttpdContext.EndRequest;
begin
  inherited;
  FConnected:=false;
end;

procedure TxxmAhttpdContext.HandleRequest;
var
  x,y:AnsiString;
  i:integer;
begin
  try
    //AddResponseHeader('X-Powered-By',SelfVersion);
    if XxmProjectCache=nil then XxmProjectCache:=TXxmProjectCache.Create;

    //parse url
    x:=rq.uri;
    y:=rq.path_info;

    i:=2;
    if XxmProjectCache.ProjectFromURI(Self,x,i,FProjectName,FFragmentName) then
      FRedirectPrefix:=FRedirectPrefix+'/'+FProjectName
    else
      FRedirectPrefix:=UTF8ToWideString(Copy(x,1,Length(x)-Length(y)));//?

    x:=apr_table_get(rq.headers_in,'Content-Length');
    if x<>'' then FPostData:=TxxmAhttpdClientStream.Create(rq);

    BuildPage;

    //TODO: rq.header_only?

  except
    on EXxmPageRedirected do Flush;
    on EXxmAutoBuildFailed do ;//assert output done
    on e:Exception do
      if not HandleException(e) then
       begin
        rq.status:=500;
        rq.status_line:=apr_pstrdup(rq.pool,'500 Internal Server Error');
        try
          if FPostData=nil then x:='none' else
            x:=AnsiString(IntToStr(FPostData.Size))+' bytes';
        except
          x:='unknown';
        end;
        SendError('error',e.ClassName,e.Message);
       end;
  end;
end;

function TxxmAhttpdContext.Connected: boolean;
begin
  //Result:=not(rq.connection.aborted);
  Result:=((rq.connection.flags1 and 1)=0);
end;

function TxxmAhttpdContext.ContextString(
  cs: TXxmContextString): WideString;
begin
  case cs of
    csVersion:Result:=SelfVersion+', '+WideString(ap_get_server_description);//+ap_get_server_banner?
    csExtraInfo:Result:='';//TODO?
    csVerb:Result:=WideString(rq.method);
    csQueryString:Result:=WideString(rq.args);
    csUserAgent:Result:=WideString(apr_table_get(rq.headers_in,'User-Agent'));
    csAcceptedMimeTypes:Result:=WideString(apr_table_get(rq.headers_in,'Accept'));
    csPostMimeType:Result:=WideString(apr_table_get(rq.headers_in,'Content-Type'));
    csURL:Result:=GetURL;
    csReferer:Result:=WideString(apr_table_get(rq.headers_in,'Referer'));
    csLanguage:Result:=WideString(apr_table_get(rq.headers_in,'Accept-Language'));
    csRemoteAddress:Result:=WideString(rq.main.connection.client_id);
    csRemoteHost:
      if rq.main.connection.remote_host=nil then
        Result:=WideString(rq.main.connection.client_id) //TODO: resolve now?
      else
        Result:=WideString(rq.main.connection.remote_host);
    csAuthUser,csAuthPassword:Result:=UTF8ToWideString(AuthValue(cs));
    csProjectName:Result:=FProjectName;
    csLocalURL:Result:=FFragmentName;
  end;
end;

function TxxmAhttpdContext.GetCookie(const Name: WideString): WideString;
begin
  if not(FCookieParsed) then
   begin
    FCookie:=apr_table_get(rq.headers_in,'Cookie');
    SplitHeaderValue(FCookie,0,Length(FCookie),FCookieIdx);
    FCookieParsed:=true;
   end;
  Result:=UTF8ToWideString(GetParamValue(FCookie,FCookieIdx,UTF8Encode(Name)));
end;

function TxxmAhttpdContext.GetRequestHeaders: IxxmDictionaryEx;
begin
  //TODO: check freed by ref counting?
  Result:=TxxmAhttpdTable.Create(rq.pool,rq.headers_in);
end;

function TxxmAhttpdContext.GetResponseHeaders: IxxmDictionaryEx;
begin
  //TODO: check freed by ref counting?
  Result:=TxxmAhttpdTable.Create(rq.pool,rq.headers_out);
end;

procedure TxxmAhttpdContext.Redirect(const RedirectURL: WideString;
  Relative: boolean);
var
  NewURL,RedirBody:WideString;
begin
  //HeaderOK;//see SetStatus
  SetStatus(301,'Moved Permanently');
  NewURL:=RedirectURL;
  if Relative and (NewURL<>'') and (NewURL[1]='/') then NewURL:=FRedirectPrefix+NewURL;
  AddResponseHeader('Location',NewURL);
  //TODO: move this to execute's except?
  RedirBody:='<a href="'+HTMLEncode(NewURL)+'">'+HTMLEncode(NewURL)+'</a>'#13#10;
  case FAutoEncoding of
    aeUtf8:
      AddResponseHeader('Content-Length',IntToStr(Length(UTF8Encode(RedirBody))+3));
    aeUtf16:
      AddResponseHeader('Content-Length',IntToStr(Length(RedirBody)*2+2));
    aeIso8859:
      AddResponseHeader('Content-Length',IntToStr(Length(AnsiString(RedirBody))));
  end;
  SendStr(RedirBody);
  if BufferSize<>0 then Flush;
  raise EXxmPageRedirected.Create(RedirectURL);
end;

function TxxmAhttpdContext.SendData(const Buffer; Count: LongInt): LongInt;
begin
  if Count=0 then Result:=0 else
    Result:=ap_rwrite(pointer(@Buffer)^,Count,rq);
end;

procedure TxxmAhttpdContext.SetStatus(Code: integer; const Text: WideString);
begin
  inherited;
  rq.status:=Code;
  rq.status_line:=apr_pstrdup(rq.pool,
    PAnsiChar(AnsiString(IntToStr(Code))+' '+AnsiString(Text)));
end;

procedure TxxmAhttpdContext.SendHeader;
begin
  //rq.status_line Sent by first ap_rwrite?
  if FContentType<>'' then
   begin
    rq.content_type:=apr_pstrdup(rq.pool,PAnsiChar(AnsiString(FContentType)));
    case FAutoEncoding of
      aeUtf8:   rq.content_encoding:=apr_pstrdup(rq.pool,PAnsiChar('utf-8'));
      aeUtf16:  rq.content_encoding:=apr_pstrdup(rq.pool,PAnsiChar('utf-16'));
      aeIso8859:rq.content_encoding:=apr_pstrdup(rq.pool,PAnsiChar('iso-8859-15'));
      else      ;//rq.content_encoding:=apr_pstrdup(rq.pool,PAnsiChar('?
    end;
   end;
  //TODO: else get from response headers?
  //rq.connection.keepalive?//TODO
  //ap_rflush? ap_send_interim_response?
  inherited;
end;

function TxxmAhttpdContext.GetRequestHeader(const Name: WideString): WideString;
begin
  Result:=WideString(apr_table_get(rq.headers_in,PAnsiChar(AnsiString(Name))));
end;

procedure TxxmAhttpdContext.AddResponseHeader(const Name, Value: WideString);
begin
  HeaderNameSet(Name);
  HeaderCheckValue(Value);
  if SettingCookie then
   begin
    SettingCookie:=false;
    apr_table_add(rq.headers_out,
      apr_pstrdup(rq.pool,PAnsiChar(AnsiString(Name))),
      apr_pstrdup(rq.pool,PAnsiChar(AnsiString(Value))));
   end
  else
    apr_table_set(rq.headers_out,
      apr_pstrdup(rq.pool,PAnsiChar(AnsiString(Name))),
      apr_pstrdup(rq.pool,PAnsiChar(AnsiString(Value))));
end;

function TxxmAhttpdContext.GetRawSocket: IStream;
var
  f:PFilter;
begin
  if GetRequestHeader('Upgrade')='' then Result:=nil else
   begin
    //send header
    FContentType:='';
    CheckSendStart(false);
    SetBufferSize(0);//!
    {
    c:=ap_setup_client_block(rq,REQUEST_CHUNKED_DECHUNK);
    if c<>AP_OK then raise Exception.Create('ap_setup_client_block:'+IntToStr(c));
    }
    ap_send_interim_response(rq,1);

    rq.connection.keepalive:=AP_CONN_CLOSE;

    //TODO: apr_socket_timeout_set

    //remove http filters
    f:=rq.input_filters;
    while (f<>nil) and not((f.frec<>nil) and (f.frec.name='http_in')) do
      f:=f.next;
    if f<>nil then ap_remove_input_filter(f);
    {
    f:=rq.output_filters;
    while (f<>nil) and not((f.frec<>nil) and (f.frec.name='http_out')) do
      f:=f.next;
    if f<>nil then ap_remove_output_filter(f);
    }

    //raw socket
    Result:=TRawSocketData.Create(rq);
   end;
end;

initialization
  StatusBuildError:=503;//TODO: from settings
  StatusException:=500;
  StatusFileNotFound:=404;
end.
