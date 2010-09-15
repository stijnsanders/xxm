unit xxmApacheContext;

interface

uses SysUtils, Classes, ActiveX, HTTPD2, xxm, xxmContext,
  xxmHeaders, xxmParams, xxmPReg, xxmHttpPReg, xxmParUtils;

type
  TxxmApacheContext=class(TXxmGeneralContext, IxxmHttpHeaders)
  private
    rq: Prequest_rec;

    FConnected: boolean;
    FRedirectPrefix, FSessionID: AnsiString;
    FCookieParsed: boolean;
    FCookie: AnsiString;
    FCookieIdx: TParamIndexes;

  protected
    procedure SendStream(s:IStream); override;
    function GetSessionID:WideString; override;
    procedure DispositionAttach(FileName: WideString); override;
    function ContextString(cs:TXxmContextString):WideString; override;
    function Connected:boolean; override;
    procedure Redirect(RedirectURL:WideString; Relative:boolean); override;
    function GetCookie(Name:WideString):WideString; override;
    procedure SetCookie(Name,Value:WideString); overload; override;
    procedure SetCookie(Name,Value:WideString; KeepSeconds:cardinal;
      Comment,Domain,Path:WideString; Secure,HttpOnly:boolean); overload; override;
    //procedure SetCookie2();
    procedure SendHeader; override;
    procedure SendRaw(Data: WideString); override;
    function GetProjectEntry(ProjectName: WideString):TXxmProjectEntry; override;
    procedure AddResponseHeader(Name: WideString; Value: WideString); override;
    procedure SetStatus(Code:integer;Text:WideString); override;

    //IxxmHttpHeaders
    function GetRequestHeaders:IxxmDictionaryEx;
    function GetResponseHeaders:IxxmDictionaryEx;

  public
    constructor Create(r:Prequest_rec);
    destructor Destroy; override;

    procedure Execute;
  end;

  EXxmRWriteFailed=class(Exception);
  EXxmAutoBuildFailed=class(Exception);
  EXxmPageRedirected=class(Exception);

implementation

uses Windows, Variants, ComObj, xxmCommonUtils, xxmApacheClientStream, xxmApachePars;

resourcestring
  SXxmRWriteFailed='ap_rwrite failed';

{ TxxmApacheContext }

constructor TxxmApacheContext.Create(r: Prequest_rec);
begin
  inherited Create(ap_construct_url(r.pool,r.unparsed_uri,r));
  rq:=r;
  FConnected:=true;
  FRedirectPrefix:='';//see Execute
  FCookieParsed:=false;
  FSessionID:='';//see GetSessionID
end;

destructor TxxmApacheContext.Destroy;
begin
  rq:=nil;
  inherited;
end;

procedure TxxmApacheContext.Execute;
var
  x,y:AnsiString;
  i,l:integer;
begin
  try
    AddResponseHeader('X-Powered-By',SelfVersion);
    if XxmProjectCache=nil then XxmProjectCache:=TXxmProjectCache.Create;

    //parse url
    x:=rq.uri;
    y:=rq.path_info;
    FRedirectPrefix:=copy(x,1,Length(x)-Length(y));

    l:=Length(x);
    i:=2;
    if XxmProjectCache.SingleProject='' then
     begin
      while (i<=l) and not(char(x[i]) in ['/','?','&','$','#']) do inc(i);
      FProjectName:=Copy(x,2,i-2);
      if FProjectName='' then
       begin
        if (i<=l) and not(x[i]='/') then x:='/'+x;
        Redirect('/'+XxmProjectCache.DefaultProject+x,true);
       end;
      FPageClass:='['+FProjectName+']';
      if (i>l) and (l>1) then Redirect(x+'/',true) else
        if (x[i]='/') then inc(i);
     end
    else
     begin
      FProjectName:=XxmProjectCache.SingleProject;
      FPageClass:='[SingleProject]';
     end;
    FFragmentName:=Copy(x,i,l-i+1);

    x:=apr_table_get(rq.headers_in,'Content-Length');
    if not(x='') then FPostData:=TxxmApacheClientStream.Create(rq);

    BuildPage;

  except
    on e:EXxmPageRedirected do
      ;//assert output done
    on EXxmAutoBuildFailed do
      ;//assert output done
    on e:Exception do
     begin
      //TODO: get fragment 500.xxm?
      rq.status:=500;
      rq.status_line:=apr_pstrdup(rq.pool,'500 Internal Server Error');
      try
        if FPostData=nil then x:='none' else x:=IntToStr(FPostData.Size)+' bytes';
      except
        x:='unknown';
      end;
      SendError('error',[
        'URL',HTMLEncode(rq.unparsed_uri),
        'CLASS',FPageClass,
        'POSTDATA',x,
        'QUERYSTRING',HTMLEncode(rq.args),
        'ERROR',HTMLEncode(e.Message),
        'ERRORCLASS',e.ClassName,
        'VERSION',SelfVersion
      ]);
     end;
  end;
end;

function TxxmApacheContext.GetProjectEntry(
  ProjectName: WideString): TXxmProjectEntry;
begin
  Result:=XxmProjectCache.GetProject(FProjectName);
end;

function TxxmApacheContext.Connected: boolean;
begin
  //Result:=not(rq.connection.aborted);
  Result:=((rq.connection.flags1 and 1)=0);
end;

function TxxmApacheContext.ContextString(
  cs: TXxmContextString): WideString;
begin
  case cs of
    csVersion:Result:=SelfVersion+', '+ap_get_server_version;
    csExtraInfo:Result:='';//TODO?
    csVerb:Result:=rq.method;
    csQueryString:Result:=rq.args;
    csUserAgent:Result:=apr_table_get(rq.headers_in,'User-Agent');
    csAcceptedMimeTypes:Result:=apr_table_get(rq.headers_in,'Accept');
    csPostMimeType:Result:=apr_table_get(rq.headers_in,'Content-Type');
    csURL:Result:=GetURL;
    csReferer:Result:=apr_table_get(rq.headers_in,'Referer');
    csLanguage:Result:=apr_table_get(rq.headers_in,'Accept-Language');
    csRemoteAddress:Result:=rq.main.connection.remote_ip;
    csRemoteHost:Result:=rq.main.connection.remote_host;
    csAuthUser:Result:=apr_table_get(rq.headers_in,'Auth-User');
    csAuthPassword:Result:=apr_table_get(rq.headers_in,'Auth-Password');
    csProjectName:Result:=FProjectName;
    csLocalURL:Result:=FFragmentName;
  end;
end;

procedure TxxmApacheContext.DispositionAttach(FileName: WideString);
begin
  if FileName='' then
    AddResponseHeader('Content-Disposition','attachment')
  else
    AddResponseHeader('Content-Disposition','attachment; filename="'+FileName+'"');
end;

function TxxmApacheContext.GetCookie(Name: WideString): WideString;
begin
  if not(FCookieParsed) then
   begin
    FCookie:=apr_table_get(rq.headers_in,'Cookie');
    SplitHeaderValue(FCookie,0,Length(FCookie),FCookieIdx);
    FCookieParsed:=true;
   end;
  Result:=GetParamValue(FCookie,FCookieIdx,Name);
end;

procedure TxxmApacheContext.SetCookie(Name, Value: WideString);
begin
  CheckHeaderNotSent;
  //check name?
  //TODO: "quoted string"?
  AddResponseHeader('Cache-Control','no-cache="set-cookie"');
  AddResponseHeader('Set-Cookie',Name+'="'+Value+'"');
end;

procedure TxxmApacheContext.SetCookie(Name, Value: WideString;
  KeepSeconds: cardinal; Comment, Domain, Path: WideString; Secure,
  HttpOnly: boolean);
var
  x:AnsiString;
begin
  CheckHeaderNotSent;
  //check name?
  //TODO: "quoted string"?
  AddResponseHeader('Cache-Control','no-cache="set-cookie"');
  x:=Name+'="'+Value+'"';
  //'; Version=1';
  if not(Comment='') then
    x:=x+'; Comment="'+Comment+'"';
  if not(Domain='') then
    x:=x+'; Domain="'+Domain+'"';
  if not(Path='') then
    x:=x+'; Path="'+Path+'"';
  x:=x+'; Max-Age='+IntToStr(KeepSeconds)+
    '; Expires="'+RFC822DateGMT(Now+KeepSeconds/86400)+'"';
  if Secure then
    x:=x+'; Secure'+#13#10;
  if HttpOnly then
    x:=x+'; HttpOnly'+#13#10;
  AddResponseHeader('Set-Cookie',x);
  //TODO: Set-Cookie2
end;

function TxxmApacheContext.GetRequestHeaders: IxxmDictionaryEx;
begin
  //TODO: check freed by ref counting?
  Result:=TxxmApacheTable.Create(rq.pool,rq.headers_in);
end;

function TxxmApacheContext.GetResponseHeaders: IxxmDictionaryEx;
begin
  //TODO: check freed by ref counting?
  Result:=TxxmApacheTable.Create(rq.pool,rq.headers_out);
end;

function TxxmApacheContext.GetSessionID: WideString;
const
  SessionCookie='xxmSessionID';
begin
  if FSessionID='' then
   begin
    FSessionID:=GetCookie(SessionCookie);
    if FSessionID='' then
     begin
      FSessionID:=Copy(CreateClassID,2,32);
      SetCookie(SessionCookie,FSessionID);//expiry?
     end;
   end;
  Result:=FSessionID;
end;

procedure TxxmApacheContext.Redirect(RedirectURL: WideString;
  Relative: boolean);
begin
  //HeaderOK;//see SetStatus
  SetStatus(301,'Moved Permanently');
  //TODO: relative
  AddResponseHeader('Location',RedirectURL);
  //TODO: move this to execute's except?
  SendHTML('<a href="'+HTMLEncode(RedirectURL)+'">'+HTMLEncode(RedirectURL)+'</a>');
  raise EXxmPageRedirected.Create(RedirectURL);
end;

procedure TxxmApacheContext.SendRaw(Data: WideString);
const
  Utf8ByteOrderMark=#$EF#$BB#$BF;
  Utf16ByteOrderMark=#$FF#$FE;
var
  s:AnsiString;
  l:integer;
begin
  if not(Data='') then
   begin
    if CheckSendStart then
      case FAutoEncoding of
        aeUtf8:
         begin
          s:=Utf8ByteOrderMark;
          if not(ap_rwrite(s[1],3,rq)=3) then raise EXxmRWriteFailed.Create(SXxmRWriteFailed);
         end;
        aeUtf16:
         begin
          s:=Utf16ByteOrderMark;
          if not(ap_rwrite(s[1],2,rq)=2) then raise EXxmRWriteFailed.Create(SXxmRWriteFailed);
         end;
      end;
    case FAutoEncoding of
      aeUtf16:
       begin
        l:=Length(Data)*2;
        if not(ap_rwrite(Data[1],l,rq)=l) then raise EXxmRWriteFailed.Create(SXxmRWriteFailed);
       end;
      aeUtf8:
       begin
        s:=UTF8Encode(Data);
        l:=Length(s);
        if not(ap_rwrite(s[1],l,rq)=l) then raise EXxmRWriteFailed.Create(SXxmRWriteFailed);
       end;
      else
       begin
        s:=Data;
        l:=Length(s);
        if not(ap_rwrite(s[1],l,rq)=l) then raise EXxmRWriteFailed.Create(SXxmRWriteFailed);
       end;
    end;
    ap_rflush(rq);//? only every x bytes?
   end;
end;

procedure TxxmApacheContext.SendStream(s: IStream);
const
  dSize=$10000;
var
  d:array[0..dSize-1] of byte;
  l:integer;
begin
  {
  if not(FHeaderSent) then
   begin
    //TODO: (Apache does 'Transfer-Encoding: chunked' for us!)
    //'Content-Length':=IntToStr(s.Size);
    //'Accept-Ranges':='bytes';
   end;
  }
  CheckSendStart;
  repeat
    OleCheck(s.Read(@d[0],dSize,@l));
    if not(ap_rwrite(d[0],l,rq)=l) then raise EXxmRWriteFailed.Create(SXxmRWriteFailed);
    ap_rflush(rq);//?
  until l<>dSize;
end;

procedure TxxmApacheContext.SetStatus(Code: integer; Text: WideString);
begin
  inherited;
  rq.status:=Code;
  rq.status_line:=apr_pstrdup(rq.pool,PAnsiChar(IntToStr(Code)+' '+AnsiString(Text)));
end;

procedure TxxmApacheContext.SendHeader;
begin
  //rq.status_line Sent by first ap_rwrite?
  rq.content_type:=apr_pstrdup(rq.pool,PAnsiChar(AnsiString(FContentType)));
  case FAutoEncoding of
    aeUtf8:   rq.content_encoding:=apr_pstrdup(rq.pool,PAnsiChar('utf-8'));
    aeUtf16:  rq.content_encoding:=apr_pstrdup(rq.pool,PAnsiChar('utf-16'));
    aeIso8859:rq.content_encoding:=apr_pstrdup(rq.pool,PAnsiChar('iso-8859-15'));
    else      ;//rq.content_encoding:=apr_pstrdup(rq.pool,PAnsiChar('?
  end;
  //rq.connection.keepalive?//TODO
end;

procedure TxxmApacheContext.AddResponseHeader(Name: WideString; Value: WideString);
begin
  apr_table_set(rq.headers_out,
    apr_pstrdup(rq.pool,PAnsiChar(AnsiString(Name))),
    apr_pstrdup(rq.pool,PAnsiChar(AnsiString(Value))));
end;

end.
