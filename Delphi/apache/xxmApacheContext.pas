unit xxmApacheContext;

interface

uses SysUtils, Classes, HTTPD2, xxm, xxmHeaders, xxmParams, xxmHttpPReg, xxmParUtils;

type
  TxxmApacheContext=class(TInterfacedObject, IxxmContext, IxxmHttpHeaders)
  private
    rq:Prequest_rec;

    FConnected,FHeaderSent:boolean;
    FAutoEncoding: TXxmAutoEncoding;
    FContentType,FRedirectPrefix,FPageClass,FProjectName,FFragmentName,FSessionID: AnsiString;
    FProjectEntry:TXxmProjectCacheEntry;
    FPage, FBuilding: IXxmFragment;
    FPostData: TStream;
    FIncludeDepth:integer;
    FParams: TXxmReqPars;
    FCookieParsed: boolean;
    FCookie: AnsiString;
    FCookieIdx: TParamIndexes;

    function CheckHeader:boolean;
    procedure HeaderOK;
    procedure SendRaw(Data: WideString);
    procedure SendError(res:AnsiString;vals:array of AnsiString);

  private
    //IxxmContext
    function GetURL:WideString;
    function GetPage:IXxmFragment;
    function GetContentType:WideString;
    procedure SetContentType(const Value: WideString);
    function GetAutoEncoding:TXxmAutoEncoding;
    procedure SetAutoEncoding(const Value: TXxmAutoEncoding);
    function GetParameter(Key:OleVariant):IXxmParameter;
    function GetParameterCount:integer;
    function GetSessionID:WideString;

    procedure Send(Data: OleVariant); overload;
    procedure SendHTML(Data: OleVariant); overload;
    procedure SendFile(FilePath: WideString);
    procedure SendStream(s:TStream); //TODO: IStream
    procedure Include(Address: WideString); overload;
    procedure Include(Address: WideString;
      const Values: array of OleVariant); overload;
    procedure Include(Address: WideString;
      const Values: array of OleVariant;
      const Objects: array of TObject); overload;
    procedure DispositionAttach(FileName: WideString);

    function ContextString(cs:TXxmContextString):WideString;
    function PostData:TStream; //TODO: IStream
    function Connected:boolean;

    //(local:)progress
    procedure SetStatus(Code:integer;Text:WideString);
    procedure Redirect(RedirectURL:WideString; Relative:boolean);
    function GetCookie(Name:WideString):WideString;
    procedure SetCookie(Name,Value:WideString); overload;
    procedure SetCookie(Name,Value:WideString; KeepSeconds:cardinal;
      Comment,Domain,Path:WideString; Secure,HttpOnly:boolean); overload;
    //procedure SetCookie2();

    //added V1.0.1
    procedure Send(Value: integer); overload;
    procedure Send(Value: int64); overload;
    procedure Send(Value: cardinal); overload;
    procedure Send(const Values:array of OleVariant); overload;
    procedure SendHTML(const Values:array of OleVariant); overload;

    //IxxmHttpHeaders
    function GetRequestHeaders:IxxmDictionaryEx;
    function GetResponseHeaders:IxxmDictionaryEx;

  public
    constructor Create(r:Prequest_rec);
    destructor Destroy; override;

    procedure Execute;
  end;

  EXxmRWriteFailed=class(Exception);
  EXxmDirectInclude=class(Exception);
  EXxmAutoBuildFailed=class(Exception);
  EXxmPageRedirected=class(Exception);
  EXxmIncludeStackFull=class(Exception);
  EXxmIncludeFragmentNotFound=class(Exception);

implementation

uses Windows, Variants, ComObj, xxmCommonUtils, xxmPReg, xxmApacheClientStream, xxmApachePars;

resourcestring
  SXxmRWriteFailed='ap_rwrite failed';
  SXxmDirectInclude='Direct call to include fragment is not allowed.';
  SXxmIncludeStackFull='Maximum level of includes exceeded';
  SXxmIncludeFragmentNotFound='Include fragment not found "__"';

const
  XxmMaxIncludeDepth=64;//TODO: setting?

{ TxxmApacheContext }

constructor TxxmApacheContext.Create(r: Prequest_rec);
begin
  inherited Create;
  rq:=r;
  FConnected:=true;
  FHeaderSent:=false;
  FContentType:='text/html';
  FAutoEncoding:=aeUtf8;//default (setting?)
  FRedirectPrefix:='';//see Execute
  FPageClass:='';
  FProjectName:='';
  FFragmentName:='';
  FProjectEntry:=nil;
  FPage:=nil;
  FBuilding:=nil;
  FPostData:=nil;
  FIncludeDepth:=0;
  FParams:=nil;//see GetParameter
  FCookieParsed:=false;
  FSessionID:='';//see GetSessionID
end;

destructor TxxmApacheContext.Destroy;
begin
  rq:=nil;
  FreeAndNil(FPostData);
  FreeAndNil(FParams);
  if not(FProjectEntry=nil) then
   begin
    FProjectEntry.CloseContext;
    FProjectEntry:=nil;
   end;
  inherited;
end;

procedure TxxmApacheContext.Execute;
var
  x,y:AnsiString;
  i,l:integer;
  p:IXxmPage;
begin
  try
    apr_table_set(rq.headers_out,'X-Powered-By',PAnsiChar(SelfVersion));
    if XxmProjectCache=nil then XxmProjectCache:=TXxmProjectCache.Create;

    //parse url
    x:=rq.uri;
    y:=rq.path_info;
    FRedirectPrefix:=copy(x,1,Length(x)-Length(y));

    l:=Length(x);
    i:=2;
    if XxmProjectCache.SingleProject='' then
     begin
      while (i<=l) and not(x[i] in ['/','?','&','$','#']) do inc(i);
      FProjectName:=Copy(x,2,i-2);
      if FProjectName='' then
       begin
        if (i<=l) and not(x[i]='/') then x:='/'+x;
        Redirect('/'+XxmProjectCache.DefaultProject+x,true);
       end;
      FPageClass:='['+FProjectName+']';
      if i<=l then inc(i) else if l>1 then Redirect(x+'/',true);
     end
    else
     begin
      FProjectName:=XxmProjectCache.SingleProject;
      FPageClass:='[SingleProject]';
     end;
    FFragmentName:=Copy(x,i,l-i+1);

    x:=apr_table_get(rq.headers_in,'Content-Length');
    if not(x='') then FPostData:=TxxmApacheClientStream.Create(rq);

    FProjectEntry:=XxmProjectCache.GetProject(FProjectName);
    if not(@XxmAutoBuildHandler=nil) then
      if not(XxmAutoBuildHandler(FProjectEntry,Self,FProjectName)) then
        raise EXxmAutoBuildFailed.Create(FProjectName);
    FProjectEntry.OpenContext;
    FPage:=FProjectEntry.Project.LoadPage(Self,FFragmentName);

    if FPage=nil then
     begin
      //find a file
      //ask project to translate? project should have given a fragment!
      FPageClass:='['+FProjectName+']GetFilePath';
      FProjectEntry.GetFilePath(FFragmentName,x,y);
      if FileExists(x) then
       begin
        //TODO: Last Modified
        //TODO: if directory file-list?
        FContentType:=y;
        SendFile(x);
       end
      else
       begin
        FPageClass:='['+FProjectName+']404:'+FFragmentName;
        FPage:=FProjectEntry.Project.LoadPage(Self,'404.xxm');
        if FPage=nil then
          SendError('fnf',[
            'URL',HTMLEncode(ContextString(csURL)),
            'PROJECT',FProjectName,
            'ADDRESS',FFragmentName,
            'PATH',HTMLEncode(x),
            'VERSION',ContextString(csVersion)
          ])
        else
          try
            FPageClass:=FPage.ClassNameEx;
            FBuilding:=FPage;
            FPage.Build(Self,nil,[FFragmentName,x,y],[]);//any parameters?
          finally
            FBuilding:=nil;
            //let project free, cache or recycle
            FProjectEntry.Project.UnloadFragment(FPage);
            FPage:=nil;
          end;
       end;
     end
    else
      try
        FPageClass:=FPage.ClassNameEx;
        //mime type moved to CheckSendStart;
        //OleCheck(ProtSink.ReportProgress(BINDSTATUS_CACHEFILENAMEAVAILABLE,));
        //TODO: cache output?

        //TODO: setting?
        if not(FPage.QueryInterface(IID_IXxmPage,p)=S_OK) then
          raise EXxmDirectInclude.Create(SXxmDirectInclude);
        p:=nil;

        //build page
        FBuilding:=FPage;
        FPage.Build(Self,nil,[],[]);//any parameters?

      finally
        FBuilding:=nil;
        //let project decide to free or not
        FProjectEntry.Project.UnloadFragment(FPage);
        FPage:=nil;
      end;

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
var
  s:AnsiString;
begin
  if FileName='' then
    s:='attachment'
  else
    s:='attachment; filename="'+FileName+'"';
  apr_table_set(rq.headers_out,
    apr_pstrdup(rq.pool,PAnsiChar('Content-Disposition')),
    apr_pstrdup(rq.pool,PAnsiChar(s)));
end;

function TxxmApacheContext.GetAutoEncoding: TXxmAutoEncoding;
begin
  Result:=FAutoEncoding;
end;

function TxxmApacheContext.GetContentType: WideString;
begin
  //Result:=rq.content_type;//see CheckHeader
  Result:=FContentType;
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
  HeaderOK;
  //check name?
  //TODO: "quoted string"?
  apr_table_set(rq.headers_out,
    apr_pstrdup(rq.pool,'Cache-Control'),
    apr_pstrdup(rq.pool,'no-cache="set-cookie"'));
  apr_table_set(rq.headers_out,
    apr_pstrdup(rq.pool,'Set-Cookie'),
    apr_pstrdup(rq.pool,PAnsiChar(AnsiString(Name+'="'+Value+'"'))));
end;

procedure TxxmApacheContext.SetCookie(Name, Value: WideString;
  KeepSeconds: cardinal; Comment, Domain, Path: WideString; Secure,
  HttpOnly: boolean);
var
  x:AnsiString;
begin
  HeaderOK;
  //check name?
  //TODO: "quoted string"?
  apr_table_set(rq.headers_out,
    apr_pstrdup(rq.pool,'Cache-Control'),
    apr_pstrdup(rq.pool,'no-cache="set-cookie"'));
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
  apr_table_set(rq.headers_out,
    apr_pstrdup(rq.pool,'Set-Cookie'),
    apr_pstrdup(rq.pool,PAnsiChar(x)));
  //TODO: Set-Cookie2
end;

function TxxmApacheContext.GetPage: IXxmFragment;
begin
  Result:=FPage;
end;

function TxxmApacheContext.GetParameter(Key: OleVariant): IXxmParameter;
begin
  if FParams=nil then FParams:=TXxmReqPars.CreateNoSeek(Self);
  if VarIsNumeric(Key) then Result:=FParams.GetItem(Key) else
    Result:=FParams.Get(VarToWideStr(Key));
end;

function TxxmApacheContext.GetParameterCount: integer;
begin
  if FParams=nil then FParams:=TXxmReqPars.CreateNoSeek(Self);
  Result:=FParams.Count;
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

function TxxmApacheContext.GetURL: WideString;
begin
  Result:=ap_construct_url(rq.pool,rq.unparsed_uri,rq);
end;

procedure TxxmApacheContext.Include(Address: WideString);
begin
  Include(Address,[],[]);
end;

procedure TxxmApacheContext.Include(Address: WideString;
  const Values: array of OleVariant);
begin
  Include(Address,Values,[]);
end;

procedure TxxmApacheContext.Include(Address: WideString;
  const Values: array of OleVariant; const Objects: array of TObject);
var
  f,fb:IXxmFragment;
  pc:string;
begin
  if FIncludeDepth=XxmMaxIncludeDepth then
    raise EXxmIncludeStackFull.Create(SXxmIncludeStackFull);
  //FPage.Project??
  f:=FProjectEntry.Project.LoadFragment(Address);
  if f=nil then
    raise EXxmIncludeFragmentNotFound.Create(StringReplace(
      SXxmIncludeFragmentNotFound,'__',Address,[]));
  fb:=FBuilding;
  pc:=FPageClass;
  FBuilding:=f;
  inc(FIncludeDepth);
  try
    FPageClass:=f.ClassNameEx+' < '+pc;
    f.Build(Self,fb,Values,Objects);//queue to avoid building up stack?
  finally
    dec(FIncludeDepth);
    FBuilding:=fb;
    FPageClass:=pc;
    fb:=nil;
    FProjectEntry.Project.UnloadFragment(f);
    f:=nil;
  end;
end;

function TxxmApacheContext.PostData: TStream;
begin
  Result:=FPostData;
end;

procedure TxxmApacheContext.Redirect(RedirectURL: WideString;
  Relative: boolean);
begin
  //HeaderOK;//see SetStatus
  SetStatus(301,'Moved Permanently');
  //TODO: relative
  apr_table_set(rq.headers_out,'Location',PAnsiChar(AnsiString(RedirectURL)));
  //TODO: move this to execute's except?
  SendHTML('<a href="'+HTMLEncode(RedirectURL)+'">'+HTMLEncode(RedirectURL)+'</a>');
  raise EXxmPageRedirected.Create(RedirectURL);
end;

procedure TxxmApacheContext.SendRaw(Data: WideString);
const
  Utf8ByteOrderMark=#$EF#$BB#$BF;
  Utf16ByteOrderMark=#$FF#$FE;
var
  s:string;
  l:integer;
begin
  if not(Data='') then
   begin
    if CheckHeader then
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

procedure TxxmApacheContext.Send(Value: integer);
begin
  SendRaw(IntToStr(Value));
end;

procedure TxxmApacheContext.Send(Data: OleVariant);
begin
  SendRaw(HTMLEncode(Data));
end;

procedure TxxmApacheContext.Send(Value: int64);
begin
  SendRaw(IntToStr(Value));
end;

procedure TxxmApacheContext.Send(const Values: array of OleVariant);
var
  i:integer;
begin
  for i:=0 to Length(Values)-1 do SendRaw(HTMLEncode(Values[i]));
end;

procedure TxxmApacheContext.Send(Value: cardinal);
begin
  SendRaw(IntToStr(Value));
end;

procedure TxxmApacheContext.SendFile(FilePath: WideString);
var
  f:TFileStream;
begin
  f:=TFileStream.Create(FilePath,fmOpenRead or fmShareDenyNone);
  try
    SendStream(f);
  finally
    f.Free;
  end;
end;

procedure TxxmApacheContext.SendHTML(Data: OleVariant);
begin
  SendRaw(VarToWideStr(Data));
end;

procedure TxxmApacheContext.SendHTML(const Values: array of OleVariant);
var
  i:integer;
begin
  for i:=0 to Length(Values)-1 do SendRaw(VarToWideStr(Values[i]));
end;

procedure TxxmApacheContext.SendStream(s: TStream);
var
  d:array[0..$FFF] of byte;
  l:integer;
begin
  if not(FHeaderSent) then
   begin
    //TODO: (Apache does 'Transfer-Encoding: chunked' for us!)
    //'Content-Length':=IntToStr(s.Size);
    //'Accept-Ranges':='bytes';
   end;
  CheckHeader;
  repeat
    l:=s.Read(d[0],$1000);
    if not(ap_rwrite(d[0],l,rq)=l) then raise EXxmRWriteFailed.Create(SXxmRWriteFailed);
    ap_rflush(rq);//?
  until not(l=$1000);//s.Position=s.Size?
end;

procedure TxxmApacheContext.HeaderOK;
begin
  if FHeaderSent then
    raise EXxmResponseHeaderAlreadySent.Create(SXxmResponseHeaderAlreadySent);
end;

procedure TxxmApacheContext.SetAutoEncoding(const Value: TXxmAutoEncoding);
begin
  HeaderOK;
  FAutoEncoding:=Value;
end;

procedure TxxmApacheContext.SetContentType(const Value: WideString);
begin
  HeaderOK;
  FContentType:=Value;
end;

procedure TxxmApacheContext.SetStatus(Code: integer; Text: WideString);
begin
  HeaderOK;
  rq.status:=Code;
  rq.status_line:=apr_pstrdup(rq.pool,PAnsiChar(IntToStr(Code)+' '+AnsiString(Text)));
end;

function TxxmApacheContext.CheckHeader: boolean;
begin
  Result:=not(FHeaderSent);
  if Result then
   begin
    //rq.status_line Sent by first ap_rwrite?
    rq.content_type:=apr_pstrdup(rq.pool,PAnsiChar(FContentType));
    case FAutoEncoding of
      aeUtf8:   rq.content_encoding:=apr_pstrdup(rq.pool,PAnsiChar('utf-8'));
      aeUtf16:  rq.content_encoding:=apr_pstrdup(rq.pool,PAnsiChar('utf-16'));
      aeIso8859:rq.content_encoding:=apr_pstrdup(rq.pool,PAnsiChar('iso-8859-15'));
      else      ;//rq.content_encoding:=apr_pstrdup(rq.pool,PAnsiChar('?
    end;
    //rq.connection.keepalive?//TODO
    FHeaderSent:=true;
   end;
end;

procedure TxxmApacheContext.SendError(res: AnsiString; vals: array of AnsiString);
var
  s:string;
  i:integer;
  r:TResourceStream;
  l:Int64;
const
  RT_HTML = MakeIntResource(23);
begin
  r:=TResourceStream.Create(HInstance,res,RT_HTML);
  try
    l:=r.Size;
    SetLength(s,l);
    r.Read(s[1],l);
  finally
    r.Free;
  end;
  for i:=0 to (Length(vals) div 2)-1 do
    s:=StringReplace(s,'[['+vals[i*2]+']]',vals[i*2+1],[rfReplaceAll]);
  if not(FHeaderSent) then
   begin
    FContentType:='text/html';
    FAutoEncoding:=aeContentDefined;//?
   end;
  SendHTML(s);
end;

end.
