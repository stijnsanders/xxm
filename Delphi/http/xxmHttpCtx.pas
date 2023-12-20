unit xxmHttpCtx;

interface

uses
  SysUtils, Windows, xxmSock, xxmThreadPool, xxm, Classes, ActiveX,
  xxmContext, xxmPReg, xxmParams, xxmParUtils, xxmHeaders,
  xxmKeptCon, xxmSpoolingCon;

type
  TXxmHttpContext=class(TXxmQueueContext,
    IXxmHttpHeaders,
    IXxmSocketSuspend)
  private
    FSocket: TTcpSocket;
    FReqHeaders: TRequestHeaders;
    FResHeaders: TResponseHeaders;
    FHTTPVersion,FVerb,FURI: AnsiString;
    FRedirectPrefix: WideString;
    FCookieParsed: boolean;
    FCookie: AnsiString;
    FCookieIdx: TParamIndexes;
    FQueryStringIndex:integer;
    FCredNTLM,FCredNego:TCredHandle;
    FCtxt:TCtxtHandle;
    procedure ResponseStr(const Body,RedirMsg:WideString);
    procedure AuthSChannel(const Package:AnsiString; var Cred: TCredHandle);
  protected
    function ContextString(cs: TXxmContextString): WideString; override;
    function Connected: Boolean; override;
    procedure Redirect(const RedirectURL: WideString; Relative:boolean); override;
    function GetCookie(const Name: WideString): WideString; override;

    procedure SendHeader; override;
    function GetRequestHeader(const Name: WideString): WideString; override;
    function GetResponseHeader(const Name: WideString): WideString; override;
    procedure AddResponseHeader(const Name, Value: WideString); override;

    procedure BeginRequest; override;
    procedure HandleRequest; override;
    procedure EndRequest; override;
    procedure FlushFinal; override;
    procedure FlushStream(AData:TStream;ADataSize:int64); override;
    function GetRawSocket: IStream; override;

    { IXxmHttpHeaders }
    function GetRequestHeaders:IxxmDictionaryEx;
    function GetResponseHeaders:IxxmDictionaryEx;

    { IXxmSocketSuspend }
    procedure SuspendSocket(Handler: IXxmRawSocket); override;

    {  }
    function GetProjectPage(const FragmentName: WideString):IXxmFragment; override;

    procedure ProcessRequestHeaders; virtual;
    procedure PreProcessRequest; virtual;
    procedure PreProcessRequestPage; virtual;
    procedure PostProcessRequest; virtual;

    property HTTPVersion: AnsiString read FHTTPVersion;
    property ReqHeaders:TRequestHeaders read FReqHeaders;
    property ResHeaders:TResponseHeaders read FResHeaders;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    function Accept(Socket:TTcpSocket):TXxmHttpContext;
    procedure Recycle; override;
    property Socket: TTcpSocket read FSocket;
  end;

  EXxmMaximumHeaderLines=class(Exception);
  EXxmContextStringUnknown=class(Exception);
  EXxmUnknownPostDataTymed=class(Exception);

var
  //TODO: array, spread evenly over background threads
  KeptConnections:TXxmKeptConnections;
  SpoolingConnections:TXxmSpoolingConnections;

implementation

uses Variants, ComObj, xxmCommonUtils, xxmReadHandler;

resourcestring
  SXxmMaximumHeaderLines='Maximum header lines exceeded.';
  SXxmContextStringUnknown='Unknown ContextString __';

const
  HTTPMaxHeaderLines=$400;//1KiB
  HTTPMaxHeaderParseTimeMS=10000;
  PostDataThreshold=$100000;//1MiB
  SpoolingThreshold=$10000;//64KiB

type
  EXxmConnectionLost=class(Exception);

{ TXxmHttpContext }

procedure TXxmHttpContext.AfterConstruction;
begin
  inherited;
  FSocket:=nil;
  SendDirect:=nil;
  FReqHeaders:=TRequestHeaders.Create;
  FResHeaders:=TResponseHeaders.Create;
  FCookieIdx.ParsSize:=0;
  FCookieIdx.ParsIndex:=0;
  FCredNTLM.dwUpper:=nil;
  FCredNTLM.dwLower:=nil;
  FCredNego.dwUpper:=nil;
  FCredNego.dwLower:=nil;
  FCtxt.dwUpper:=nil;
  FCtxt.dwLower:=nil;
end;

destructor TXxmHttpContext.Destroy;
begin
  BufferStore.AddBuffer(FContentBuffer);
  FreeAndNil(FSocket);
  FReqHeaders.Free;
  FResHeaders.Free;
  if (FCredNTLM.dwLower<>nil) or (FCredNTLM.dwUpper<>nil) then
    FreeCredentialsHandle(@FCredNTLM);
  if (FCredNego.dwLower<>nil) or (FCredNego.dwUpper<>nil) then
    FreeCredentialsHandle(@FCredNego);
  inherited;
end;

function TXxmHttpContext.Accept(Socket: TTcpSocket): TXxmHttpContext;
var
  i:integer;
begin
  FSocket:=Socket;
  SendDirect:=FSocket.SendBuf;
  i:=30000;//TODO: setting
  if (setsockopt(FSocket.Handle,SOL_SOCKET,SO_RCVTIMEO,@i,4)<>0) or
     (setsockopt(FSocket.Handle,SOL_SOCKET,SO_SNDTIMEO,@i,4)<>0) then
    ;//RaiseLastOSError;
  i:=$10000;//TODO: setting
  if (setsockopt(FSocket.Handle,SOL_SOCKET,SO_RCVBUF,@i,4)<>0) or
     (setsockopt(FSocket.Handle,SOL_SOCKET,SO_SNDBUF,@i,4)<>0) then
    ;//RaiseLastOSError;
  Result:=Self;
end;

procedure TXxmHttpContext.BeginRequest;
begin
  inherited;
  FReqHeaders.Reset;
  FResHeaders.Reset;
  FCookieParsed:=false;
  FCookieIdx.ParsIndex:=0;
  FQueryStringIndex:=1;
  FURI:='';//see Execute
  FRedirectPrefix:='';
end;

procedure TXxmHttpContext.EndRequest;
begin
  inherited;
  //FURL:='';
  //Disconnect: see Recycle
  //ZeroMemory(FHeaderData[1],Length(FHeaderData));//?
end;

procedure TXxmHttpContext.Recycle;
var
  i:integer;
begin
  if (State<>ctSocketDisconnect) and (Chunked or (State=ctHeaderOnly) or
     (FResHeaders['Content-Length']<>'')) then
   begin
    try
      EndRequest;
    except
      //silent
    end;
    KeptConnections.Queue(Self,ctHeaderNotSent);
   end
  else
   begin
    if FSocket<>nil then
     begin
      if FSocket.Connected then
       begin
        i:=1;
        setsockopt(FSocket.Handle,SOL_SOCKET,SO_REUSEADDR,@i,4);
        FSocket.Disconnect;
       end;
      FreeAndNil(FSocket);
     end;
    inherited;
   end;
end;

procedure TXxmHttpContext.FlushFinal;
begin
  //no inherited! override default behaviour:
  if (BufferSize<>0) //and (ContentBuffer.Position<>0) then
    and not(Chunked)
    and (FContentBuffer.Position>SpoolingThreshold) then
   begin
    CheckSendStart(true);
    SpoolingConnections.Add(Self,FContentBuffer,false);
    FContentBuffer:=nil;//since spooling will free it when done
   end
  else
    inherited;//Flush;
end;

procedure TXxmHttpContext.FlushStream(AData: TStream; ADataSize: int64);
begin
  //no inherited! override default behaviour:
  if ADataSize>SpoolingThreshold then
   begin
    AData.Seek(0,soFromEnd);//used by SpoolingConnections.Add
    CheckSendStart(true);
    SpoolingConnections.Add(Self,AData,true);
   end
  else
    inherited;
end;

procedure TXxmHttpContext.HandleRequest;
var
  i,j,k,l,m,n,p:integer;
  s:TStream;
  si:int64;
  tc:cardinal;
  cl:AnsiString;
begin
  try
    tc:=GetTickCount;
    //command line and headers
    //TODO: absorb this into FReqHeaders.Load
    k:=0;
    l:=0;
    m:=0;
    i:=1;
    j:=1;
    p:=1;
    repeat
      if j>l then   
       begin
        if l=k then
         begin
          inc(k,$10000);
          if Length(FReqHeaders.Data)<k then
            SetLength(FReqHeaders.Data,k);
         end;
        n:=FSocket.ReceiveBuf(FReqHeaders.Data[l+1],k-l);
        if (n<=0) or (cardinal(GetTickCount-tc)>HTTPMaxHeaderParseTimeMS) then
         begin
          i:=1;
          setsockopt(FSocket.Handle,SOL_SOCKET,SO_REUSEADDR,@i,4);
          FSocket.Disconnect;
          raise EXxmConnectionLost.Create('Connection Lost');
         end;
        inc(l,n);
       end;
      while (j<=l) and (FReqHeaders.Data[j]<>#13) and (FReqHeaders.Data[j]<>#10) do inc(j);
      if (j<=l) and ((FReqHeaders.Data[j]=#13) or (FReqHeaders.Data[j]=#10)) then
       begin
        if m=0 then
         begin
          //i:=1;
          while (i<=l) and (FReqHeaders.Data[i]>' ') do inc(i);
          FVerb:=AnsiUpper(PAnsiChar(Copy(FReqHeaders.Data,1,i-1)));
          inc(i);
          j:=i;
          while (j<=l) and (FReqHeaders.Data[j]>' ') do inc(j);
          FURI:=Copy(FReqHeaders.Data,i,j-i);
          inc(j);
          i:=j;
          while (j<=l) and (FReqHeaders.Data[j]<>#13) and (FReqHeaders.Data[j]<>#10) do inc(j);
          FHTTPVersion:=Copy(FReqHeaders.Data,i,j-i);
          AllowChunked:=FHTTPVersion='HTTP/1.1';
          inc(m);
          p:=j;
         end
        else
         begin
          if i=j then m:=-1 else
           begin
            inc(m);
            if m=HTTPMaxHeaderLines then
              raise EXxmMaximumHeaderLines.Create(SXxmMaximumHeaderLines);
           end;
         end;
        inc(j);//#13
        if (j<=l) and (FReqHeaders.Data[j]=#10) then inc(j);//#10
        i:=j;
       end;
    until m=-1;
    FReqHeaders.Load(p,l);

    ProcessRequestHeaders;

    if (FURI<>'') and (FURI[1]='/') then
     begin
      FQueryStringIndex:=2;
      if XxmProjectCache.ProjectFromURI(Self,
        FURI,FQueryStringIndex,FProjectName,FFragmentName) then
        FRedirectPrefix:='/'+FProjectName;
      FPageClass:='['+FProjectName+']';
     end
    else
     begin
      ForceStatus(400,'Bad Request');
      FProjectName:='';
      FFragmentName:='';
      SendError('error','','Bad Request');
      raise EXxmPageRedirected.Create(string(FHTTPVersion)+' 400 Bad Request');
     end;

    //assert headers read and parsed
    //TODO: HTTP/1.1 100 Continue?

    PreProcessRequest;

    //if Verb<>'GET' then?
    cl:=FReqHeaders.KnownHeader(khContentLength);
    if cl<>'' then
     begin
      //si:=StrToInt(cl);
      si:=0;
      for i:=1 to Length(cl) do
        if cl[i] in ['0'..'9'] then
          si:=si*10+(byte(cl[i]) and $F)
        else
          raise EConvertError.Create('Invalid Content-Length value');
      if si<PostDataThreshold then
        s:=THeapStream.Create
      else
       begin
        SetLength(FPostTempFile,$400);
        SetLength(FPostTempFile,GetTempPath($400,PChar(FPostTempFile)));
        FPostTempFile:=FPostTempFile+'xxm_'+
          IntToHex(integer(Self),8)+'_'+IntToHex(GetTickCount,8)+'.dat';
        s:=TFileStream.Create(FPostTempFile,fmCreate);
       end;
      s.Size:=si;
      s.Position:=0;
      FPostData:=THandlerReadStreamAdapter.Create(FSocket,si,s,FReqHeaders.Data,j,l);
     end;

    if FVerb='OPTIONS' then
     begin
      FAutoEncoding:=aeContentDefined;//prevent Content-Type
      AddResponseHeader('Allow','OPTIONS, GET, HEAD, POST');
      AddResponseHeader('Public','OPTIONS, GET, HEAD, POST');
      AddResponseHeader('Content-Length','0');
      LoadPage;//IXxmProject.LoadPage without IXxmPage.Build
      CheckSendStart(true);
      State:=ctHeaderOnly;
     end
    else if FVerb='TRACE' then
     begin
      SetStatus(501,'Not Implemented');
      SendStr('<h1>Not Implemented</h1>');
      Flush;
     end
    else
     begin
      if FVerb='HEAD' then
       begin
        State:=ctHeaderOnly;//see SendHeader
        AddResponseHeader('Content-Length','0');
       end;
      BuildPage;
     end;

  except
    on EXxmPageRedirected do Flush;
    on EXxmAutoBuildFailed do ;//assert output done
    on EXxmConnectionLost do ;
    on e:Exception do
     begin
      if not HandleException(e) then
       begin
        ForceStatus(StatusException,'Internal Server Error');
        SendError('error',e.ClassName,e.Message);
       end;
      FlushFinal;
     end;
  end;

  try
    PostProcessRequest;
  except
    //silent!
  end;
end;

procedure TXxmHttpContext.AuthSChannel(const Package:AnsiString;
  var Cred:TCredHandle);
var
  s,t:AnsiString;
  p:PCtxtHandle;
  r,f:cardinal;
  d1,d2:TSecBufferDesc;
  d:array of TSecBuffer;
  n:TSecPkgContextNames;
begin
  s:=XxmProjectCache.GetAuthCache(GetCookie(XxmSessionCookieName));
  if s<>'' then
    AuthSet(s,'') //TODO: update Expires?
  else
   begin
    s:=AuthParse(string(Package));
    if s='' then
     begin
      SetStatus(401,'Unauthorized');
      AddResponseHeader('Connection','keep-alive');
      AddResponseHeader('WWW-Authenticate',WideString(Package));
      if FPostData<>nil then
        try
          //flush post data if any
          r:=$10000;
          SetLength(s,r);
          while FPostData.Read(s[1],r)<>0 do ;
        except
          //ignore
        end;
      ResponseStr('<h1>Authorization required</h1>','401');
     end
    else
     begin
      if Cred.dwLower=nil then
        if AcquireCredentialsHandle(nil,PAnsiChar(Package),SECPKG_CRED_INBOUND,
          nil,nil,nil,nil,@Cred,nil)<>0 then RaiseLastOSError;

      SetLength(d,3);
      SetLength(t,$10000);

      d1.ulVersion:=SECBUFFER_VERSION;
      d1.cBuffers:=2;
      d1.pBuffers:=@d[0];

      d[0].cbBuffer:=Length(s);
      d[0].BufferType:=SECBUFFER_TOKEN;
      d[0].pvBuffer:=@s[1];

      d[1].cbBuffer:=0;
      d[1].BufferType:=SECBUFFER_EMPTY;
      d[1].pvBuffer:=nil;

      d2.ulVersion:=SECBUFFER_VERSION;
      d2.cBuffers:=1;
      d2.pBuffers:=@d[2];

      d[2].cbBuffer:=$10000;;
      d[2].BufferType:=SECBUFFER_TOKEN;
      d[2].pvBuffer:=@t[1];

      if (FCtxt.dwLower=nil) and (FCtxt.dwUpper=nil) then
        p:=nil
      else
        p:=@FCtxt;

      r:=AcceptSecurityContext(@Cred,p,@d1,
        ASC_REQ_REPLAY_DETECT or ASC_REQ_SEQUENCE_DETECT,SECURITY_NATIVE_DREP,
        @FCtxt,@d2,@f,nil);

      if r=SEC_E_OK then
       begin
        r:=QueryContextAttributes(@FCtxt,SECPKG_ATTR_NAMES,@n);
        if r=0 then
          AuthSet(n.sUserName,'')
        else
          AuthSet('???'+AnsiString(SysErrorMessage(r)),'');//raise?

        DeleteSecurityContext(@FCtxt);
        FCtxt.dwLower:=nil;
        FCtxt.dwUpper:=nil;

        FreeCredentialsHandle(@Cred);
        Cred.dwLower:=nil;
        Cred.dwUpper:=nil;

        AuthStoreCache:=true;//see GetSessionID
        //AddResponseHeader('Persistent-Auth','false');
       end
      else
      if r=SEC_I_CONTINUE_NEEDED then
       begin
        SetLength(t,d[2].cbBuffer);
        SetStatus(401,'Unauthorized');
        AddResponseHeader('Connection','keep-alive');
        AddResponseHeader('WWW-Authenticate',UTF8ToWideString(Package+' '+Base64Encode(t)));
        ResponseStr('<h1>Authorization required</h1>','401.1');
       end
      else
        raise Exception.Create(SysErrorMessage(r));
     end;
   end;
end;

function TXxmHttpContext.GetProjectPage(const FragmentName: WideString):IXxmFragment;
begin
  //TODO: support both Negotiate and NTLM (and Digest?)
  if ProjectEntry.Negotiate then AuthSChannel('Negotiate',FCredNego) else
    if ProjectEntry.NTLM then AuthSChannel('NTLM',FCredNTLM);
      Result:=inherited GetProjectPage(FragmentName);
  PreProcessRequestPage;
end;

function TXxmHttpContext.Connected: boolean;
begin
  Result:=(FSocket<>nil) and FSocket.Connected;
end;

function TXxmHttpContext.ContextString(cs: TXxmContextString): WideString;
begin
  case cs of
    csVersion:Result:=SelfVersion;
    csExtraInfo:Result:='';//???
    csVerb:Result:=UTF8ToWideString(FVerb);
    csQueryString:Result:=UTF8ToWideString(Copy(FURI,FQueryStringIndex,Length(FURI)-FQueryStringIndex+1));
    csUserAgent:Result:=UTF8ToWideString(FReqHeaders.KnownHeader(khUserAgent));
    csAcceptedMimeTypes:Result:=UTF8ToWideString(FReqHeaders.KnownHeader(khAccept));
    csPostMimeType:Result:=UTF8ToWideString(FReqHeaders.KnownHeader(khContentType));
    csURL:Result:=GetURL;
    csProjectName:Result:=FProjectName;
    csLocalURL:Result:=FFragmentName;
    csReferer:Result:=UTF8ToWideString(FReqHeaders.KnownHeader(khReferer));
    csLanguage:Result:=UTF8ToWideString(FReqHeaders.KnownHeader(khAcceptLanguage));
    csRemoteAddress:Result:=FSocket.Address;
    csRemoteHost:Result:=FSocket.HostName;
    csAuthUser,csAuthPassword:Result:=UTF8ToWideString(AuthValue(cs));//?
    else
      raise EXxmContextStringUnknown.Create(StringReplace(
        SXxmContextStringUnknown,'__',IntToHex(integer(cs),8),[]));
  end;
end;

function TXxmHttpContext.GetCookie(const Name: WideString): WideString;
begin
  if not(FCookieParsed) then
   begin
    FCookie:=FReqHeaders.KnownHeader(khCookie);
    SplitHeaderValue(FCookie,0,Length(FCookie),FCookieIdx);
    FCookieParsed:=true;
   end;
  Result:=UTF8ToWideString(GetParamValue(FCookie,FCookieIdx,Name));
end;

procedure TXxmHttpContext.Redirect(const RedirectURL: WideString; Relative: boolean);
var
  NewURL:WideString;
begin
  inherited;
  SetStatus(302,'Object moved');//SetStatus(301,'Moved Permanently');
  //if FResHeaders['Cache-Control']='' then
  // FResHeaders['Cache-Control']:='no-cache, no-store';
  NewURL:=RedirectURL;
  if Relative and (NewURL<>'') and (NewURL[1]='/') then
    NewURL:=FRedirectPrefix+NewURL;
  FResHeaders['Location']:=NewURL;
  ResponseStr('<h1>Object moved</h1><p><a href="'+
    HTMLEncode(NewURL)+'">'+HTMLEncode(NewURL)+'</a></p>'#13#10,RedirectURL);
end;

procedure TXxmHttpContext.ResponseStr(const Body,RedirMsg:WideString);
begin
  case FAutoEncoding of
    aeUtf8:FResHeaders['Content-Length']:=IntToStr(Length(UTF8Encode(Body))+3);
    aeUtf16:FResHeaders['Content-Length']:=IntToStr(Length(Body)*2+2);
    aeIso8859:FResHeaders['Content-Length']:=IntToStr(Length(AnsiString(Body)));
  end;
  SendStr(Body);
  raise EXxmPageRedirected.Create(RedirMsg);
end;

procedure TXxmHttpContext.SendHeader;
var
  x:AnsiString;
  l:integer;
const
  AutoEncodingCharset:array[TXxmAutoEncoding] of string=(
    '',//aeContentDefined
    '; charset="utf-8"',
    '; charset="utf-16"',
    '; charset="iso-8859-15"'
  );
begin
  //use FResHeader.Complex?
  if FContentType<>'' then
    FResHeaders['Content-Type']:=FContentType+
      AutoEncodingCharset[FAutoEncoding];
  x:=FHTTPVersion+' '+AnsiString(IntToStr(StatusCode)+' '+StatusText)+#13#10+
    FResHeaders.Build+#13#10;
  l:=Length(x);
  if FSocket.SendBuf(x[1],l)<>l then
    raise EXxmTransferError.Create(SysErrorMessage(GetLastError));
  //TODO: transfer encoding chunked
  inherited;
end;

function TXxmHttpContext.GetRequestHeader(const Name: WideString): WideString;
begin
  Result:=FReqHeaders[Name];
end;

function TXxmHttpContext.GetResponseHeader(const Name: WideString): WideString;
begin
  Result:=FResHeaders[Name];
end;

procedure TXxmHttpContext.AddResponseHeader(const Name, Value: WideString);
begin
  if SettingCookie then
   begin
    SettingCookie:=false;
    FResHeaders.Add(Name,Value);
   end
  else
    FResHeaders[Name]:=Value;
end;

function TXxmHttpContext.GetRequestHeaders: IxxmDictionaryEx;
begin
  Result:=FReqHeaders;
end;

function TXxmHttpContext.GetResponseHeaders: IxxmDictionaryEx;
begin
  Result:=FResHeaders;
end;

procedure TXxmHttpContext.ProcessRequestHeaders;
begin
  //'Authorization' ?
  //'If-Modified-Since' ? 304
  //'Connection: Keep-alive' ? with sent Content-Length

  FURL:=UTF8ToWideString(FReqHeaders.KnownHeader(khHost));
  if FURL='' then
   begin
    FURL:='localhost';//TODO: from binding? setting?
    if (FSocket.Port<>0) and (FSocket.Port<>80) then
      FURL:=FURL+':'+IntToStr(FSocket.Port);
   end;
  FURL:='http://'+FURL+UTF8ToWideString(FURI);//TODO: 'https' if SSL?

  //FResHeaders['Server']:=HttpSelfVersion; //X-Powered-By?
end;

procedure TXxmHttpContext.PreProcessRequest;
begin
  //inheritants can perform pre-page-build logging or checking here
end;

procedure TXxmHttpContext.PreProcessRequestPage;
begin
  //similar to PreProcessRequest, but right after project and fragment load
end;

procedure TXxmHttpContext.PostProcessRequest;
begin
  //inheritants can perform post-page logging here
  //ATTENTION: (v1.2.2) when Context.BufferSize is set,
  //this may get called with data on the buffer not sent yet
  //  see also TXxmSpoolingConnections
end;

function TXxmHttpContext.GetRawSocket: IStream;
begin
  if FReqHeaders.KnownHeader(khUpgrade)='' then Result:=nil else
   begin
    FContentType:='';
    SetBufferSize(0);//!
    CheckSendStart(false);
    Result:=TRawSocketData.Create(FSocket);
   end;
end;

procedure TXxmHttpContext.SuspendSocket(Handler: IXxmRawSocket);
begin
  inherited;
  KeptConnections.Queue(Self,ctSocketResume);
end;

initialization
  StatusBuildError:=503;//TODO: from settings
  StatusException:=500;
  StatusFileNotFound:=404;
end.
