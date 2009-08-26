unit xxmHostMain;

interface

uses
  SysUtils, xxm, Classes, xxmHttpPReg, xxmParams, xxmParUtils, xxmHeaders;

const
  XxmMaxIncludeDepth=64;//TODO: setting?

type
  TXxmHostedContext=class(TInterfacedObject, IXxmContext, IxxmHttpHeaders)
  private
    FPipeIn,FPipeOut:THandle;
    FReqHeaders:TRequestHeaders;
    FResHeaders:TResponseHeaders;
    FHeaderSent:boolean;
    FPage, FBuilding: IXxmFragment;
    FConnected:boolean;
    FHTTPVersion,FVerb,FURI,FSessionID:string;
    FProjectEntry:TXxmProjectCacheEntry;
    FParams: TXxmReqPars;
    FIncludeDepth:integer;
    FStatusCode:integer;
    FStatusText,FProjectName,FFragmentName:string;
    FContentType: WideString;
    FAutoEncoding: TXxmAutoEncoding;
    FCookieParsed: boolean;
    FCookie: string;
    FCookieIdx: TParamIndexes;
    FPostData:TStream;
    FPageClass:string;
    FQueryStringIndex:integer;
    function PipeReadLine:string;
    procedure HeaderOK;
    function CheckHeader: boolean;
    procedure SendRaw(Data: WideString);
    procedure SendError(res:string;vals:array of string);
  public
    Queue:TXxmHostedContext;

    constructor Create(PipeIn,PipeOut:THandle);
    destructor Destroy; override;

    procedure Execute;

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
    procedure Send(Value: integer); overload;
    procedure Send(Value: int64); overload;
    procedure Send(Value: cardinal); overload;
    procedure Send(const Values:array of OleVariant); overload;
    procedure SendHTML(Data: OleVariant); overload;
    procedure SendHTML(const Values:array of OleVariant); overload;
    procedure SendFile(FilePath: WideString);
    procedure SendStream(s:TStream);
    procedure Include(Address: WideString); overload;
    procedure Include(Address: WideString;
      const Values: array of OleVariant); overload;
    procedure Include(Address: WideString;
      const Values: array of OleVariant;
      const Objects: array of TObject); overload;
    procedure DispositionAttach(FileName: WideString);

    function ContextString(cs:TXxmContextString):WideString;
    function PostData:TStream;
    function Connected:boolean;

    procedure SetStatus(Code:integer;Text:WideString);
    procedure Redirect(RedirectURL:WideString; Relative:boolean);
    function GetCookie(Name:WideString):WideString;
    procedure SetCookie(Name,Value:WideString); overload;
    procedure SetCookie(Name,Value:WideString; KeepSeconds:cardinal;
      Comment,Domain,Path:WideString; Secure,HttpOnly:boolean); overload;

    function GetRequestHeaders:IxxmDictionaryEx;
    function GetResponseHeaders:IxxmDictionaryEx;
  end;

  EXxmMaximumHeaderLines=class(Exception);
  EXxmDirectInclude=class(Exception);
  EXxmAutoBuildFailed=class(Exception);
  EXxmContextStringUnknown=class(Exception);
  EXxmIncludeFragmentNotFound=class(Exception);
  EXxmUnknownPostDataTymed=class(Exception);
  EXxmIncludeStackFull=class(Exception);
  EXxmPageRedirected=class(Exception);

implementation

uses Windows, Variants, ActiveX, ComObj, xxmCommonUtils, xxmThreadPool, xxmPReg;

resourcestring
  SXxmMaximumHeaderLines='Maximum header lines exceeded.';
  SXxmDirectInclude='Direct call to include fragment is not allowed.';
  SXxmContextStringUnknown='Unknown ContextString __';
  SXxmIncludeFragmentNotFound='Include fragment not found "__"';
  SXxmIncludeStackFull='Maximum level of includes exceeded';

const
  HTTPMaxHeaderLines=$400;

{ TXxmHostedContext }

constructor TXxmHostedContext.Create(PipeIn,PipeOut:THandle);
begin
  inherited Create;
  Queue:=nil;//used by thread pool
  FPipeIn:=PipeIn;
  FPipeOut:=PipeOut;
  FProjectEntry:=nil;
  FReqHeaders:=nil;
  FResHeaders:=TResponseHeaders.Create;
  (FResHeaders as IUnknown)._AddRef;
  FHeaderSent:=false;
  FConnected:=true;
  FParams:=nil;//see GetParameter
  FContentType:='text/html';//default (setting?)
  FAutoEncoding:=aeUtf8;//default (setting?)
  FProjectName:='';//parsed from URL later
  FFragmentName:='';//parsed from URL later
  FPage:=nil;
  FCookieParsed:=false;
  FStatusCode:=200;
  FStatusText:='OK';
  FPostData:=nil;
  FIncludeDepth:=0;
  FPageClass:='';
  FQueryStringIndex:=1;
  FSessionID:='';//see GetSessionID
  FURI:='';//see Execute
end;

destructor TXxmHostedContext.Destroy;
begin
  FlushFileBuffers(FPipeOut);
  CloseHandle(FPipeIn);
  CloseHandle(FPipeOut);
  FreeAndNil(FParams);
  if not(FReqHeaders=nil) then
   begin
    (FReqHeaders as IUnknown)._Release;
    FReqHeaders:=nil;
   end;
  if not(FResHeaders=nil) then
   begin
    (FResHeaders as IUnknown)._Release;
    FResHeaders:=nil;
   end;
  FreeAndNil(FPostData);
  if not(FProjectEntry=nil) then
   begin
    FProjectEntry.CloseContext;
    FProjectEntry:=nil;
   end;
  inherited;
end;

procedure TXxmHostedContext.Execute;
var
  i,j,l:integer;
  x,y:string;
  p:IxxmPage;
begin
  try
    //command line
    x:=PipeReadLine;
    l:=Length(x);
    j:=l;
    while (j>0) and not(x[j]=' ') do dec(j);
    FHTTPVersion:=Copy(x,j+1,l-j);
    dec(j);
    i:=0;
    while (i<l) and not(x[i]=' ') do inc(i);
    FVerb:=UpperCase(Copy(x,1,i-1));
    inc(i);

    FURI:=Copy(x,i,j-i+1);

    //headers
    i:=0;
    x:='';
    repeat
     y:=PipeReadLine;
     if not(y='') then
      begin
       inc(i);
       if i=HTTPMaxHeaderLines then
         raise EXxmMaximumHeaderLines.Create(SXxmMaximumHeaderLines);
       x:=x+y+#13#10;
      end;
    until y='';
    FReqHeaders:=TRequestHeaders.Create(x);
    (FReqHeaders as IUnknown)._AddRef;

    //'Authorization' ?
    //'If-Modified-Since' ? 304
    //'Connection: Keep-alive' ? with sent Content-Length

    //data (Content-Length

    FResHeaders['X-Powered-By']:=SelfVersion;

    if XxmProjectCache=nil then XxmProjectCache:=TXxmProjectCache.Create;

    //TODO: RequestHeaders['Host']?
    l:=Length(FURI);
    if not(FURI='') and (FURI[1]='/') then
     begin
      i:=2;
      if XxmProjectCache.SingleProject='' then
       begin
        while (i<=l) and not(FURI[i] in ['/','?','&','$','#']) do inc(i);
        FProjectName:=Copy(FURI,2,i-2);
        if FProjectName='' then
         begin
          if (i<=l) and (FURI[i]='/') then x:='' else x:='/';
          Redirect('/'+XxmProjectCache.DefaultProject+x+Copy(FURI,i,l-i+1),true);
         end;
        FPageClass:='['+FProjectName+']';
        if (i<=l) then inc(i) else if l>1 then Redirect(FURI+'/',true);
       end
      else
       begin
        FProjectName:=XxmProjectCache.SingleProject;
        FPageClass:='[SingleProject]';
       end;
      j:=i;
      while (i<=l) and not(FURI[i] in ['?','&','$','#']) do inc(i);
      FFragmentName:=Copy(FURI,j,i-j);
      if (i<=l) then inc(i);
      FQueryStringIndex:=i;
     end
    else
     begin
      FStatusCode:=400;
      FStatusText:='Bad Request';
      FProjectName:='';
      FFragmentName:='';
      SendError('error',[
        'URL',HTMLEncode(FURI),
        'CLASS','',
        'POSTDATA','',
        'QUERYSTRING','',
        'ERROR','Bad Request',
        'ERRORCLASS','',
        'VERSION',ContextString(csVersion)
      ]);
      raise EXxmPageRedirected.Create(FHTTPVersion+' 400 Bad Request');
     end;

    //assert headers read and parsed
    //TODO: HTTP/1.1 100 Continue?

    //if not(Verb='GET') then?
    x:=FReqHeaders['Content-Length'];
    if not(x='') then FPostData:=THandleStream.Create(FPipeIn);

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
      FStatusCode:=500;//TODO:setting?
      FStatusText:='Internal Server Error';
      try
        if FPostData=nil then x:='none' else x:=IntToStr(FPostData.Size)+' bytes';
      except
        x:='unknown';
      end;
      SendError('error',[
        'URL',HTMLEncode(ContextString(csURL)),
        'CLASS',FPageClass,
        'POSTDATA',x,
        'QUERYSTRING',HTMLEncode(ContextString(csQueryString)),
        'ERROR',HTMLEncode(e.Message),
        'ERRORCLASS',e.ClassName,
        'VERSION',ContextString(csVersion)
      ]);
     end;
  end;
end;

function TXxmHostedContext.Connected: boolean;
begin
  Result:=FConnected;
  //TODO: set to false when client disconnect
end;

function TXxmHostedContext.ContextString(cs: TXxmContextString): WideString;
begin
  case cs of
    csVersion:Result:=SelfVersion;
    csExtraInfo:Result:='';//???
    csVerb:Result:=FVerb;
    csQueryString:Result:=Copy(FURI,FQueryStringIndex,Length(FURI)-FQueryStringIndex+1);
    csUserAgent:Result:=FReqHeaders['User-Agent'];
    csAcceptedMimeTypes:Result:=FReqHeaders['Accept'];//TODO:
    csPostMimeType:Result:=FReqHeaders['Post-Mime'];//TODO:
    csURL:Result:=GetURL;
    csProjectName:Result:=FProjectName;
    csLocalURL:Result:=FFragmentName;
    csReferer:Result:=FReqHeaders['Referer'];//TODO:
    csLanguage:Result:=FReqHeaders['Language'];//TODO:

    csRemoteAddress:Result:='127.0.0.1';//TODO:??
    csRemoteHost:
     begin
      Result:=FReqHeaders['Host'];//TODO:??
      if Result='' then Result:='localhost';
     end;
    csAuthUser:Result:='';//TODO:
    csAuthPassword:Result:='';//TODO:
    else
      raise EXxmContextStringUnknown.Create(StringReplace(
        SXxmContextStringUnknown,'__',IntToHex(integer(cs),8),[]));
  end;
end;

procedure TXxmHostedContext.DispositionAttach(FileName: WideString);
begin
  FResHeaders.SetComplex('Content-disposition','attachment')
    ['filename']:=FileName;
end;

procedure TXxmHostedContext.HeaderOK;
begin
  if FHeaderSent then
    raise EXxmResponseHeaderAlreadySent.Create(SXxmResponseHeaderAlreadySent);
end;

function TXxmHostedContext.GetAutoEncoding: TXxmAutoEncoding;
begin
  Result:=FAutoEncoding;
end;

procedure TXxmHostedContext.SetAutoEncoding(const Value: TXxmAutoEncoding);
begin
  HeaderOK;
  FAutoEncoding:=Value;
end;

function TXxmHostedContext.GetContentType: WideString;
begin
  Result:=FContentType;
end;

procedure TXxmHostedContext.SetContentType(const Value: WideString);
begin
  HeaderOK;
  FContentType:=Value;
  FAutoEncoding:=aeContentDefined;//parse from value? (charset)
end;

function TXxmHostedContext.GetCookie(Name: WideString): WideString;
begin
  if not(FCookieParsed) then
   begin
    FCookie:=';'+FReqHeaders['Cookie'];
    SplitHeaderValue(FCookie,1,Length(FCookie),FCookieIdx);
    FCookieParsed:=true;
   end;
  Result:=GetParamValue(FCookie,FCookieIdx,Name);
end;

procedure TXxmHostedContext.SetCookie(Name, Value: WideString);
begin
  HeaderOK;
  //check name?
  //TODO: "quoted string"?
  FResHeaders['Cache-Control']:='no-cache="set-cookie"';
  FResHeaders.Add('Set-Cookie',Name+'="'+Value+'"');
end;

procedure TXxmHostedContext.SetCookie(Name, Value: WideString;
  KeepSeconds: cardinal; Comment, Domain, Path: WideString; Secure,
  HttpOnly: boolean);
var
  x:WideString;
begin
  HeaderOK;
  //check name?
  //TODO: "quoted string"?
  FResHeaders['Cache-Control']:='no-cache="set-cookie"';
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
  FResHeaders.Add('Set-Cookie',x);
  //TODO: Set-Cookie2
end;

function TXxmHostedContext.GetPage: IXxmFragment;
begin
  Result:=FPage;
end;

function TXxmHostedContext.GetParameter(Key: OleVariant): IXxmParameter;
begin
  if FParams=nil then FParams:=TXxmReqPars.Create(Self);
  if VarIsNumeric(Key) then Result:=FParams.GetItem(Key) else
    Result:=FParams.Get(VarToWideStr(Key));
end;

function TXxmHostedContext.GetParameterCount: integer;
begin
  if FParams=nil then FParams:=TXxmReqPars.Create(Self);
  Result:=FParams.Count;
end;

function TXxmHostedContext.GetSessionID: WideString;
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

function TXxmHostedContext.GetURL: WideString;
var
  s:string;
begin
  if FReqHeaders=nil then Result:='' else
   begin
    Result:='http://';//TODO: get from port? ssl?
    s:=FReqHeaders['Host'];
    if s='' then
     begin
      s:='localhost';//TODO: from binding? setting;
      //if not(Port='80') then s:=s+':'+IntToStr(Port);//TODO:?
     end;
    Result:=Result+s+FURI;
   end;
end;

procedure TXxmHostedContext.SetStatus(Code: integer; Text: WideString);
begin
  HeaderOK;
  FStatusCode:=Code;
  FStatusText:=Text;
  //StatusSet:=true;
end;

procedure TXxmHostedContext.Include(Address: WideString);
begin
  Include(Address, [], []);
end;

procedure TXxmHostedContext.Include(Address: WideString;
  const Values: array of OleVariant);
begin
  Include(Address, Values, []);
end;

procedure TXxmHostedContext.Include(Address: WideString;
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
    FPageClass:=f.ClassNameEx;
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

function TXxmHostedContext.PostData: TStream;
begin
  Result:=FPostData;
end;

procedure TXxmHostedContext.Redirect(RedirectURL: WideString;
  Relative: boolean);
begin
  inherited;
  HeaderOK;
  FStatusCode:=301;
  FStatusText:='Moved Permanently';
  //TODO: relative
  FResHeaders['Location']:=RedirectURL;
  //TODO: move this to execute's except?
  SendHTML('<a href="'+HTMLEncode(RedirectURL)+'">'+HTMLEncode(RedirectURL)+'</a>');
  raise EXxmPageRedirected.Create(RedirectURL);
end;

procedure TXxmHostedContext.Send(Data: OleVariant);
begin
  SendRaw(HTMLEncode(Data));
end;

procedure TXxmHostedContext.SendHTML(Data: OleVariant);
begin
  SendRaw(VarToWideStr(Data));
end;

procedure TXxmHostedContext.SendFile(FilePath: WideString);
var
  f:TFileStream;
begin
  inherited;
  f:=TFileStream.Create(FilePath,fmOpenRead or fmShareDenyNone);
  try
    SendStream(f);
  finally
    f.Free;
  end;
end;

procedure TXxmHostedContext.SendRaw(Data:WideString);
const
  Utf8ByteOrderMark=#$EF#$BB#$BF;
  Utf16ByteOrderMark=#$FF#$FE;
var
  s:string;
  l:cardinal;
begin
  //TODO: catch WriteFile returned values!
  if not(Data='') then
   begin
    if CheckHeader then
      case FAutoEncoding of
        aeUtf8:
          WriteFile(FPipeOut,Utf8ByteOrderMark[1],3,l,nil);
        aeUtf16:
          WriteFile(FPipeOut,Utf16ByteOrderMark[1],2,l,nil);
      end;
    case FAutoEncoding of
      aeUtf16:
        WriteFile(FPipeOut,Data[1],Length(Data)*2,l,nil);
      aeUtf8:
       begin
        s:=UTF8Encode(Data);
        WriteFile(FPipeOut,s[1],Length(s),l,nil);
       end;
      else
       begin
        s:=Data;
        WriteFile(FPipeOut,s[1],Length(s),l,nil);
       end;
    end;
   end;
end;

procedure TXxmHostedContext.SendStream(s: TStream);
var
  l:Int64;
  l1:cardinal;
  d:array[0..$FFF] of byte;
begin
  l:=s.Size;
  FResHeaders['Content-Length']:=IntToStr(l);
  FResHeaders['Accept-Ranges']:='bytes';
  //TODO: keep-connection since content-length known?
  //if not(s.Size=0) then
   begin
    CheckHeader;
    while not(l=0) do
     begin
      if l>$1000 then l1:=$1000 else l1:=l;
      l1:=s.Read(d[0],l1);
      WriteFile(FPipeOut,d[0],l1,l1,nil);
     end;
   end;
end;

function TXxmHostedContext.CheckHeader:boolean;
var
  x:string;
  l:cardinal;
begin
  Result:=not(FHeaderSent);
  if Result then
   begin
    //TODO: Content-Length?
    //TODO: Connection keep?
    //use FResHeader.Complex?
    case FAutoEncoding of
      aeUtf8:   FResHeaders['Content-Type']:=FContentType+'; charset="utf-8"';
      aeUtf16:  FResHeaders['Content-Type']:=FContentType+'; charset="utf-16"';
      aeIso8859:FResHeaders['Content-Type']:=FContentType+'; charset="iso-8859-15"';
      else      FResHeaders['Content-Type']:=FContentType;
    end;
    x:=FHTTPVersion+' '+IntToStr(FStatusCode)+' '+FStatusText+#13#10+
      FResHeaders.Build+#13#10;
    WriteFile(FPipeOut,x[1],Length(x),l,nil);
    FHeaderSent:=true;
   end;
end;

procedure TXxmHostedContext.SendError(res: string; vals: array of string);
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

procedure TXxmHostedContext.Send(Value: int64);
begin
  SendRaw(IntToStr(Value));
end;

procedure TXxmHostedContext.Send(Value: integer);
begin
  SendRaw(IntToStr(Value));
end;

procedure TXxmHostedContext.Send(const Values: array of OleVariant);
var
  i:integer;
begin
  for i:=0 to Length(Values)-1 do SendRaw(HTMLEncode(Values[i]));
end;

procedure TXxmHostedContext.Send(Value: cardinal);
begin
  SendRaw(IntToStr(Value));
end;

procedure TXxmHostedContext.SendHTML(const Values: array of OleVariant);
var
  i:integer;
begin
  for i:=0 to Length(Values)-1 do SendRaw(VarToWideStr(Values[i]));
end;

function TXxmHostedContext.GetRequestHeaders: IxxmDictionaryEx;
begin
  //assert not(FReqHeaders=nil) since parsed at start of Execute
  Result:=FReqHeaders;
end;

function TXxmHostedContext.GetResponseHeaders: IxxmDictionaryEx;
begin
  Result:=FResHeaders;
end;

function TXxmHostedContext.PipeReadLine: string;
var
  dl,dx:integer;
  l:cardinal;
begin
  dl:=$1000;
  dx:=0;
  SetLength(Result,dl);
  while not((dx>1) and (Result[dx-1]=#13) and (Result[dx]=#10)) do
   begin
    if not(ReadFile(FPipeIn,Result[dx+1],1,l,nil)) then RaiseLastOSError;
    if l=0 then Sleep(1) else
      inc(dx,l);
   end;
  SetLength(Result,dx-2);
end;

end.
