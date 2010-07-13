unit xxmHostMain;

interface

uses
  SysUtils, ActiveX, xxm, Classes, xxmContext, xxmPReg,
  xxmHttpPReg, xxmParams, xxmParUtils, xxmHeaders;

type
  TXxmPostDataStream=class(TCustomMemoryStream)
  private
    FInput:THandle;
    FInputRead,FInputSize:cardinal;
  public
    constructor Create(Input:THandle;InputSize:cardinal);
    destructor Destroy; override;
    function Write(const Buffer; Count: Integer): Integer; override;
    function Read(var Buffer; Count: Integer): Integer; override;
    procedure SetSize(NewSize: Integer); override;
  end;

  TXxmHostedContext=class(TXxmGeneralContext, IxxmHttpHeaders)
  private
    FPipeIn,FPipeOut:THandle;
    FCGIValues:array of record
      Name,Value:AnsiString;
    end;
    FReqHeaders:TRequestHeaders;
    FResHeaders:TResponseHeaders;
    FConnected:boolean;
    FURI,FURLPrefix,FRedirectPrefix,FSessionID:AnsiString;
    FCookieParsed: boolean;
    FCookie: AnsiString;
    FCookieIdx: TParamIndexes;
    FQueryStringIndex:integer;
    function GetCGIValue(Name:AnsiString):AnsiString;
  protected
    procedure SendRaw(Data: WideString); override;
    procedure SendStream(s:IStream); override;
    procedure DispositionAttach(FileName: WideString); override;
    function ContextString(cs:TXxmContextString):WideString; override;
    procedure Redirect(RedirectURL:WideString; Relative:boolean); override;
    function Connected:boolean; override;
    function GetSessionID:WideString; override;
    procedure SendHeader; override;

    function GetCookie(Name:WideString):WideString; override;
    procedure SetCookie(Name,Value:WideString); overload; override;
    procedure SetCookie(Name,Value:WideString; KeepSeconds:cardinal;
      Comment,Domain,Path:WideString; Secure,HttpOnly:boolean); overload; override;

    function GetProjectEntry(ProjectName: WideString):TXxmProjectEntry; override;
    procedure AddResponseHeader(Name, Value: WideString); override;

    function GetRequestHeaders:IxxmDictionaryEx;
    function GetResponseHeaders:IxxmDictionaryEx;
  public
    Queue:TXxmHostedContext;

    constructor Create(PipeIn,PipeOut:THandle);
    destructor Destroy; override;

    procedure Execute;
  end;

  EXxmMaximumHeaderLines=class(Exception);
  EXxmContextStringUnknown=class(Exception);
  EXxmUnknownPostDataTymed=class(Exception);
  EXxmPageRedirected=class(Exception);

implementation

uses Windows, Variants, ComObj, xxmCommonUtils, xxmThreadPool;

resourcestring
  SXxmMaximumHeaderLines='Maximum header lines exceeded.';
  SXxmContextStringUnknown='Unknown ContextString __';

const
  HTTPMaxHeaderLines=$400;

{ TXxmHostedContext }

constructor TXxmHostedContext.Create(PipeIn,PipeOut:THandle);
begin
  inherited Create('');//empty here, see Execute
  Queue:=nil;//used by thread pool
  FPipeIn:=PipeIn;
  FPipeOut:=PipeOut;
  FReqHeaders:=nil;
  FResHeaders:=TResponseHeaders.Create;
  (FResHeaders as IUnknown)._AddRef;
  FConnected:=true;
  FCookieParsed:=false;
  FQueryStringIndex:=1;
  FSessionID:='';//see GetSessionID
  FRedirectPrefix:='';
end;

destructor TXxmHostedContext.Destroy;
begin
  FlushFileBuffers(FPipeOut);
  CloseHandle(FPipeIn);
  CloseHandle(FPipeOut);
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
  SetLength(FCGIValues,0);
  inherited;
end;

procedure TXxmHostedContext.Execute;
var
  i,j,k,l,m:integer;
  l1:cardinal;
  x,y:AnsiString;
begin
  try
    //read CGI values
    if not(ReadFile(FPipeIn,l,4,l1,nil)) then RaiseLastOSError;
    SetLength(x,l);
    if not(ReadFile(FPipeIn,x[1],l,l1,nil)) then RaiseLastOSError;
    //process values
    i:=1;
    m:=0;
    while (i<l) do
     begin
      j:=i;
      while (j<=l) and not(x[j]='=') do inc(j);
      k:=j+1;
      while (k<=l) and not(x[k]=#0) do inc(k);
      if (j-i>4) and (Copy(x,i,5)='HTTP_') then
       begin
        y:=y+x[i+5]+LowerCase(StringReplace(Copy(x,i+6,j-i-6),'_','-',[rfReplaceAll]))+': '+Copy(x,j+1,k-j-1)+#13#10;
       end
      else
        if j<=l then
         begin
          SetLength(FCGIValues,m+1);
          FCGIValues[m].Name:=Copy(x,i,j-i);
          FCGIValues[m].Value:=Copy(x,j+1,k-j-1);
          inc(m);
          if i=HTTPMaxHeaderLines then
            raise EXxmMaximumHeaderLines.Create(SXxmMaximumHeaderLines);
         end;
      i:=k+1;
     end;
    y:=y+#13#10;

    FReqHeaders:=TRequestHeaders.Create(y);
    (FReqHeaders as IUnknown)._AddRef;

    x:=GetCGIValue('SERVER_PROTOCOL');//http or https
    i:=1;
    l:=Length(x);
    while (i<=l) and not(x[i]='/') do inc(i);
    y:=FReqHeaders['Host'];
    if y='' then y:='localhost';//if not port=80 then +':'+?
    FURLPrefix:=LowerCase(Copy(x,1,i-1))+'://'+y;

    x:=GetCGIValue('SCRIPT_NAME');
    y:=GetCGIValue('REQUEST_URI');
    l:=Length(x);
    if x=Copy(y,1,l) then
     begin
      FURI:=Copy(y,l+1,Length(y)-l);
      FURLPrefix:=FURLPrefix+x;
     end
    else
     begin
      FURI:=y;
      //FURLPrefix:= should be ok
     end;

    FURL:=FURLPrefix+FURI;
     
    //'Authorization' ?
    //'If-Modified-Since' ? 304
    //'Connection: Keep-alive' ? with sent Content-Length

    //data (Content-Length

    FResHeaders['X-Powered-By']:=SelfVersion;

    if XxmProjectCache=nil then XxmProjectCache:=TXxmProjectCache.Create;

    //TODO: RequestHeaders['Host']?
    l:=Length(FURI);
    i:=2;
    if XxmProjectCache.SingleProject='' then
     begin
      while (i<=l) and not(FURI[i] in ['/','?','&','$','#']) do inc(i);
      FProjectName:=Copy(FURI,2,i-2);
      if FProjectName='' then
       begin
        if (i<=l) and (FURI[i]='/') then x:='' else x:='/';
        Redirect(FURLPrefix+'/'+XxmProjectCache.DefaultProject+x+Copy(FURI,i,l-i+1),false);
       end;
      FPageClass:='['+FProjectName+']';
      if (i>l) and (l>1) then Redirect(FURLPrefix+FURI+'/',false) else
        if (FURI[i]='/') then inc(i);
      FRedirectPrefix:=FURLPrefix+'/'+FProjectName;
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

    //assert headers read and parsed
    //TODO: HTTP/1.1 100 Continue?

    //if not(Verb='GET') then?
    x:=GetCGIValue('CONTENT_LENGTH');
    if not(x='') then FPostData:=TXxmPostDataStream.Create(FPipeIn,StrToInt(x));

    BuildPage;

  except
    on e:EXxmPageRedirected do
      ;//assert output done
    on EXxmAutoBuildFailed do
      ;//assert output done
    on e:Exception do
     begin
      //TODO: get fragment 500.xxm?
      try
        SetStatus(500,'Internal Server Error');//TODO:setting?
      except
        //silent
      end;
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

function TXxmHostedContext.GetProjectEntry(
  ProjectName: WideString): TXxmProjectEntry;
begin
  Result:=XxmProjectCache.GetProject(FProjectName);
end;

function TXxmHostedContext.Connected: boolean;
begin
  Result:=FConnected;
  //TODO: set to false when client disconnect
end;

function TXxmHostedContext.ContextString(cs: TXxmContextString): WideString;
begin
  case cs of
    csVersion:Result:=SelfVersion+' '+GetCGIValue('SERVER_SOFTWARE');
    csExtraInfo:Result:='';//???
    csVerb:Result:=GetCGIValue('REQUEST_METHOD');
    csQueryString:Result:=Copy(FURI,FQueryStringIndex,Length(FURI)-FQueryStringIndex+1);
    csUserAgent:Result:=FReqHeaders['User-Agent'];
    csAcceptedMimeTypes:Result:=FReqHeaders['Accept'];//TODO:
    csPostMimeType:Result:=GetCGIValue('CONTENT_TYPE');//TODO:
    csURL:Result:=GetURL;
    csProjectName:Result:=FProjectName;
    csLocalURL:Result:=FFragmentName;
    csReferer:Result:=FReqHeaders['Referer'];//TODO:
    csLanguage:Result:=FReqHeaders['Language'];//TODO:

    csRemoteAddress:Result:=GetCGIValue('REMOTE_ADDR');
    csRemoteHost:Result:=GetCGIValue('REMOTE_HOST');
    csAuthUser:GetCGIValue('AUTH_USER');
    csAuthPassword:GetCGIValue('AUTH_PASSWORD');
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

function TXxmHostedContext.GetCookie(Name: WideString): WideString;
begin
  if not(FCookieParsed) then
   begin
    FCookie:=FReqHeaders['Cookie'];
    SplitHeaderValue(FCookie,0,Length(FCookie),FCookieIdx);
    FCookieParsed:=true;
   end;
  Result:=GetParamValue(FCookie,FCookieIdx,Name);
end;

procedure TXxmHostedContext.SetCookie(Name, Value: WideString);
begin
  CheckHeaderNotSent;
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
  CheckHeaderNotSent;
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

procedure TXxmHostedContext.Redirect(RedirectURL: WideString;
  Relative: boolean);
var
  s:AnsiString;
begin
  inherited;
  CheckHeaderNotSent;
  SetStatus(301,'Moved Permamently');
  s:=RedirectURL;
  if Relative and not(RedirectURL='') and (RedirectURL[1]='/') then s:=FRedirectPrefix+s;
  FResHeaders['Location']:=s;
  SendHTML('<a href="'+HTMLEncode(s)+'">'+HTMLEncode(s)+'</a>');
  raise EXxmPageRedirected.Create(s);
end;

procedure TXxmHostedContext.SendRaw(Data:WideString);
const
  Utf8ByteOrderMark=#$EF#$BB#$BF;
  Utf16ByteOrderMark=#$FF#$FE;
var
  s:AnsiString;
  l:cardinal;
begin
  //TODO: catch WriteFile returned values!
  if not(Data='') then
   begin
    if CheckSendStart then
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

procedure TXxmHostedContext.SendStream(s: IStream);
const
  dSize=$10000;
var
  l,l1:cardinal;
  d:array[0..dSize-1] of byte;
begin
  CheckSendStart;
  l:=dSize;
  while l=dSize do
   begin
    OleCheck(s.Read(@d[0],l,@l));
    if not(WriteFile(FPipeOut,d[0],l,l1,nil)) then RaiseLastOSError;
    if l<>l1 then raise Exception.Create('Stream Write Failed');
   end;
end;

procedure TXxmHostedContext.SendHeader;
var
  x:AnsiString;
  i:integer;
  l:cardinal;
const
  AutoEncodingCharset:array[TXxmAutoEncoding] of string=(
    '',//aeContentDefined
    '; charset="utf-8"',
    '; charset="utf-16"',
    '; charset="iso-8859-15"'
  );
begin
  //TODO: Content-Length?
  //TODO: Connection keep?
  //use FResHeader.Complex?
  FResHeaders['Content-Type']:=FContentType+AutoEncodingCharset[FAutoEncoding];
  i:=StatusCode;
  WriteFile(FPipeOut,i,4,l,nil);
  x:=//GetCGIValue('SERVER_PROTOCOL')+' '+
    'Status: '+IntToStr(i)+' '+StatusText+#13#10+
    FResHeaders.Build+#13#10;
  WriteFile(FPipeOut,x[1],Length(x),l,nil);
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

function TXxmHostedContext.GetCGIValue(Name: AnsiString): AnsiString;
var
  i,l:integer;
begin
  i:=0;
  l:=Length(FCGIValues);
  while (i<l) and not(Name=FCGIValues[i].Name) do inc(i); //TODO: case-insensitive?
  if i=l then Result:='' else Result:=FCGIValues[i].Value;
end;

procedure TXxmHostedContext.AddResponseHeader(Name, Value: WideString);
begin
  inherited;
  FResHeaders[Name]:=Value;
end;

{ TXxmPostDataStream }

constructor TXxmPostDataStream.Create(Input:THandle;InputSize:cardinal);
begin
  inherited Create;
  FInput:=Input;
  FInputRead:=0;
  FInputSize:=InputSize;
  SetPointer(GlobalAllocPtr(GMEM_MOVEABLE,FInputSize),FInputSize);
end;

destructor TXxmPostDataStream.Destroy;
begin
  GlobalFreePtr(Memory);
  inherited;
end;

function TXxmPostDataStream.Read(var Buffer; Count: Integer): Integer;
var
  l:cardinal;
  p:pointer;
begin
  l:=Position+Count;
  if l>FInputSize then l:=FInputSize;
  if l>FInputRead then
   begin
    dec(l,FInputRead);
    if not(l=0) then
     begin
      p:=Memory;
      inc(cardinal(p),FInputRead);
      if not(ReadFile(FInput,p^,l,l,nil)) then RaiseLastOSError;
      inc(FInputRead,l);
     end;
   end;
    l:=inherited Read(Buffer,Count);
  Result:=l;
end;

procedure TXxmPostDataStream.SetSize(NewSize: Integer);
begin
  raise Exception.Create('Post data is read-only.');
end;

function TXxmPostDataStream.Write(const Buffer; Count: Integer): Integer;
begin
  raise Exception.Create('Post data is read-only.');
end;

end.
