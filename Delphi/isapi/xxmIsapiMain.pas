unit xxmIsapiMain;

interface

uses Windows, SysUtils, Classes, isapi4, xxm, xxmPReg, xxmParams, xxmParUtils;

function GetExtensionVersion(var Ver: THSE_VERSION_INFO): BOOL; stdcall;
function HttpExtensionProc(PECB: PEXTENSION_CONTROL_BLOCK): DWORD; stdcall;
function TerminateExtension(dwFlags: DWORD): BOOL; stdcall;

const
  XxmMaxIncludeDepth=64;//TODO: setting?

type
  TXxmIsapiContext=class(TInterfacedObject, IXxmContext)
  private
    FURL:WideString;
    FContentType: WideString;
    FAutoEncoding: TXxmAutoEncoding;
    FProjectEntry:TXxmProjectCacheEntry;
    FPage, FBuilding: IXxmFragment;
    FHeaderSent:boolean;
    FStatusCode:integer;
    FStatusText,FProjectName,FFragmentName,FExtraHeaders,FRedirectPrefix,FSessionID:string;
    FIncludeDepth:integer;
    ecb:PEXTENSION_CONTROL_BLOCK;
    FPostData: TStream;
    FPostTempFile,FPageClass:string;
    FParams: TXxmReqPars;
    FCookieParsed: boolean;
    FCookie: string;
    FCookieIdx: TParamIndexes;
    procedure SendRaw(Data:WideString);
    procedure SendError(res:string;vals:array of string);
    procedure HeaderOK;
    function CheckHeader:boolean;
    procedure ServerFunction(HSERRequest: DWORD; Buffer: Pointer; Size, DataType: LPDWORD);
    function GetURLPrefix:string;
  protected
    function GetURL: WideString;
    function GetPage: IXxmFragment;
    function GetContentType: WideString;
    procedure SetContentType(const Value: WideString);
    function GetAutoEncoding: TXxmAutoEncoding;
    procedure SetAutoEncoding(const Value: TXxmAutoEncoding);
    function GetParameter(Key: OleVariant): IXxmParameter;
    function GetParameterCount: Integer;
    function GetSessionID: WideString;
  public
    Queue:TXxmIsapiContext;//used by thread pool

    constructor Create(pecb:PEXTENSION_CONTROL_BLOCK);
    destructor Destroy; override;
    procedure Execute;

    function ContextString(cs: TXxmContextString): WideString;
    procedure DispositionAttach(FileName: WideString);
    procedure Include(Address: WideString); overload;
    procedure Include(Address: WideString;
      const Values: array of OleVariant); overload;
    procedure Include(Address: WideString;
      const Values: array of OleVariant;
      const Objects: array of TObject); overload;
    function Connected: Boolean;
    function PostData: TStream;
    procedure Send(Data: OleVariant); overload;
    procedure Send(Value: integer); overload;
    procedure Send(Value: int64); overload;
    procedure Send(Value: cardinal); overload;
    procedure Send(const Values:array of OleVariant); overload;
    procedure SendFile(FilePath: WideString);
    procedure SendHTML(Data: OleVariant); overload;
    procedure SendHTML(const Values:array of OleVariant); overload;
    procedure SendStream(s: TStream);
    procedure SetStatus(Code: Integer; Text: WideString);
    procedure Redirect(RedirectURL: WideString; Relative: Boolean);
    function GetCookie(Name: WideString): WideString;
    procedure SetCookie(Name,Value:WideString); overload;
    procedure SetCookie(Name,Value:WideString; KeepSeconds:cardinal;
      Comment,Domain,Path:WideString; Secure,HttpOnly:boolean); overload;

    property URL: WideString read GetURL;
  end;

  TXxmIsapiHandler=class(TThread)
  private
    FInUse:boolean;
  protected
    procedure Execute; override;
  public
    constructor Create;
    property InUse:boolean read FInUse;
  end;

  TXxmIsapiHandlerPool=class(TObject)
  private
    FHandlers:array of TXxmIsapiHandler;
    FHandlerSize:integer;
    FLock:TRTLCriticalSection;
    FQueue:TXxmIsapiContext;
    procedure SetSize(x:integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Queue(Context:TXxmIsapiContext);//called from handler
    function Unqueue:TXxmIsapiContext;//called from threads
  end;

  TXxmAutoBuildHandler=function(pce:TXxmProjectCacheEntry;
    Context: IXxmContext; ProjectName:WideString):boolean;

  EXxmContextStringUnknown=class(Exception);
  EXxmResponseHeaderAlreadySent=class(Exception);
  EXxmAutoBuildFailed=class(Exception);
  EXxmDirectInclude=class(Exception);
  EXxmIncludeFragmentNotFound=class(Exception);
  EXxmPageRedirected=class(Exception);
  EXxmIncludeStackFull=class(Exception);

const
  PoolMaxThreads=64;//TODO: from setting?

var
  IsapiHandlerPool:TXxmIsapiHandlerPool;
  XxmAutoBuildHandler:TXxmAutoBuildHandler;

implementation

uses ActiveX, Variants, ComObj, xxmCommonUtils;

resourcestring
  SXxmContextStringUnknown='Unknown ContextString __';
  SXxmResponseHeaderAlreadySent='Response header has already been sent.';
  SXxmDirectInclude='Direct call to include fragment is not allowed.';
  SXxmIncludeFragmentNotFound='Include fragment not found "__"';
  SXxmIncludeStackFull='Maximum level of includes exceeded';

function GetExtensionVersion(var Ver: THSE_VERSION_INFO): BOOL; stdcall;
var
  verblock:PVSFIXEDFILEINFO;
  verlen:cardinal;
  r:TResourceStream;
  m:TMemoryStream;
  p:PChar;
begin
  m:=TMemoryStream.Create;
  try
    r:=TResourceStream.CreateFromID(HInstance,1,RT_VERSION);
    try
      r.SaveToStream(m);
    finally
      r.Free;
    end;
    m.Position:=0;
    if VerQueryValue(m.Memory,'\',pointer(verblock),verlen) then
      Ver.dwExtensionVersion:=verblock.dwFileVersionMS;
    if VerQueryValue(m.Memory,'\StringFileInfo\040904E4\FileDescription',pointer(p),verlen) then
      Move(p^,Ver.lpszExtensionDesc[0],verlen);
  finally
    m.Free;
  end;
  Result:=true;
  //IsapiHandlerPool:=TXxmIsapiHandlerPool.Create;?
end;

function HttpExtensionProc(PECB: PEXTENSION_CONTROL_BLOCK): DWORD; stdcall;
begin
  try
    IsapiHandlerPool.Queue(TXxmIsapiContext.Create(PECB));
    Result:=HSE_STATUS_PENDING; //HSE_STATUS_SUCCESS
  except
    on e:Exception do
     begin
      //TODO output error?
      Result:=HSE_STATUS_ERROR;
     end;
  end;
end;

function TerminateExtension(dwFlags: DWORD): BOOL; stdcall;
begin
  //TODO: terminate all threads (thread pool?)
  Result:=true;
end;

{
procedure ContextIOCompletion(var ECB: TEXTENSION_CONTROL_BLOCK;
  pContext: Pointer; cbIO, dwError: DWORD) stdcall;
begin
  //assert TXxmIsapiContext(pContext).ecb=ECB
  TXxmIsapiContext(pContext).ReportComplete(cbIO,dwError);
end;
}

{ TXxmIsapiContext }

function GetVar(pecb: PEXTENSION_CONTROL_BLOCK; key:string):string;
var
  l:cardinal;
begin
  l:=$10000;
  SetLength(Result,l);
  //TODO: 'UNICODE_'+
  if not(pecb.GetServerVariable(pecb.ConnID,PChar(key),PChar(Result),l)) then
    if GetLastError=ERROR_INVALID_INDEX then l:=1 else RaiseLastOSError;
  SetLength(Result,l-1);
end;

constructor TXxmIsapiContext.Create(pecb: PEXTENSION_CONTROL_BLOCK);
begin
  inherited Create;
  ecb:=pecb;
  FProjectEntry:=nil;
  FURL:=GetVar(pecb,'HTTP_URL');//unicode?
  FContentType:='text/html';//default (setting?)
  FAutoEncoding:=aeUtf8;//default (setting?)
  FPage:=nil;
  FBuilding:=nil;
  FHeaderSent:=false;
  FStatusCode:=200;
  FStatusText:='OK';
  FExtraHeaders:='';
  FProjectName:='';//parsed from URL later
  FFragmentName:='';//parsed from URL later
  FPostData:=nil;
  FPostTempFile:='';
  FIncludeDepth:=0;
  FParams:=nil;//see GetParameter
  FCookieParsed:=false;
  FPageClass:='';
  FSessionID:='';//see GetSessionID
end;

destructor TXxmIsapiContext.Destroy;
begin
  FreeAndNil(FParams);
  FreeAndNil(FPostData);
  if not(FProjectEntry=nil) then
   begin
    FProjectEntry.CloseContext;
    FProjectEntry:=nil;
   end;
  try
    if not(FPostTempFile='') then
     begin
      DeleteFile(FPostTempFile);
      FPostTempFile:='';
     end;
  except
    //silent
  end;
  inherited;
end;

function TXxmIsapiContext.GetURL: WideString;
begin
  //attention: FURL contains FRedirectPrefix
  Result:=GetURLPrefix+FURL;
end;

function TXxmIsapiContext.GetPage: IXxmFragment;
begin
  Result:=FPage;
end;

procedure TXxmIsapiContext.Execute;
var
  c,l:cardinal;
  x,y:string;
  i,j:integer;
  p:IXxmPage;//for directinclude check
  d:TDateTime;
begin
  //ServerFunction(HSE_REQ_IO_COMPLETION,@ContextIOCompletion,nil,PDWORD(Self));
  try
    //parse url
    x:=GetVar(ecb,'HTTP_URL');
    y:=GetVar(ecb,'SCRIPT_NAME');
    if y=ecb.lpszPathInfo then
     begin
      //called mapped
      FRedirectPrefix:='';
     end
    else
     begin
      //called directly
      FRedirectPrefix:=y;
      x:=Copy(x,Length(y)+1,Length(x)-Length(y));
     end;

    XxmProjectCache.Refresh;

    //project name
    i:=1;
    if i>Length(x) then Redirect('/',true) else
      if not(x[i]='/') then Redirect('/'+Copy(x,i,Length(x)-i+1),true);
    //redirect raises EXxmPageRedirected
    inc(i);
    if XxmProjectCache.SingleProject='' then
     begin
      while (i<=Length(x)) and not(Char(x[i]) in ['/','?','&','$','#']) do inc(i);
      FProjectName:=Copy(x,2,i-2);
      if FProjectName='' then
       begin
        //FProjectName:=XxmProjectCache.DefaultProject;
        if (i<=Length(x)) and (x[i]='/') then y:='' else y:='/';
        Redirect('/'+XxmProjectCache.DefaultProject+y+Copy(x,i,Length(x)-i+1),true)
        //redirect raises EXxmPageRedirected
       end;
      FPageClass:='['+FProjectName+']';
      FRedirectPrefix:=FRedirectPrefix+'/'+FProjectName;
      if i>Length(x) then Redirect('/',true) else
        if not(x[i]='/') then Redirect('/'+Copy(x,i,Length(x)-i+1),true);
      //redirect raises EXxmPageRedirected
      inc(i);
     end
    else
     begin
      FProjectName:=XxmProjectCache.SingleProject;
      FPageClass:='[SingleProject]';
     end;

    //fragment name
    j:=i;
    while (j<=Length(x)) and not(x[j] in ['?','&','$','#']) do inc(j);
    FFragmentName:=Copy(x,i,j-i);

    //create object
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
      FProjectEntry.GetFilePath(FFragmentName,x,y);
      d:=GetFileModifiedDateTime(x);
      if not(d=0) then
       begin
        //TODO: if directory file-list?
        FContentType:=y;
        FAutoEncoding:=aeContentDefined;

        FExtraHeaders:=FExtraHeaders+'Last-Modified: '+RFC822DateGMT(d)+#13#10;
        //TODO:Content-Length

        SendFile(x);
       end
      else
       begin
        //TODO: consider HSE_REQ_SEND_CUSTOM_ERROR?
        FStatusCode:=404;
        FStatusText:='File not found';
        FPage:=FProjectEntry.Project.LoadPage(Self,'404.xxm');
        if FPage=nil then
          SendError('fnf',[
            'URL',HTMLEncode(URL),
            'PROJECT',FProjectName,
            'ADDRESS',FFragmentName,
            'PATH',x,
            'VERSION',ContextString(csVersion)
          ])
        else
          try
            FPageClass:=FPage.ClassNameEx;
            FBuilding:=FPage;
            FPage.Build(Self,nil,[FFragmentName,x,y],[]);
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

        //TODO: postpone till first GetParameter?
        if not(ecb.cbTotalBytes=0) then
         begin
          l:=ecb.cbTotalBytes;
          c:=ecb.cbAvailable;
          if c=l then
           begin
            FPostData:=TMemoryStream.Create;
            FPostData.Write(ecb.lpbData^,c);
           end
          else
           begin
            SetLength(FPostTempFile,$400);
            SetLength(FPostTempFile,GetTempPath($400,PChar(FPostTempFile)));//TODO: setting
            FPostTempFile:=FPostTempFile+'xxm_'+IntToHex(ecb.ConnID,8)+'.dat';
            FPostData:=TFileStream.Create(FPostTempFile,fmCreate);
            FPostData.Write(ecb.lpbData^,c);
            dec(l,c);
            c:=$10000;
            SetLength(x,c);
            while l>0 do
             begin
              if c>l then c:=l;
              if not(ecb.ReadClient(ecb.ConnID,@x[1],c)) then RaiseLastOSError;
              FPostData.Write(x[1],c);
              dec(l,c);
             end;
           end;
          FPostData.Seek(0,soFromBeginning);
         end;

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
    on EXxmPageRedirected do ;//silent
    on EXxmAutoBuildFailed do ;
     //assert AutoBuild handler already displays message
    on e:Exception do
     begin
      FStatusCode:=500;
      FStatusText:='ERROR';
      try
        if Connected then
         begin
          //TODO: consider HSE_REQ_SEND_CUSTOM_ERROR?
          //TODO: get fragment 500.xxm?
          try
            if FPostData=nil then y:='none' else y:=IntToStr(FPostData.Size)+' bytes';
          except
            y:='unknown';
          end;
          SendError('error',[
            'URL',HTMLEncode(URL),
            'CLASS',FPageClass,
            'POSTDATA',y,
            'QUERYSTRING',ecb.lpszQueryString,
            'ERROR',e.Message,
            'ERRORCLASS',e.ClassName,
            'VERSION',ContextString(csVersion)
          ]);
         end;
      except
        //silent
      end;

      //TODO:ServerFunction(HSE_REQ_ABORTIVE_CLOSE,nil,nil,nil);?
     end;
  end;
  //TODO: support keep connection?
  ecb.dwHttpStatusCode:=FStatusCode;
  ServerFunction(HSE_REQ_CLOSE_CONNECTION,nil,nil,nil);
  ServerFunction(HSE_REQ_DONE_WITH_SESSION,nil,nil,nil);
end;

function TXxmIsapiContext.ContextString(cs: TXxmContextString): WideString;
begin
  //TODO
  case cs of
    csVersion:Result:=SelfVersion+', '+GetVar(ecb,'SERVER_SOFTWARE');
      //'IIS '+IntToStr(HiWord(ecb.dwVersion))+'.'+IntToStr(LoWord(ecb.dwVersion));
    csExtraInfo:         Result:='';//TODO
    csVerb:              Result:=ecb.lpszMethod;
    csQueryString:       Result:=ecb.lpszQueryString;
    csUserAgent:         Result:=GetVar(ecb,'HTTP_USER_AGENT');
    csAcceptedMimeTypes: Result:=GetVar(ecb,'HTTP_ACCEPT');
    csPostMimeType:      Result:=ecb.lpszContentType;
    csURL:               Result:=GetURL;//'HTTP_URL'?
    csProjectName:       Result:=FProjectName;
    csLocalURL:          Result:=FFragmentName;
    csReferer:           Result:=GetVar(ecb,'HTTP_REFERER');
    csLanguage:          Result:=GetVar(ecb,'HTTP_ACCEPT_LANGUAGE');
    csRemoteAddress:     Result:=GetVar(ecb,'REMOTE_ADDR');
    csRemoteHost:        Result:=GetVar(ecb,'REMOTE_HOST');
    csAuthUser:          Result:=GetVar(ecb,'AUTH_USER');
    csAuthPassword:      Result:=GetVar(ecb,'AUTH_PASSWORD');
    else
      raise EXxmContextStringUnknown.Create(StringReplace(
        SXxmContextStringUnknown,'__',IntToHex(integer(cs),8),[]));
  end;
  //ecb.lpszPathInfo;
  //ecb.lpszPathTranslated;
end;

procedure TXxmIsapiContext.DispositionAttach(FileName: WideString);
begin
  FExtraHeaders:=FExtraHeaders+
    'Content-disposition: attachment; filename="'+FileName+'"'#13#10;
  //TODO: test this!!!
end;

procedure TXxmIsapiContext.SendRaw(Data: WideString);
const
  Utf8ByteOrderMark=#$EF#$BB#$BF;
  Utf16ByteOrderMark=#$FF#$FE;
var
  s:string;
  l:cardinal;
begin
  if not(Data='') then
   begin
    if CheckHeader then
      case FAutoEncoding of
        aeUtf8:
         begin
          l:=3;
          if not(ecb.WriteClient(ecb.ConnID,PChar(Utf8ByteOrderMark),l,HSE_IO_SYNC)) then
            RaiseLastOSError;
         end;
        aeUtf16:
         begin
          l:=2;
          if not(ecb.WriteClient(ecb.ConnID,PChar(Utf16ByteOrderMark),l,HSE_IO_SYNC)) then
            RaiseLastOSError;
         end;
      end;
    case FAutoEncoding of
      aeUtf16:
       begin
        l:=Length(Data)*2;
        if not(ecb.WriteClient(ecb.ConnID,PWideChar(Data),l,HSE_IO_SYNC)) then
          RaiseLastOSError;
       end;
      aeUtf8:
       begin
        s:=UTF8Encode(Data);
        l:=Length(s);
        if not(ecb.WriteClient(ecb.ConnID,PChar(s),l,HSE_IO_SYNC)) then
          RaiseLastOSError;
       end;
      else
       begin
        s:=Data;
        l:=Length(s);
        if not(ecb.WriteClient(ecb.ConnID,PChar(s),l,HSE_IO_SYNC)) then
          RaiseLastOSError;
       end;
    end;
    //ReportData;
   end;
end;

procedure TXxmIsapiContext.Send(Data: OleVariant);
begin
  if not(VarIsNull(Data)) then SendRaw(HTMLEncode(Data));
end;

procedure TXxmIsapiContext.SendHTML(Data: OleVariant);
begin
  if not(VarIsNull(Data)) then SendRaw(Data);
end;

procedure TXxmIsapiContext.SendFile(FilePath: WideString);
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

procedure TXxmIsapiContext.SendStream(s: TStream);
var
  l:cardinal;
  d:array[0..$FFFF] of byte;
begin
  inherited;
  FExtraHeaders:=FExtraHeaders+'Content-Length: '+IntToStr(s.Size)+
    #13#10'Accept-Ranges: bytes'#13#10;
  //TODO: keep-connection since content-length known?
  //if not(s.Size=0) then
   begin
    CheckHeader;
    //no autoencoding here
    l:=$10000;
    repeat
      l:=s.Read(d[0],l);
      if not(ecb.WriteClient(ecb.ConnID,@d[0],l,HSE_IO_SYNC)) then
        RaiseLastOSError;
      //ReportData;
    until not(l=$10000);
   end;
end;

function TXxmIsapiContext.GetAutoEncoding: TXxmAutoEncoding;
begin
  Result:=FAutoEncoding;
end;

procedure TXxmIsapiContext.SetAutoEncoding(const Value: TXxmAutoEncoding);
begin
  HeaderOK;
  FAutoEncoding:=Value;
end;

function TXxmIsapiContext.GetContentType: WideString;
begin
  Result:=FContentType;
end;

procedure TXxmIsapiContext.SetContentType(const Value: WideString);
begin
  HeaderOK;
  FContentType:=Value;
  FAutoEncoding:=aeContentDefined;//parse from value? (charset)
end;

procedure TXxmIsapiContext.HeaderOK;
begin
  if FHeaderSent then
    raise EXxmResponseHeaderAlreadySent.Create(SXxmResponseHeaderAlreadySent);
end;

procedure TXxmIsapiContext.ServerFunction(HSERRequest: DWORD;
  Buffer: Pointer; Size, DataType: LPDWORD);
begin
  if not(ecb.ServerSupportFunction(ecb.ConnID,HSERRequest,Buffer,Size,DataType)) then
    RaiseLastOSError;
end;

function TXxmIsapiContext.CheckHeader:boolean;
var
  head:THSE_SEND_HEADER_EX_INFO;
  s,t:string;
begin
  Result:=not(FHeaderSent);
  if Result then
   begin
    s:=IntToStr(FStatusCode)+' '+FStatusText;
    head.pszStatus:=PChar(s);
    head.cchStatus:=Length(s);
    t:='X-Powered-By: '+SelfVersion+#13#10;
    case FAutoEncoding of
      aeUtf8:   t:=t+'Content-Type: '+FContentType+'; charset="utf-8"'#13#10;
      aeUtf16:  t:=t+'Content-Type: '+FContentType+'; charset="utf-16"'#13#10;
      aeIso8859:t:=t+'Content-Type: '+FContentType+'; charset="iso-8859-15"'#13#10;
      else      t:=t+'Content-Type: '+FContentType+#13#10;
    end;
    t:=t+FExtraHeaders+#13#10;
    //TODO cookies? redirect?
    head.pszHeader:=PChar(t);
    head.cchHeader:=Length(t);
    head.fKeepConn:=false;//TODO: true if content-length known?
    ServerFunction(HSE_REQ_SEND_RESPONSE_HEADER_EX,@head,nil,nil);
    FHeaderSent:=true;
   end;
end;

function TXxmIsapiContext.Connected: Boolean;
var
  b:BOOL;
begin
  ServerFunction(HSE_REQ_IS_CONNECTED,@b,nil,nil);
  Result:=b;
end;

procedure TXxmIsapiContext.Include(Address: WideString);
begin
  Include(Address, [], []);
end;

procedure TXxmIsapiContext.Include(Address: WideString;
  const Values: array of OleVariant);
begin
  Include(Address, Values, []);
end;

procedure TXxmIsapiContext.Include(Address: WideString;
  const Values: array of OleVariant;
  const Objects: array of TObject);
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

procedure TXxmIsapiContext.SetStatus(Code: Integer; Text: WideString);
begin
  HeaderOK;
  FStatusCode:=Code;
  FStatusText:=Text;
  //TODO
  ecb.dwHttpStatusCode:=Code;
  //ecb.
end;

procedure TXxmIsapiContext.SendError(res: string; vals: array of string);
var
  s:string;
  i:integer;
const
  RT_HTML = MakeIntResource(23);
begin
  with TResourceStream.Create(HInstance,res,RT_HTML) do
   begin
    SetLength(s,Size);
    Read(s[1],Size);
    Free;
   end;
  for i:=0 to (Length(vals) div 2)-1 do
    s:=StringReplace(s,'[['+vals[i*2]+']]',HTMLEncode(vals[i*2+1]),[rfReplaceAll]);
  if not(FHeaderSent) then
   begin
    FContentType:='text/html';
    FAutoEncoding:=aeContentDefined;//?
   end;
  SendHTML(s);
end;

function TXxmIsapiContext.GetURLPrefix: string;
begin
  Result:='http://'+GetVar(ecb,'HTTP_HOST');
  //TODO: https? other? port?
end;

procedure TXxmIsapiContext.Redirect(RedirectURL: WideString;
  Relative: Boolean);
var
  s:WideString;
  i:integer;
begin
  inherited;
  if Relative then
   begin
    //TODO: proper combine?
    if not(RedirectURL='') and (RedirectURL[1]='/') then
      s:=GetURLPrefix+FRedirectPrefix+RedirectURL
    else
     begin
      s:=FURL;
      i:=Length(s);
      while not(i=0) and not(s[i]='/') do dec(i);
      s:=GetURLPrefix+Copy(s,1,i)+RedirectURL;
     end;
   end
  else
    s:=RedirectURL;
  //utf?
  ServerFunction(HSE_REQ_SEND_URL_REDIRECT_RESP,PChar(UTF8Encode(s)),nil,nil);
  raise EXxmPageRedirected.Create(s);
end;

function TXxmIsapiContext.PostData: TStream;
begin
  Result:=FPostData;
end;

function TXxmIsapiContext.GetParameter(Key: OleVariant): IXxmParameter;
begin
  //parse parameters on first use
  if FParams=nil then FParams:=TXxmReqPars.Create(Self);
  if VarIsNumeric(Key) then Result:=FParams.GetItem(Key) else
    Result:=FParams.Get(VarToWideStr(Key));
end;

function TXxmIsapiContext.GetParameterCount: Integer;
begin
  if FParams=nil then FParams:=TXxmReqPars.Create(Self);
  Result:=FParams.Count;
end;

function TXxmIsapiContext.GetCookie(Name: WideString): WideString;
begin
  if not(FCookieParsed) then
   begin
    FCookie:=';'+GetVar(ecb,'HTTP_COOKIE');
    SplitHeaderValue(FCookie,FCookieIdx);
    FCookieParsed:=true;
   end;
  Result:=GetParamValue(FCookie,FCookieIdx,Name);
end;

procedure TXxmIsapiContext.SetCookie(Name, Value: WideString);
begin
  HeaderOK;
  //check name?
  //TODO: "quoted string"?
  FExtraHeaders:=FExtraHeaders+'Cache-Control: no-cache="set-cookie"'#13#10;//only once!
  FExtraHeaders:=FExtraHeaders+'Set-Cookie: '+Name+'="'+Value+'"'+#13#10;
end;

procedure TXxmIsapiContext.SetCookie(Name,Value:WideString;
  KeepSeconds:cardinal; Comment,Domain,Path:WideString;
  Secure,HttpOnly:boolean); 
begin
  HeaderOK;
  //check name?
  //TODO: "quoted string"?
  FExtraHeaders:=FExtraHeaders+'Cache-Control: no-cache="set-cookie"'#13#10;//only once!
  FExtraHeaders:=FExtraHeaders+'Set-Cookie: '+Name+'="'+Value+'"';
  //'; Version=1';
  if not(Comment='') then
    FExtraHeaders:=FExtraHeaders+'; Comment="'+Comment+'"';
  if not(Domain='') then
    FExtraHeaders:=FExtraHeaders+'; Domain="'+Domain+'"';
  if not(Path='') then
    FExtraHeaders:=FExtraHeaders+'; Path="'+Path+'"';
  FExtraHeaders:=FExtraHeaders+'; Max-Age='+IntToStr(KeepSeconds)+
    '; Expires="'+RFC822DateGMT(Now+KeepSeconds/86400)+'"';
  if Secure then
    FExtraHeaders:=FExtraHeaders+'; Secure'+#13#10;
  if HttpOnly then
    FExtraHeaders:=FExtraHeaders+'; HttpOnly'+#13#10;
  FExtraHeaders:=FExtraHeaders+#13#10;
  //TODO: Set-Cookie2
end;

function TXxmIsapiContext.GetSessionID: WideString;
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

procedure TXxmIsapiContext.Send(Value: int64);
begin

end;

procedure TXxmIsapiContext.Send(Value: integer);
begin
  SendRaw(IntToStr(Value));
end;

procedure TXxmIsapiContext.Send(const Values: array of OleVariant);
var
  i:integer;
begin
  for i:=0 to Length(Values)-1 do SendRaw(HTMLEncode(Values[i]));
end;

procedure TXxmIsapiContext.Send(Value: cardinal);
begin
  SendRaw(IntToStr(Value));
end;

procedure TXxmIsapiContext.SendHTML(const Values: array of OleVariant);
var
  i:integer;
begin
  for i:=0 to Length(Values)-1 do SendRaw(VarToWideStr(Values[i]));
end;

{ TXxmIsapiHandler }

constructor TXxmIsapiHandler.Create;
begin
  inherited Create(false);
  //FInUse:=false;
end;

procedure TXxmIsapiHandler.Execute;
var
  Context:TXxmIsapiContext;
begin
  CoInitialize(nil);
  SetErrorMode(SEM_FAILCRITICALERRORS);
  while not(Terminated) do
   begin
    Context:=IsapiHandlerPool.Unqueue;
    if Context=nil then
     begin
      FInUse:=false;//used by PageLoaderPool.Queue
      Suspend;
      FInUse:=true;
     end
    else
     begin
      Context.Execute;//assert all exceptions handled!
      Context._Release;
     end;
   end;
  CoUninitialize;
end;

{ TXxmIsapiHandlerPool }

constructor TXxmIsapiHandlerPool.Create;
begin
  inherited Create;
  FHandlerSize:=0;
  FQueue:=nil;
  InitializeCriticalSection(FLock);
  SetSize(PoolMaxThreads);//TODO: setting
  //TODO: setting no pool
end;

destructor TXxmIsapiHandlerPool.Destroy;
begin
  SetSize(0);
  DeleteCriticalSection(FLock);
  inherited;
end;

procedure TXxmIsapiHandlerPool.SetSize(x: integer);
begin
  EnterCriticalSection(FLock);
  try
    if FHandlerSize<x then
     begin
      SetLength(FHandlers,x);
      while not(FHandlerSize=x) do
       begin
        FHandlers[FHandlerSize]:=nil;
        inc(FHandlerSize);
       end;
     end
    else
     begin
      while not(FHandlerSize=X) do
       begin
        dec(FHandlerSize);
        //FreeAndNil(FHandlers[FHandlerSize]);
        if not(FHandlers[FHandlerSize]=nil) then
         begin
          FHandlers[FHandlerSize].FreeOnTerminate:=true;
          FHandlers[FHandlerSize].Terminate;
          FHandlers[FHandlerSize].Resume;
          FHandlers[FHandlerSize]:=nil;
         end;
       end;
      SetLength(FHandlers,x);
     end;
    //if FLoaderIndex>=FLoaderSize then FLoaderIndex:=0;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TXxmIsapiHandlerPool.Queue(Context: TXxmIsapiContext);
var
  c:TXxmIsapiContext;
  i:integer;
begin
  EnterCriticalSection(FLock);
  try
    //add to queue
    Context._AddRef;
    if FQueue=nil then FQueue:=Context else
     begin
      c:=FQueue;
      while not(c.Queue=nil) do c:=c.Queue;
      c.Queue:=Context;
     end;
  finally
    LeaveCriticalSection(FLock);
  end;

  //fire thread
  //TODO: see if a rotary index matters in any way
  i:=0;
  while (i<FHandlerSize) and not(FHandlers[i]=nil) and FHandlers[i].InUse do inc(i);
  if i=FHandlerSize then
   begin
    //pool full, leave on queue
   end
  else
   begin
    if FHandlers[i]=nil then
      FHandlers[i]:=TXxmIsapiHandler.Create //start thread
    else
      FHandlers[i].Resume; //resume on waiting unqueues
    //TODO: expire unused threads on low load
   end;
end;

function TXxmIsapiHandlerPool.Unqueue: TXxmIsapiContext;
begin
  if FQueue=nil then Result:=nil else
   begin
    EnterCriticalSection(FLock);
    try
      Result:=FQueue;
      if not(Result=nil) then
       begin
        FQueue:=FQueue.Queue;
        Result.Queue:=nil;
       end;
    finally
      LeaveCriticalSection(FLock);
    end;
   end;
end;

initialization
  IsapiHandlerPool:=TXxmIsapiHandlerPool.Create;
finalization
  FreeAndNil(IsapiHandlerPool);
end.

