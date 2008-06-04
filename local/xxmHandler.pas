unit xxmHandler;

{$WARN SYMBOL_PLATFORM OFF}

interface

//ms-help://MS.MSDNQTR.2003FEB.1033/networking/workshop/networking/pluggable/pluggable.htm

uses
  Windows, SysUtils, ActiveX, Classes, ComObj, UrlMon,
  xxm, xxmLoader;

type
  TXxmLocalHandler=class(TComObject, IInternetProtocol, IWinInetHttpInfo, IInternetProtocolInfo)
  //TODO: IInternetProtocolInfo
  private
    FDataPos: Int64;
    FContext: TXxmLocalContext;
    FContextI: IXxmContext;
    FTerminateTC: cardinal;
  protected
    { IInternetProtocolRoot }
    function Start(szUrl: PWideChar; OIProtSink: IInternetProtocolSink;
      OIBindInfo: IInternetBindInfo; grfPI: Cardinal;
      dwReserved: Cardinal): HRESULT; stdcall;
    function Suspend: HRESULT; stdcall;
    function Resume: HRESULT; stdcall;
    function Continue(const ProtocolData: _tagPROTOCOLDATA): HRESULT; stdcall;
    function Abort(hrReason: HRESULT; dwOptions: Cardinal): HRESULT; stdcall;
    function Terminate(dwOptions: Cardinal): HRESULT; stdcall;
    { IInternetProtocol }
    function LockRequest(dwOptions: Cardinal): HRESULT; stdcall;
    function Read(pv: Pointer; cb: Cardinal; out cbRead: Cardinal): HRESULT;
      stdcall;
    function Seek(dlibMove: _LARGE_INTEGER; dwOrigin: Cardinal;
      out libNewPosition: ULARGE_INTEGER): HRESULT; stdcall;
    function UnlockRequest: HRESULT; stdcall;
    { IWinInetInfo }
    function QueryOption(dwOption: DWORD; Buffer: Pointer; var cbBuf: DWORD): HResult; stdcall;
    { IWinInetHttpInfo }
    function QueryInfo(dwOption: DWORD; Buffer: Pointer;
      var cbBuf, dwFlags, dwReserved: DWORD): HResult; stdcall;
    { IInternetProtocolInfo }
    function ParseUrl(pwzUrl: LPCWSTR; ParseAction: TParseAction; dwParseFlags: DWORD;
      pwzResult: LPWSTR; cchResult: DWORD; pcchResult: DWORD;
      dwReserved: DWORD): HResult; stdcall;
    function CombineUrl(pwzBaseUrl, pwzRelativeUrl: LPCWSTR; dwCombineFlags: DWORD;
      pwzResult: LPWSTR; cchResult: DWORD; out pcchResult: DWORD;
      dwReserved: DWORD): HResult; stdcall;
    function CompareUrl(pwzUrl1, pwzUrl2: LPCWSTR; dwCompareFlags: DWORD): HResult; stdcall;
    function P1QueryInfo(pwzUrl: LPCWSTR; QueryOption: TQueryOption; dwQueryFlags: DWORD;
      pBuffer: Pointer; cbBuffer: DWORD; var cbBuf: DWORD; dwReserved: DWORD): HResult; stdcall;
    function IInternetProtocolInfo.QueryInfo=P1QueryInfo;

  public
    procedure Initialize; override;
    destructor Destroy; override;
  end;

  TXxmLocalHandlerFactory=class(TComObjectFactory)
  public
    procedure UpdateRegistry(Register: Boolean); override;
  end;

var
  XxmLocalHandlerFactory:TXxmLocalHandlerFactory;

const
  Class_xxmLocalHandler:TGUID='{78786D00-0000-0001-C000-000000000001}';
  URLSchema='xxm';
  URLSchemaDescription='xxm Local Handler';

implementation

uses ComServ, Registry, xxmWinInet, xxmThreadPool;

{ TXxmLocalHandler }

procedure TXxmLocalHandler.Initialize;
begin
  inherited;
  FContext:=nil;
  FContextI:=nil;
  FDataPos:=0;
  FTerminateTC:=0;
end;

destructor TXxmLocalHandler.Destroy;
begin
  FContextI:=nil;//FreeAndNil(FContext);
  FContext:=nil;
  inherited;
end;

{ TXxmLocalHandler::IInternetProtocolRoot }

function TXxmLocalHandler.Start(szUrl: PWideChar;
  OIProtSink: IInternetProtocolSink; OIBindInfo: IInternetBindInfo; grfPI,
  dwReserved: Cardinal): HRESULT;
begin
  FContext:=TXxmLocalContext.Create(szUrl,OIProtSink,OIBindInfo);
  FContextI:=FContext;//use refcount to clean with later
  if PageLoaderPool=nil then PageLoaderPool:=TXxmPageLoaderPool.Create;
  PageLoaderPool.Queue(FContext);

  Result:=HResult(E_PENDING);
end;

function TXxmLocalHandler.Suspend: HRESULT;
begin
  //Context.Loader.Suspend;Result:=S_OK;
  Result:=E_NOTIMPL;
end;

function TXxmLocalHandler.Resume: HRESULT;
begin
  //Context.Loader.Resume;Result:=S_OK;
  Result:=E_NOTIMPL;
end;

function TXxmLocalHandler.Continue(
  const ProtocolData: _tagPROTOCOLDATA): HRESULT;
begin
  Result:=E_NOTIMPL;
end;

function TXxmLocalHandler.Abort(hrReason: HRESULT;
  dwOptions: Cardinal): HRESULT;
begin
  FContext.Disconnect;
  Result:=S_OK;
end;

function TXxmLocalHandler.Terminate(dwOptions: Cardinal): HRESULT;
begin
  //while not(FContext.PageComplete) do Sleep(5);
  FContextI:=nil;//FreeAndNil(FContext);
  FContext:=nil;
  Result:=S_OK;
end;

{ TXxmLocalHandler::IInternetProtocol }

function TXxmLocalHandler.LockRequest(dwOptions: Cardinal): HRESULT;
begin
  Result:=S_OK;
end;

function TXxmLocalHandler.UnlockRequest: HRESULT;
begin
  Result:=S_OK;
end;

function TXxmLocalHandler.Read(pv: Pointer; cb: Cardinal;
  out cbRead: Cardinal): HRESULT;
type
  TBArr=array[0..0] of byte;
  PBArr=^TBArr;
var
  ReadSize:integer;
  BArr:PBArr;
const
  CollapseTreshold=$20000;//128KB
begin
  FContext.Lock;
  try
    //read how much now?
    ReadSize:=cb;
    if FDataPos+ReadSize>FContext.OutputSize then
      ReadSize:=FContext.OutputSize-FDataPos;
    if ReadSize<0 then ReadSize:=0;

    //read!
    if ReadSize=0 then cbRead:=0 else
     begin
      FContext.OutputData.Position:=FDataPos;
      cbRead:=FContext.OutputData.Read(pv^,ReadSize);
      inc(FDataPos,cbRead);
      //cache to file??
     end;

    if (FDataPos>=FContext.OutputSize) then
     begin
      if (FContext.OutputData is TMemoryStream) then
       begin
        inc(FContext.ClippedSize,FContext.OutputSize);
        FContext.OutputSize:=0;//no SetSize, just reset pointer, saves on realloc calls
        FDataPos:=0;
       end;
      ReadSize:=0;
     end;

    if ReadSize=0 then
     begin
      if FContext.PageComplete then
       begin
        //if FContext.Redirected then Result:=INET_E_USE_DEFAULT_PROTOCOLHANDLER else
        Result:=S_FALSE;
       end
      else
        Result:=HResult(E_PENDING);
     end
    else
     begin
      if (FContext.OutputData is TMemoryStream) and (FDataPos>=CollapseTreshold) then
       begin
        dec(FContext.OutputSize,FDataPos);
        BArr:=PBArr((FContext.OutputData as TMemoryStream).Memory);
        Move(BArr[FDataPos],BArr[0],FContext.OutputSize);
        FDataPos:=0;
       end;
      Result:=S_OK;
     end;
    //INET_E_DATA_NOT_AVAILABLE //all read but more data was expected
    //except Result:=INET_E_DOWNLOAD_FAILURE?
  finally
    FContext.DataReported:=false;
    FContext.Unlock;
  end;
end;

function TXxmLocalHandler.Seek(dlibMove: _LARGE_INTEGER;
  dwOrigin: Cardinal; out libNewPosition: ULARGE_INTEGER): HRESULT;
begin
  Result:=E_NOTIMPL;
end;

function TXxmLocalHandler.QueryOption(dwOption: DWORD; Buffer: Pointer;
  var cbBuf: DWORD): HResult;
begin
  //Result:=E_NOTIMPL;
  case dwOption of
    INTERNET_OPTION_REQUEST_FLAGS:
     begin
      //assert(cbBuf=4)
      PDWORD(Buffer)^:=0;
      //INTERNET_REQFLAG_VIA_PROXY?
      //INTERNET_REQFLAG_NET_TIMEOUT?
      //if not(SingleFileSent then INTERNET_REQFLAG_FROM_CACHE?
      Result:=S_OK;
     end;
    INTERNET_OPTION_SECURITY_FLAGS:
     begin
      //assert(cbBuf=4)
      PDWORD(Buffer)^:=0;
      Result:=S_OK;
     end;

    INTERNET_OPTION_ERROR_MASK:
     begin
      PDWORD(Buffer)^:=10;
      Result:=S_OK;
     end;

    else Result:=S_FALSE;
  end;
end;

function TXxmLocalHandler.QueryInfo(dwOption: DWORD; Buffer: Pointer;
  var cbBuf, dwFlags, dwReserved: DWORD): HResult;
var
  s:string;
  f:HFILE;
  dt1,dt2,dt3:TFileTime;
  st:TSystemTime;
begin
  try
    case dwOption and $F0000000 of
      HTTP_QUERY_FLAG_SYSTEMTIME:
        case dwOption and $0FFFFFFF of
          HTTP_QUERY_LAST_MODIFIED:
           begin
            s:=FContext.SingleFileSent;
            Result:=S_FALSE;//default
            if not(s='') then
             begin
              f:=CreateFile(PChar(s),GENERIC_READ,7,nil,OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,0);
              if not(f=INVALID_HANDLE_VALUE) then
               begin
                if GetFileTime(f,@dt1,@dt2,@dt3) and FileTimeToSystemTime(dt3,st) then
                 begin
                  //assert cbBuf=SizeOf(TSystemTime);
                  Move(st,Buffer^,cbBuf);
                  Result:=S_OK;
                 end;
                CloseHandle(f);
               end;
             end;
           end;
          else Result:=E_INVALIDARG;
        end;
      HTTP_QUERY_FLAG_NUMBER:
        case dwOption and $0FFFFFFF of
          HTTP_QUERY_STATUS_CODE:
           begin
            //assert cbBuf:=4;
            PDWORD(Buffer)^:=FContext.StatusCode;
            Result:=S_OK;
           end;
          else Result:=E_INVALIDARG;
        end;
      else
       begin
        Result:=S_FALSE;
        case dwOption and $0FFFFFFF of
          HTTP_QUERY_REFRESH:Result:=S_FALSE;
          HTTP_QUERY_STATUS_TEXT:s:=FContext.StatusText+#0;
          HTTP_QUERY_REQUEST_METHOD:s:=FContext.Verb+#0;
          HTTP_QUERY_CONTENT_TYPE:s:=FContext.ContentType+#0;
          else Result:=E_INVALIDARG;
        end;
        if Result=S_OK then
          if cbBuf<DWORD(Length(s)) then Result:=E_OUTOFMEMORY else
           begin
            Move(s[1],Buffer^,Length(s));
            cbBuf:=Length(s);
           end;
       end;
    end;
  except
    Result:=E_FAIL;
  end;
end;

function TXxmLocalHandler.CombineUrl(pwzBaseUrl, pwzRelativeUrl: LPCWSTR;
  dwCombineFlags: DWORD; pwzResult: LPWSTR; cchResult: DWORD;
  out pcchResult: DWORD; dwReserved: DWORD): HResult;
begin
  //TODO: CombineURL
  Result:=INET_E_DEFAULT_ACTION;
end;

function TXxmLocalHandler.CompareUrl(pwzUrl1, pwzUrl2: LPCWSTR;
  dwCompareFlags: DWORD): HResult;
begin
  //TODO: CompareURL
  Result:=INET_E_DEFAULT_ACTION;
end;

function TXxmLocalHandler.P1QueryInfo(pwzUrl: LPCWSTR;
  QueryOption: TQueryOption; dwQueryFlags: DWORD; pBuffer: Pointer;
  cbBuffer: DWORD; var cbBuf: DWORD; dwReserved: DWORD): HResult;
begin
  Result:=INET_E_DEFAULT_ACTION;
end;

function TXxmLocalHandler.ParseUrl(pwzUrl: LPCWSTR;
  ParseAction: TParseAction; dwParseFlags: DWORD; pwzResult: LPWSTR;
  cchResult, pcchResult, dwReserved: DWORD): HResult;
var
  i,j,l:integer;
  FURL,w:WideString;
begin
  case ParseAction of
    //PARSE_SECURITY_URL://TODO: strip context/param data from URL
    PARSE_SECURITY_DOMAIN:
     begin
      //see also loader!!
      FURL:=pwzUrl;
      l:=Length(FURL);
      i:=1;
      while (i<=l) and not(FURL[i]=':') do inc(i);
      //assert starts with 'xxm:'
      w:=Copy(FURL,1,i);
      inc(i);
      if (i<=l) and (FURL[i]='/') then inc(i);
      if (i<=l) and (FURL[i]='/') then inc(i);
      j:=i;
      while (i<=Length(FURL)) and not(Char(FURL[i]) in ['/','?','&','$','#']) do inc(i);
      w:=w+Copy(FURL,j,i-j);
      if cchResult<cardinal(Length(w)+1) then Result:=S_FALSE else
       begin
        Move(PWideChar(w)^,pwzResult^,Length(w)*2+2);
        PDWORD(pcchResult)^:=Length(w)+1;
        Result:=S_OK;
       end;
     end;
    else
      Result:=INET_E_DEFAULT_ACTION;
  end;
end;

{ TXxmLocalHandlerFactory }

procedure TXxmLocalHandlerFactory.UpdateRegistry(Register: Boolean);
var
  r:TRegistry;
  fn:string;
  procedure SimpleAdd(Key,Value:string);
  begin
    r.OpenKey(Key,true);
    r.WriteString('',Value);
    r.CloseKey;
  end;
begin
  inherited;
  r:=TRegistry.Create;
  try
    if Register then
     begin
      fn:=ComServer.ServerFileName;

      r.RootKey:=HKEY_CLASSES_ROOT;
      r.OpenKey('\'+URLSchema,true);
      r.WriteString('','URL:'+URLSchemaDescription);
      r.WriteInteger('EditFlags',2);
      r.WriteString('FriendlyTypeName',URLSchemaDescription);
      r.WriteString('URL Protocol','');
      r.CloseKey;

      r.OpenKey('\'+URLSchema+'\shell',true);
      r.WriteString('','open');
      r.CloseKey;

      r.OpenKey('\'+URLSchema+'\shell\open',true);
      r.WriteString('','Open');
      r.CloseKey;

      r.OpenKey('\'+URLSchema+'\shell\open\command',true);
      r.WriteString('','iexplore "%l"');
      r.CloseKey;

      SimpleAdd('\'+URLSchema+'\DefaultIcon',fn+',1');

      //r.OpenKey('\'+URLSchema+'\Extensions',true);
      //r.OpenKey('\'+URLSchema+'\shell',true);
      //r.OpenKey('\'+URLSchema+'\shell\open',true);
      //r.OpenKey('\'+URLSchema+'\shell\open\command',true);

      r.OpenKey('\Protocols\Handler\'+URLSchema,true);
      r.WriteString('',URLSchemaDescription);
      r.WriteString('CLSID',GUIDToString(Class_xxmLocalHandler));
      r.CloseKey;

      //filetypes

      //SimpleAdd('.xxmp','xxmpfile');
      r.OpenKey('.xxmp',true);
      r.WriteString('','xxmpfile');
      r.WriteString('Content Type','text/xml');
      r.CloseKey;
      SimpleAdd('xxmpfile','xxm Project File');
      SimpleAdd('xxmpfile\DefaultIcon',fn+',3');
      //SimpleAdd('xxmpfile\CLSID',);
      SimpleAdd('xxmpfile\shell','');

      SimpleAdd('.xxm','xxmfile');
      //TODO: mime type?
      SimpleAdd('xxmfile','xxm Page File');
      SimpleAdd('xxmfile\DefaultIcon',fn+',4');
      //SimpleAdd('xxmfile\CLSID',);
      SimpleAdd('xxmfile\shell','');
      SimpleAdd('.xxmi','xxmifile');
      SimpleAdd('xxmifile','xxm Include File');
      SimpleAdd('xxmifile\DefaultIcon',fn+',5');
      //SimpleAdd('xxmifile\CLSID',);
      SimpleAdd('xxmifile\shell','');
      SimpleAdd('.xxl','xxlfile');
      SimpleAdd('xxlfile','xxm Library');
      SimpleAdd('xxlfile\DefaultIcon',fn+',2');
      //SimpleAdd('xxlfile\CLSID',);
      SimpleAdd('xxlfile\shell','');

      SimpleAdd('xxlfile\Shell\RegLocal','Register for local handler');
      SimpleAdd('xxlfile\Shell\RegLocal\command','rundll32.exe "'+fn+'",XxmProjectRegister %l');

      //Security Zone: Local Intranet
      r.RootKey:=HKEY_CURRENT_USER;
      r.OpenKey('\Software\Microsoft\Windows\CurrentVersion\Internet Settings\ZoneMap\ProtocolDefaults',true);
      r.WriteInteger(URLSchema,1);
      r.CloseKey;
      r.RootKey:=HKEY_LOCAL_MACHINE;
      r.OpenKey('\Software\Microsoft\Windows\CurrentVersion\Internet Settings\ZoneMap\ProtocolDefaults',true);
      r.WriteInteger(URLSchema,1);
      r.CloseKey;
      //HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\URL\Prefixes?

     end
    else
     begin
      r.RootKey:=HKEY_CLASSES_ROOT;
      r.DeleteKey('\'+URLSchema);
      r.DeleteKey('\Protocols\Handler\'+URLSchema);

      r.RootKey:=HKEY_CLASSES_ROOT;
      r.DeleteKey('.xxmp');
      r.DeleteKey('xxmpfile');
      r.DeleteKey('.xxm');
      r.DeleteKey('xxmfile');
      r.DeleteKey('.xxmi');
      r.DeleteKey('xxmifile');
      r.DeleteKey('.xxl');
      r.DeleteKey('xxlfile');

      r.RootKey:=HKEY_CURRENT_USER;
      r.OpenKey('\Software\Microsoft\Windows\CurrentVersion\Internet Settings\ZoneMap\ProtocolDefaults',true);
      r.DeleteValue(URLSchema);
      r.CloseKey;

      r.RootKey:=HKEY_LOCAL_MACHINE;
      r.OpenKey('\Software\Microsoft\Windows\CurrentVersion\Internet Settings\ZoneMap\ProtocolDefaults',true);
      r.DeleteValue(URLSchema);
      r.CloseKey;
      //TODO: switch keep project registry?
      r.DeleteKey('\Software\xxm');
     end;
  finally
    r.Free;
  end;
end;

initialization
  XxmLocalHandlerFactory:=TXxmLocalHandlerFactory.Create(ComServer,
    TXxmLocalHandler, Class_xxmLocalHandler,
    'XxmLocalHandler', URLSchemaDescription,
    ciMultiInstance, tmApartment);
finalization
  FreeAndNil(PageLoaderPool);//clear all loader threads first!
  FreeAndNil(XxmLocalHandlerFactory);
end.
