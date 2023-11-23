unit xxmHSys1Run;

interface

type
  TXxmHandleMessagesProc=procedure(var QuitApp:boolean);

procedure XxmRunHSys(HandleMessagesProc:TXxmHandleMessagesProc);

procedure HandleWindowsMessages(var QuitApp:boolean);

implementation

uses Windows, SysUtils, Classes, ActiveX, httpapi1, xxmContext, xxmPReg,
  xxmThreadPool, xxmHSysMain;

type
  THSysParameters=(
    hpPort,
    hpHostMask,
    hpSecurePort,
    hpThreads,
    //add new here above
    hp_Unknown);

const
  HSysParamNames:array[THSysParameters] of WideString=(
    'port',
    'host',
    'secureport',
    'threads',
    //add new here above
    ''
  );

procedure XxmRunHSys(HandleMessagesProc:TXxmHandleMessagesProc);
var
  i,j,l,c,Port,SecurePort,Threads:integer;
  s,t,Host:WideString;
  hp:THSysParameters;
  hrq:THandle;
  QuitApp:boolean;
begin
  CoInitialize(nil);
  QuitApp:=false;

  HttpCheck(HttpInitialize(HTTPAPI_VERSION_1_0,HTTP_INITIALIZE_SERVER,nil));
  HttpCheck(HttpCreateHttpHandle(hrq,0));

  //defaults
  Port:=80;
  SecurePort:=0;//443;
  Host:='+';
  Threads:=$100;

  c:=0;
  for i:=1 to ParamCount do
   begin
    s:=ParamStr(i);
    l:=Length(s);
    j:=1;
    while (j<=l) and (s[j]<>'=') do inc(j);
    if (j<=l) then
     begin
      t:=LowerCase(Copy(s,1,j-1));
      s:=Copy(s,j+1,l-j);
      hp:=Low(THSysParameters);
      while (hp<>hp_Unknown) and (HSysParamNames[hp]<>t) do inc(hp);
      case hp of

        hpPort:
          Port:=StrToInt(s);
        hpHostMask:
          Host:=s;
        hpSecurePort: //see also http://msdn.microsoft.com/en-us/library/ms733791.aspx
          if s='' then SecurePort:=443 else SecurePort:=StrToInt(s);
        hpThreads:
          Threads:=StrToInt(s);

        //add new here above
        else raise Exception.Create('Unknown parameter "'+t+'"');
      end;
     end
    else
     begin

      if Port<>0 then
       begin
        HttpCheck(HttpAddUrl(hrq,PWideChar(
          'http://'+Host+':'+IntToStr(Port)+'/'+s+'/'),nil));
        inc(c);
       end;
      if SecurePort<>0 then
       begin
        HttpCheck(HttpAddUrl(hrq,PWideChar(
          'https://'+Host+':'+IntToStr(SecurePort)+'/'+s+'/'),nil));
        inc(c);
       end;

     end;
   end;
  //TODO: load from xxm.json?
  if c=0 then raise Exception.Create('No projects loaded');

  XxmProjectCache:=TXxmProjectCache.Create;
  ContextPool:=TXxmContextPool.Create(TXxmHSysContext);
  PageLoaderPool:=TXxmPageLoaderPool.Create(Threads);
  
  //TODO: try except
  //TODO: mutex?
  //TODO: overlapped/completionport
  while not QuitApp do
   begin
    //if WaitForSingleObject(hrq,0)=WAIT_OBJECT_0 then ???
    (ContextPool.GetContext as TXxmHSysContext).Load(hrq);
    HandleMessagesProc(QuitApp);
   end;

  //HttpCheck(HttpRemoveUrl(
  HttpTerminate(HTTP_INITIALIZE_SERVER,nil);
end;

procedure HandleWindowsMessages(var QuitApp:boolean);
var
  Msg:TMsg;
const
  WM_QUIT = $0012;//from Messages
begin
  if PeekMessage(Msg,0,0,0,PM_REMOVE) then
    if Msg.message=WM_QUIT then
      QuitApp:=true
    else
     begin
      TranslateMessage(Msg);
      DispatchMessage(Msg);
     end
  else
    Sleep(1);//don't take 100% CPU!
end;

end.
