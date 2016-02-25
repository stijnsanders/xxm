unit xxmAhttpdModule;

interface

uses httpd24;

var
  xxm_module: TModule;

exports
  xxm_module name 'xxm_module';

implementation

uses Windows, SysUtils, xxmAhttpdContext, ActiveX, xxmContext;

function xxm_handler(r:PRequest): integer; cdecl;
begin
  if AnsiString(r.handler)<>'xxm-handler' then Result:=AP_DECLINED else
   begin
    CoInitialize(nil);
    (ContextPool.GetContext as TxxmAhttpdContext).Perform(r);
    Result:=AP_OK;//pending?
   end;
end;

procedure xxm_register_hooks(p:PPool); cdecl;
begin
  ap_hook_handler(xxm_handler,nil,nil,APR_HOOK_MIDDLE);
end;

var
  XxmModuleName:AnsiString;

const
  PoolMaxThreads=$200;//TODO: from setting?

procedure InitXxmAhttpdModule;
var
  i:integer;
begin
  SetLength(XxmModuleName,MAX_PATH+1);
  i:=GetModuleFileNameA(HInstance,PAnsiChar(XxmModuleName),MAX_PATH);
  SetLength(XxmModuleName,i);
  while (i<>0) and (XxmModuleName[i]<>'\') do dec(i);
  XxmModuleName:=Copy(XxmModuleName,i+1,Length(XxmModuleName)-i);
  ZeroMemory(@xxm_module,SizeOf(TModule));
  with xxm_module do
   begin
    version := MODULE_MAGIC_NUMBER_MAJOR;
    minor_version := MODULE_MAGIC_NUMBER_MINOR;
    module_index := -1;
    name := PAnsiChar(XxmModuleName);
    magic := MODULE_MAGIC_COOKIE;
    register_hooks := xxm_register_hooks;
   end;
  ContextPool:=TXxmContextPool.Create(TxxmAhttpdContext);
end;

initialization
  InitXxmAhttpdModule;
end.
