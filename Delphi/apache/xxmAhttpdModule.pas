unit xxmAhttpdModule;

interface

uses httpd24;

var
  xxm_module: TModule;

exports
  xxm_module name 'xxm_module';

implementation

uses Windows, SysUtils, xxmAhttpdContext, ActiveX;

function xxm_handler(r:PRequest): integer; cdecl;
var
  ctx:TxxmAhttpdContext;
begin
  if AnsiString(r.handler)<>'xxm-handler' then Result:=AP_DECLINED else
   begin
    CoInitialize(nil);//TODO: keep threadvar flag?

    ctx:=TxxmAhttpdContext.Create(r);
    (ctx as IUnknown)._AddRef;
    try
      ctx.Execute;
    finally
      (ctx as IUnknown)._Release;
      //ctx.Free;//TODO: pool these?
    end;

    Result:=AP_OK;
   end;
end;

procedure xxm_register_hooks(p:PPool); cdecl;
begin
  ap_hook_handler(xxm_handler,nil,nil,APR_HOOK_MIDDLE);
end;

var
  XxmModuleName:AnsiString;

procedure InitXxmAhttpdModule;
var
  i:integer;
begin
  SetLength(XxmModuleName,MAX_PATH+1);
  i:=GetModuleFileNameA(HInstance,PAnsiChar(XxmModuleName),MAX_PATH);
  SetLength(XxmModuleName,i);
  while not(i=0) and not(XxmModuleName[i]='\') do dec(i);
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
end;

initialization
  InitXxmAhttpdModule;
end.
