unit xxmApacheModule;

interface

uses HTTPD2;

var
  xxm_module: module;
  {$EXTERNALSYM xxm_module}

exports
  xxm_module name 'xxm_module';

implementation

uses Windows, SysUtils, xxmApacheContext, ActiveX;

function xxm_handler(r:Prequest_rec): integer; cdecl;
var
  ctx:TxxmApacheContext;
begin
  if not(AnsiString(r.handler)='xxm-handler') then Result:=DECLINED else
   begin
    CoInitialize(nil);//TODO: keep threadvar flag?

    ctx:=TxxmApacheContext.Create(r);
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

procedure xxm_register_hooks(p:Papr_pool_t); cdecl;
begin
  ap_hook_handler(xxm_handler,nil,nil,APR_HOOK_MIDDLE);
end;

var
  XxmModuleName:AnsiString;

procedure InitXxmApacheModule;
var
  i:integer;
begin
  SetLength(XxmModuleName,MAX_PATH+1);
  i:=GetModuleFileNameA(HInstance,PAnsiChar(XxmModuleName),MAX_PATH);
  SetLength(XxmModuleName,i);
  while not(i=0) and not(XxmModuleName[i]='\') do dec(i);
  XxmModuleName:=Copy(XxmModuleName,i+1,Length(XxmModuleName)-i);
  ZeroMemory(@xxm_module,SizeOf(module));
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
  InitXxmApacheModule;
end.
