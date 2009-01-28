unit xxmGeckoModule;

interface

uses nsXPCOM, nsTypes;

type
  TxxmGeckoModule=class(TInterfacedObject, nsIModule)
  private
    FCompMgr:nsIComponentManager;
  protected
    procedure GetClassObject(aCompMgr: nsIComponentManager;
      const aClass: TGUID; const aIID: TGUID; out aResult); safecall;
    procedure RegisterSelf(aCompMgr: nsIComponentManager;
      aLocation: nsIFile; const aLoaderStr: PAnsiChar;
      const aType: PAnsiChar); safecall;
    procedure UnregisterSelf(aCompMgr: nsIComponentManager;
      aLocation: nsIFile; const aLoaderStr: PAnsiChar); safecall;
    function CanUnload(aCompMgr: nsIComponentManager): LongBool; safecall;
  public
    constructor Create(ACompMgr:nsIComponentManager);
    destructor Destroy; override;
    property CompMgr:nsIComponentManager read FCompMgr;
  end;

  TxxmGeckoComponent=class(TInterfacedObject)
  private
    FModule:TxxmGeckoModule;
  protected
    property Module:TxxmGeckoModule read FModule;
  public
    constructor Create(AOwner:TxxmGeckoModule);
  end;

  TxxmGeckoComponentClass=class of TxxmGeckoComponent;

  TxxmGeckoComponentFactory=class(TInterfacedObject, nsIFactory)
  private
    FOwner:TxxmGeckoModule;
    FClass:TxxmGeckoComponentClass;
  protected
    procedure CreateInstance(aOuter: nsISupports; const iid: TGUID; out _result); safecall;
    procedure LockFactory(lock: PRBool); safecall;
  public
    constructor Create(AOwner:TxxmGeckoModule;AClass:TxxmGeckoComponentClass);
  end;

procedure RegisterComponent(AName,AContract:string;ACID:TGUID;AClass:TxxmGeckoComponentClass);

function NSGetModule(aCompMgr: nsIComponentManager; location: nsIFile; out return_cobj: nsIModule): nsresult; cdecl;

exports
  NSGetModule;

implementation

uses nsInit, nsError, ComObj, SysUtils;

var
  RegisteredComponents:array of record
    Name,Contract:string;
    CID:TGUID;
    CCl:TxxmGeckoComponentClass;
  end;

procedure RegisterComponent(AName,AContract:string;ACID:TGUID;AClass:TxxmGeckoComponentClass);
var
  l:integer;
begin
  l:=Length(RegisteredComponents);
  SetLength(RegisteredComponents,l+1);
  RegisteredComponents[l].Name:=AName;
  RegisteredComponents[l].Contract:=AContract;
  RegisteredComponents[l].CID:=ACID;
  RegisteredComponents[l].CCl:=AClass;
end;

function NSGetModule(aCompMgr: nsIComponentManager; location: nsIFile; out return_cobj: nsIModule): nsresult; cdecl;
begin
  return_cobj:=TxxmGeckoModule.Create(aCompMgr);
  Result:=NS_OK;
end;

{ TxxmGeckoModule }

procedure TxxmGeckoModule.GetClassObject(aCompMgr: nsIComponentManager;
  const aClass, aIID: TGUID; out aResult);
var
  i,l:integer;
begin
  l:=Length(RegisteredComponents);
  i:=0;
  while (i<l) and not(IsEqualGUID(RegisteredComponents[i].CID,aClass)) do inc(i);
  if (i<l) then
   begin
    if not((TxxmGeckoComponentFactory.Create(Self,RegisteredComponents[i].Ccl) as IInterface).QueryInterface(aIID,aResult)=S_OK) then
      Error(reIntfCastError);
   end
  else
    Error(reInvalidOp);
end;

procedure TxxmGeckoModule.RegisterSelf(aCompMgr: nsIComponentManager;
  aLocation: nsIFile; const aLoaderStr, aType: PAnsiChar);
var
  r:nsIComponentRegistrar;
  i:integer;
begin
  r:=aCompMgr as nsIComponentRegistrar;
  for i:=0 to Length(RegisteredComponents)-1 do
    r.RegisterFactoryLocation(
      RegisteredComponents[i].CID,
      PAnsiChar(RegisteredComponents[i].Name),
      PAnsiChar(RegisteredComponents[i].Contract),
      aLocation,aLoaderStr,aType);
  //nsIXULAppInfo?
  //HKEY_CURRENT_USER\Software\Mozilla\Firefox\Extensions?
  //HKEY_LOCAL_MACHINE\Software\Mozilla\Firefox\Extensions?
end;

procedure TxxmGeckoModule.UnregisterSelf(aCompMgr: nsIComponentManager;
  aLocation: nsIFile; const aLoaderStr: PAnsiChar);
var
  r:nsIComponentRegistrar;
  i:integer;
begin
  r:=aCompMgr as nsIComponentRegistrar;
  for i:=0 to Length(RegisteredComponents)-1 do
    r.UnregisterFactoryLocation(
      RegisteredComponents[i].CID,
      aLocation);
end;

function TxxmGeckoModule.CanUnload(
  aCompMgr: nsIComponentManager): LongBool;
begin
  //TODO:
  Result:=true;
end;

constructor TxxmGeckoModule.Create(ACompMgr: nsIComponentManager);
begin
  inherited Create;
  FCompMgr:=ACompMgr;
end;

destructor TxxmGeckoModule.Destroy;
begin
  FCompMgr:=nil;
  inherited;
end;

{ TxxmGeckoComponent }

constructor TxxmGeckoComponent.Create(AOwner: TxxmGeckoModule);
begin
  inherited Create;
  FModule:=AOwner;
end;

{ TxxmGeckoComponentFactory }

constructor TxxmGeckoComponentFactory.Create(AOwner: TxxmGeckoModule;
  AClass: TxxmGeckoComponentClass);
begin
  inherited Create;
  FOwner:=AOwner;
  FClass:=AClass;
end;

procedure TxxmGeckoComponentFactory.CreateInstance(aOuter: nsISupports;
  const iid: TGUID; out _result);
begin
  if not((FClass.Create(FOwner) as IInterface).QueryInterface(iid,_result)=S_OK) then
    Error(reIntfCastError);
end;

procedure TxxmGeckoComponentFactory.LockFactory(lock: PRBool);
begin
  //not implemented
  Error(reInvalidOp);
end;

initialization
  XPCOMGlueStartup(nil);
finalization
  XPCOMGlueShutdown;
end.
