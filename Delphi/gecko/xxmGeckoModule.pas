unit xxmGeckoModule;

interface

uses nsXPCOM, nsTypes, xxmGeckoProtocol;

type
  TConstructorProcPtr=function(aOuter:nsISupports;const aIID:TGUID;var aResult:pointer):nsresult; cdecl;
  TLoadFuncPrt=function:nsresult; cdecl;
  TUnloadFuncPrt=procedure; cdecl;
  TCIDEntry=record
    cid:PGUID;
    service:boolean;
    getFactoryProc:pointer;//TGetFactoryProcPtr;
    constructorProc:TConstructorProcPtr;
  end;
  TContractIDEntry=record
    contractid:PAnsiChar;
    cid:PGUID;
  end;
  TCategoryEntry=record
    category,entry,value:PAnsiChar;
  end;

  //ported from http://mxr.mozilla.org/mozilla-central/source/xpcom/components/Module.h
  TXPCOMModule=packed record
    //kVersion:integer;//=2; static;
    mVersion:cardinal;//kModuleVersion
    mCIDs:^TCIDEntry;//pointer to first in array, last should be nil
    mContractIDs:^TContractIDEntry;//pointer to first in array, last should be nil
    mCategoryEntries:^TCategoryEntry;//pointer to first in array, last should be nil
    getFactoryProcPtr:pointer;//TGetFactoryProcPtr;
    loadProc:TLoadFuncPrt;
    unloadProd:TUnloadFuncPrt;
  end;

  TGetFactoryProcPtr=function(const module:TXPCOMModule;const entry:TCIDEntry):nsIFactory; cdecl; //already_AddRefed<nsIFactory>

const
  CIDs:array[0..1] of TCIDEntry=(
    (cid:@CID_xxmProtocolHandler;service:false;getFactoryProc:nil;constructorProc:xxmProtocolHandlerConstructor),
    (cid:nil;service:false;getFactoryProc:nil;constructorProc:nil)
  );

  ContractIDs:array[0..1] of TContractIDEntry=(
    (contractid:PAnsiChar(CONTRACT_xxmProtocolHandler);cid:@CID_xxmProtocolHandler;),
    (contractid:nil;cid:nil;)
  );

  Categories:array[0..0] of TCategoryEntry=(
    (category:nil;entry:nil;value:nil;)
  );

  NSModuleData:TXPCOMModule=(
    mVersion:2;
    mCIDs:@CIDs[0];
    mContractIDs:@ContractIDs[0];
    mCategoryEntries:@Categories[0];
    getFactoryProcPtr:nil;
    loadProc:nil;//TODO:
    unloadProd:nil;//TODO:
  );

var
  NSModule:^TXPCOMModule;

exports
  NSModule;

implementation

uses nsInit;

initialization
  NSModule:=@NSModuleData;
  XPCOMGlueStartup(nil);
finalization
  XPCOMGlueShutdown;
end.
