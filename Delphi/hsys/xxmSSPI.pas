unit xxmSSPI;

interface

uses Windows;

type
  TCredHandle=record
    dwLower:pointer;
    dwUpper:pointer;
  end;
  PCredHandle=^TCredHandle;

  TCtxtHandle=type TCredHandle;
  PCtxtHandle=^TCtxtHandle;

type
  TSChannelCred=record
    dwVersion: cardinal;
    cCreds: cardinal;
    paCred: pointer;//PCertContext;
    hRootStore: THandle;
    cMappers: cardinal;
    aphMappers: pointer;
    cSupportedAlgs: cardinal;
    palgSupportedAlgs: PCardinal;
    grbitEnabledProtocols: cardinal;
    dwMinimumCipherStrength: cardinal;
    dwMaximumCipherStrength: cardinal;
    dwSessionLifespan: cardinal;
    dwFlags: cardinal;
    dwCredFormat: cardinal;
  end;
  PSChannelCred=^TSChannelCred;

  TSecBuffer=record
    cbBuffer: cardinal;
    BufferType: cardinal;
    pvBuffer: pointer;
  end;
  PSecBuffer=^TSecBuffer;

  TSecBufferDesc=record
    ulVersion: cardinal;
    cBuffers: cardinal;
    pBuffers: PSecBuffer;
  end;
  PSecBufferDesc=^TSecBufferDesc;

  TTimeStamp=record
    dwLowDateTime: cardinal;
    dwHighDateTime: cardinal;
  end;
  PTimeStamp=^TTimeStamp;

  TSecPkgContextNames=record
    sUserName:PAnsiChar;
  end;
  PSecPkgContextNames=^TSecPkgContextNames;

  TSecPkgContextStreamSizes=record
    cbHeader: cardinal;
    cbTrailer: cardinal;
    cbMaximumMessage: cardinal;
    cBuffers: cardinal;
    cbBlockSize: cardinal;
  end;
  PSecPkgContextStreamSizes=^TSecPkgContextStreamSizes;

  
function AcquireCredentialsHandle(pszPrincipal: PAnsiChar;
    pszPackage: PAnsiChar; fCredentialUse: cardinal; pvLogonID: PInt64;
    pAuthData: PSChannelCred; pGetKeyFn: pointer; pvGetKeyArgument: pointer;
    phCredential: PCredHandle; ptsExpiry: PTimeStamp): cardinal; stdcall;

function FreeCredentialsHandle (phCredential: PCredHandle): cardinal; stdcall;

function InitializeSecurityContext(phCredential: PCredHandle;
    phContext: PCtxtHandle; pszTargetName: PAnsiChar; fContextReq: cardinal;
    Reserved1: cardinal; TargetDataRep: cardinal; pInput: PSecBufferDesc;
    Reserved2: cardinal; phNewContext: PCtxtHandle; pOutput: PSecBufferDesc;
    pfContextAttr: PCardinal; ptsExpiry: PTimeStamp): cardinal; stdcall;

function AcceptSecurityContext(phCredential: PCredHandle;
    phContext: PCtxtHandle; pInput: PSecBufferDesc; fContextReq: cardinal;
    TargetDataRep: cardinal; phNewContext: PCtxtHandle;
    pOutput: PSecBufferDesc; pfContextAttr: PCardinal;
    ptsTimeStamp: PTimeStamp): cardinal; stdcall;

function DeleteSecurityContext(phContext: PCtxtHandle): cardinal; stdcall;

function ApplyControlToken(phContext: PCtxtHandle;
    pInput: PSecBufferDesc): cardinal; stdcall;

function QueryContextAttributes(phContext: PCtxtHandle;
    ulAttribute: cardinal; pBuffer: pointer): cardinal; stdcall;

function FreeContextBuffer(pvContextBuffer: pointer): cardinal; stdcall;

function EncryptMessage(phContext: PCtxtHandle; fQOP: cardinal;
    pMessage: PSecBufferDesc; MessageSeqNo: cardinal): cardinal; stdcall;

function DecryptMessage(phContext: PCtxtHandle; pMessage: PSecBufferDesc;
    MessageSeqNo: cardinal; pfQOP: PCardinal): cardinal; stdcall;

const
  SP_PROT_TLS1          = $0C0;
  SP_PROT_TLS1_SERVER   = $040;
  SP_PROT_TLS1_CLIENT   = $080;
  SP_PROT_TLS1_1        = $300;
  SP_PROT_TLS1_1_SERVER = $100;
  SP_PROT_TLS1_1_CLIENT = $200;
  SP_PROT_TLS1_2        = $C00;
  SP_PROT_TLS1_2_SERVER = $400;
  SP_PROT_TLS1_2_CLIENT = $800;

  SECPKG_CRED_INBOUND  = 1;
  SECPKG_CRED_OUTBOUND = 2;

  ISC_REQ_DELEGATE               = $00000001;
  ISC_REQ_MUTUAL_AUTH            = $00000002;
  ISC_REQ_REPLAY_DETECT          = $00000004;
  ISC_REQ_SEQUENCE_DETECT        = $00000008;
  ISC_REQ_CONFIDENTIALITY        = $00000010;
  ISC_REQ_USE_SESSION_KEY        = $00000020;
  ISC_REQ_PROMPT_FOR_CREDS       = $00000040;
  ISC_REQ_USE_SUPPLIED_CREDS     = $00000080;
  ISC_REQ_ALLOCATE_MEMORY        = $00000100;
  ISC_REQ_USE_DCE_STYLE          = $00000200;
  ISC_REQ_DATAGRAM               = $00000400;
  ISC_REQ_CONNECTION             = $00000800;
  ISC_REQ_CALL_LEVEL             = $00001000;
  ISC_REQ_FRAGMENT_SUPPLIED      = $00002000;
  ISC_REQ_EXTENDED_ERROR         = $00004000;
  ISC_REQ_STREAM                 = $00008000;
  ISC_REQ_INTEGRITY              = $00010000;
  ISC_REQ_IDENTIFY               = $00020000;
  ISC_REQ_NULL_SESSION           = $00040000;
  ISC_REQ_MANUAL_CRED_VALIDATION = $00080000;
  ISC_REQ_RESERVED1              = $00100000;
  ISC_REQ_FRAGMENT_TO_FIT        = $00200000;

  ASC_REQ_DELEGATE                = $00000001;
  ASC_REQ_MUTUAL_AUTH             = $00000002;
  ASC_REQ_REPLAY_DETECT           = $00000004;
  ASC_REQ_SEQUENCE_DETECT         = $00000008;
  ASC_REQ_CONFIDENTIALITY         = $00000010;
  ASC_REQ_USE_SESSION_KEY         = $00000020;
  ASC_REQ_ALLOCATE_MEMORY         = $00000100;
  ASC_REQ_USE_DCE_STYLE           = $00000200;
  ASC_REQ_DATAGRAM                = $00000400;
  ASC_REQ_CONNECTION              = $00000800;
  ASC_REQ_CALL_LEVEL              = $00001000;
  ASC_REQ_EXTENDED_ERROR          = $00008000;
  ASC_REQ_STREAM                  = $00010000;
  ASC_REQ_INTEGRITY               = $00020000;
  ASC_REQ_LICENSING               = $00040000;
  ASC_REQ_IDENTIFY                = $00080000;
  ASC_REQ_ALLOW_NULL_SESSION      = $00100000;
  ASC_REQ_ALLOW_NON_USER_LOGONS   = $00200000;
  ASC_REQ_ALLOW_CONTEXT_REPLAY    = $00400000;
  ASC_REQ_FRAGMENT_TO_FIT         = $00800000;
  ASC_REQ_FRAGMENT_SUPPLIED       = $00002000;
  ASC_REQ_NO_TOKEN                = $01000000;
  ASC_REQ_HTTP                    = $10000000;

  SECURITY_NATIVE_DREP = $10;

  SECBUFFER_VERSION = 0;
  SECBUFFER_EMPTY   = 0;
  SECBUFFER_DATA    = 1;
  SECBUFFER_TOKEN   = 2;
  SECBUFFER_EXTRA   = 5;
  SECBUFFER_STREAM_TRAILER = 6;
  SECBUFFER_STREAM_HEADER  = 7;

  SEC_E_OK = 0;
  SEC_I_CONTINUE_NEEDED        = $00090312;
  SEC_E_INCOMPLETE_MESSAGE     = $80090318;

  SECPKG_ATTR_NAMES        = 1;
  SECPKG_ATTR_STREAM_SIZES = 4;

  SCHANNEL_SHUTDOWN = 1;

type
  TXxmSSPICache=class(TObject)
  private
    FLock:TRTLCriticalSection;
    FData:array of record
      ConnectionID:UInt64;
      Package:AnsiString;
      Cred:TCredHandle;
      Ctxt:TCtxtHandle;
    end;
    FDataSize,FDataIndex:integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure GetContext(ConnectionID:UInt64;const Package:AnsiString;
      var Cred:PCredHandle;var Ctxt:PCtxtHandle);
    procedure Clear(ConnectionID:UInt64);
  end;

var
  SSPICache:TXxmSSPICache;

implementation

uses SysUtils;

const
  SecurityDLL='secur32.dll'; //'sspicli.dll'

function AcquireCredentialsHandle; external SecurityDLL name 'AcquireCredentialsHandleA';
function FreeCredentialsHandle; external SecurityDLL name 'FreeCredentialsHandle';
function InitializeSecurityContext; external SecurityDLL name 'InitializeSecurityContextA';
function AcceptSecurityContext; external SecurityDLL name 'AcceptSecurityContext';
function DeleteSecurityContext; external SecurityDLL name 'DeleteSecurityContext';
function ApplyControlToken; external SecurityDLL name 'ApplyControlToken';
function QueryContextAttributes; external SecurityDLL name 'QueryContextAttributesA';
function FreeContextBuffer; external SecurityDLL name 'FreeContextBuffer';
function EncryptMessage; external SecurityDLL name 'EncryptMessage';
function DecryptMessage; external SecurityDLL name 'DecryptMessage';

{ TXxmSSPICache }

constructor TXxmSSPICache.Create;
begin
  inherited Create;
  InitializeCriticalSection(FLock);
  FDataSize:=0;
  FDataIndex:=0;
end;

destructor TXxmSSPICache.Destroy;
begin
  DeleteCriticalSection(FLock);
  inherited;
end;

procedure TXxmSSPICache.GetContext(ConnectionID:UInt64;
  const Package:AnsiString;var Cred:PCredHandle;var Ctxt:PCtxtHandle);
var
  i:integer;
begin
  EnterCriticalSection(FLock);
  try
    //TODO: more performant lookup algo?
    i:=0;
    while (i<FDataIndex) and not((FData[i].ConnectionID=ConnectionID) and
      (FData[i].Package=Package)) do inc(i);
    if i=FDataIndex then
     begin
      //not found: add, first find a free spot
      i:=0;
      while (i<FDataIndex) and (FData[i].ConnectionID<>0) do inc(i);
      if i=FDataIndex then
       begin
        //current data full? grow
        if i=FDataSize then
         begin
          inc(FDataSize,$100);//growstep
          SetLength(FData,FDataSize);
         end;
        inc(FDataIndex);
       end;
      FData[i].ConnectionID:=ConnectionID;
      FData[i].Package:=Package;
      if AcquireCredentialsHandle(nil,PAnsiChar(Package),SECPKG_CRED_INBOUND,
        nil,nil,nil,nil,@FData[i].Cred,nil)<>0 then RaiseLastOSError;
      FData[i].Ctxt.dwLower:=nil;
      FData[i].Ctxt.dwUpper:=nil;
     end;
    Cred:=@FData[i].Cred;
    Ctxt:=@FData[i].Ctxt;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TXxmSSPICache.Clear(ConnectionID: UInt64);
var
  i:integer;
begin
  EnterCriticalSection(FLock);
  try
    //TODO: more performant lookup algo?
    i:=0;
    while (i<FDataIndex) and (FData[i].ConnectionID<>ConnectionID) do inc(i);
    if i<FDataIndex then
     begin
      FData[i].ConnectionID:=0;
      FData[i].Package:='';
      DeleteSecurityContext(@FData[i].Ctxt);
      FreeCredentialsHandle(@FData[i].Cred);
      FData[i].Cred.dwLower:=nil;
      FData[i].Cred.dwUpper:=nil;
      FData[i].Ctxt.dwLower:=nil;
      FData[i].Ctxt.dwUpper:=nil;
     end;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

initialization
  SSPICache:=TXxmSSPICache.Create;
finalization
  SSPICache.Free;
end.
