unit httpapi1;

interface

uses Windows, WinSock;

{$A8}
{$Z4}

type
  USHORT = Word;//?
  ULONGLONG = UInt64;
  PWSTR = PWideChar;
  PCWSTR = PWideChar;
  PCSTR = PAnsiChar;//??
  PVOID = pointer;

const
  HTTP_INITIALIZE_SERVER = 1;
  HTTP_INITIALIZE_CONFIG = 2;

  HTTP_MAX_SERVER_QUEUE_LENGTH = $7FFFFFFF;
  HTTP_MIN_SERVER_QUEUE_LENGTH = 1;

  HTTP_MIN_ALLOWED_BANDWIDTH_THROTTLING_RATE {:ULONG} =1024;
  HTTP_LIMIT_INFINITE {:ULONG} =-1;

  HTTP_AUTH_ENABLE_BASIC          =$00000001;
  HTTP_AUTH_ENABLE_DIGEST         =$00000002;
  HTTP_AUTH_ENABLE_NTLM           =$00000004;
  HTTP_AUTH_ENABLE_NEGOTIATE      =$00000008;
  HTTP_AUTH_ENABLE_ALL            =$0000000F;

  HTTP_LOG_FIELD_DATE                = $00000001;
  HTTP_LOG_FIELD_TIME                = $00000002;
  HTTP_LOG_FIELD_CLIENT_IP           = $00000004;
  HTTP_LOG_FIELD_USER_NAME           = $00000008;
  HTTP_LOG_FIELD_SITE_NAME           = $00000010;
  HTTP_LOG_FIELD_COMPUTER_NAME       = $00000020;
  HTTP_LOG_FIELD_SERVER_IP           = $00000040;
  HTTP_LOG_FIELD_METHOD              = $00000080;
  HTTP_LOG_FIELD_URI_STEM            = $00000100;
  HTTP_LOG_FIELD_URI_QUERY           = $00000200;
  HTTP_LOG_FIELD_STATUS              = $00000400;
  HTTP_LOG_FIELD_WIN32_STATUS        = $00000800;
  HTTP_LOG_FIELD_BYTES_SENT          = $00001000;
  HTTP_LOG_FIELD_BYTES_RECV          = $00002000;
  HTTP_LOG_FIELD_TIME_TAKEN          = $00004000;
  HTTP_LOG_FIELD_SERVER_PORT         = $00008000;
  HTTP_LOG_FIELD_USER_AGENT          = $00010000;
  HTTP_LOG_FIELD_COOKIE              = $00020000;
  HTTP_LOG_FIELD_REFERER             = $00040000;
  HTTP_LOG_FIELD_VERSION             = $00080000;
  HTTP_LOG_FIELD_HOST                = $00100000;
  HTTP_LOG_FIELD_SUB_STATUS          = $00200000;

  HTTP_LOG_FIELD_CLIENT_PORT         = $00400000;
  HTTP_LOG_FIELD_URI                 = $00800000;
  HTTP_LOG_FIELD_SITE_ID             = $01000000;
  HTTP_LOG_FIELD_REASON              = $02000000;
  HTTP_LOG_FIELD_QUEUE_NAME          = $04000000;

  HTTP_CREATE_REQUEST_QUEUE_FLAG_OPEN_EXISTING = 1;
  HTTP_CREATE_REQUEST_QUEUE_FLAG_CONTROLLER = 2;

  HTTP_RECEIVE_REQUEST_FLAG_COPY_BODY = 1;
  HTTP_RECEIVE_REQUEST_FLAG_FLUSH_BODY = 2;

  HTTP_RECEIVE_REQUEST_ENTITY_BODY_FLAG_FILL_BUFFER = 1;

  HTTP_SEND_RESPONSE_FLAG_DISCONNECT        = $00000001;
  HTTP_SEND_RESPONSE_FLAG_MORE_DATA         = $00000002;
  HTTP_SEND_RESPONSE_FLAG_BUFFER_DATA       = $00000004;
  HTTP_SEND_RESPONSE_FLAG_ENABLE_NAGLING    = $00000008;

  HTTP_FLUSH_RESPONSE_FLAG_RECURSIVE = 1;

type
  THTTP_REQUEST_ID = ULONGLONG;
  PHTTP_REQUEST_ID=^THTTP_REQUEST_ID;
  THTTP_CONNECTION_ID = ULONGLONG;
  PHTTP_CONNECTION_ID=^THTTP_CONNECTION_ID;
  THTTP_RAW_CONNECTION_ID = ULONGLONG;
  PHTTP_RAW_CONNECTION_ID=^THTTP_RAW_CONNECTION_ID;
  THTTP_URL_GROUP_ID = ULONGLONG;
  PHTTP_URL_GROUP_ID=^THTTP_URL_GROUP_ID;
  THTTP_SERVER_SESSION_ID = ULONGLONG;
  PHTTP_SERVER_SESSION_ID=^THTTP_SERVER_SESSION_ID;

const
  HTTP_NULL_ID {:UINT64} = 0;
  HTTP_BYTE_RANGE_TO_EOF {:UINT64} = -1;

type
  THTTP_BYTE_RANGE = record
    StartingOffset: UInt64;
    Length: UInt64;
  end;
  PHTTP_BYTE_RANGE=^THTTP_BYTE_RANGE;

  THTTP_VERSION = record
    MajorVersion: USHORT;
    MinorVersion: USHORT;
  end;
  PHTTP_VERSION=^THTTP_VERSION;

const
  HTTP_VERSION_UNKNOWN:THTTP_VERSION=(MajorVersion:0;MinorVersion:0);
  HTTP_VERSION_0_9:THTTP_VERSION=(MajorVersion:0;MinorVersion:9);
  HTTP_VERSION_1_0:THTTP_VERSION=(MajorVersion:1;MinorVersion:0);
  HTTP_VERSION_1_1:THTTP_VERSION=(MajorVersion:1;MinorVersion:1);

type
  THTTP_VERB=(
    HttpVerbUnparsed,
    HttpVerbUnknown,
    HttpVerbInvalid,
    HttpVerbOPTIONS,
    HttpVerbGET,
    HttpVerbHEAD,
    HttpVerbPOST,
    HttpVerbPUT,
    HttpVerbDELETE,
    HttpVerbTRACE,
    HttpVerbCONNECT,
    HttpVerbTRACK,
    HttpVerbMOVE,
    HttpVerbCOPY,
    HttpVerbPROPFIND,
    HttpVerbPROPPATCH,
    HttpVerbMKCOL,
    HttpVerbLOCK,
    HttpVerbUNLOCK,
    HttpVerbSEARCH,
    HttpVerbMaximum
  );
  PHTTP_VERB=^THTTP_VERB;

  THTTP_HEADER_ID=(
    HttpHeaderStart = 0,//used only for array declaration

    HttpHeaderCacheControl          = 0,   
    HttpHeaderConnection            = 1,   
    HttpHeaderDate                  = 2,   
    HttpHeaderKeepAlive             = 3,   
    HttpHeaderPragma                = 4,   
    HttpHeaderTrailer               = 5,   
    HttpHeaderTransferEncoding      = 6,   
    HttpHeaderUpgrade               = 7,   
    HttpHeaderVia                   = 8,   
    HttpHeaderWarning               = 9,   

    HttpHeaderAllow                 = 10,  
    HttpHeaderContentLength         = 11,  
    HttpHeaderContentType           = 12,  
    HttpHeaderContentEncoding       = 13,  
    HttpHeaderContentLanguage       = 14,  
    HttpHeaderContentLocation       = 15,  
    HttpHeaderContentMd5            = 16,  
    HttpHeaderContentRange          = 17,  
    HttpHeaderExpires               = 18,  
    HttpHeaderLastModified          = 19,  

    HttpHeaderAccept                = 20,  
    HttpHeaderAcceptCharset         = 21,  
    HttpHeaderAcceptEncoding        = 22,  
    HttpHeaderAcceptLanguage        = 23,  
    HttpHeaderAuthorization         = 24,  
    HttpHeaderCookie                = 25,  
    HttpHeaderExpect                = 26,  
    HttpHeaderFrom                  = 27,
    HttpHeaderHost                  = 28,  
    HttpHeaderIfMatch               = 29,  

    HttpHeaderIfModifiedSince       = 30,  
    HttpHeaderIfNoneMatch           = 31,
    HttpHeaderIfRange               = 32,  
    HttpHeaderIfUnmodifiedSince     = 33,  
    HttpHeaderMaxForwards           = 34,  
    HttpHeaderProxyAuthorization    = 35,  
    HttpHeaderReferer               = 36,  
    HttpHeaderRange                 = 37,  
    HttpHeaderTe                    = 38,  
    HttpHeaderTranslate             = 39,  

    HttpHeaderUserAgent             = 40,  

    HttpHeaderRequestMaximum        = 41-1,//used only for array declaration

    HttpHeaderAcceptRanges          = 20,
    HttpHeaderAge                   = 21,
    HttpHeaderEtag                  = 22,
    HttpHeaderLocation              = 23,
    HttpHeaderProxyAuthenticate     = 24,
    HttpHeaderRetryAfter            = 25,
    HttpHeaderServer                = 26,
    HttpHeaderSetCookie             = 27,
    HttpHeaderVary                  = 28,
    HttpHeaderWwwAuthenticate       = 29,

    HttpHeaderResponseMaximum       = 30-1,//used only for array declaration

    HttpHeaderMaximum               = 41-1//used only for array declaration
  );
  PHTTP_HEADER_ID=^THTTP_HEADER_ID;

  THTTP_KNOWN_HEADER = record
    RawValueLength: USHORT;
    pRawValue: PCSTR;
  end;
  PHTTP_KNOWN_HEADER=^THTTP_KNOWN_HEADER;

  THTTP_UNKNOWN_HEADER = record
    NameLength: USHORT;
    RawValueLength: USHORT;
    pName: PCSTR;
    pRawValue: PCSTR;
  end;
  PHTTP_UNKNOWN_HEADER=^THTTP_UNKNOWN_HEADER;

  THTTP_DATA_CHUNK_TYPE=(
    HttpDataChunkFromMemory,
    HttpDataChunkFromFileHandle,
    HttpDataChunkFromFragmentCache,
    HttpDataChunkFromFragmentCacheEx,
    HttpDataChunkMaximum
  );
  PHTTP_DATA_CHUNK_TYPE=^THTTP_DATA_CHUNK_TYPE;

  THTTP_DATA_CHUNK = packed record //packed and extra members to correct alignment
    case DataChunkType: THTTP_DATA_CHUNK_TYPE of
      HttpDataChunkFromMemory:(
        xxAlign1: PVOID;
        pBuffer: PVOID;
        BufferLength: ULONG;
      );
      HttpDataChunkFromFileHandle:(
        xxAlign2: PVOID;
        ByteRange: THTTP_BYTE_RANGE;
        FileHandle: THandle;
        xxAlign3: PVOID;
      );
      HttpDataChunkFromFragmentCache:(
        xxAlign4: PVOID;
        FragmentNameLength: USHORT;
        xxAlign5: USHORT;
        pFragmentName: PCWSTR;
      );
  end;
  PHTTP_DATA_CHUNK=^THTTP_DATA_CHUNK;

  THTTP_REQUEST_HEADERS = record
    UnknownHeaderCount: USHORT;
    pUnknownHeaders: PHTTP_UNKNOWN_HEADER;
    TrailerCount: USHORT;
    pTrailers: PHTTP_UNKNOWN_HEADER;
    KnownHeaders:array[HttpHeaderStart..HttpHeaderRequestMaximum] of THTTP_KNOWN_HEADER;
  end;
  PHTTP_REQUEST_HEADERS=^THTTP_REQUEST_HEADERS;

  THTTP_RESPONSE_HEADERS = record
    UnknownHeaderCount: USHORT;
    pUnknownHeaders: PHTTP_UNKNOWN_HEADER;
    TrailerCount: USHORT;
    pTrailers: PHTTP_UNKNOWN_HEADER;
    KnownHeaders:array[HttpHeaderStart..HttpHeaderResponseMaximum] of THTTP_KNOWN_HEADER;
  end;
  PHTTP_RESPONSE_HEADERS=^THTTP_RESPONSE_HEADERS;

  THTTP_TRANSPORT_ADDRESS = record
    pRemoteAddress: PSOCKADDR;
    pLocalAddress: PSOCKADDR;      
  end;
  PHTTP_TRANSPORT_ADDRESS=^THTTP_TRANSPORT_ADDRESS;

  THTTP_COOKED_URL = record
    FullUrlLength: USHORT;
    HostLength: USHORT;
    AbsPathLength: USHORT;
    QueryStringLength: USHORT;
    pFullUrl: PCWSTR;
    pHost: PCWSTR;
    pAbsPath: PCWSTR;
    pQueryString: PCWSTR;
  end;
  PHTTP_COOKED_URL=^THTTP_COOKED_URL;

  THTTP_URL_CONTEXT = ULONGLONG;

const
  HTTP_URL_FLAG_REMOVE_ALL = 1;

type
  THTTP_SSL_CLIENT_CERT_INFO = record
    CertFlags: ULONG;
    CertEncodedSize: ULONG;
    pCertEncoded: PUCHAR;
    Token: THandle;
    CertDeniedByMapper: BOOL;
  end;
  PHTTP_SSL_CLIENT_CERT_INFO=^THTTP_SSL_CLIENT_CERT_INFO;

  THTTP_SSL_INFO = record
    ServerCertKeySize: USHORT;
    ConnectionKeySize: USHORT;
    ServerCertIssuerSize: ULONG;
    ServerCertSubjectSize: ULONG;
    pServerCertIssuer: PCSTR;
    pServerCertSubject: PCSTR;
    pClientCertInfo: PHTTP_SSL_CLIENT_CERT_INFO;
    SslClientCertNegotiated: ULONG;
  end;
  PHTTP_SSL_INFO=^THTTP_SSL_INFO;

  TSECURITY_STATUS = LongInt;//LONG;

  THTTP_REQUEST_V1 = record
    Flags: ULONG;
    ConnectionId: THTTP_CONNECTION_ID;
    RequestId: THTTP_REQUEST_ID;
    UrlContext: THTTP_URL_CONTEXT;
    Version: THTTP_VERSION;
    Verb: THTTP_VERB;
    UnknownVerbLength: USHORT;
    RawUrlLength: USHORT;
    pUnknownVerb: PCSTR;
    pRawUrl: PCSTR;
    CookedUrl: THTTP_COOKED_URL;
    Address: THTTP_TRANSPORT_ADDRESS;
    Headers: THTTP_REQUEST_HEADERS;
    BytesReceived: ULONGLONG;
    EntityChunkCount: USHORT;
    pEntityChunks: PHTTP_DATA_CHUNK;
    RawConnectionId: THTTP_RAW_CONNECTION_ID;
    pSslInfo: PHTTP_SSL_INFO;         
  end;
  PHTTP_REQUEST_V1=^THTTP_REQUEST_V1;

  THTTP_REQUEST=THTTP_REQUEST_V1;
  PHTTP_REQUEST=^THTTP_REQUEST;

const
  HTTP_REQUEST_FLAG_MORE_ENTITY_BODY_EXISTS = 1;
  HTTP_REQUEST_FLAG_IP_ROUTED = 2;

type
  THTTP_RESPONSE_V1 = record
    Flags: ULONG;
    Version: THTTP_VERSION;
    StatusCode: USHORT;
    ReasonLength: USHORT;
    pReason: PCSTR;
    Headers: THTTP_RESPONSE_HEADERS;
    EntityChunkCount: USHORT;
    pEntityChunks: PHTTP_DATA_CHUNK;
  end;
  PHTTP_RESPONSE_V1=^THTTP_RESPONSE_V1;

const
  HTTP_RESPONSE_INFO_FLAGS_PRESERVE_ORDER = 1;

type
  THTTP_RESPONSE=THTTP_RESPONSE_V1;
  PHTTP_RESPONSE=^THTTP_RESPONSE;

  THTTPAPI_VERSION = record
    HttpApiMajorVersion: USHORT;
    HttpApiMinorVersion: USHORT;
  end;
  PHTTPAPI_VERSION=^THTTPAPI_VERSION;

const
  //IFDEF WIN32_WINNT?
  HTTPAPI_VERSION_1_0:THTTPAPI_VERSION=(HttpApiMajorVersion:1;HttpApiMinorVersion:0);

type
  THTTP_CACHE_POLICY_TYPE=(
    HttpCachePolicyNocache,
    HttpCachePolicyUserInvalidates,
    HttpCachePolicyTimeToLive,
    HttpCachePolicyMaximum
  );
  PHTTP_CACHE_POLICY_TYPE=^THTTP_CACHE_POLICY_TYPE;

  THTTP_CACHE_POLICY = record
    Policy: THTTP_CACHE_POLICY_TYPE;
    SecondsToLive: ULONG;                   
  end;
  PHTTP_CACHE_POLICY=^THTTP_CACHE_POLICY;

  THTTP_SERVICE_CONFIG_ID=(
    HttpServiceConfigIPListenList,
    HttpServiceConfigSSLCertInfo,
    HttpServiceConfigUrlAclInfo,
    HttpServiceConfigTimeout,       
    HttpServiceConfigMax
  );
  PHTTP_SERVICE_CONFIG_ID=^THTTP_SERVICE_CONFIG_ID;

  THTTP_SERVICE_CONFIG_QUERY_TYPE=(
    HttpServiceConfigQueryExact,
    HttpServiceConfigQueryNext,
    HttpServiceConfigQueryMax
  );
  PHTTP_SERVICE_CONFIG_QUERY_TYPE=^THTTP_SERVICE_CONFIG_QUERY_TYPE;

  THTTP_SERVICE_CONFIG_SSL_KEY = record
    pIpPort: PSOCKADDR;
  end;
  PHTTP_SERVICE_CONFIG_SSL_KEY=^THTTP_SERVICE_CONFIG_SSL_KEY;

  THTTP_SERVICE_CONFIG_SSL_PARAM = record
    SslHashLength: ULONG;
    pSslHash: PVOID;
    AppId: TGUID;
    pSslCertStoreName: PWSTR;
    DefaultCertCheckMode: DWORD;
    DefaultRevocationFreshnessTime: DWORD;
    DefaultRevocationUrlRetrievalTimeout: DWORD;
    pDefaultSslCtlIdentifier: PWSTR;
    pDefaultSslCtlStoreName: PWSTR;
    DefaultFlags: DWORD;
  end;
  PHTTP_SERVICE_CONFIG_SSL_PARAM=^THTTP_SERVICE_CONFIG_SSL_PARAM;

const
  HTTP_SERVICE_CONFIG_SSL_FLAG_USE_DS_MAPPER = 1;
  HTTP_SERVICE_CONFIG_SSL_FLAG_NEGOTIATE_CLIENT_CERT = 2;
  HTTP_SERVICE_CONFIG_SSL_FLAG_NO_RAW_FILTER = 4;

type
  THTTP_SERVICE_CONFIG_SSL_SET = record
    KeyDesc: THTTP_SERVICE_CONFIG_SSL_KEY;
    ParamDesc: THTTP_SERVICE_CONFIG_SSL_PARAM;
  end;
  PHTTP_SERVICE_CONFIG_SSL_SET=^THTTP_SERVICE_CONFIG_SSL_SET;

  THTTP_SERVICE_CONFIG_SSL_QUERY = record
    QueryDesc: THTTP_SERVICE_CONFIG_QUERY_TYPE;
    KeyDesc: THTTP_SERVICE_CONFIG_SSL_KEY;
    dwToken: DWORD;                           
  end;
  PHTTP_SERVICE_CONFIG_SSL_QUERY=^THTTP_SERVICE_CONFIG_SSL_QUERY;

  THTTP_SERVICE_CONFIG_IP_LISTEN_PARAM = record
    AddrLength: USHORT;
    pAddress: PSOCKADDR;   
  end;
  PHTTP_SERVICE_CONFIG_IP_LISTEN_PARAM=^THTTP_SERVICE_CONFIG_IP_LISTEN_PARAM;

  THTTP_SERVICE_CONFIG_IP_LISTEN_QUERY = record
    AddrCount: ULONG;
    AddrList: array of TSockAddr;//???SOCKADDR_STORAGE;//[ANYSIZE_ARRAY]?
  end;
  PHTTP_SERVICE_CONFIG_IP_LISTEN_QUERY=^THTTP_SERVICE_CONFIG_IP_LISTEN_QUERY;

  THTTP_SERVICE_CONFIG_URLACL_KEY = record
    pUrlPrefix: PWSTR;
  end;
  PHTTP_SERVICE_CONFIG_URLACL_KEY=^THTTP_SERVICE_CONFIG_URLACL_KEY;

  THTTP_SERVICE_CONFIG_URLACL_PARAM = record
    pStringSecurityDescriptor: PWSTR;
  end;
  PHTTP_SERVICE_CONFIG_URLACL_PARAM=^THTTP_SERVICE_CONFIG_URLACL_PARAM;

  THTTP_SERVICE_CONFIG_URLACL_SET = record
    KeyDesc: THTTP_SERVICE_CONFIG_URLACL_KEY;
    ParamDesc: THTTP_SERVICE_CONFIG_URLACL_PARAM; 
  end;
  PHTTP_SERVICE_CONFIG_URLACL_SET=^THTTP_SERVICE_CONFIG_URLACL_SET;

  THTTP_SERVICE_CONFIG_URLACL_QUERY = record
    QueryDesc: THTTP_SERVICE_CONFIG_QUERY_TYPE;
    KeyDesc: THTTP_SERVICE_CONFIG_URLACL_KEY;
    dwToken: DWORD;                           
  end;
  PHTTP_SERVICE_CONFIG_URLACL_QUERY=^THTTP_SERVICE_CONFIG_URLACL_QUERY;

function HttpInitialize(Version: THTTPAPI_VERSION; Flags: ULONG; pReserved: PVOID): ULONG; stdcall;
function HttpTerminate(Flags: ULONG; pReserved: PVOID): ULONG; stdcall;
function HttpCreateHttpHandle(var pReqQueueHandle: THandle; Reserved: ULONG): ULONG; stdcall;
function HttpReceiveClientCertificate(ReqQueueHandle: THandle; ConnectionId: THTTP_CONNECTION_ID; Flags: ULONG;
  var pSslClientCertInfo: THTTP_SSL_CLIENT_CERT_INFO; SslClientCertInfoSize: ULONG; var pBytesReceived: ULONG;
  pOverlapped: POverlapped): ULONG; stdcall;
function HttpAddUrl(ReqQueueHandle: THandle; pFullyQualifiedUrl: PCWSTR; pReserved: PVOID): ULONG; stdcall;
function HttpRemoveUrl(ReqQueueHandle: THandle; pFullyQualifiedUrl: PCWSTR): ULONG; stdcall;
function HttpReceiveHttpRequest(ReqQueueHandle: THandle; RequestId: THTTP_REQUEST_ID; Flags: ULONG;
  pRequestBuffer: PHTTP_REQUEST; RequestBufferLength: ULONG; var pBytesReceived: ULONG;
  pOverlapped: POverlapped): ULONG; stdcall;
function HttpReceiveRequestEntityBody(ReqQueueHandle: THandle; RequestId: THTTP_REQUEST_ID; Flags: ULONG;
  pBuffer: PVOID; BufferLength: ULONG; var pBytesReceived: ULONG; pOverlapped: POverlapped): ULONG; stdcall;
function HttpSendHttpResponse(ReqQueueHandle: THandle; RequestId: THTTP_REQUEST_ID; Flags: ULONG;
  pHttpResponse: PHTTP_RESPONSE; pReserved1: PVOID; var pBytesSend: ULONG; pReserved2: PVOID; Reserved3: ULONG;
  pOverlapped: POverlapped; pReserved4: PVOID): ULONG; stdcall;
function HttpSendResponseEntityBody(ReqQueueHandle: THandle; RequestId: THTTP_REQUEST_ID; Flags: ULONG;
  EntityChunkCount: USHORT; pEntityChunks: PHTTP_DATA_CHUNK; var pBytesSent: ULONG; pReserved1: PVOID; Reserved2: ULONG;
  pOverlapped: POverlapped; pReserved3: PVOID): ULONG; stdcall;
function HttpWaitForDisconnect(ReqQueueHandle: THandle; ConnectionId: THTTP_CONNECTION_ID;
  pOverlapped: POverlapped): ULONG; stdcall;
function HttpCancelHttpRequest(ReqQueueHandle: THandle; RequestId: THTTP_REQUEST_ID;
  pOverlapped: POverlapped): ULONG; stdcall;
function HttpFlushResponseCache(ReqQueueHandle: THandle; pUrlPrefix: PCWSTR; Flags: ULONG;
  pOverlapped: POverlapped): ULONG; stdcall;
function HttpAddFragmentToCache(ReqQueueHandle: THandle; pUrlPrefix: PCWSTR; pDataChunk: PHTTP_DATA_CHUNK;
  pCachePolicy: PHTTP_CACHE_POLICY; pOverlapped: POverlapped): ULONG; stdcall;
function HttpReadFragmentFromCache(ReqQueueHandle: THandle; pUrlPrefix: PCWSTR; pByteRange: PHTTP_BYTE_RANGE;
  pBuffer: PVOID; BufferLength: ULONG; var pBytesRead: ULONG; pOverlapped: POverlapped): ULONG; stdcall;
function HttpSetServiceConfiguration(ServiceHandle: THandle; ConfigId: THTTP_SERVICE_CONFIG_ID;
  pConfigInformation: PVOID; ConfigInformationLength: ULONG; pOverlapped: POverlapped): ULONG; stdcall;
function HttpDeleteServiceConfiguration(ServiceHandle: THandle; ConfigId: THTTP_SERVICE_CONFIG_ID;
  pConfigInformation: PVOID; ConfigInformationLength: ULONG; pOverlapped: POverlapped): ULONG; stdcall;
function HttpQueryServiceConfiguration(ServiceHandle: THandle; ConfigId: THTTP_SERVICE_CONFIG_ID;
  pInputConfigInformation: PVOID; InputConfigInformationLength: ULONG;
  pOutputConfigInformation: PVOID; OutputConfigInformationLength: ULONG; var pReturnLength: ULONG;
  pOverlapped: POverlapped): ULONG; stdcall;

procedure HttpCheck(HttpCallResult: ULONG);//drop this and below HttpCheck section if you want to avoid using SysUtils

implementation

{HttpCheck begin}
uses SysUtils;//only needed for HttpCheck

type
  EHttpApiException=class(Exception);

procedure HttpCheck(HttpCallResult: ULONG);
begin
  if HttpCallResult<>NO_ERROR then
    raise EHttpApiException.Create('httpapi: '+SysErrorMessage(HttpCallResult));
end;
{HttpCheck end}

const
  httpapidll='httpapi.dll';

function HttpInitialize; external httpapidll;
function HttpTerminate; external httpapidll;
function HttpCreateHttpHandle; external httpapidll;
function HttpReceiveClientCertificate; external httpapidll;
function HttpAddUrl; external httpapidll;
function HttpRemoveUrl; external httpapidll;
function HttpReceiveHttpRequest; external httpapidll;
function HttpReceiveRequestEntityBody; external httpapidll;
function HttpSendHttpResponse; external httpapidll;
function HttpSendResponseEntityBody; external httpapidll;
function HttpWaitForDisconnect; external httpapidll;
function HttpCancelHttpRequest; external httpapidll;
function HttpFlushResponseCache; external httpapidll;
function HttpAddFragmentToCache; external httpapidll;
function HttpReadFragmentFromCache; external httpapidll;
function HttpSetServiceConfiguration; external httpapidll;
function HttpDeleteServiceConfiguration; external httpapidll;
function HttpQueryServiceConfiguration; external httpapidll;

end.
