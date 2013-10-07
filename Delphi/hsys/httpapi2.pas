unit httpapi2;

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

type
  THTTP_SERVER_PROPERTY=(
    HttpServerAuthenticationProperty,
    HttpServerLoggingProperty,
    HttpServerQosProperty,
    HttpServerTimeoutsProperty,
    HttpServerQueueLengthProperty,
    HttpServerStateProperty,
    HttpServer503VerbosityProperty,
    HttpServerBindingProperty,
    HttpServerExtendedAuthenticationProperty,
    HttpServerListenEndpointProperty,
    //WIN7 or newer
    HttpServerChannelBindProperty,
    HttpServerProtectionLevelProperty
  );
  PHTTP_SERVER_PROPERTY=^THTTP_SERVER_PROPERTY;

const
  HTTP_MAX_SERVER_QUEUE_LENGTH = $7FFFFFFF;
  HTTP_MIN_SERVER_QUEUE_LENGTH = 1;

//Delphi doesn't do bit-fields, use masks!
type
  THTTP_PROPERTY_FLAGS=type ULONG;
  PHTTP_PROPERTY_FLAGS=^THTTP_PROPERTY_FLAGS;
const
  HTTP_PROPERTY_FLAG_PRESENT=$00000001;

type
  THTTP_ENABLED_STATE=(
    HttpEnabledStateActive,
    HttpEnabledStateInactive
  );
  PHTTP_ENABLED_STATE=^THTTP_ENABLED_STATE;

  THTTP_STATE_INFO = record
    Flags: THTTP_PROPERTY_FLAGS;
    State: THTTP_ENABLED_STATE;
  end;
  PHTTP_STATE_INFO=^THTTP_STATE_INFO;

  THTTP_503_RESPONSE_VERBOSITY=(
    Http503ResponseVerbosityBasic,
    Http503ResponseVerbosityLimited,
    Http503ResponseVerbosityFull
  );
  PHTTP_503_RESPONSE_VERBOSITY=^THTTP_503_RESPONSE_VERBOSITY;

  THTTP_QOS_SETTING_TYPE=(
    HttpQosSettingTypeBandwidth,
    HttpQosSettingTypeConnectionLimit
  );
  PHTTP_QOS_SETTING_TYPE=^THTTP_QOS_SETTING_TYPE;

  THTTP_QOS_SETTING_INFO = record
    QosType: THTTP_QOS_SETTING_TYPE;
    QosSetting: PVOID;
  end;
  PHTTP_QOS_SETTING_INFO=^THTTP_QOS_SETTING_INFO;

  THTTP_CONNECTION_LIMIT_INFO = record
    Flags: THTTP_PROPERTY_FLAGS;
    MaxConnections: ULONG;
  end;
  PHTTP_CONNECTION_LIMIT_INFO=^THTTP_CONNECTION_LIMIT_INFO;

  THTTP_BANDWIDTH_LIMIT_INFO=record
    Flags: THTTP_PROPERTY_FLAGS;
    MaxBandwidth: ULONG;
  end;
  PHTTP_BANDWIDTH_LIMIT_INFO=^THTTP_BANDWIDTH_LIMIT_INFO;

const
  HTTP_MIN_ALLOWED_BANDWIDTH_THROTTLING_RATE {:ULONG} =1024;
  HTTP_LIMIT_INFINITE {:ULONG} =-1;

type
  THTTP_SERVICE_CONFIG_TIMEOUT_KEY=(
    IdleConnectionTimeout = 0,
    HeaderWaitTimeout
  );
  PHTTP_SERVICE_CONFIG_TIMEOUT_KEY=^THTTP_SERVICE_CONFIG_TIMEOUT_KEY;

  THTTP_SERVICE_CONFIG_TIMEOUT_PARAM=USHORT;
  PHTTP_SERVICE_CONFIG_TIMEOUT_PARAM=^THTTP_SERVICE_CONFIG_TIMEOUT_PARAM;

  THTTP_SERVICE_CONFIG_TIMEOUT_SET = record
    KeyDesc: THTTP_SERVICE_CONFIG_TIMEOUT_KEY;
    ParamDesc: THTTP_SERVICE_CONFIG_TIMEOUT_PARAM;
  end;
  PHTTP_SERVICE_CONFIG_TIMEOUT_SET=^THTTP_SERVICE_CONFIG_TIMEOUT_SET;

  THTTP_TIMEOUT_LIMIT_INFO = record
    Flags: THTTP_PROPERTY_FLAGS;
    EntityBody: USHORT;
    DrainEntityBody: USHORT;
    RequestQueue: USHORT;
    IdleConnection: USHORT;
    HeaderWait: USHORT;
    MinSendRate: ULONG;
  end;
  PHTTP_TIMEOUT_LIMIT_INFO=^THTTP_TIMEOUT_LIMIT_INFO;

  THTTP_LISTEN_ENDPOINT_INFO = record
    Flags: THTTP_PROPERTY_FLAGS;
    EnableSharing: BOOLEAN;
  end;
  PHTTP_LISTEN_ENDPOINT_INFO=^THTTP_LISTEN_ENDPOINT_INFO;

  THTTP_SERVER_AUTHENTICATION_DIGEST_PARAMS = record
    DomainNameLength: USHORT;
    DomainName: PWSTR;
    RealmLength: USHORT;
    Realm: PWSTR;
  end;
  PHTTP_SERVER_AUTHENTICATION_DIGEST_PARAMS=^THTTP_SERVER_AUTHENTICATION_DIGEST_PARAMS;

  THTTP_SERVER_AUTHENTICATION_BASIC_PARAMS = record
    RealmLength: USHORT;
    Realm: PWSTR;
  end;
  PHTTP_SERVER_AUTHENTICATION_BASIC_PARAMS=^THTTP_SERVER_AUTHENTICATION_BASIC_PARAMS;

const
  HTTP_AUTH_ENABLE_BASIC          =$00000001;
  HTTP_AUTH_ENABLE_DIGEST         =$00000002;
  HTTP_AUTH_ENABLE_NTLM           =$00000004;
  HTTP_AUTH_ENABLE_NEGOTIATE      =$00000008;
  HTTP_AUTH_ENABLE_ALL            =$0000000F;

type
  THTTP_SERVER_AUTHENTICATION_INFO = record
    Flags: THTTP_PROPERTY_FLAGS;
    AuthSchemes: ULONG;
    ReceiveMutualAuth: ByteBool;
    ReceiveContextHandle: ByteBool;
    DisableNTLMCredentialCaching: ByteBool;
    ExFlags: UCHAR;
    DigestParams: THTTP_SERVER_AUTHENTICATION_DIGEST_PARAMS;
    BasicParams: THTTP_SERVER_AUTHENTICATION_BASIC_PARAMS; 
  end;
  PHTTP_SERVER_AUTHENTICATION_INFO=^THTTP_SERVER_AUTHENTICATION_INFO;

  THTTP_SERVICE_BINDING_TYPE=(
    HttpServiceBindingTypeNone = 0,
    HttpServiceBindingTypeW,
    HttpServiceBindingTypeA
  );

  THTTP_SERVICE_BINDING_BASE = record
    BindingType: THTTP_SERVICE_BINDING_TYPE;
  end;
  PHTTP_SERVICE_BINDING_BASE=^THTTP_SERVICE_BINDING_BASE;

  THTTP_SERVICE_BINDING_A = record
    Base: THTTP_SERVICE_BINDING_BASE;
    Buffer: PAnsiChar;
    BufferSize: ULONG;
  end;
  PHTTP_SERVICE_BINDING_A=^THTTP_SERVICE_BINDING_A;

  THTTP_SERVICE_BINDING_W = record
    Base: THTTP_SERVICE_BINDING_BASE;
    Buffer: PWCHAR;
    BufferSize: ULONG;
  end;
  PHTTP_SERVICE_BINDING_W=^THTTP_SERVICE_BINDING_W;

  THTTP_AUTHENTICATION_HARDENING_LEVELS=(
    HttpAuthenticationHardeningLegacy = 0,
    HttpAuthenticationHardeningMedium,
    HttpAuthenticationHardeningStrict
  );

const
  HTTP_CHANNEL_BIND_PROXY =$1;
  HTTP_CHANNEL_BIND_PROXY_COHOSTING =$20;

  HTTP_CHANNEL_BIND_NO_SERVICE_NAME_CHECK =$2;
  HTTP_CHANNEL_BIND_DOTLESS_SERVICE =$4;

  HTTP_CHANNEL_BIND_SECURE_CHANNEL_TOKEN =$8;
  HTTP_CHANNEL_BIND_CLIENT_SERVICE =$10;

type
  THTTP_CHANNEL_BIND_INFO = record
    Hardening: THTTP_AUTHENTICATION_HARDENING_LEVELS;
    Flags: ULONG;
    ServiceNames: PHTTP_SERVICE_BINDING_BASE;
    NumberOfServiceNames: ULONG;
  end;
  PHTTP_CHANNEL_BIND_INFO=^THTTP_CHANNEL_BIND_INFO;

  THTTP_REQUEST_CHANNEL_BIND_STATUS = record
    ServiceName: PHTTP_SERVICE_BINDING_BASE;
    ChannelToken: PUCHAR;
    ChannelTokenSize: ULONG;
    Flags: ULONG;
  end;
  PHTTP_REQUEST_CHANNEL_BIND_STATUS=^THTTP_REQUEST_CHANNEL_BIND_STATUS;

const
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

type
  THTTP_LOGGING_TYPE=(
    HttpLoggingTypeW3C,
    HttpLoggingTypeIIS,
    HttpLoggingTypeNCSA,
    HttpLoggingTypeRaw
  );
  PHTTP_LOGGING_TYPE=^THTTP_LOGGING_TYPE;  

  THTTP_LOGGING_ROLLOVER_TYPE=(
    HttpLoggingRolloverSize,
    HttpLoggingRolloverDaily,
    HttpLoggingRolloverWeekly,
    HttpLoggingRolloverMonthly,
    HttpLoggingRolloverHourly
  );
  PHTTP_LOGGING_ROLLOVER_TYPE=^THTTP_LOGGING_ROLLOVER_TYPE;

const
  HTTP_MIN_ALLOWED_LOG_FILE_ROLLOVER_SIZE=$00100000;//1MB

  HTTP_LOGGING_FLAG_LOCAL_TIME_ROLLOVER         = $00000001;
  HTTP_LOGGING_FLAG_USE_UTF8_CONVERSION         = $00000002;
  HTTP_LOGGING_FLAG_LOG_ERRORS_ONLY             = $00000004;
  HTTP_LOGGING_FLAG_LOG_SUCCESS_ONLY            = $00000008;

type
  THTTP_LOGGING_INFO = record
    Flags: THTTP_PROPERTY_FLAGS;
    LoggingFlags: ULONG;
    SoftwareName: PCWSTR;
    SoftwareNameLength: USHORT;
    DirectoryNameLength: USHORT;
    DirectoryName: PCWSTR;
    Format: THTTP_LOGGING_TYPE;
    Fields: ULONG;
    pExtFields: PVOID;
    NumOfExtFields: USHORT;
    MaxRecordSize: USHORT;
    RolloverType: THTTP_LOGGING_ROLLOVER_TYPE;
    RolloverSize: ULONG;
    pSecurityDescriptor: PSECURITY_DESCRIPTOR;
  end;
  PHTTP_LOGGING_INFO=^THTTP_LOGGING_INFO;

  THTTP_BINDING_INFO = record
    Flags: THTTP_PROPERTY_FLAGS;
    RequestQueueHandle: THandle;
  end;
  PHTTP_BINDING_INFO=^THTTP_BINDING_INFO;

  THTTP_PROTECTION_LEVEL_TYPE=(
    HttpProtectionLevelUnrestricted,
    HttpProtectionLevelEdgeRestricted,
    HttpProtectionLevelRestricted
  );

  THTTP_PROTECTION_LEVEL_INFO = record
    Flags: THTTP_PROPERTY_FLAGS;
    Level: THTTP_PROTECTION_LEVEL_TYPE;
  end;
  PHTTP_PROTECTION_LEVEL_INFO=^THTTP_PROTECTION_LEVEL_INFO;

const
  HTTP_CREATE_REQUEST_QUEUE_FLAG_OPEN_EXISTING = 1;
  HTTP_CREATE_REQUEST_QUEUE_FLAG_CONTROLLER = 2;

  HTTP_RECEIVE_REQUEST_FLAG_COPY_BODY = 1;
  HTTP_RECEIVE_REQUEST_FLAG_FLUSH_BODY = 2;

  HTTP_RECEIVE_REQUEST_ENTITY_BODY_FLAG_FILL_BUFFER = 1;

  HTTP_SEND_RESPONSE_FLAG_DISCONNECT        = $00000001;
  HTTP_SEND_RESPONSE_FLAG_MORE_DATA         = $00000002;
  HTTP_SEND_RESPONSE_FLAG_BUFFER_DATA       = $00000004;
  HTTP_SEND_RESPONSE_FLAG_ENABLE_NAGLING    = $00000008;
  HTTP_SEND_RESPONSE_FLAG_PROCESS_RANGES    = $00000020;

  HTTP_FLUSH_RESPONSE_FLAG_RECURSIVE = 1;

type
  THTTP_OPAQUE_ID = ULONGLONG;
  PHTTP_OPAQUE_ID=^THTTP_OPAQUE_ID;
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
  HTTP_VERSION_2_0:THTTP_VERSION=(MajorVersion:2;MinorVersion:0);

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

  THTTP_LOG_DATA_TYPE=(
    HttpLogDataTypeFields
  );
  PHTTP_LOG_DATA_TYPE=^THTTP_LOG_DATA_TYPE;

  THTTP_LOG_DATA = record
    Type_: THTTP_LOG_DATA_TYPE;
  end;
  PHTTP_LOG_DATA=^THTTP_LOG_DATA;

  THTTP_LOG_FIELDS_DATA = record
    Base: THTTP_LOG_DATA;
    UserNameLength: USHORT;
    UriStemLength: USHORT;
    ClientIpLength: USHORT;
    ServerNameLength: USHORT;
    ServiceNameLength: USHORT;
    ServerIpLength: USHORT;
    MethodLength: USHORT;
    UriQueryLength: USHORT;
    HostLength: USHORT;
    UserAgentLength: USHORT;
    CookieLength: USHORT;
    ReferrerLength: USHORT;
    UserName: PWCHAR;
    UriStem: PWCHAR;
    ClientIp: PCHAR;
    ServerName: PCHAR;
    ServiceName: PCHAR;
    ServerIp: PCHAR;
    Method: PCHAR;
    UriQuery: PCHAR;
    Host: PCHAR;
    UserAgent: PCHAR;
    Cookie: PCHAR;
    Referrer: PCHAR;
    ServerPort: USHORT;
    ProtocolStatus: USHORT;
    Win32Status: ULONG;
    MethodNum: THTTP_VERB;
    SubStatus: USHORT;
  end;
  PHTTP_LOG_FIELDS_DATA=^THTTP_LOG_FIELDS_DATA;

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
      HttpDataChunkFromFragmentCacheEx:(
        xxAlign6: PVOID;
        ByteRangeEx: THTTP_BYTE_RANGE;
        pFragmentNameEx: PCWSTR;
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
  THTTP_AUTH_STATUS=(
    HttpAuthStatusSuccess,
    HttpAuthStatusNotAuthenticated,
    HttpAuthStatusFailure
  );
  PHTTP_AUTH_STATUS=^THTTP_AUTH_STATUS;

  THTTP_REQUEST_AUTH_TYPE=(
    HttpRequestAuthTypeNone = 0,
    HttpRequestAuthTypeBasic,
    HttpRequestAuthTypeDigest,
    HttpRequestAuthTypeNTLM,
    HttpRequestAuthTypeNegotiate
  );
  PHTTP_REQUEST_AUTH_TYPE=^THTTP_REQUEST_AUTH_TYPE;

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

  THTTP_REQUEST_INFO_TYPE=(
    HttpRequestInfoTypeAuth
  );
  PHTTP_REQUEST_INFO_TYPE=^THTTP_REQUEST_INFO_TYPE;

  THTTP_REQUEST_INFO = record
    InfoType: THTTP_REQUEST_INFO_TYPE;
    InfoLength: ULONG;
    pInfo: PVOID;
  end;
  PHTTP_REQUEST_INFO=^THTTP_REQUEST_INFO;

  TSECURITY_STATUS = LongInt;//LONG;

const
  HTTP_REQUEST_AUTH_FLAG_TOKEN_FOR_CACHED_CRED = 1;

type
  THTTP_REQUEST_AUTH_INFO = record
    AuthStatus: THTTP_AUTH_STATUS;
    SecStatus: TSECURITY_STATUS;
    Flags: ULONG;
    AuthType: THTTP_REQUEST_AUTH_TYPE;
    AccessToken: THandle;
    ContextAttributes: ULONG;
    PackedContextLength: ULONG;
    PackedContextType: ULONG;
    PackedContext: PVOID;
    MutualAuthDataLength: ULONG;
    pMutualAuthData: PCHAR;
    PackageNameLength: USHORT;
    pPackageName: PWSTR;
  end;
  PHTTP_REQUEST_AUTH_INFO=^THTTP_REQUEST_AUTH_INFO;

  THTTP_REQUEST_V2 = record
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
    xPadding1: DWORD;
    RequestInfoCount: USHORT;
    pRequestInfo: PHTTP_REQUEST_INFO;  
  end;
  PHTTP_REQUEST_V2=^THTTP_REQUEST_V2;

  THTTP_REQUEST=THTTP_REQUEST_V2;
  PHTTP_REQUEST=^THTTP_REQUEST;

const
  HTTP_REQUEST_FLAG_MORE_ENTITY_BODY_EXISTS = 1;
  HTTP_REQUEST_FLAG_IP_ROUTED = 2;

type
  THTTP_RESPONSE_INFO_TYPE=(
    HttpResponseInfoTypeMultipleKnownHeaders
  );
  PHTTP_RESPONSE_INFO_TYPE=^THTTP_RESPONSE_INFO_TYPE;

  THTTP_RESPONSE_INFO = record
    Type_: THTTP_RESPONSE_INFO_TYPE;
    Length: ULONG;
    pInfo: PVOID;                   
  end;
  PHTTP_RESPONSE_INFO=^THTTP_RESPONSE_INFO;

const
  HTTP_RESPONSE_INFO_FLAGS_PRESERVE_ORDER = 1;

type
  THTTP_MULTIPLE_KNOWN_HEADERS = record
    HeaderId: THTTP_HEADER_ID;
    Flags: ULONG;
    KnownHeaderCount: USHORT;
    KnownHeaders: PHTTP_KNOWN_HEADER;
  end;
  PHTTP_MULTIPLE_KNOWN_HEADERS=^THTTP_MULTIPLE_KNOWN_HEADERS;

  THTTP_RESPONSE_V2 = record
    Flags: ULONG;
    Version: THTTP_VERSION;
    StatusCode: USHORT;
    ReasonLength: USHORT;
    pReason: PCSTR;
    Headers: THTTP_RESPONSE_HEADERS;
    EntityChunkCount: USHORT;
    pEntityChunks: PHTTP_DATA_CHUNK;
    ResponseInfoCount: USHORT;
    pResponseInfo: PHTTP_RESPONSE_INFO;
  end;
  PHTTP_RESPONSE_V2=^THTTP_RESPONSE_V2;

  //IFDEF?
  THTTP_RESPONSE=THTTP_RESPONSE_V2;//???
  PHTTP_RESPONSE=^THTTP_RESPONSE;

  THTTPAPI_VERSION = record
    HttpApiMajorVersion: USHORT;
    HttpApiMinorVersion: USHORT;
  end;
  PHTTPAPI_VERSION=^THTTPAPI_VERSION;

const
  //IFDEF WIN32_WINNT?
  HTTPAPI_VERSION_1_0:THTTPAPI_VERSION=(HttpApiMajorVersion:1;HttpApiMinorVersion:0);
  HTTPAPI_VERSION_2_0:THTTPAPI_VERSION=(HttpApiMajorVersion:2;HttpApiMinorVersion:0);

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
function HttpCreateRequestQueue(Version: THTTPAPI_VERSION; pName: PCWSTR; pSecurityAttributes: PSecurityAttributes;
  Flags: ULONG; var pReqQueueHandle: THandle): ULONG; stdcall;
function HttpCloseRequestQueue(ReqQueueHandle: THandle): ULONG; stdcall;
function HttpSetRequestQueueProperty(Handle: THandle; Property_: THTTP_SERVER_PROPERTY; pPropertyInformation: PVOID;
  PropertyInformationLength: ULONG; Reserved: ULONG; pReserved: PVOID): ULONG; stdcall;
function HttpQueryRequestQueueProperty(Handle: THandle; Property_: THTTP_SERVER_PROPERTY; pPropertyInformation: PVOID;//?
  PropertyInformationLength: ULONG; Reserved: ULONG; var pReturnLength: ULONG; pReserved: PVOID): ULONG; stdcall;
function HttpShutdownRequestQueue(ReqQueueHandle: THandle): ULONG; stdcall;
function HttpReceiveClientCertificate(ReqQueueHandle: THandle; ConnectionId: THTTP_CONNECTION_ID; Flags: ULONG;
  var pSslClientCertInfo: THTTP_SSL_CLIENT_CERT_INFO; SslClientCertInfoSize: ULONG; var pBytesReceived: ULONG;
  pOverlapped: POverlapped): ULONG; stdcall;
function HttpCreateServerSession(Version: THTTPAPI_VERSION;
  var pServerSessionId: THTTP_SERVER_SESSION_ID; Reserved: ULONG): ULONG; stdcall;
function HttpCloseServerSession(ServerSessionId: THTTP_SERVER_SESSION_ID): ULONG; stdcall;
function HttpQueryServerSessionProperty(ServerSessionId: THTTP_SERVER_SESSION_ID; Property_: THTTP_SERVER_PROPERTY;
  pPropertyInformation: PVOID; PropertyInformationLength: ULONG; var pReturnLength: ULONG): ULONG; stdcall;
function HttpSetServerSessionProperty(ServerSessionId: THTTP_SERVER_SESSION_ID; Property_: THTTP_SERVER_PROPERTY;
  pPropertyInformation: PVOID; PropertyInformationLength: ULONG): ULONG; stdcall;
function HttpAddUrl(ReqQueueHandle: THandle; pFullyQualifiedUrl: PCWSTR; pReserved: PVOID): ULONG; stdcall;
function HttpRemoveUrl(ReqQueueHandle: THandle; pFullyQualifiedUrl: PCWSTR): ULONG; stdcall;
function HttpCreateUrlGroup(ServerSessionId: THTTP_SERVER_SESSION_ID; var pUrlGroupId: THTTP_URL_GROUP_ID;
  Reserved: ULONG): ULONG; stdcall;
function HttpCloseUrlGroup(UrlGroupId: THTTP_URL_GROUP_ID): ULONG; stdcall;
function HttpAddUrlToUrlGroup(UrlGroupId: THTTP_URL_GROUP_ID; pFullyQualifiedUrl: PCWSTR; UrlContext: THTTP_URL_CONTEXT;
  Reserved: ULONG): ULONG; stdcall;
function HttpRemoveUrlFromUrlGroup(UrlGroupId: THTTP_URL_GROUP_ID; pFullyQualifiedUrl: PCWSTR; Flags: ULONG): ULONG; stdcall;
function HttpSetUrlGroupProperty(UrlGroupId: THTTP_URL_GROUP_ID; Property_: THTTP_SERVER_PROPERTY;
  pPropertyInformation: PVOID; PropertyInformationLength: ULONG): ULONG; stdcall;
function HttpQueryUrlGroupProperty(UrlGroupId: THTTP_URL_GROUP_ID; Property_: THTTP_SERVER_PROPERTY;
  pPropertyInformation: PVOID; PropertyInformationLength: ULONG; var pReturnLength: ULONG): ULONG; stdcall;
function HttpReceiveHttpRequest(ReqQueueHandle: THandle; RequestId: THTTP_REQUEST_ID; Flags: ULONG;
  pRequestBuffer: PHTTP_REQUEST; RequestBufferLength: ULONG; var pBytesReceived: ULONG;
  pOverlapped: POverlapped): ULONG; stdcall;
function HttpReceiveRequestEntityBody(ReqQueueHandle: THandle; RequestId: THTTP_REQUEST_ID; Flags: ULONG;
  pBuffer: PVOID; BufferLength: ULONG; var pBytesReceived: ULONG; pOverlapped: POverlapped): ULONG; stdcall;
function HttpSendHttpResponse(ReqQueueHandle: THandle; RequestId: THTTP_REQUEST_ID; Flags: ULONG;
  pHttpResponse: PHTTP_RESPONSE; pCachePolicy: PHTTP_CACHE_POLICY; var pBytesSend: ULONG; pReserved1: PVOID; Reserved2: ULONG;
  pOverlapped: POverlapped; pLogData: PHTTP_LOG_DATA): ULONG; stdcall;
function HttpSendResponseEntityBody(ReqQueueHandle: THandle; RequestId: THTTP_REQUEST_ID; Flags: ULONG;
  EntityChunkCount: USHORT; pEntityChunks: PHTTP_DATA_CHUNK; var pBytesSent: ULONG; pReserved1: PVOID; Reserved2: ULONG;
  pOverlapped: POverlapped; pLogData: PHTTP_LOG_DATA): ULONG; stdcall;
function HttpWaitForDisconnect(ReqQueueHandle: THandle; ConnectionId: THTTP_CONNECTION_ID;
  pOverlapped: POverlapped): ULONG; stdcall;
function HttpCancelHttpRequest(ReqQueueHandle: THandle; RequestId: THTTP_REQUEST_ID;
  pOverlapped: POverlapped): ULONG; stdcall;
function HttpWaitForDemandStart(ReqQueueHandle: THandle; pOverlapped: POverlapped): ULONG; stdcall;
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
function HttpCreateRequestQueue; external httpapidll;
function HttpCloseRequestQueue; external httpapidll;
function HttpSetRequestQueueProperty; external httpapidll;
function HttpQueryRequestQueueProperty; external httpapidll;
function HttpShutdownRequestQueue; external httpapidll;
function HttpReceiveClientCertificate; external httpapidll;
function HttpCreateServerSession; external httpapidll;
function HttpCloseServerSession; external httpapidll;
function HttpQueryServerSessionProperty; external httpapidll;
function HttpSetServerSessionProperty; external httpapidll;
function HttpAddUrl; external httpapidll;
function HttpRemoveUrl; external httpapidll;
function HttpCreateUrlGroup; external httpapidll;
function HttpCloseUrlGroup; external httpapidll;
function HttpAddUrlToUrlGroup; external httpapidll;
function HttpRemoveUrlFromUrlGroup; external httpapidll;
function HttpSetUrlGroupProperty; external httpapidll;
function HttpQueryUrlGroupProperty; external httpapidll;
function HttpReceiveHttpRequest; external httpapidll;
function HttpReceiveRequestEntityBody; external httpapidll;
function HttpSendHttpResponse; external httpapidll;
function HttpSendResponseEntityBody; external httpapidll;
function HttpWaitForDisconnect; external httpapidll;
function HttpCancelHttpRequest; external httpapidll;
function HttpWaitForDemandStart; external httpapidll;
function HttpFlushResponseCache; external httpapidll;
function HttpAddFragmentToCache; external httpapidll;
function HttpReadFragmentFromCache; external httpapidll;
function HttpSetServiceConfiguration; external httpapidll;
function HttpDeleteServiceConfiguration; external httpapidll;
function HttpQueryServiceConfiguration; external httpapidll;

end.
