unit isapi4;

{$WEAKPACKAGEUNIT}

interface

uses Windows;

const
  HSE_VERSION_MAJOR         =   4;      // major version of this spec
  HSE_VERSION_MINOR         =   0;      // minor version of this spec
  HSE_LOG_BUFFER_LEN        =  80;
  HSE_MAX_EXT_DLL_NAME_LEN  = 256;

//  HSE_VERSION = MakeLong(HSE_VERSION_MINOR, HSE_VERSION_MAJOR);

// the following are the status codes returned by the Extension DLL

const
  HSE_STATUS_SUCCESS                      = 1;
  HSE_STATUS_SUCCESS_AND_KEEP_CONN        = 2;
  HSE_STATUS_PENDING                      = 3;
  HSE_STATUS_ERROR                        = 4;

// The following are the values to request services with the ServerSupportFunction.
//  Values from 0 to 1000 are reserved for future versions of the interface

  HSE_REQ_BASE                             = 0;
  HSE_REQ_SEND_URL_REDIRECT_RESP           = ( HSE_REQ_BASE + 1 );
  HSE_REQ_SEND_URL                         = ( HSE_REQ_BASE + 2 );
  HSE_REQ_SEND_RESPONSE_HEADER             = ( HSE_REQ_BASE + 3 );
  HSE_REQ_DONE_WITH_SESSION                = ( HSE_REQ_BASE + 4 );
  HSE_REQ_END_RESERVED                     = 1000;

//
//  These are Microsoft specific extensions
//

  HSE_REQ_MAP_URL_TO_PATH                  = (HSE_REQ_END_RESERVED+1);
  HSE_REQ_GET_SSPI_INFO                    = (HSE_REQ_END_RESERVED+2);
  HSE_APPEND_LOG_PARAMETER                 = (HSE_REQ_END_RESERVED+3);
  HSE_REQ_SEND_URL_EX                      = (HSE_REQ_END_RESERVED+4);
  HSE_REQ_IO_COMPLETION                    = (HSE_REQ_END_RESERVED+5);
  HSE_REQ_TRANSMIT_FILE                    = (HSE_REQ_END_RESERVED+6);
  HSE_REQ_REFRESH_ISAPI_ACL                = (HSE_REQ_END_RESERVED+7);
  HSE_REQ_IS_KEEP_CONN                     = (HSE_REQ_END_RESERVED+8);
  HSE_REQ_ASYNC_READ_CLIENT                = (HSE_REQ_END_RESERVED+10);
  HSE_REQ_GET_IMPERSONATION_TOKEN          = (HSE_REQ_END_RESERVED+11);
  HSE_REQ_MAP_URL_TO_PATH_EX               = (HSE_REQ_END_RESERVED+12);
  HSE_REQ_ABORTIVE_CLOSE                   = (HSE_REQ_END_RESERVED+14);
  HSE_REQ_GET_CERT_INFO_EX                 = (HSE_REQ_END_RESERVED+15);
  HSE_REQ_SEND_RESPONSE_HEADER_EX          = (HSE_REQ_END_RESERVED+16);
  HSE_REQ_CLOSE_CONNECTION                 = (HSE_REQ_END_RESERVED+17);
  HSE_REQ_IS_CONNECTED                     = (HSE_REQ_END_RESERVED+18);
  HSE_REQ_MAP_UNICODE_URL_TO_PATH          = (HSE_REQ_END_RESERVED+23);
  HSE_REQ_MAP_UNICODE_URL_TO_PATH_EX       = (HSE_REQ_END_RESERVED+24);
  HSE_REQ_EXEC_UNICODE_URL                 = (HSE_REQ_END_RESERVED+25);
  HSE_REQ_EXEC_URL                         = (HSE_REQ_END_RESERVED+26);
  HSE_REQ_GET_EXEC_URL_STATUS              = (HSE_REQ_END_RESERVED+27);
  HSE_REQ_SEND_CUSTOM_ERROR                = (HSE_REQ_END_RESERVED+28);
  HSE_REQ_IS_IN_PROCESS                    = (HSE_REQ_END_RESERVED+30);
  HSE_REQ_REPORT_UNHEALTHY                 = (HSE_REQ_END_RESERVED+32);
  HSE_REQ_NORMALIZE_URL                    = (HSE_REQ_END_RESERVED+33);
  HSE_REQ_VECTOR_SEND                      = (HSE_REQ_END_RESERVED+37);
  HSE_REQ_GET_ANONYMOUS_TOKEN              = (HSE_REQ_END_RESERVED+38);
  HSE_REQ_GET_CACHE_INVALIDATION_CALLBACK  = (HSE_REQ_END_RESERVED+40);
  HSE_REQ_GET_UNICODE_ANONYMOUS_TOKEN      = (HSE_REQ_END_RESERVED+41);
  HSE_REQ_SET_FLUSH_FLAG                   = (HSE_REQ_END_RESERVED+43);


//
//  Bit Flags for TerminateExtension
//
//    HSE_TERM_ADVISORY_UNLOAD - Server wants to unload the extension,
//          extension can return TRUE if OK, FALSE if the server should not
//          unload the extension
//
//    HSE_TERM_MUST_UNLOAD - Server indicating the extension is about to be
//          unloaded, the extension cannot refuse.
//

  HSE_TERM_ADVISORY_UNLOAD                 = $00000001;
  HSE_TERM_MUST_UNLOAD                     = $00000002;

//
// Flags for IO Functions, supported for IO Funcs.
//  TF means ServerSupportFunction( HSE_REQ_TRANSMIT_FILE)
//

  HSE_IO_SYNC                      = $00000001;   // for WriteClient
  HSE_IO_ASYNC                     = $00000002;   // for WriteClient/TF
  HSE_IO_DISCONNECT_AFTER_SEND     = $00000004;   // for TF
  HSE_IO_SEND_HEADERS              = $00000008;   // for TF
  HSE_IO_FINAL_SEND                = $00000010;
  HSE_IO_CACHE_RESPONSE            = $00000020;
  HSE_IO_NODELAY                   = $00001000;   // turn off TCP nagling

  HSE_VECTOR_ELEMENT_TYPE_MEMORY_BUFFER   = 0;
  HSE_VECTOR_ELEMENT_TYPE_FILE_HANDLE     = 1;

type
  PHSE_VECTOR_ELEMENT = ^THSE_VECTOR_ELEMENT;
  THSE_VECTOR_ELEMENT = record
    ElementType : DWORD;    // type of element (buffer/file/fragment etc)
    pvContext   : Pointer;  // the context representing the element to be sent
    cbOffset    : LONGLONG; // offset from the start of hFile
    cbSize      : LONGLONG; // number of bytes to send
  End;

  PHSE_VECTOR_ELEMENT_Array = ^THSE_VECTOR_ELEMENT_Array;
  THSE_VECTOR_ELEMENT_Array = array[0..0] of THSE_VECTOR_ELEMENT;

  PHSE_RESPONSE_VECTOR = ^THSE_RESPONSE_VECTOR;
  THSE_RESPONSE_VECTOR = record
    dwFlags        : DWORD;                     // combination of HSE_IO_* flags
    pszStatus      : LPSTR;                     // status line to send like "200 OK"
    pszHeaders     : LPSTR;                     // headers to send
    nElementCount  : DWORD;                     // number of THSE_VECTOR_ELEMENTs
    lpElementArray : PHSE_VECTOR_ELEMENT_Array; // pointer to those elements 
  end;

type
  HCONN = THandle;

//
// passed to GetExtensionVersion
//

type
  PHSE_VERSION_INFO = ^HSE_VERSION_INFO;
  HSE_VERSION_INFO = packed record
    dwExtensionVersion: DWORD;
    lpszExtensionDesc: array [0..HSE_MAX_EXT_DLL_NAME_LEN-1] of Char;
  end;
  THSE_VERSION_INFO = HSE_VERSION_INFO;
  LPHSE_VERSION_INFO = PHSE_VERSION_INFO;

type
  TGetServerVariableProc = function ( hConn: HCONN;
                                      VariableName: PAnsiChar;
                                      Buffer: Pointer;
                                      var Size: DWORD ): BOOL stdcall;

  TWriteClientProc = function ( ConnID: HCONN;
                                Buffer: Pointer;
                                var Bytes: DWORD;
                                dwReserved: DWORD ): BOOL stdcall;

  TReadClientProc = function ( ConnID: HCONN;
                               Buffer: Pointer;
                               var Size: DWORD ): BOOL stdcall;

  TServerSupportFunctionProc = function ( hConn: HCONN;
                                          HSERRequest: DWORD;
                                          Buffer: Pointer;
                                          Size: LPDWORD;
                                          DataType: LPDWORD ): BOOL stdcall;

//
// passed to extension procedure on a new request
//
type

  PEXTENSION_CONTROL_BLOCK = ^TEXTENSION_CONTROL_BLOCK;
  TEXTENSION_CONTROL_BLOCK = packed record
    cbSize: DWORD;                    // size of this struct.
    dwVersion: DWORD;                 // version info of this spec
    ConnID: HCONN;                    // Context number not to be modified!
    dwHttpStatusCode: DWORD;          // HTTP Status code
                     // null terminated log info specific to this Extension DLL
    lpszLogData: array [0..HSE_LOG_BUFFER_LEN-1] of Char;
    lpszMethod: PAnsiChar;                // REQUEST_METHOD
    lpszQueryString: PAnsiChar;           // QUERY_STRING
    lpszPathInfo: PAnsiChar;              // PATH_INFO
    lpszPathTranslated: PAnsiChar;        // PATH_TRANSLATED
    cbTotalBytes: DWORD;              // Total bytes indicated from client
    cbAvailable: DWORD;               // Available number of bytes
    lpbData: Pointer;                 // pointer to cbAvailable bytes
    lpszContentType: PAnsiChar;           // Content type of client data

    GetServerVariable: TGetServerVariableProc;
    WriteClient: TWriteClientProc;
    ReadClient: TReadClientProc;
    ServerSupportFunction: TServerSupportFunctionProc;
  end;

//
//  these are the prototypes that must be exported from the extension DLL
//

//  function GetExtensionVersion(var Ver: THSE_VERSION_INFO): BOOL; stdcall;
//  function HttpExtensionProc(var ECB: TEXTENSION_CONTROL_BLOCK): DWORD; stdcall;
//  function TerminateExtension(dwFlags: DWORD): BOOL; stdcall;

// the following type declarations are for the server side

  TGetExtensionVersion = function (var Ver: THSE_VERSION_INFO): BOOL stdcall;
  THttpExtensionProc = function (var ECB: TEXTENSION_CONTROL_BLOCK): DWORD stdcall;
  TTerminateExtension = function (dwFlags: DWORD): BOOL stdcall;

//
//  Bit field of flags that can be on a virtual directory
//

const
  HSE_URL_FLAGS_READ          = $00000001;    // Allow for Read
  HSE_URL_FLAGS_WRITE         = $00000002;    // Allow for Write
  HSE_URL_FLAGS_EXECUTE       = $00000004;    // Allow for Execute
  HSE_URL_FLAGS_SSL           = $00000008;    // Require SSL
  HSE_URL_FLAGS_DONT_CACHE    = $00000010;    // Don't cache (vroot only)
  HSE_URL_FLAGS_NEGO_CERT     = $00000020;    // Allow client SSL certs
  HSE_URL_FLAGS_REQUIRE_CERT  = $00000040;    // Require client SSL certs
  HSE_URL_FLAGS_MAP_CERT      = $00000080;    // Map SSL cert to NT account
  HSE_URL_FLAGS_SSL128        = $00000100;    // Require 128 bit SSL
  HSE_URL_FLAGS_SCRIPT        = $00000200;    // Allow for Script execution

  HSE_URL_FLAGS_MASK          = $000003FF;

//
// for extended information on a URL mapping
//
type

  PHSE_URL_MAPEX_INFO = ^THSE_URL_MAPEX_INFO;
  THSE_URL_MAPEX_INFO = packed record
    lpszPath: array [0..MAX_PATH-1] of Char; // Physical path root mapped to
    dwFlags: DWORD;            // Flags associated with this URL path
    cchMatchingPath: DWORD;    // Number of matching characters in physical path
    cchMatchingURL: DWORD;     // Number of matching characters in URL
    dwReserved1: DWORD;
    dwReserved2: DWORD;
  end;

//
// the callback function for the Async I/O Completion.
//
type

  THseIoCompletion = procedure (var ECB: TEXTENSION_CONTROL_BLOCK; pContext: Pointer;
    cbIO: DWORD; dwError: DWORD) stdcall;

//
// HSE_TF_INFO defines the type for HTTP SERVER EXTENSION support for
//  ISAPI applications to send files using TransmitFile.
// A pointer to this object should be used with ServerSupportFunction()
//  for HSE_REQ_TRANSMIT_FILE.
//
type

  PHSE_TF_INFO = ^THSE_TF_INFO;
  THSE_TF_INFO = record

    //
    // callback and context information
    // the callback function will be called when IO is completed.
    // the context specified will be used during such callback.
    //
    // These values (if non-NULL) will override the one set by calling
    //  ServerSupportFunction() with HSE_REQ_IO_COMPLETION
    //
    pfnHseIO: THseIoCompletion;
    pContext: Pointer;

    // file should have been opened with FILE_FLAG_SEQUENTIAL_SCAN
    hFile: THandle;

    //
    // HTTP header and status code
    // These fields are used only if HSE_IO_SEND_HEADERS is present in dwFlags
    //

    pszStatusCode: PAnsiChar; // HTTP Status Code  eg: "200 OK"

    BytesToWrite: DWORD;  // special value of "0" means write entire file.
    Offset: DWORD;        // offset value within the file to start from

    pHead: Pointer;       // Head buffer to be sent before file data
    HeadLength: DWORD;    // header length
    pTail: Pointer;       // Tail buffer to be sent after file data
    TailLength: DWORD;    // tail length

    dwFlags: DWORD;       // includes HSE_IO_DISCONNECT_AFTER_SEND, ...
  end;

//
// HSE_SEND_HEADER_EX_INFO allows an ISAPI application to send headers
//  and specify keep-alive behavior in the same call.
//
type

  PHSE_SEND_HEADER_EX_INFO = ^THSE_SEND_HEADER_EX_INFO;
  THSE_SEND_HEADER_EX_INFO = record

    //
    // HTTP status code and header
    //
    pszStatus: PAnsiChar;  // HTTP status code  eg: "200 OK"
    pszHeader: PAnsiChar;  // HTTP header

    cchStatus: DWORD;  // number of characters in status code
    cchHeader: DWORD;  // number of characters in header

    fKeepConn: BOOL;  // keep client connection alive?
  end;

(*** not implemented ***

#if(_WIN32_WINNT >= 0x400)
#include <wincrypt.h>
//
// CERT_CONTEXT_EX is passed as an an argument to
//  ServerSupportFunction( HSE_REQ_GET_CERT_INFO_EX )
//
type

  PCERT_CONTEXT_EX = ^TCERT_CONTEXT_EX;
  TCERT_CONTEXT_EX = record
    CertContext: CERT_CONTEXT;
    cbAllocated: DWORD;
    dwCertificateFlags: DWORD;
  end;

************************)

/////////////////////////////////////////////////////////////////////////////

{********
*
*    This module contains the Microsoft HTTP filter extension info
*
******************}

//
//  Current version of the filter spec is 4.0
//

const
  HTTP_FILTER_REVISION = $00040000;

  SF_MAX_USERNAME         = (256+1);
  SF_MAX_PASSWORD         = (256+1);
  SF_MAX_AUTH_TYPE = (32+1);

  SF_MAX_FILTER_DESC_LEN  = (256+1);

  //
  //  These values can be used with the pfnSFCallback function supplied in
  //  the filter context structure
  //

  //
  //  Sends a complete HTTP server response header including
  //  the status, server version, message time and MIME version.
  //
  //  Server extensions should append other information at the end,
  //  such as Content-type, Content-length etc followed by an extra
  //  '\r\n'.
  //
  //  pData - Zero terminated string pointing to optional
  //      status string (i.e., "401 Access Denied") or NULL for
  //      the default response of "200 OK".
  //
  //  ul1 - Zero terminated string pointing to optional data to be
  //      appended and set with the header.  If NULL, the header will
  //      be terminated with an empty line.
  //

  SF_REQ_SEND_RESPONSE_HEADER = 0;

  //
  //  If the server denies the HTTP request, add the specified headers
  //  to the server error response.
  //
  //  This allows an authentication filter to advertise its services
  //  w/o filtering every request.  Generally the headers will be
  //  WWW-Authenticate headers with custom authentication schemes but
  //  no restriction is placed on what headers may be specified.
  //
  //  pData - Zero terminated string pointing to one or more header lines
  //      with terminating '\r\n'.
  //

  SF_REQ_ADD_HEADERS_ON_DENIAL = 1;

  //
  //  Only used by raw data filters that return SF_STATUS_READ_NEXT
  //
  //  ul1 - size in bytes for the next read
  //

  SF_REQ_SET_NEXT_READ_SIZE = 2;

  //
  //  Used to indicate this request is a proxy request
  //
  //  ul1 - The proxy flags to set
  //      0x00000001 - This is a HTTP proxy request
  //
  //

  SF_REQ_SET_PROXY_INFO = 3;

  //
  //  Returns the connection ID contained in the ConnID field of an
  //  ISAPI Application's Extension Control Block.  This value can be used
  //  as a key to cooridinate shared data between Filters and Applications.
  //
  //  pData - Pointer to DWORD that receives the connection ID.
  //

  SF_REQ_GET_CONNID = 4;

  //
  // Used to set a SSPI security context + impersonation token
  // derived from a client certificate.
  //
  // pData - certificate info ( PHTTP_FILTER_CERTIFICATE_INFO )
  // ul1 - CtxtHandle*
  // ul2 - impersonation handle
  //

  SF_REQ_SET_CERTIFICATE_INFO = 5;

  //
  // Used to get an IIS property
  // as defined in SF_PROPERTY_IIS
  //
  // ul1 - Property ID
  //

  SF_REQ_GET_PROPERTY = 6;

  //
  // Used to normalize an URL
  //
  // pData - URL to normalize
  //

  SF_REQ_NORMALIZE_URL = 7;

  //
  // Disable Notifications
  //
  // ul1 - notifications to disable
  //

  SF_REQ_DISABLE_NOTIFICATIONS = 8;

  SF_PROPERTY_SSL_CTXT = 0;
  SF_PROPERTY_INSTANCE_NUM_ID = 1;

  //
  //  These values are returned by the filter entry point when a new request is
  //  received indicating their interest in this particular request
  //

  //
  //  The filter has handled the HTTP request.  The server should disconnect
  //  the session.
  //

  SF_STATUS_REQ_FINISHED = $8000000;

  //
  //  Same as SF_STATUS_FINISHED except the server should keep the TCP
  //  session open if the option was negotiated
  //

  SF_STATUS_REQ_FINISHED_KEEP_CONN = $8000001;

  //
  //  The next filter in the notification chain should be called
  //

  SF_STATUS_REQ_NEXT_NOTIFICATION = $8000002;

  //
  //  This filter handled the notification.  No other handles should be
  //  called for this particular notification type
  //

  SF_STATUS_REQ_HANDLED_NOTIFICATION = $8000003;

  //
  //  An error occurred.  The server should use GetLastError() and indicate
  //  the error to the client
  //

  SF_STATUS_REQ_ERROR = $8000004;

  //
  //  The filter is an opaque stream filter and we're negotiating the
  //  session parameters.  Only valid for raw read notification.
  //

  SF_STATUS_REQ_READ_NEXT = $8000005;

//
//  pvNotification points to this structure for all request notification types
//

type

  TFilterGetServerVariableProc = function (var pfc{: THTTP_FILTER_CONTEXT};
    VariableName: PAnsiChar; Buffer: Pointer; var Size: DWORD ): BOOL stdcall;

  TFilterAddResponseHeadersProc = function (var pfc{: THTTP_FILTER_CONTEXT};
    lpszHeaders: PAnsiChar; dwReserved: DWORD): BOOL stdcall;

  TFilterWriteClientProc = function (var pfc{: THTTP_FILTER_CONTEXT};
    Buffer: Pointer; var Bytes: DWORD; dwReserved: DWORD ): BOOL stdcall;

  TFilterAllocMemProc = function (var pfc{: THTTP_FILTER_CONTEXT}; cbSize: DWORD;
    dwReserved: DWORD): Pointer stdcall;

  TFilterServerSupportFunctionProc = function (var pfc{: THTTP_FILTER_CONTEXT};
    sfReq: DWORD; pData: Pointer; ul1, ul2: DWORD): BOOL stdcall;

  PHTTP_FILTER_CONTEXT = ^THTTP_FILTER_CONTEXT;
  THTTP_FILTER_CONTEXT = record
    cbSize: DWORD;

    //
    //  This is the structure revision level.
    //

    Revision: DWORD;

    //
    //  Private context information for the server.
    //

    ServerContext: Pointer;
    ulReserved: DWORD;

    //
    //  TRUE if this request is coming over a secure port
    //

    fIsSecurePort: BOOL;

    //
    //  A context that can be used by the filter
    //

    pFilterContext: Pointer;

    //
    //  Server callbacks
    //

    GetServerVariable: TFilterGetServerVariableProc;
    AddResponseHeaders: TFilterAddResponseHeadersProc;
    WriteClient: TFilterWriteClientProc;
    AllocMem: TFilterAllocMemProc;
    ServerSupportFunction: TFilterServerSupportFunctionProc;
  end;

  //
  //  This structure is the notification info for the read and send raw data
  //  notification types
  //

  PHTTP_FILTER_RAW_DATA = ^HTTP_FILTER_RAW_DATA;
  HTTP_FILTER_RAW_DATA = record
    //
    //  This is a pointer to the data for the filter to process.
    //

    pvInData: Pointer;
    cbInData: DWORD;       // Number of valid data bytes
    cbInBuffer: DWORD;     // Total size of buffer

    dwReserved: DWORD;
  end;
  THTTP_FILTER_RAW_DATA = HTTP_FILTER_RAW_DATA;
  LPHTTP_FILTER_RAW_DATA = PHTTP_FILTER_RAW_DATA;

  //
  //  This structure is the notification info for when the server is about to
  //  process the client headers
  //

  TGetHeaderProc = function (var pfc: THTTP_FILTER_CONTEXT; lpszName: PAnsiChar;
    var lpvBuffer; var lpdwSize: DWORD): BOOL stdcall;

  TSetHeaderProc = function (var pfc: THTTP_FILTER_CONTEXT; lpszName,
    lpszValue: PAnsiChar): BOOL stdcall;

  TAddHeaderProc = function (var pfc: THTTP_FILTER_CONTEXT; lpszName,
    lpszValue: PAnsiChar): BOOL stdcall;

  PHTTP_FILTER_PREPROC_HEADERS = ^THTTP_FILTER_PREPROC_HEADERS;
  THTTP_FILTER_PREPROC_HEADERS = record
    //
    //  For SF_NOTIFY_PREPROC_HEADERS, retrieves the specified header value.
    //  Header names should include the trailing ':'.  The special values
    //  'method', 'url' and 'version' can be used to retrieve the individual
    //  portions of the request line
    //

    GetHeader: TGetHeaderProc;

    //
    //  Replaces this header value to the specified value.  To delete a header,
    //  specified a value of '\0'.
    //

    SetHeader: TSetHeaderProc;

    //
    //  Adds the specified header and value
    //

    AddHeader: TAddHeaderProc;

    HttpStatus: DWORD;               // New in 4.0, status for SEND_RESPONSE
    dwReserved: DWORD;               // New in 4.0
  end;

  //
  //  Authentication information for this request.
  //

  PHTTP_FILTER_AUTHENT = ^HTTP_FILTER_AUTHENT;
  HTTP_FILTER_AUTHENT = record
    //
    //  Pointer to username and password, empty strings for the anonymous user
    //
    //  Client's can overwrite these buffers which are guaranteed to be at
    //  least SF_MAX_USERNAME and SF_MAX_PASSWORD bytes large.
    //

    pszUser: PAnsiChar;
    cbUserBuff: DWORD;

    pszPassword: PAnsiChar;
    cbPasswordBuff: DWORD;
  end;
  THTTP_FILTER_AUTHENT = HTTP_FILTER_AUTHENT;
  LPHTTP_FILTER_AUTHENT = PHTTP_FILTER_AUTHENT;

  //
  //  Indicates the server is going to use the specific physical mapping for
  //  the specified URL.  Filters can modify the physical path in place.
  //

  PHTTP_FILTER_URL_MAP = ^HTTP_FILTER_URL_MAP;
  HTTP_FILTER_URL_MAP = record
    pszURL: PAnsiChar;
    pszPhysicalPath: PAnsiChar;
    cbPathBuff: DWORD;
  end;
  THTTP_FILTER_URL_MAP = HTTP_FILTER_URL_MAP;
  LPHTTP_FILTER_URL_MAP = PHTTP_FILTER_URL_MAP;

const
  //
  //  Bitfield indicating the requested resource has been denied by the server due
  //  to a logon failure, an ACL on a resource, an ISAPI Filter or an
  //  ISAPI Application/CGI Application.
  //
  //  SF_DENIED_BY_CONFIG can appear with SF_DENIED_LOGON if the server
  //  configuration did not allow the user to logon.
  //

  SF_DENIED_LOGON             = $00000001;
  SF_DENIED_RESOURCE          = $00000002;
  SF_DENIED_FILTER            = $00000004;
  SF_DENIED_APPLICATION       = $00000008;
  SF_DENIED_BY_CONFIG         = $00010000;

type
  PHTTP_FILTER_ACCESS_DENIED = ^HTTP_FILTER_ACCESS_DENIED;
  HTTP_FILTER_ACCESS_DENIED = record
    pszURL: PAnsiChar;            // Requesting URL
    pszPhysicalPath: PAnsiChar;   // Physical path of resource
    dwReason: DWORD;          // Bitfield of SF_DENIED flags
  end;
  THTTP_FILTER_ACCESS_DENIED = HTTP_FILTER_ACCESS_DENIED;
  LPHTTP_FILTER_ACCESS_DENIED = PHTTP_FILTER_ACCESS_DENIED;

  //
  //  The log information about to be written to the server log file.  The
  //  string pointers can be replaced but the memory must remain valid until
  //  the next notification
  //

  PHTTP_FILTER_LOG = ^HTTP_FILTER_LOG;
  HTTP_FILTER_LOG = record
    pszClientHostName: PAnsiChar;
    pszClientUserName: PAnsiChar;
    pszServerName: PAnsiChar;
    pszOperation: PAnsiChar;
    pszTarget: PAnsiChar;
    pszParameters: PAnsiChar;
    dwHttpStatus: DWORD;
    dwWin32Status: DWORD;
    dwBytesSent: DWORD;             // IIS 4.0 and later
    dwBytesRecvd: DWORD;            // IIS 4.0 and later
    msTimeForProcessing: DWORD;     // IIS 4.0 and later
  end;
  THTTP_FILTER_LOG = HTTP_FILTER_LOG;
  LPHTTP_FILTER_LOG = PHTTP_FILTER_LOG;

const
  //
  //  Notification Flags
  //
  //  SF_NOTIFY_SECURE_PORT
  //  SF_NOTIFY_NONSECURE_PORT
  //
  //      Indicates whether the application wants to be notified for transactions
  //      that are happenning on the server port(s) that support data encryption
  //      (such as PCT and SSL), on only the non-secure port(s) or both.
  //
  //  SF_NOTIFY_READ_RAW_DATA
  //
  //      Applications are notified after the server reads a block of memory
  //      from the client but before the server does any processing on the
  //      block.  The data block may contain HTTP headers and entity data.
  //
  //
  //

  SF_NOTIFY_SECURE_PORT               = $00000001;
  SF_NOTIFY_NONSECURE_PORT            = $00000002;
  SF_NOTIFY_READ_RAW_DATA             = $00008000;
  SF_NOTIFY_PREPROC_HEADERS           = $00004000;
  SF_NOTIFY_AUTHENTICATION            = $00002000;
  SF_NOTIFY_URL_MAP                   = $00001000;
  SF_NOTIFY_ACCESS_DENIED             = $00000800;
  SF_NOTIFY_SEND_RESPONSE             = $00000040;
  SF_NOTIFY_SEND_RAW_DATA             = $00000400;
  SF_NOTIFY_LOG                       = $00000200;
  SF_NOTIFY_END_OF_REQUEST            = $00000080;
  SF_NOTIFY_END_OF_NET_SESSION        = $00000100;

  //
  //  Filter ordering flags
  //
  //  Filters will tend to be notified by their specified
  //  ordering.  For ties, notification order is determined by load order.
  //
  //  SF_NOTIFY_ORDER_HIGH - Authentication or data transformation filters
  //  SF_NOTIFY_ORDER_MEDIUM
  //  SF_NOTIFY_ORDER_LOW  - Logging filters that want the results of any other
  //                      filters might specify this order.
  //

  SF_NOTIFY_ORDER_HIGH               = $00080000;
  SF_NOTIFY_ORDER_MEDIUM             = $00040000;
  SF_NOTIFY_ORDER_LOW                = $00020000;
  SF_NOTIFY_ORDER_DEFAULT            = SF_NOTIFY_ORDER_LOW;
  SF_NOTIFY_ORDER_MASK               = SF_NOTIFY_ORDER_HIGH or
                                       SF_NOTIFY_ORDER_MEDIUM or
                                       SF_NOTIFY_ORDER_LOW;

  //
  //  Filter version information, passed to GetFilterVersion
  //

type
  PHTTP_FILTER_VERSION = ^HTTP_FILTER_VERSION;
  HTTP_FILTER_VERSION = record
    //
    //  Version of the spec the server is using
    //

    dwServerFilterVersion: DWORD;

    //
    //  Fields specified by the client
    //

    dwFilterVersion: DWORD;
    lpszFilterDesc: array[0..SF_MAX_FILTER_DESC_LEN - 1] of Char;
    dwFlags: DWORD;
  end;
  THTTP_FILTER_VERSION = HTTP_FILTER_VERSION;
  LPHTTP_FILTER_VERSION = PHTTP_FILTER_VERSION;

//
//  A filter DLL's entry point looks like this.  The return code should be
//  an SF_STATUS_TYPE
//
//  NotificationType - Type of notification
//  pvNotification - Pointer to notification specific data
//

// function HttpFilterProc(var pfc: THTTP_FILTER_CONTEXT; Notificationtype: DWORD;
//   pvNotification: Pointer): DWORD; stdcall;
//
// function GetFilterVersion(var pVer: THTTP_FILTER_VERSION): BOOL; stdcall;
//
// function TerminateFilter(dwFlags: DWORD): BOOL; stdcall;

// the following type declarations are for the server side


  THttpFilterProc = function (var pfc: THTTP_FILTER_CONTEXT;
    Notificationtype: DWORD; pvNotification: Pointer): DWORD stdcall;

  TGetFilterVersion = function (var pVer: THTTP_FILTER_VERSION): BOOL stdcall;

  TTerminateFilter = function (dwFlags: DWORD): BOOL stdcall;

implementation

end.
