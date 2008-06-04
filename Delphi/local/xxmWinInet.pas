unit xxmWinInet;

interface

//selected items from the WinInet unit

const
  HTTP_QUERY_MIME_VERSION                     = 0;
  HTTP_QUERY_CONTENT_TYPE                     = 1;
  HTTP_QUERY_CONTENT_TRANSFER_ENCODING        = 2;
  HTTP_QUERY_CONTENT_ID                       = 3;
  HTTP_QUERY_CONTENT_DESCRIPTION              = 4;
  HTTP_QUERY_CONTENT_LENGTH                   = 5;
  HTTP_QUERY_CONTENT_LANGUAGE                 = 6;
  HTTP_QUERY_ALLOW                            = 7;
  HTTP_QUERY_PUBLIC                           = 8;
  HTTP_QUERY_DATE                             = 9;
  HTTP_QUERY_EXPIRES                          = 10;
  HTTP_QUERY_LAST_MODIFIED                    = 11;
  HTTP_QUERY_MESSAGE_ID                       = 12;
  HTTP_QUERY_URI                              = 13;
  HTTP_QUERY_DERIVED_FROM                     = 14;
  HTTP_QUERY_COST                             = 15;
  HTTP_QUERY_LINK                             = 16;
  HTTP_QUERY_PRAGMA                           = 17;
  HTTP_QUERY_VERSION                          = 18; { special: part of status line }
  HTTP_QUERY_STATUS_CODE                      = 19; { special: part of status line }
  HTTP_QUERY_STATUS_TEXT                      = 20; { special: part of status line }
  HTTP_QUERY_RAW_HEADERS                      = 21; { special: all headers as ASCIIZ }
  HTTP_QUERY_RAW_HEADERS_CRLF                 = 22; { special: all headers }
  HTTP_QUERY_CONNECTION                       = 23;
  HTTP_QUERY_ACCEPT                           = 24;
  HTTP_QUERY_ACCEPT_CHARSET                   = 25;
  HTTP_QUERY_ACCEPT_ENCODING                  = 26;
  HTTP_QUERY_ACCEPT_LANGUAGE                  = 27;
  HTTP_QUERY_AUTHORIZATION                    = 28;
  HTTP_QUERY_CONTENT_ENCODING                 = 29;
  HTTP_QUERY_FORWARDED                        = 30;
  HTTP_QUERY_FROM                             = 31;
  HTTP_QUERY_IF_MODIFIED_SINCE                = 32;
  HTTP_QUERY_LOCATION                         = 33;
  HTTP_QUERY_ORIG_URI                         = 34;
  HTTP_QUERY_REFERER                          = 35;
  HTTP_QUERY_RETRY_AFTER                      = 36;
  HTTP_QUERY_SERVER                           = 37;
  HTTP_QUERY_TITLE                            = 38;
  HTTP_QUERY_USER_AGENT                       = 39;
  HTTP_QUERY_WWW_AUTHENTICATE                 = 40;
  HTTP_QUERY_PROXY_AUTHENTICATE               = 41;
  HTTP_QUERY_ACCEPT_RANGES                    = 42;
  HTTP_QUERY_SET_COOKIE                       = 43;
  HTTP_QUERY_COOKIE                           = 44;
  HTTP_QUERY_REQUEST_METHOD                   = 45;  { special: GET/POST etc. }
  HTTP_QUERY_REFRESH                          = 46;
  HTTP_QUERY_CONTENT_DISPOSITION              = 47;

{ HTTP 1.1 defined headers }

  HTTP_QUERY_AGE                              = 48;
  HTTP_QUERY_CACHE_CONTROL                    = 49;
  HTTP_QUERY_CONTENT_BASE                     = 50;
  HTTP_QUERY_CONTENT_LOCATION                 = 51;
  HTTP_QUERY_CONTENT_MD5                      = 52;
  HTTP_QUERY_CONTENT_RANGE                    = 53;
  HTTP_QUERY_ETAG                             = 54;
  HTTP_QUERY_HOST                             = 55;
  HTTP_QUERY_IF_MATCH                         = 56;
  HTTP_QUERY_IF_NONE_MATCH                    = 57;
  HTTP_QUERY_IF_RANGE                         = 58;
  HTTP_QUERY_IF_UNMODIFIED_SINCE              = 59;
  HTTP_QUERY_MAX_FORWARDS                     = 60;
  HTTP_QUERY_PROXY_AUTHORIZATION              = 61;
  HTTP_QUERY_RANGE                            = 62;
  HTTP_QUERY_TRANSFER_ENCODING                = 63;
  HTTP_QUERY_UPGRADE                          = 64;
  HTTP_QUERY_VARY                             = 65;
  HTTP_QUERY_VIA                              = 66;
  HTTP_QUERY_WARNING                          = 67;
  HTTP_QUERY_MAX                              = 67;

{ HTTP_QUERY_CUSTOM - if this special value is supplied as the dwInfoLevel }
{ parameter of HttpQueryInfo then the lpBuffer parameter contains the name }
{ of the header we are to query }
  HTTP_QUERY_CUSTOM                           = 65535;

{ HTTP_QUERY_FLAG_REQUEST_HEADERS - if this bit is set in the dwInfoLevel }
{ parameter of HttpQueryInfo then the request headers will be queried for the }
{ request information }
  HTTP_QUERY_FLAG_REQUEST_HEADERS             = $80000000;

{ HTTP_QUERY_FLAG_SYSTEMTIME - if this bit is set in the dwInfoLevel parameter }
{ of HttpQueryInfo AND the header being queried contains date information, }
{ e.g. the "Expires:" header then lpBuffer will contain a SYSTEMTIME structure }
{ containing the date and time information converted from the header string }
  HTTP_QUERY_FLAG_SYSTEMTIME                  = $40000000;

{ HTTP_QUERY_FLAG_NUMBER - if this bit is set in the dwInfoLevel parameter of }
{ HttpQueryInfo, then the value of the header will be converted to a number }
{ before being returned to the caller, if applicable }
  HTTP_QUERY_FLAG_NUMBER                      = $20000000;

{ HTTP_QUERY_FLAG_COALESCE - combine the values from several headers of the }
{ same name into the output buffer }
  HTTP_QUERY_FLAG_COALESCE                    = $10000000;

  HTTP_QUERY_MODIFIER_FLAGS_MASK              = HTTP_QUERY_FLAG_REQUEST_HEADERS or
                                                HTTP_QUERY_FLAG_SYSTEMTIME or
                                                HTTP_QUERY_FLAG_NUMBER or
                                                HTTP_QUERY_FLAG_COALESCE;

  HTTP_QUERY_HEADER_MASK                      = not HTTP_QUERY_MODIFIER_FLAGS_MASK;

{  HTTP Response Status Codes: }
  HTTP_STATUS_CONTINUE            = 100;    { OK to continue with request }
  HTTP_STATUS_SWITCH_PROTOCOLS    = 101;    { server has switched protocols in upgrade header }
  HTTP_STATUS_OK                  = 200;    { request completed }
  HTTP_STATUS_CREATED             = 201;    { object created, reason = new URI }
  HTTP_STATUS_ACCEPTED            = 202;    { async completion (TBS) }
  HTTP_STATUS_PARTIAL             = 203;    { partial completion }
  HTTP_STATUS_NO_CONTENT          = 204;    { no info to return }
  HTTP_STATUS_RESET_CONTENT       = 205;    { request completed, but clear form }
  HTTP_STATUS_PARTIAL_CONTENT     = 206;    { partial GET furfilled }
  HTTP_STATUS_AMBIGUOUS           = 300;    { server couldn't decide what to return }
  HTTP_STATUS_MOVED               = 301;    { object permanently moved }
  HTTP_STATUS_REDIRECT            = 302;    { object temporarily moved }
  HTTP_STATUS_REDIRECT_METHOD     = 303;    { redirection w/ new access method }
  HTTP_STATUS_NOT_MODIFIED        = 304;    { if-modified-since was not modified }
  HTTP_STATUS_USE_PROXY           = 305;    { redirection to proxy, location header specifies proxy to use }
  HTTP_STATUS_REDIRECT_KEEP_VERB  = 307;    { HTTP/1.1: keep same verb }
  HTTP_STATUS_BAD_REQUEST         = 400;    { invalid syntax }
  HTTP_STATUS_DENIED              = 401;    { access denied }
  HTTP_STATUS_PAYMENT_REQ         = 402;    { payment required }
  HTTP_STATUS_FORBIDDEN           = 403;    { request forbidden }
  HTTP_STATUS_NOT_FOUND           = 404;    { object not found }
  HTTP_STATUS_BAD_METHOD          = 405;    { method is not allowed }
  HTTP_STATUS_NONE_ACCEPTABLE     = 406;    { no response acceptable to client found }
  HTTP_STATUS_PROXY_AUTH_REQ      = 407;    { proxy authentication required }
  HTTP_STATUS_REQUEST_TIMEOUT     = 408;    { server timed out waiting for request }
  HTTP_STATUS_CONFLICT            = 409;    { user should resubmit with more info }
  HTTP_STATUS_GONE                = 410;    { the resource is no longer available }
  HTTP_STATUS_AUTH_REFUSED        = 411;    { couldn't authorize client }
  HTTP_STATUS_PRECOND_FAILED      = 412;    { precondition given in request failed }
  HTTP_STATUS_REQUEST_TOO_LARGE   = 413;    { request entity was too large }
  HTTP_STATUS_URI_TOO_LONG        = 414;    { request URI too long }
  HTTP_STATUS_UNSUPPORTED_MEDIA   = 415;    { unsupported media type }
  HTTP_STATUS_SERVER_ERROR        = 500;    { internal server error }
  HTTP_STATUS_NOT_SUPPORTED       = 501;    { required not supported }
  HTTP_STATUS_BAD_GATEWAY         = 502;    { error response received from gateway }
  HTTP_STATUS_SERVICE_UNAVAIL     = 503;    { temporarily overloaded }
  HTTP_STATUS_GATEWAY_TIMEOUT     = 504;    { timed out waiting for gateway }
  HTTP_STATUS_VERSION_NOT_SUP     = 505;    { HTTP version not supported }
  HTTP_STATUS_FIRST               = HTTP_STATUS_CONTINUE;
  HTTP_STATUS_LAST                = HTTP_STATUS_VERSION_NOT_SUP;

const
  INTERNET_OPTION_CALLBACK = 1;
  INTERNET_OPTION_CONNECT_TIMEOUT = 2;
  INTERNET_OPTION_CONNECT_RETRIES = 3;
  INTERNET_OPTION_CONNECT_BACKOFF = 4;
  INTERNET_OPTION_SEND_TIMEOUT = 5;
  INTERNET_OPTION_CONTROL_SEND_TIMEOUT       = INTERNET_OPTION_SEND_TIMEOUT;
  INTERNET_OPTION_RECEIVE_TIMEOUT = 6;
  INTERNET_OPTION_CONTROL_RECEIVE_TIMEOUT    = INTERNET_OPTION_RECEIVE_TIMEOUT;
  INTERNET_OPTION_DATA_SEND_TIMEOUT = 7;
  INTERNET_OPTION_DATA_RECEIVE_TIMEOUT = 8;
  INTERNET_OPTION_HANDLE_TYPE = 9;
  INTERNET_OPTION_READ_BUFFER_SIZE = 12;
  INTERNET_OPTION_WRITE_BUFFER_SIZE = 13;
  INTERNET_OPTION_ASYNC_ID = 15;
  INTERNET_OPTION_ASYNC_PRIORITY = 16;
  INTERNET_OPTION_PARENT_HANDLE               = 21;
  INTERNET_OPTION_KEEP_CONNECTION             = 22;
  INTERNET_OPTION_REQUEST_FLAGS               = 23;
  INTERNET_OPTION_EXTENDED_ERROR              = 24;
  INTERNET_OPTION_OFFLINE_MODE                = 26;
  INTERNET_OPTION_CACHE_STREAM_HANDLE         = 27;
  INTERNET_OPTION_USERNAME                    = 28;
  INTERNET_OPTION_PASSWORD                    = 29;
  INTERNET_OPTION_ASYNC                       = 30;
  INTERNET_OPTION_SECURITY_FLAGS              = 31;
  INTERNET_OPTION_SECURITY_CERTIFICATE_STRUCT = 32;
  INTERNET_OPTION_DATAFILE_NAME               = 33;
  INTERNET_OPTION_URL                         = 34;
  INTERNET_OPTION_SECURITY_CERTIFICATE        = 35;
  INTERNET_OPTION_SECURITY_KEY_BITNESS        = 36;
  INTERNET_OPTION_REFRESH                     = 37;
  INTERNET_OPTION_PROXY                       = 38;
  INTERNET_OPTION_SETTINGS_CHANGED            = 39;
  INTERNET_OPTION_VERSION                     = 40;
  INTERNET_OPTION_USER_AGENT                  = 41;
  INTERNET_OPTION_END_BROWSER_SESSION         = 42;
  INTERNET_OPTION_PROXY_USERNAME              = 43;
  INTERNET_OPTION_PROXY_PASSWORD              = 44;
  INTERNET_OPTION_CONTEXT_VALUE               = 45;
  INTERNET_OPTION_CONNECT_LIMIT               = 46;
  INTERNET_OPTION_SECURITY_SELECT_CLIENT_CERT = 47;
  INTERNET_OPTION_POLICY                      = 48;
  INTERNET_OPTION_DISCONNECTED_TIMEOUT        = 49;
  INTERNET_OPTION_CONNECTED_STATE             = 50;
  INTERNET_OPTION_IDLE_STATE                  = 51;
  INTERNET_OPTION_OFFLINE_SEMANTICS           = 52;
  INTERNET_OPTION_SECONDARY_CACHE_KEY         = 53;
  INTERNET_OPTION_CALLBACK_FILTER             = 54;
  INTERNET_OPTION_CONNECT_TIME                = 55;
  INTERNET_OPTION_SEND_THROUGHPUT             = 56;
  INTERNET_OPTION_RECEIVE_THROUGHPUT          = 57;
  INTERNET_OPTION_REQUEST_PRIORITY            = 58;
  INTERNET_OPTION_HTTP_VERSION                = 59;
  INTERNET_OPTION_RESET_URLCACHE_SESSION      = 60;
  INTERNET_OPTION_ERROR_MASK                  = 62;
  INTERNET_FIRST_OPTION                      = INTERNET_OPTION_CALLBACK;
  INTERNET_LAST_OPTION                       = INTERNET_OPTION_PROXY;


  QUERY_IS_SECURE          =13;
  QUERY_IS_SAFE            =14;
  QUERY_USES_HISTORYFOLDER =15;


implementation

end.


