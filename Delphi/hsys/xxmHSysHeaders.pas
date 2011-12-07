unit xxmHSysHeaders;

interface

uses
  xxmHeaders, httpapi1;

  {
type
  TxxmHSysResponseHeaders=class(TInterfacedObject, IxxmDictionary, IxxmDictionaryEx)
  private
    xxx
  protected
    function GetItem(Name: OleVariant): WideString;
    procedure SetItem(Name: OleVariant; const Value: WideString);
    function GetName(Idx: integer): WideString;
    procedure SetName(Idx: integer; Value: WideString);
    function GetCount:integer;
    function Complex(Name: OleVariant; out Items: IxxmDictionary): WideString;
  public
    constructor Create();
  end;
  }

const
  HttpRequestHeaderName:array[THTTP_HEADER_ID] of AnsiString=(
    'Cache-Control',
    'Connection',
    'Date',
    'Keep-Alive',
    'Pragma',
    'Trailer',
    'Transfer-Encoding',
    'Upgrade',
    'Via',
    'Warning',
    'Allow',
    'Content-Length',
    'Content-Type',
    'Content-Encoding',
    'Content-Language',
    'Content-Location',
    'Content-MD5',
    'Content-Range',
    'Expires',
    'Last-Modified',
    'Accept',
    'Accept-Charset',
    'Accept-Encoding',
    'Accept-Language',
    'Authorization',
    'Cookie',
    'Expect',
    'From',
    'Host',
    'If-Match',
    'If-Modified-Since',
    'If-None-Match',
    'If-Range',
    'If-Unmodified-Since',
    'Max-Forwards',
    'Proxy-Authorization',
    'Referer',
    'Range',
    'TE',
    'Translate',
    'User-Agent');

const
  HttpResponseHeaderName:array[THTTP_HEADER_ID] of AnsiString=(
    'Cache-Control',
    'Connection',
    'Date',
    'Keep-Alive',
    'Pragma',
    'Trailer',
    'Transfer-Encoding',
    'Upgrade',
    'Via',
    'Warning',
    'Allow',
    'Content-Length',
    'Content-Type',
    'Content-Encoding',
    'Content-Language',
    'Content-Location',
    'Content-MD5',
    'Content-Range',
    'Expires',
    'Last-Modified',
    'Accept-Ranges',
    'Age',
    'ETag',
    'Location',
    'Proxy-Authenticate',
    'Retry-After',
    'Server',
    'Set-Cookie',
    'Vary',
    'WWW-Authenticate',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '');

implementation

end.
