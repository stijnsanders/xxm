unit xxm2;

interface

const
  //$Date$
  XxmRevision = '$Rev$';
  XxmAPILevel = 2000;//v2.0

type
  PxxmContext=type pointer; //opaque
  PxxmProject=type pointer; //opaque

  CxxmFragment=procedure (Context:PxxmContext;
    const Values:array of Variant;const Objects:array of pointer); stdcall;

  PxxmParameter=type pointer;

  TxxmAutoEncoding = (
    aeContentDefined, //content will specify which content to use
    aeUtf8,           //send UTF-8 byte order mark
    aeUtf16,          //send UTF-16 byte order mark
    aeIso8859         //send using the closest new thing to ASCII
  );

  TxxmContextString=type NativeInt; // enumeration

const
  csVersion           = -1000;
  csProjectName       = -1001;
  csURL               = -1002;
  csLocalURL          = -1003;
  csVerb              = -1004;
  csExtraInfo         = -1005;
  csUserAgent         = -1006;
  csQueryString       = -1007;
  csPostMimeType      = -1008;
  csReferer           = -1009;
  csLanguage          = -1010;
  csAcceptedMimeTypes = -1011;
  csRemoteAddress     = -1012;
  csRemoteHost        = -1013;
  csAuthUser          = -1014;
  csAuthPassword      = -1015;

type
  CxxmProgress=procedure (Context:PxxmContext;FieldName,FileName:PUTF8Char;
    RequestID,Position:NativeUInt); stdcall;

const
  xxmProgress_PostData   = 1;
  xxmProgress_FileFields = 2;

type
  TContext_URL=function (Context:PxxmContext):PUTF8Char; stdcall;
  TContext_SessionID=function (Context:PxxmContext):PUTF8Char; stdcall;

  TContext_ContextString = function (Context:PxxmContext;Value:TxxmContextString):PUTF8Char; stdcall;
  TContext_BufferSize = function (Context:PxxmContext):NativeUInt; stdcall;
  TContext_Set_BufferSize = procedure (Context:PxxmContext;Value:NativeUInt); stdcall;
  TContext_Connected = function (Context:PxxmContext):boolean; stdcall;

  TContext_Set_Status = procedure (Context:PxxmContext;Status:word;Text:PUTF8Char); stdcall;
  TContext_Redirect = procedure (Context:PxxmContext;RedirectURL:PUTF8Char;
    Relative:boolean); stdcall;
  TContext_Cookie = function (Context:PxxmContext;Name:PUTF8Char):PUTF8Char; stdcall;
  TContext_Set_Cookie = procedure (Context:PxxmContext;Name,Value:PUTF8Char); stdcall;
  TContext_Set_CookieEx = procedure (Context:PxxmContext;Name,Value:PUTF8Char;
    KeepSeconds:NativeUInt;Comment,Domain,Path:PUTF8Char;Secure,HttpOnly:boolean); stdcall;

  TContext_ContentType = function (Context:PxxmContext):PUTF8Char; stdcall;
  TContext_Set_ContentType = procedure (Context:PxxmContext;ContentType:PUTF8Char); stdcall;
  TContext_AutoEncoding = function (Context:PxxmContext):TxxmAutoEncoding; stdcall;
  TContext_Set_AutoEncoding = procedure (Context:PxxmContext;AutoEncoding:TxxmAutoEncoding); stdcall;
  TContext_DispositionAttach = procedure (Context:PxxmContext;FileName:PUTF8Char); stdcall;

  TContext_Send = procedure (Context:PxxmContext;Data:PUTF8Char); stdcall;
  TContext_SendHTML = procedure (Context:PxxmContext;HTML:PUTF8Char); stdcall;
  TContext_SendFile = procedure (Context:PxxmContext;FilePath:PUTF8Char); stdcall;
  TContext_SendStream = procedure (Context:PxxmContext;Stream:TObject); stdcall;
  TContext_Flush = procedure (Context:PxxmContext); stdcall;

  TContext_Parameter = function (Context:PxxmContext;Name:PUTF8Char):PxxmParameter; stdcall;
  TContext_ParameterCount = function (Context:PxxmContext):NativeUInt; stdcall;
  TContext_ParameterByIdx = function (Context:PxxmContext;Index:NativeUInt):PxxmParameter; stdcall;
  TContext_Add_Parameter = procedure (Context:PxxmContext;Origin,Name,Value:PUTF8Char); stdcall;

  TContext_RequestHeader = function (Context:PxxmContext;Name:PUTF8Char):PUTF8Char; stdcall;
  TContext_RequestHeaderCount = function (Context:PxxmContext):NativeUInt; stdcall;
  TContext_RequestHeaderByIdx = procedure (Context:PxxmContext;Index:NativeUInt;var Name,Value:PUTF8Char); stdcall;
  TContext_ResponseHeader = function (Context:PxxmContext;Name:PUTF8Char):PUTF8Char; stdcall;
  TContext_ResponseHeaderCount = function (Context:PxxmContext):NativeUInt; stdcall;
  TContext_ResponseHeaderByIdx = procedure (Context:PxxmContext;Index:NativeUInt;var Name,Value:PUTF8Char); stdcall;
  TContext_Set_ResponseHeader = procedure (Context:PxxmContext;Name,Value:PUTF8Char); stdcall;

  TContext_Data = function (Context:PxxmContext):pointer; stdcall;
  TContext_Set_Data = procedure (Context:PxxmContext;Data:pointer); stdcall;

  TContext_Include = procedure (Context:PxxmContext;Address:PUTF8Char;
    const Values:array of Variant;const Objects:array of TObject); stdcall;
  TContext_PostData = function (Context:PxxmContext):TObject; stdcall;
  TContext_Set_ProgressCallback = procedure (Context:PxxmContext;Callback:CxxmProgress;
    RequestID,Flags,Step:NativeUInt); stdcall;

  TParameter_Origin = function (Parameter:PxxmParameter):PUTF8Char; stdcall; //'GET','POST','FILE'...
  TParameter_Name = function (Parameter:PxxmParameter):PUTF8Char; stdcall;
  TParameter_Value = function (Parameter:PxxmParameter):PUTF8Char; stdcall;
  TParameter_AsInteger = function (Parameter:PxxmParameter):NativeInt ; stdcall;
  TParameter_NextBySameName = function (Parameter:PxxmParameter):PxxmParameter; stdcall;

  TParameter_ContentType = function (Parameter:PxxmParameter):PUTF8Char; stdcall;
  TParameter_SaveToFile = function (Parameter:PxxmParameter;FilePath:PUTF8Char):NativeUInt; stdcall;
  TParameter_SaveToStream = function (Parameter:PxxmParameter;Stream:TObject):NativeUInt; stdcall;


  Txxm2 = record
    Context_APILevel: NativeInt;
    Context_URL: TContext_URL;
    Context_SessionID: TContext_SessionID;

    Context_ContextString: TContext_ContextString;
    Context_BufferSize: TContext_BufferSize;
    Context_Set_BufferSize: TContext_Set_BufferSize;
    Context_Connected: TContext_Connected;

    Context_Set_Status: TContext_Set_Status;
    Context_Redirect: TContext_Redirect;
    Context_Cookie: TContext_Cookie;
    Context_Set_Cookie: TContext_Set_Cookie;
    Context_Set_CookieEx: TContext_Set_CookieEx;

    Context_ContentType: TContext_ContentType;
    Context_Set_ContentType: TContext_Set_ContentType;
    Context_AutoEncoding: TContext_AutoEncoding;
    Context_Set_AutoEncoding: TContext_Set_AutoEncoding;
    Context_DispositionAttach: TContext_DispositionAttach;

    Context_Send: TContext_Send;
    Context_SendHTML: TContext_SendHTML;
    Context_SendFile: TContext_SendFile;
    Context_SendStream: TContext_SendStream;
    Context_Flush: TContext_Flush;

    Context_Parameter: TContext_Parameter;
    Context_ParameterCount: TContext_ParameterCount;
    Context_ParameterByIdx: TContext_ParameterByIdx;
    Context_Add_Parameter: TContext_Add_Parameter;

    Context_RequestHeader: TContext_RequestHeader;
    Context_RequestHeaderCount: TContext_RequestHeaderCount;
    Context_RequestHeaderByIdx: TContext_RequestHeaderByIdx;
    Context_ResponseHeader: TContext_ResponseHeader;
    Context_ResponseHeaderCount: TContext_ResponseHeaderCount;
    Context_ResponseHeaderByIdx: TContext_ResponseHeaderByIdx;
    Context_Set_ResponseHeader: TContext_Set_ResponseHeader;

    Context_Data: TContext_Data;
    Context_Set_Data: TContext_Set_Data;

    Context_Include: TContext_Include;
    Context_PostData: TContext_PostData;
    Context_Set_ProgressCallback: TContext_Set_ProgressCallback;

    Parameter_Origin: TParameter_Origin;
    Parameter_Name: TParameter_Name;
    Parameter_Value: TParameter_Value;
    Parameter_AsInteger: TParameter_AsInteger;
    Parameter_NextBySameName: TParameter_NextBySameName;

    Parameter_ContentType: TParameter_ContentType;
    Parameter_SaveToFile: TParameter_SaveToFile;
    Parameter_SaveToStream: TParameter_SaveToStream;

    //TODO: IxxmContextSuspend
    //TODO: IxxmProjectEvents2
    //TODO: IxxmRawSocket
    //TODO: IxxmSocketSuspend

  end;

  Pxxm2 = ^Txxm2;

type
  //expected export 'XxmInitialize'
  FxxmInitialize=function(APILevel: NativeUInt; xxm2: Pxxm2;
    const AProjectName: PUTF8Char):PxxmProject; stdcall;

  //expected export 'XxmPage'
  FxxmPage=function (Project: PxxmProject; Context: PxxmContext;
    Address: PUTF8Char): CxxmFragment; stdcall;
  //optional export 'XxmFragment'
  FxxmFragment=function (Project: PxxmProject; Context: PxxmContext;
    Address: PUTF8Char): CxxmFragment; stdcall;

  //optional export 'XxmClearContext'
  FxxmClearContext=procedure (Project: PxxmProject; Context: PxxmContext); stdcall;
  //optional export 'XxmHandleException'
  FxxmHandleException=function (Project: PxxmProject; Context: PxxmContext;
    PageClass, ExceptionClass, ExceptionMessage: PUTF8Char): boolean; stdcall;

  //optional export 'XxmReleasingContexts'
  FxxmReleasingContexts=procedure (Project: PxxmProject); stdcall;
  //optional export 'XxmReleasingProject'
  FxxmReleasingProject=procedure (Project: PxxmProject); stdcall;



// For use by xxm projects:

var
  xxm:Pxxm2;

type
  CxxmParameter=record
    __Parameter:PxxmParameter;
    function Origin:UTF8String; inline; //'GET','POST','FILE'...
    function Name:UTF8String; inline;
    function Value:UTF8String; inline;
    function AsInteger:NativeInt; inline;
    function NextBySameName:CxxmParameter; inline;

    function ContentType:UTF8String; inline;
    function SaveToFile(const FilePath:UTF8String):NativeUInt; inline;
    function SaveToStream(Stream:TObject):NativeUInt; inline;//TStream
    class operator Implicit(c:CxxmParameter):pointer;
  end;

  CxxmContext=record
    __Context:PxxmContext;
    function APILevel:NativeInt; inline;
    function URL:UTF8String; inline;
    function SessionID:UTF8String; inline;

    function ContextString(Value:TxxmContextString):UTF8String; inline;
    function Get_BufferSize:NativeUInt; inline;
    procedure Set_BufferSize(Value:NativeUInt); inline;
    property BufferSize:NativeUInt read Get_BufferSize write Set_BufferSize;
    function Connected:boolean; inline;
    procedure SetStatus(Status:word;const Text:UTF8String); inline;
    procedure Redirect(const RedirectURL:UTF8String;Relative:boolean); inline;
    function GetCookie(const Name:UTF8String):UTF8String; inline;
    procedure SetCookie(const Name,Value:UTF8String); inline;
    property Cookie[const Name:UTF8String]:UTF8String read GetCookie write SetCookie;
    procedure SetCookieEx(const Name,Value:UTF8String;KeepSeconds:NativeUInt;
      const Comment,Domain,Path:UTF8String;Secure,HttpOnly:boolean); inline;

    function GetContentType:UTF8String; inline;
    procedure SetContentType(const ContentType:UTF8String); inline;
    property ContentType:UTF8String read GetContentType write SetContentType;
    function GetAutoEncoding:TxxmAutoEncoding; inline;
    procedure SetAutoEncoding(AutoEncoding:TxxmAutoEncoding); inline;
    procedure DispositionAttach(const FileName:UTF8String); inline;

    procedure Send(const Data:UTF8String); inline;
    procedure SendHTML(const HTML:UTF8String); inline;
    procedure SendFile(const FilePath:UTF8String); inline;
    procedure SendStream(Stream:TObject); inline;
    procedure Flush; inline;

    function GetParameter(const Name:UTF8String):CxxmParameter; overload; inline;
    function GetParameter(Index:NativeUInt):CxxmParameter; overload; inline;
    function ParameterCount:NativeUInt; inline;
    property Parameter[const Name:UTF8String]:CxxmParameter read GetParameter; default;//overload;
    //property Parameter[Index:NativeUInt]:CxxmParameter read GetParameter; //overload;

    function GetRequestHeader(const Name:UTF8String):UTF8String; overload; inline;
    property RequestHeader[const Name:UTF8String]:UTF8String read GetRequestHeader;
    function RequestHeaderCount:NativeUInt; inline;
    procedure GetRequestHeader(Index:NativeUInt;var Name,Value:UTF8String); overload; inline;
    function GetResponseHeader(const Name:UTF8String):UTF8String; overload; inline;
    function ResponseHeaderCount:NativeUInt; inline;
    procedure GetResponseHeader(Index:NativeUInt;var Name,Value:UTF8String); overload; inline;
    procedure SetResponseHeader(const Name,Value:UTF8String); inline;
    property ResponseHeader[const Name:UTF8String]:UTF8String read GetResponseHeader write SetResponseHeader;

    function GetData:pointer; inline;
    procedure SetData(Data:pointer); inline;
    property Data:pointer read GetData write SetData;

    procedure Include(const Address:UTF8String); overload; inline;
    procedure Include(const Address:UTF8String;
      const Values:array of Variant); overload;
    procedure Include(const Address:UTF8String;
      const Values:array of Variant;const Objects:array of TObject); overload;

    class operator Implicit(c:CxxmContext):pointer;
  end;

implementation

{ CxxmContext }

function CxxmContext.APILevel: NativeInt;
begin
  Result:=xxm.Context_APILevel;
end;

function CxxmContext.URL: UTF8String;
begin
  Result:=xxm.Context_URL(__Context);
end;

function CxxmContext.SessionID: UTF8String;
begin
  Result:=xxm.Context_SessionID(__Context);
end;

function CxxmContext.ContextString(Value: TxxmContextString): UTF8String;
begin
  Result:=xxm.Context_ContextString(__Context,Value);
end;

function CxxmContext.Get_BufferSize: NativeUInt;
begin
  Result:=xxm.Context_BufferSize(__Context);
end;

procedure CxxmContext.Set_BufferSize(Value: NativeUInt);
begin
  xxm.Context_Set_BufferSize(__Context,Value);
end;

function CxxmContext.Connected: boolean;
begin
  Result:=xxm.Context_Connected(__Context);
end;

procedure CxxmContext.SetStatus(Status: word; const Text: UTF8String);
begin
  xxm.Context_Set_Status(__Context,Status,PUTF8Char(Text));
end;

procedure CxxmContext.Redirect(const RedirectURL: UTF8String; Relative: boolean);
begin
  xxm.Context_Redirect(__Context,PUTF8Char(RedirectURL),Relative);
end;

function CxxmContext.GetCookie(const Name: UTF8String): UTF8String;
begin
  Result:=xxm.Context_Cookie(__Context,PUTF8Char(Name));
end;

procedure CxxmContext.SetCookie(const Name, Value: UTF8String);
begin
  xxm.Context_Set_Cookie(__Context,PUTF8Char(Name),PUTF8Char(Value));
end;

procedure CxxmContext.SetCookieEx(const Name, Value: UTF8String;
  KeepSeconds: NativeUInt; const Comment, Domain, Path: UTF8String; Secure,
  HttpOnly: boolean);
begin
  xxm.Context_Set_CookieEx(__Context,PUTF8Char(Name),PUTF8Char(Value),
    KeepSeconds,PUTF8Char(Comment),PUTF8Char(Domain),PUTF8Char(Path),
    Secure,HttpOnly);
end;

function CxxmContext.GetContentType: UTF8String;
begin
  Result:=xxm.Context_ContentType(__Context);
end;

procedure CxxmContext.SetContentType(const ContentType: UTF8String);
begin
  xxm.Context_Set_ContentType(__Context,PUTF8Char(ContentType));
end;

function CxxmContext.GetAutoEncoding: TxxmAutoEncoding;
begin
  Result:=xxm.Context_AutoEncoding(__Context);
end;

procedure CxxmContext.SetAutoEncoding(AutoEncoding: TxxmAutoEncoding);
begin
  xxm.Context_Set_AutoEncoding(__Context,AutoEncoding);
end;

procedure CxxmContext.DispositionAttach(const FileName: UTF8String);
begin
  xxm.Context_DispositionAttach(__Context,PUTF8Char(FileName));
end;

procedure CxxmContext.Send(const Data: UTF8String);
begin
  xxm.Context_Send(__Context,PUTF8Char(Data));
end;

procedure CxxmContext.SendHTML(const HTML: UTF8String);
begin
  xxm.Context_SendHTML(__Context,PUTF8Char(HTML));
end;

procedure CxxmContext.SendFile(const FilePath: UTF8String);
begin
  xxm.Context_SendFile(__Context,PUTF8Char(FilePath));
end;

procedure CxxmContext.SendStream(Stream: TObject);
begin
  xxm.Context_SendStream(__Context,Stream);
end;

procedure CxxmContext.Flush;
begin
  xxm.Context_Flush(__Context);
end;

function CxxmContext.GetParameter(const Name: UTF8String): CxxmParameter;
begin
  Result.__Parameter:=xxm.Context_Parameter(__Context,PUTF8Char(Name));
end;

function CxxmContext.ParameterCount: NativeUInt;
begin
  Result:=xxm.Context_ParameterCount(__Context);
end;

function CxxmContext.GetParameter(Index: NativeUInt): CxxmParameter;
begin
  Result.__Parameter:=xxm.Context_ParameterByIdx(__Context,Index);
end;

function CxxmContext.GetRequestHeader(const Name: UTF8String): UTF8String;
begin
  Result:=xxm.Context_ResponseHeader(__Context,PUTF8Char(Name));
end;

function CxxmContext.RequestHeaderCount: NativeUInt;
begin
  Result:=xxm.Context_RequestHeaderCount(__Context);
end;

procedure CxxmContext.GetRequestHeader(Index: NativeUInt; var Name, Value: UTF8String);
var
  n,v:PUTF8Char;
begin
  xxm.Context_RequestHeaderByIdx(__Context,Index,n,v);
  Name:=n;
  Value:=v;
end;

function CxxmContext.GetResponseHeader(const Name: UTF8String): UTF8String;
begin
  Result:=xxm.Context_ResponseHeader(__Context,PUTF8Char(Name));
end;

function CxxmContext.ResponseHeaderCount: NativeUInt;
begin
  Result:=xxm.Context_ResponseHeaderCount(__Context);
end;

procedure CxxmContext.GetResponseHeader(Index: NativeUInt; var Name, Value: UTF8String);
var
  n,v:PUTF8Char;
begin
  xxm.Context_ResponseHeaderByIdx(__Context,Index,n,v);
  Name:=v;
  Value:=v;
end;

procedure CxxmContext.SetResponseHeader(const Name, Value: UTF8String);
begin
  xxm.Context_Set_ResponseHeader(__Context,PUTF8Char(Name),PUTF8Char(Value));
end;

function CxxmContext.GetData: pointer;
begin
  Result:=xxm.Context_Data(__Context);
end;

procedure CxxmContext.SetData(Data: pointer);
begin
  xxm.Context_Set_Data(__Context,Data);
end;

procedure CxxmContext.Include(const Address: UTF8String);
begin
  xxm.Context_Include(__Context,PUTF8Char(Address),[],[]);
end;

procedure CxxmContext.Include(const Address: UTF8String;
  const Values: array of Variant);
begin
  xxm.Context_Include(__Context,PUTF8Char(Address),Values,[]);
end;

procedure CxxmContext.Include(const Address: UTF8String;
  const Values: array of Variant; const Objects: array of TObject);
begin
  xxm.Context_Include(__Context,PUTF8Char(Address),Values,Objects);
end;

{ CxxmParameter }

function CxxmParameter.Origin: UTF8String;
begin
  Result:=xxm.Parameter_Origin(__Parameter);
end;

function CxxmParameter.Name: UTF8String;
begin
  Result:=xxm.Parameter_Name(__Parameter);
end;

function CxxmParameter.Value: UTF8String;
begin
  Result:=xxm.Parameter_Value(__Parameter);
end;

function CxxmParameter.AsInteger: NativeInt;
begin
  Result:=xxm.Parameter_AsInteger(__Parameter);
end;

function CxxmParameter.NextBySameName: CxxmParameter;
begin
  Result.__Parameter:=xxm.Parameter_NextBySameName(__Parameter);
end;

class operator CxxmContext.Implicit(c: CxxmContext): pointer;
begin
  Result:=c.__Context;
end;

function CxxmParameter.ContentType: UTF8String;
begin
  Result:=xxm.Parameter_ContentType(__Parameter);
end;

function CxxmParameter.SaveToFile(const FilePath: UTF8String): NativeUInt;
begin
  Result:=xxm.Parameter_SaveToFile(__Parameter,PUTF8Char(FilePath));
end;

function CxxmParameter.SaveToStream(Stream: TObject): NativeUInt;
begin
  Result:=xxm.Parameter_SaveToStream(__Parameter,Stream);
end;

class operator CxxmParameter.Implicit(c: CxxmParameter): pointer;
begin
  Result:=c.__Parameter;
end;

end.

