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
    const Values:array of OleVariant;const Objects: array of TObject); stdcall;
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

implementation

end.

