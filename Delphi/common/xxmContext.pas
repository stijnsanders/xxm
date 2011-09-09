unit xxmContext;

interface

uses Windows, SysUtils, Classes, ActiveX, xxm, xxmPReg, xxmHeaders, xxmParams;

const
  XxmMaxIncludeDepth=64;//TODO: setting?

type
  TXxmGeneralContext=class(TInterfacedObject, IXxmContext, IxxmParameterCollection) //abstract!
  private
    FProjectEntry: TXxmProjectEntry;
    FPage, FBuilding: IXxmFragment;
    FStatusCode, FIncludeDepth: integer;
    FStatusText, FSingleFileSent: WideString;
    FHeaderSent: boolean;
    FParams: TXxmReqPars;
  protected
    FURL, FContentType, FProjectName, FPageClass, FFragmentName: WideString;
    FAutoEncoding: TXxmAutoEncoding;
    FPostData: TStream;
    FPostTempFile: AnsiString;
    StatusSet: boolean;

    { IXxmContext }
    function GetURL: WideString;
    function GetPage: IXxmFragment;
    function GetContentType: WideString;
    procedure SetContentType(const Value: WideString);
    function GetAutoEncoding: TXxmAutoEncoding;
    procedure SetAutoEncoding(const Value: TXxmAutoEncoding);
    function GetParameter(Key: OleVariant): IXxmParameter;
    function GetParameterCount: Integer;
    //TODO: progress
    procedure Send(Data: OleVariant); overload;
    procedure Send(Value: integer); overload;
    procedure Send(Value: int64); overload;
    procedure Send(Value: cardinal); overload;
    procedure Send(const Values:array of OleVariant); overload;
    procedure SendHTML(Data: OleVariant); overload;
    procedure SendHTML(const Values:array of OleVariant); overload;
    procedure SendFile(FilePath: WideString);
    function PostData: IStream;
    procedure SetStatus(Code: Integer; Text: WideString); virtual;
    procedure Include(Address: WideString); overload;
    procedure Include(Address: WideString;
      const Values: array of OleVariant); overload;
    procedure Include(Address: WideString;
      const Values: array of OleVariant;
      const Objects: array of TObject); overload;

    //abstract methods, inheriters need to implement these
    function GetSessionID: WideString; virtual; abstract;
    procedure DispositionAttach(FileName: WideString); virtual; abstract;
    procedure SendRaw(Data: WideString); virtual; abstract;
    procedure SendStream(s: IStream); virtual; abstract;
    function ContextString(cs: TXxmContextString): WideString; virtual; abstract;
    function Connected: Boolean; virtual; abstract;
    procedure Redirect(RedirectURL: WideString; Relative:boolean); virtual; abstract;
    function GetCookie(Name: WideString): WideString; virtual; abstract;
    procedure SetCookie(Name: WideString; Value: WideString); overload; virtual; abstract;
    procedure SetCookie(Name,Value:WideString; KeepSeconds:cardinal;
      Comment,Domain,Path:WideString; Secure,HttpOnly:boolean); overload; virtual; abstract;

    { IxxmParameterCollection }
    procedure AddParameter(Param: IUnknown);//IxxmParameter

    {  }
    function GetProjectEntry(ProjectName: WideString):TXxmProjectEntry; virtual; abstract;
    procedure SendHeader; virtual; abstract;
    procedure AddResponseHeader(Name, Value: WideString); virtual; abstract;

    function GetProjectPage(FragmentName: WideString):IXxmFragment; virtual;
    procedure CheckHeaderNotSent;
    function CheckSendStart:boolean;

    procedure SendError(res:AnsiString;vals:array of AnsiString);
    procedure ForceStatus(Code: Integer; Text: WideString);

    procedure BuildPage;

    property ProjectEntry: TXxmProjectEntry read FProjectEntry;
  public
    //abstract! constructor only here for private variable init
    constructor Create(URL:WideString);
    destructor Destroy; override;
    //property URL:WideString read GetURL;
    property ContentType:WideString read FContentType;
    property StatusCode:integer read FStatusCode;
    property StatusText:WideString read FStatusText;
    property SingleFileSent:WideString read FSingleFileSent;
  end;

  EXxmAutoBuildFailed=class(Exception);
  EXxmDirectInclude=class(Exception);
  EXxmIncludeStackFull=class(Exception);
  EXxmIncludeFragmentNotFound=class(Exception);

var
  //see xxmSettings
  StatusBuildError,StatusException,StatusFileNotFound:integer;

const
  Utf8ByteOrderMark=#$EF#$BB#$BF;
  Utf16ByteOrderMark=#$FF#$FE;

implementation

uses Variants, xxmCommonUtils, xxmParUtils;

const //resourcestring?
  SXxmDirectInclude='Direct call to include fragment is not allowed.';
  SXxmIncludeStackFull='Maximum level of includes exceeded';
  SXxmIncludeFragmentNotFound='Include fragment not found "__"';
  
{ TXxmGeneralContext }

constructor TXxmGeneralContext.Create(URL: WideString);
begin
  inherited Create;
  FURL:=URL;
  FProjectEntry:=nil;
  FContentType:='text/html';//default (setting?)
  FAutoEncoding:=aeUtf8;//default (setting?)
  FParams:=nil;//see GetParameter
  FPostData:=nil;
  FPostTempFile:='';
  FPage:=nil;
  FBuilding:=nil;
  FPageClass:='';
  FHeaderSent:=false;
  FIncludeDepth:=0;
  FStatusCode:=200;//default
  FStatusText:='OK';//default
  StatusSet:=false;
  FProjectName:='';//parsed from URL later
  FFragmentName:='';//parsed from URL later
end;

destructor TXxmGeneralContext.Destroy;
begin
  if FProjectEntry<>nil then
   begin
    FProjectEntry.CloseContext;
    FProjectEntry:=nil;
   end;
  FreeAndNil(FPostData);
  try
    if FPostTempFile<>'' then
     begin
      DeleteFile(FPostTempFile);
      FPostTempFile:='';
     end;
  except
    //silent
  end;
  FreeAndNil(FParams);
  inherited;
end;

function TXxmGeneralContext.GetURL: WideString;
begin
  Result:=FURL;
end;

function TXxmGeneralContext.GetProjectPage(FragmentName: WideString): IXxmFragment;
begin
  Result:=FProjectEntry.Project.LoadPage(Self,FragmentName);
end;

procedure TXxmGeneralContext.BuildPage;
var
  x:WideString;
  p:IXxmPage;
  f:TFileStream;
  fs:Int64;
  d:TDateTime;
begin
  FProjectEntry:=GetProjectEntry(FProjectName);
  if @XxmAutoBuildHandler<>nil then
    if not(XxmAutoBuildHandler(FProjectEntry,Self,FProjectName)) then
     begin
      FProjectEntry:=nil;
      raise EXxmAutoBuildFailed.Create(FProjectName);
     end;
  FProjectEntry.OpenContext;
  FPage:=GetProjectPage(FFragmentName);
  if FPage=nil then
   begin
    //find a file
    //ask project to translate? project should have given a fragment!
    FPageClass:='['+FProjectName+']GetFilePath';
    FProjectEntry.GetFilePath(FFragmentName,FSingleFileSent,x);
    d:=GetFileModifiedDateTime(FSingleFileSent,fs);
    if d<>0 then //FileExists(FSingleFileSent)
     begin
      //TODO: if directory file-list?
      FContentType:=x;
      f:=TFileStream.Create(FSingleFileSent,fmOpenRead or fmShareDenyNone);
      try
        AddResponseHeader('Last-Modified',RFC822DateGMT(d));
        AddResponseHeader('Content-Length',IntToStr(fs));
        SendStream(TStreamAdapter.Create(f,soReference));
      finally
        f.Free;
      end;
     end
    else
     begin
      FPageClass:='['+FProjectName+']404:'+FFragmentName;
      FPage:=FProjectEntry.Project.LoadPage(Self,'404.xxm');
      if FPage=nil then
       begin
        FStatusCode:=StatusFileNotFound;
        FStatusText:='File not found';
        SendError('fnf',[
          'URL',HTMLEncode(FURL),
          'PROJECT',FProjectName,
          'ADDRESS',FFragmentName,
          'PATH',HTMLEncode(FSingleFileSent),
          'VERSION',ContextString(csVersion)
        ]);
       end
      else
        try
          FPageClass:=FPage.ClassNameEx;
          FBuilding:=FPage;
          FPage.Build(Self,nil,[FFragmentName,FSingleFileSent,x],[]);//any parameters?
        finally
          FBuilding:=nil;
          //let project free, cache or recycle
          FProjectEntry.Project.UnloadFragment(FPage);
          FPage:=nil;
        end;
     end;
   end
  else
    try
      FPageClass:=FPage.ClassNameEx;
      //mime type moved to CheckSendStart;
      //OleCheck(ProtSink.ReportProgress(BINDSTATUS_CACHEFILENAMEAVAILABLE,));
      //TODO: cache output?

      //TODO: setting?
      if FPage.QueryInterface(IID_IXxmPage,p)<>S_OK then
        raise EXxmDirectInclude.Create(SXxmDirectInclude);
      p:=nil;

      //build page
      FBuilding:=FPage;
      FPage.Build(Self,nil,[],[]);//any parameters?

    finally
      FBuilding:=nil;
      //let project decide to free or not
      FProjectEntry.Project.UnloadFragment(FPage);
      FPage:=nil;
    end;
end;

function TXxmGeneralContext.GetPage: IXxmFragment;
begin
  Result:=FPage;
end;

function TXxmGeneralContext.CheckSendStart: boolean;
begin
  //FAutoEncoding: see SendHTML
  Result:=not(FHeaderSent);
  if Result then
   begin
    SendHeader;
    FHeaderSent:=true;
   end
  else
    FSingleFileSent:='';
end;

procedure TXxmGeneralContext.CheckHeaderNotSent;
begin
  if FHeaderSent then
    raise EXxmResponseHeaderAlreadySent.Create(SXxmResponseHeaderAlreadySent);
end;

procedure TXxmGeneralContext.SendError(res: AnsiString;
  vals: array of AnsiString);
var
  s:AnsiString;
  i:integer;
  r:TResourceStream;
  l:Int64;
const
  RT_HTML = MakeIntResource(23);
begin
  r:=TResourceStream.Create(HInstance,res,RT_HTML);
  try
    l:=r.Size;
    SetLength(s,l);
    r.Read(s[1],l);
  finally
    r.Free;
  end;
  for i:=0 to (Length(vals) div 2)-1 do
    s:=StringReplace(s,'[['+vals[i*2]+']]',vals[i*2+1],[rfReplaceAll]);
  if not(FHeaderSent) then
   begin
    FContentType:='text/html';
    FAutoEncoding:=aeContentDefined;//?
   end;
  SendHTML(s);
end;

function TXxmGeneralContext.GetContentType: WideString;
begin
  Result:=FContentType;
end;

procedure TXxmGeneralContext.SetContentType(const Value: WideString);
begin
  CheckHeaderNotSent;
  FContentType:=Value;
  FAutoEncoding:=aeContentDefined;//parse from value? (charset)
end;

function TXxmGeneralContext.GetAutoEncoding: TXxmAutoEncoding;
begin
  Result:=FAutoEncoding;
end;

procedure TXxmGeneralContext.SetAutoEncoding(const Value: TXxmAutoEncoding);
begin
  CheckHeaderNotSent;
  FAutoEncoding:=Value;
end;

procedure TXxmGeneralContext.SetStatus(Code: Integer; Text: WideString);
begin
  CheckHeaderNotSent;
  FStatusCode:=Code;
  FStatusText:=Text;
  StatusSet:=true;
end;

procedure TXxmGeneralContext.ForceStatus(Code: Integer; Text: WideString);
begin
  //use from exception handling only
  FStatusCode:=Code;
  FStatusText:=Text;
end;

procedure TXxmGeneralContext.Include(Address: WideString);
begin
  Include(Address, [], []);
end;

procedure TXxmGeneralContext.Include(Address: WideString;
  const Values: array of OleVariant);
begin
  Include(Address, Values, []);
end;

procedure TXxmGeneralContext.Include(Address: WideString;
  const Values: array of OleVariant; const Objects: array of TObject);
var
  f,fb:IXxmFragment;
  pc:AnsiString;
begin
  if FIncludeDepth=XxmMaxIncludeDepth then
    raise EXxmIncludeStackFull.Create(SXxmIncludeStackFull);
  //FPage.Project?
  f:=FProjectEntry.Project.LoadFragment(Self,Address,FBuilding.RelativePath);
  if f=nil then
    raise EXxmIncludeFragmentNotFound.Create(StringReplace(
      SXxmIncludeFragmentNotFound,'__',Address,[]));
  fb:=FBuilding;
  pc:=FPageClass;
  FBuilding:=f;
  inc(FIncludeDepth);
  try
    FPageClass:=f.ClassNameEx+' < '+pc;
    f.Build(Self,fb,Values,Objects);//queue to avoid building up stack?
    FPageClass:=pc;
  finally
    dec(FIncludeDepth);
    FBuilding:=fb;
    fb:=nil;
    FProjectEntry.Project.UnloadFragment(f);
    f:=nil;
  end;
end;

function VarToWideStrX(const V: Variant): WideString;
var
  p:IXxmParameter;
begin
  case VarType(V) and varTypeMask of
    varNull:Result:='';
    varUnknown:
      if IUnknown(v).QueryInterface(IID_IXxmParameter,p)=S_OK then
        Result:=p.Value
      else
        Result:=V //throw default exception
    else
      Result:=V;
  end;
end;

procedure TXxmGeneralContext.SendHTML(Data: OleVariant);
begin
  SendRaw(VarToWideStrX(Data));
end;

procedure TXxmGeneralContext.Send(Data: OleVariant);
begin
  SendRaw(HTMLEncode(VarToWideStrX(Data)));
end;

procedure TXxmGeneralContext.SendFile(FilePath: WideString);
var
  b:boolean;
begin
  inherited;
  //TODO: auto mimetype by extension?
  b:=not(FHeaderSent);
  SendStream(TStreamAdapter.Create(TFileStream.Create(FilePath,fmOpenRead or fmShareDenyNone),soOwned));//does CheckSendStart
  if b then FSingleFileSent:=FilePath;
end;

procedure TXxmGeneralContext.Send(Value: integer);
begin
  SendRaw(IntToStr(Value));
end;

procedure TXxmGeneralContext.Send(Value: int64);
begin
  SendRaw(IntToStr(Value));
end;

procedure TXxmGeneralContext.Send(Value: cardinal);
begin
  SendRaw(IntToStr(Value));
end;

procedure TXxmGeneralContext.Send(const Values: array of OleVariant);
var
  i:integer;
begin
  for i:=0 to Length(Values)-1 do SendRaw(HTMLEncode(Values[i]));
end;

procedure TXxmGeneralContext.SendHTML(const Values: array of OleVariant);
var
  i:integer;
begin
  for i:=0 to Length(Values)-1 do SendRaw(VarToWideStrX(Values[i]));
end;

function TXxmGeneralContext.PostData: IStream;
begin
  if FPostData=nil then Result:=nil else
    Result:=TStreamAdapter.Create(FPostData,soReference);
end;

function TXxmGeneralContext.GetParameter(Key: OleVariant): IXxmParameter;
var
  iKey:integer;
begin
  //parse parameters on first use
  if FParams=nil then
   begin
    FParams:=TXxmReqPars.Create(Self,FPostData);
    //redirect on post? invalidate postdata!
    if FParams.PostDataOnRedirect then FreeAndNil(FPostData);
   end;
  if VarIsNumeric(Key) then
   begin
    iKey:=integer(Key);
    if (iKey>cs_Max) and (iKey<=csVersion) then
      Result:=TXxmContextStringPar.Create(
        'Context.ContextString('+VarToStr(Key)+')',ContextString(iKey)) as IXxmParameter
    else
      Result:=FParams.GetItem(iKey);
   end
  else
    Result:=FParams.Get(VarToWideStr(Key));
end;

function TXxmGeneralContext.GetParameterCount: Integer;
begin
  if FParams=nil then FParams:=TXxmReqPars.Create(Self,FPostData);
  Result:=FParams.Count;
end;

procedure TXxmGeneralContext.AddParameter(Param: IInterface);
begin
  if FParams=nil then FParams:=TXxmReqPars.Create(Self,FPostData);
  FParams.Add(Param as IXxmParameter);
end;

end.
