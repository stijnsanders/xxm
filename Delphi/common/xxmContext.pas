unit xxmContext;

interface

uses Windows, SysUtils, Classes, ActiveX, xxm, xxmPReg, xxmHeaders, xxmParams;

const
  XxmMaxIncludeDepth=64;//TODO: setting?
  XxmHeaderSent=0;
  XxmHeaderNotSent=1;
  XxmHeaderOnNextFlush=2;

type
  TXxmSendBufHandler=function(const Buffer; Count: LongInt): LongInt of object;

  TXxmGeneralContext=class(TInterfacedObject,
    IXxmContext,
    IxxmParameterCollection,
    IxxmUploadProgressService)
    //abstract!
  private
    FProjectEntry: TXxmProjectEntry;
    FPage, FBuilding: IXxmFragment;
    FStatusCode, FIncludeDepth, FHeaderSent, FBufferSize: integer;
    FStatusText, FSingleFileSent: WideString;
    FParams: TXxmReqPars;
    FIncludeCheck: pointer;//see Include
    FAuthParsed: boolean;
    FAuthUserName, FAuthPassword: AnsiString;
  protected
    FURL, FContentType, FProjectName, FPageClass, FFragmentName: WideString;
    FAutoEncoding: TXxmAutoEncoding;
    FContentBuffer: TMemoryStream;
    FPostData: TStream;
    FPostTempFile: AnsiString;
    SendBuf, SendDirect: TXxmSendBufHandler;
    SettingCookie, BuildPageLeaveOpen: boolean;

    { IXxmContext }
    function GetURL: WideString;
    function GetPage: IXxmFragment;
    function GetContentType: WideString;
    procedure SetContentType(const Value: WideString);
    function GetAutoEncoding: TXxmAutoEncoding;
    procedure SetAutoEncoding(const Value: TXxmAutoEncoding);
    function GetParameter(Key: OleVariant): IXxmParameter;
    function GetParameterCount: Integer;
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
    //function SendDirect(const Buffer; Count: LongInt): LongInt; virtual; abstract;
    function ContextString(cs: TXxmContextString): WideString; virtual; abstract;
    function Connected: Boolean; virtual; abstract;
    procedure Redirect(RedirectURL: WideString; Relative:boolean); virtual; abstract;
    function GetCookie(Name: WideString): WideString; virtual; abstract;
    procedure SetCookie(Name,Value: WideString); overload; virtual;
    procedure SetCookie(Name,Value:WideString; KeepSeconds:cardinal;
      Comment,Domain,Path:WideString; Secure,HttpOnly:boolean); overload; virtual;

    procedure SendStr(const Data:WideString);
    procedure SendStream(s: IStream);
    function GetBufferSize: integer;
    procedure SetBufferSize(ABufferSize: integer);
    procedure Flush;
    procedure FlushFinal; virtual;
    procedure FlushStream(AData:TStream;ADataSize:int64); virtual;

    { IxxmParameterCollection }
    procedure AddParameter(Param: IUnknown);//IxxmParameter

    { IxxmUploadProgressService }
    procedure AttachAgent(Agent: IxxmUploadProgressAgent; Flags, Step: integer);

    {  }
    function GetProjectEntry:TXxmProjectEntry; virtual; abstract;
    procedure SendHeader; virtual; abstract;
    function GetRequestHeader(const Name: WideString): WideString; virtual; abstract;
    procedure AddResponseHeader(const Name, Value: WideString); virtual; abstract;

    function GetProjectPage(FragmentName: WideString):IXxmFragment; virtual;
    procedure CheckHeaderNotSent;
    function CheckSendStart(NoOnNextFlush:boolean):boolean;
    function AuthValue(cs:TXxmContextString):AnsiString;

    procedure SendError(const res,val1,val2:string);
    procedure ForceStatus(Code: Integer; Text: WideString);
    function HandleException(Ex: Exception): boolean;

    procedure BeginRequest; virtual;
    procedure LoadPage;
    procedure BuildPage;
    procedure SingleFile;
    procedure EndRequest; virtual;

    property ProjectEntry: TXxmProjectEntry read FProjectEntry;
    property BufferSize: integer read FBufferSize;
    //see also GetBufferSize,SetBufferSize, only here for inheriters
  public
    //abstract! constructor only here for private variable init
    constructor Create(const URL:WideString);
    procedure AfterConstruction; override;
    destructor Destroy; override;
    //property URL:WideString read GetURL;
    property ContentType:WideString read FContentType;
    property StatusCode:integer read FStatusCode;
    property StatusText:WideString read FStatusText;
    property SingleFileSent:WideString read FSingleFileSent;
  end;

  TXxmBufferStore=class(TObject)
  private
    FLock: TRTLCriticalSection;
    FBuffer: array of TMemoryStream;
    FBufferSize: integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure GetBuffer(var x:TMemoryStream);
    procedure AddBuffer(var x:TMemoryStream);
  end;

  EXxmTransferError=class(Exception);
  EXxmAutoBuildFailed=class(Exception);
  EXxmDirectInclude=class(Exception);
  EXxmPageRedirected=class(Exception);
  EXxmIncludeOnlyOnBuild=class(Exception);
  EXxmIncludeStackFull=class(Exception);
  EXxmIncludeFragmentNotFound=class(Exception);
  EXxmIncludeCrossProjectDisabled=class(Exception);
  EXxmParametersAlreadyParsed=class(Exception);
  EXxmBufferSizeInvalid=class(Exception);

var
  //see xxmSettings
  StatusBuildError,StatusException,StatusFileNotFound:integer;
  BufferStore:TXxmBufferStore;

const
  Utf8ByteOrderMark=#$EF#$BB#$BF;
  Utf16ByteOrderMark=#$FF#$FE;

implementation

uses Variants, ComObj, xxmCommonUtils, xxmParUtils;

const //resourcestring?
  SXxmDirectInclude='Direct call to include fragment is not allowed';
  SXxmIncludeOnlyOnBuild='Include only allowed when building a page';
  SXxmIncludeStackFull='Maximum level of includes exceeded';
  SXxmIncludeFragmentNotFound='Include fragment not found "__"';
  SXxmIncludeCrossProjectDisabled='Cross-project includes not enabled';
  SXxmParametersAlreadyParsed='Can''t attach progress agent, parameters already parsed';
  SXxmBufferSizeInvalid='BufferSize exceeds maximum';

{ TXxmGeneralContext }

constructor TXxmGeneralContext.Create(const URL: WideString);
begin
  inherited Create;
  FURL:=URL;
  FContentBuffer:=nil;
  SendDirect:=nil;//TODO: stub raise abstract error?
end;

procedure TXxmGeneralContext.AfterConstruction;
begin
  inherited;
  BeginRequest;
end;

destructor TXxmGeneralContext.Destroy;
begin
  EndRequest;
  inherited;
end;

procedure TXxmGeneralContext.BeginRequest;
begin
  FProjectEntry:=nil;
  FContentType:='text/html';//default (setting?)
  FAutoEncoding:=aeUtf8;//default (setting?)
  FParams:=nil;//see GetParameter
  FPostData:=nil;
  FPostTempFile:='';
  FAuthParsed:=false;
  FAuthUserName:='';
  FAuthPassword:='';
  FPage:=nil;
  FBuilding:=nil;
  FPageClass:='';
  FHeaderSent:=XxmHeaderNotSent;
  FIncludeDepth:=0;
  FIncludeCheck:=nil;
  FStatusCode:=200;//default
  FStatusText:='OK';//default
  SettingCookie:=false;
  FProjectName:='';//parsed from URL later
  FFragmentName:='';//parsed from URL later
  FBufferSize:=0;
  BuildPageLeaveOpen:=false;
  SendBuf:=SendDirect;
end;

procedure TXxmGeneralContext.EndRequest;
begin
  FBuilding:=nil;
  if FPage<>nil then
   begin
    try
      //let project decide to free or not
      FProjectEntry.Project.UnloadFragment(FPage);
    except
      //silent (log?HandleException?)
    end;
    FPage:=nil;
   end;
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
  BufferStore.AddBuffer(FContentBuffer);
end;

function TXxmGeneralContext.GetURL: WideString;
begin
  Result:=FURL;
end;

function TXxmGeneralContext.GetProjectPage(FragmentName: WideString): IXxmFragment;
begin
  Result:=FProjectEntry.Project.LoadPage(Self,FragmentName);
end;

procedure TXxmGeneralContext.LoadPage;
begin
  FProjectEntry:=GetProjectEntry;//(FProjectName);
  if @XxmAutoBuildHandler<>nil then
    if not(XxmAutoBuildHandler(FProjectEntry,Self,FProjectName)) then
     begin
      FProjectEntry:=nil;
      raise EXxmAutoBuildFailed.Create(FProjectName);
     end;
  FProjectEntry.OpenContext;
  SetBufferSize(FProjectEntry.BufferSize);
  FPage:=GetProjectPage(FFragmentName);
end;

procedure TXxmGeneralContext.BuildPage;
var
  p:IXxmPage;
  i:int64;
begin
  //clear buffer just in case
  if FContentBuffer<>nil then FContentBuffer.Position:=0;

  BuildPageLeaveOpen:=false;
  LoadPage;
  if FPage=nil then
    SingleFile
  else
   begin
    FPageClass:=FPage.ClassNameEx;
    //mime type moved to CheckSendStart;

    if FPage.QueryInterface(IID_IXxmPage,p)<>S_OK then
      raise EXxmDirectInclude.Create(SXxmDirectInclude);
    p:=nil;

    //build page
    FBuilding:=FPage;
    FPage.Build(Self,nil,[],[]);//any parameters?

    //any content?
    if BuildPageLeaveOpen then
     begin
     end
    else
     begin
      if FHeaderSent<>XxmHeaderSent then
       begin
        if FBufferSize=0 then i:=0 else i:=FContentBuffer.Position;
        if (i=0) and (FStatusCode=200) then
          ForceStatus(204,'No Content');
        if FStatusCode<>304 then
          AddResponseHeader('Content-Length',IntToStr(i));
        if i=0 then SendHeader;
       end;
      FlushFinal;
     end;
   end;
end;

procedure TXxmGeneralContext.SingleFile;
var
  x,y:WideString;
  fh:THandle;
  fd:TByHandleFileInformation;
  fs:int64;
  st:TSystemTime;
begin
  //find a file
  //ask project to translate? project should have given a fragment!
  FPageClass:='['+FProjectName+']GetFilePath';
  FProjectEntry.GetFilePath(FFragmentName,FSingleFileSent,x);
  fh:=CreateFileW(PWideChar(FSingleFileSent),
    GENERIC_READ,FILE_SHARE_READ,nil,OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL or FILE_FLAG_SEQUENTIAL_SCAN,0);
  if (fh<>INVALID_HANDLE_VALUE) then
   begin
    //TODO: 'Range'
    try
      if GetFileInformationByHandle(fh,fd) then
       begin
        FileTimeToSystemTime(fd.ftLastWriteTime,st);
        y:=RFC822DateGMT(SystemTimeToDateTime(st));
        fs:=fd.nFileSizeHigh shl 32 or fd.nFileSizeLow;
       end
      else
       begin
        y:='';
        fs:=0;
       end;
      FAutoEncoding:=aeContentDefined;
      FContentType:=x;
      //TODO: Cache-Control max-age (and others?), other 'If-'s?
      if (y<>'') and (GetRequestHeader('If-Modified-Since')=y) then
       begin
        ForceStatus(304,'Not Modified');
        //AddResponseHeader('Content-Length',?
        SendHeader;
       end
      else
       begin
        if y<>'' then AddResponseHeader('Last-Modified',y);
        if fs<>0 then AddResponseHeader('Content-Length',IntToStr(fs));
        FlushStream(TOwningHandleStream.Create(fh),fs);
       end;
    except
      CloseHandle(fh);
      raise;
    end;
   end
  else
   begin
    FPageClass:='['+FProjectName+']404:'+FFragmentName;
    FPage:=FProjectEntry.Project.LoadPage(Self,'404.xxm');
    if FPage=nil then
     begin
      ForceStatus(StatusFileNotFound,'File not found');
      SendError('fnf','','');
     end
    else
     begin
      FPageClass:=FPage.ClassNameEx;
      FBuilding:=FPage;
      FPage.Build(Self,nil,[FFragmentName,FSingleFileSent,x],[]);//any parameters?
      if FBufferSize<>0 then FlushFinal;
     end;
   end;
end;

function TXxmGeneralContext.GetPage: IXxmFragment;
begin
  Result:=FPage;
end;

function TXxmGeneralContext.CheckSendStart(NoOnNextFlush:boolean):boolean;
begin
  if FHeaderSent=XxmHeaderSent then
   begin
    FSingleFileSent:='';
    Result:=false;
   end
  else
    if (FBufferSize=0) or NoOnNextFlush then
     begin
      FHeaderSent:=XxmHeaderSent;
      SendHeader;
      Result:=true;
     end
    else
      if FHeaderSent=XxmHeaderNotSent then
       begin
        FHeaderSent:=XxmHeaderOnNextFlush;
        Result:=true;
       end
      else
        Result:=false;
end;

procedure TXxmGeneralContext.CheckHeaderNotSent;
begin
  if FHeaderSent<>XxmHeaderNotSent then
    raise EXxmResponseHeaderAlreadySent.Create(SXxmResponseHeaderAlreadySent);
end;

procedure TXxmGeneralContext.SendError(const res, val1, val2: string);
var
  s:AnsiString;
  ss,tt,t:string;
  i,j,l:integer;
  r:TResourceStream;
const
  RT_HTML = MakeIntResource(23);
begin
  if Connected then
   begin
    if (FHeaderSent=XxmHeaderSent) and (FContentType='text/plain') then
     begin
      tt:=#13#10'----------------------------------------'+
        #13#10'### '+res+' ###'+
        #13#10+val1+
        #13#10+val2+
        #13#10'URL: '+FURL+
        #13#10'Project: '+FProjectName+
        #13#10'Address: '+FFragmentName+
        #13#10'Class: '+FPageClass;
      if FSingleFileSent<>'' then tt:=tt+#13#10'Path: '+FSingleFileSent;
      tt:=tt+#13#10'QueryString: '+ContextString(csQueryString)+
        #13#10'PostData: ';
      try
        if FPostData=nil then
          tt:=tt+'none'
        else
          tt:=tt+IntToStr(FPostData.Size)+' bytes';
      except
        tt:=tt+'unknown';
      end;
      tt:=tt+
        #13#10'Version: '+ContextString(csVersion);
     end
    else
     begin
      r:=TResourceStream.Create(HInstance,res,RT_HTML);
      try
        l:=r.Size;
        SetLength(s,l);
        r.Read(s[1],l);
      finally
        r.Free;
      end;
      ss:=string(s);
      tt:='';
      l:=Length(ss);
      i:=1;
      t:='';//default
      while i<=l do
       begin
        j:=i;
        while (j<l) and (ss[j]<>'$') do inc(j);
        if j=l then inc(j);
        tt:=tt+Copy(ss,i,j-i);
        if j<l then
         begin
          inc(j);
          case ss[j] of
            '1':tt:=tt+HTMLEncode(val1);
            '2':tt:=tt+HTMLEncode(val2);
            'A':tt:=tt+HTMLEncode(FFragmentName);
            'C':tt:=tt+HTMLEncode(FPageClass);
            'D':
              try
                if FPostData=nil then
                  tt:=tt+'none'
                else
                  tt:=tt+IntToStr(FPostData.Size)+' bytes';
              except
                tt:=tt+'unknown';
              end;
            'L':tt:=tt+t;//see 'R','X'
            'P':tt:=tt+HTMLEncode(FProjectName);
            'Q':tt:=tt+HTMLEncode(ContextString(csQueryString));
            'R':
             begin
              t:=ContextString(csReferer);
              if t='' then
               begin
                tt:=tt+'#';
                t:='';
               end
              else
               begin
                tt:=tt+HTMLEncode(t);
                t:='back';
               end;
             end;
            'S':tt:=tt+HTMLEncode(FSingleFileSent);
            'U':tt:=tt+HTMLEncode(FURL);
            'V':tt:=tt+ContextString(csVersion);
            'X':
              if ContextString(csVerb)='GET' then
               begin
                tt:=tt+HTMLEncode(FURL);
                t:='refresh';
               end
              else
               begin
                tt:=tt+'#';
                t:='';
               end;
            //else ?
          end;
          inc(j);
         end;
        i:=j;
       end;
      if FHeaderSent<>XxmHeaderSent then
       begin
        FContentType:='text/html';
        FAutoEncoding:=aeContentDefined;//?
       end;
     end;
    SendStr(WideString(tt));
    if FBufferSize<>0 then FlushFinal;
   end;
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

type
  TXxmCrossProjectIncludeCheck=class(TObject)
  public
    Entry:TXxmProjectEntry;
    Next:TXxmCrossProjectIncludeCheck;
    constructor Create(AEntry:TXxmProjectEntry;ANext:TXxmCrossProjectIncludeCheck);
  end;

procedure TXxmGeneralContext.Include(Address: WideString;
  const Values: array of OleVariant; const Objects: array of TObject);
var
  f,fb:IXxmFragment;
  pc:AnsiString;
  pn:WideString;
  pe:TXxmProjectEntry;
  px:TXxmCrossProjectIncludeCheck;
  i,j,l:integer;
begin
  if FBuilding=nil then
    raise EXxmIncludeOnlyOnBuild.Create(SXxmIncludeOnlyOnBuild);
  if FIncludeDepth=XxmMaxIncludeDepth then
    raise EXxmIncludeStackFull.Create(SXxmIncludeStackFull);
  pe:=FProjectEntry;
  pn:=FProjectName;
  fb:=FBuilding;
  pc:=FPageClass;
  inc(FIncludeDepth);
  try
    if Copy(Address,1,4)='xxm:' then
      if pe.AllowInclude then
       begin
        //cross-project include
        l:=Length(Address);
        i:=5;
        if (i<=l) and (Address[i]='/') then inc(i);
        if (i<=l) and (Address[i]='/') then inc(i);
        j:=i;
        while (j<=l) and not(char(Address[j]) in ['/','?','&','$','#']) do inc(j);
        FProjectName:=Copy(Address,i,j-i);
        if (j<=l) and (Address[j]='/') then inc(j);
        FProjectEntry:=GetProjectEntry;
        //XxmAutoBuildHandler but check for recurring PE's to avoid deadlock
        if @XxmAutoBuildHandler<>nil then
         begin
          px:=FIncludeCheck;
          while (px<>nil) and (px.Entry<>FProjectEntry) do px:=px.Next;
          if px=nil then
            if not(XxmAutoBuildHandler(FProjectEntry,Self,FProjectName)) then
              raise EXxmAutoBuildFailed.Create(FProjectName);
          //if px<>nil then raise? just let the request complete
         end;
        f:=FProjectEntry.Project.LoadFragment(Self,Copy(Address,j,l-j+1),FBuilding.RelativePath);
        if f=nil then
          raise EXxmIncludeFragmentNotFound.Create(StringReplace(
            SXxmIncludeFragmentNotFound,'__',Address,[]));
        FBuilding:=f;
        px:=TXxmCrossProjectIncludeCheck.Create(pe,FIncludeCheck);
        try
          FIncludeCheck:=px;
          FProjectEntry.OpenContext;
          try
            FPageClass:=FProjectEntry.Name+':'+f.ClassNameEx+' < '+pc;
            f.Build(Self,fb,Values,Objects);//queue to avoid building up stack?
          finally
            FProjectEntry.Project.UnloadFragment(f);
            f:=nil;
            FProjectEntry.CloseContext;
          end;
        finally
          FIncludeCheck:=px.Next;
          px.Free;
        end;
       end
      else
        raise EXxmIncludeCrossProjectDisabled.Create(SXxmIncludeCrossProjectDisabled)
    else
     begin
      //FPage.Project?
      pn:='';
      f:=FProjectEntry.Project.LoadFragment(Self,Address,FBuilding.RelativePath);
      if f=nil then
        raise EXxmIncludeFragmentNotFound.Create(StringReplace(
          SXxmIncludeFragmentNotFound,'__',Address,[]));
      FBuilding:=f;
      try
        FPageClass:=f.ClassNameEx+' < '+pc;
        f.Build(Self,fb,Values,Objects);//queue to avoid building up stack?
      finally
        FProjectEntry.Project.UnloadFragment(f);
        f:=nil;
      end;
     end;
    FPageClass:=pc;
  finally
    dec(FIncludeDepth);
    FProjectEntry:=pe;
    FBuilding:=fb;
    fb:=nil;
  end;
end;

procedure TXxmGeneralContext.SendStr(const Data: WideString);
var
  s:AnsiString;
  l:LongInt;
begin
  if Data<>'' then
   begin
    if CheckSendStart(false) then
      case FAutoEncoding of
        aeUtf8:
          if SendBuf(Utf8ByteOrderMark[1],3)<>3 then
            raise EXxmTransferError.Create(SysErrorMessage(GetLastError));
        aeUtf16:
          if SendBuf(Utf16ByteOrderMark[1],2)<>2 then
            raise EXxmTransferError.Create(SysErrorMessage(GetLastError));
      end;
    case FAutoEncoding of
      aeUtf16:
       begin
        l:=Length(Data)*2;
        if SendBuf(Data[1],l)<>l then
          raise EXxmTransferError.Create(SysErrorMessage(GetLastError));
       end;
      aeUtf8:
       begin
        s:=UTF8Encode(Data);
        l:=Length(s);
        if SendBuf(s[1],l)<>l then
          raise EXxmTransferError.Create(SysErrorMessage(GetLastError));
       end;
      else
       begin
        s:=Data;
        l:=Length(s);
        if SendBuf(s[1],l)<>l then
          raise EXxmTransferError.Create(SysErrorMessage(GetLastError));
       end;
    end;
    if (FBufferSize<>0) and (FContentBuffer.Position>=FBufferSize) then Flush;
   end;
end;

procedure TXxmGeneralContext.SendStream(s: IStream);
const
  dSize=$10000;
var
  d:array[0..dSize-1] of byte;
  l:integer;
begin
  CheckSendStart(true);
  Flush;
  repeat
    l:=dSize;
    OleCheck(s.Read(@d[0],dSize,@l));
    if l<>0 then
      if SendDirect(d[0],l)<>l then
        raise EXxmTransferError.Create(SysErrorMessage(GetLastError));
  until l=0;
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
  SendStr(VarToWideStrX(Data));
end;

procedure TXxmGeneralContext.Send(Data: OleVariant);
begin
  SendStr(HTMLEncode(VarToWideStrX(Data)));
end;

procedure TXxmGeneralContext.SendFile(FilePath: WideString);
var
  b:boolean;
begin
  inherited;
  //TODO: auto mimetype by extension?
  b:=FHeaderSent<>XxmHeaderNotSent;
  SendStream(TStreamAdapter.Create(TFileStream.Create(FilePath,fmOpenRead or fmShareDenyNone),soOwned));//does CheckSendStart
  if b then FSingleFileSent:='' else FSingleFileSent:=FilePath;
end;

procedure TXxmGeneralContext.Send(Value: integer);
begin
  SendStr(IntToStr(Value));
end;

procedure TXxmGeneralContext.Send(Value: int64);
begin
  SendStr(IntToStr(Value));
end;

procedure TXxmGeneralContext.Send(Value: cardinal);
begin
  SendStr(IntToStr(Value));
end;

procedure TXxmGeneralContext.Send(const Values: array of OleVariant);
var
  i:integer;
begin
  for i:=0 to Length(Values)-1 do SendStr(HTMLEncode(Values[i]));
end;

procedure TXxmGeneralContext.SendHTML(const Values: array of OleVariant);
var
  i:integer;
begin
  for i:=0 to Length(Values)-1 do SendStr(VarToWideStrX(Values[i]));
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
  if FParams=nil then FParams:=TXxmReqPars.Create;
  if not FParams.Filled then
    if FParams.Fill(Self,FPostData) then
      FreeAndNil(FPostData);//redirect on post? invalidate postdata!
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
  //parse parameters on first use
  if FParams=nil then FParams:=TXxmReqPars.Create;
  if not FParams.Filled then
    if FParams.Fill(Self,FPostData) then
      FreeAndNil(FPostData);//redirect on post? invalidate postdata!
  Result:=FParams.Count;
end;

procedure TXxmGeneralContext.AddParameter(Param: IInterface);
begin
  if FParams=nil then FParams:=TXxmReqPars.Create;//fill: postpone to first GetParameter call
  FParams.Add(Param as IXxmParameter);
end;

procedure TXxmGeneralContext.AttachAgent(Agent: IxxmUploadProgressAgent;
  Flags, Step: integer);
const
  DefaultProgressStep=$10000;
begin
  if FParams=nil then FParams:=TXxmReqPars.Create;//fill: postpone to first GetParameter call
  if FParams.Filled then raise EXxmParametersAlreadyParsed.Create(SXxmParametersAlreadyParsed);
  if (Flags and xxmUploadProgressAttach_PostData)<>0 then
    FParams.DataProgressAgent:=Agent;
  if (Flags and xxmUploadProgressAttach_FileFields)<>0 then
   begin
    FParams.FileProgressAgent:=Agent;
    if Step=0 then
      FParams.FileProgressStep:=DefaultProgressStep
    else
      FParams.FileProgressStep:=Step;
   end;
end;

function TXxmGeneralContext.GetBufferSize: integer;
begin
  Result:=FBufferSize;
end;

procedure TXxmGeneralContext.SetBufferSize(ABufferSize: integer);
const
  MaxBufferSize=$10000000;//128MB
  BufferSizeStep=$10000;//64KB
begin
  if (ABufferSize<0) or (ABufferSize>MaxBufferSize) then
    raise EXxmBufferSizeInvalid.Create(SXxmBufferSizeInvalid);
  if FBufferSize>ABufferSize then Flush;
  FBufferSize:=ABufferSize;
  if FBufferSize=0 then SendBuf:=SendDirect else
   begin
    BufferStore.GetBuffer(FContentBuffer);
    if FContentBuffer.Position>ABufferSize then Flush;
    if FContentBuffer.Size<ABufferSize then
     begin
      if (ABufferSize and (BufferSizeStep-1))<>0 then
        ABufferSize:=((ABufferSize div BufferSizeStep)+1)*BufferSizeStep;
      FContentBuffer.Size:=ABufferSize;
     end;
    SendBuf:=FContentBuffer.Write;
   end;
end;

procedure TXxmGeneralContext.Flush;
var
  i:int64;
begin
  if FHeaderSent=XxmHeaderOnNextFlush then
   begin
    FHeaderSent:=XxmHeaderSent;
    SendHeader;
   end;
  if FBufferSize<>0 then
   begin
    i:=FContentBuffer.Position;
    if i<>0 then
     begin
      if SendDirect(FContentBuffer.Memory^,i)<>i then
        raise EXxmTransferError.Create(SysErrorMessage(GetLastError));
      FContentBuffer.Position:=0;
     end;
   end;
end;

function TXxmGeneralContext.HandleException(Ex: Exception): boolean;
var
  pe:IXxmProjectEvents;
  pf:IXxmProjectEvents1;
begin
  try
    pe:=FProjectEntry.GetProjectInterface(IXxmProjectEvents) as IXxmProjectEvents;
    if pe<>nil then Result:=pe.HandleException(Self,FPageClass,Ex) else
     begin
      pf:=FProjectEntry.GetProjectInterface(IXxmProjectEvents1) as IXxmProjectEvents1;
      if pf<>nil then Result:=pf.HandleException(Self,FPageClass,Ex.ClassName,Ex.Message)
        else Result:=false;
     end;
  except
    //raise?
    Result:=false;
  end;
end;

procedure TXxmGeneralContext.SetCookie(Name, Value: WideString);
begin
  CheckHeaderNotSent;
  AddResponseHeader('Cache-Control','no-cache="set-cookie"');
  SettingCookie:=true;//allow multiple?
  AddResponseHeader('Set-Cookie',Name+'='+Value);
end;

procedure TXxmGeneralContext.SetCookie(Name, Value: WideString;
  KeepSeconds: cardinal; Comment, Domain, Path: WideString; Secure,
  HttpOnly: boolean);
var
  x:WideString;
begin
  CheckHeaderNotSent;
  AddResponseHeader('Cache-Control','no-cache="set-cookie"');
  x:=Name+'='+Value;
  //'; Version=1';
  if Comment<>'' then
    x:=x+'; Comment='+Comment;
  if Domain<>'' then
    x:=x+'; Domain='+Domain;
  if Path<>'' then
    x:=x+'; Path='+Path;
  x:=x+'; Max-Age='+IntToStr(KeepSeconds)+
    '; Expires='+RFC822DateGMT(Now+KeepSeconds/86400);
  if Secure then
    x:=x+'; Secure';
  if HttpOnly then
    x:=x+'; HttpOnly';
  SettingCookie:=true;//allow multiple?
  AddResponseHeader('Set-Cookie',x);
  //TODO: Set-Cookie2
end;

function TXxmGeneralContext.AuthValue(cs:TXxmContextString):AnsiString;
var
  s,t:AnsiString;
  i,j,l:integer;
  a,b:byte;
begin
  if not FAuthParsed then
   begin
    //base64 decode see http://www.faqs.org/rfcs/rfc2045.html #6.8
    s:=AnsiString(GetRequestHeader('Authorization'));
    l:=Length(s);
    if l=0 then
     begin
      FAuthUserName:='';
      FAuthPassword:='';
     end
    else
     begin
      if (l<6) or (Copy(s,1,6)<>'Basic ') then
        raise Exception.Create('Unsupported authorization method');
      i:=7;
      j:=0;
      SetLength(t,l div 2);
      while i<=l do
       begin
        case char(s[i]) of
          'A'..'Z':a:=byte(s[i])-65;
          'a'..'z':a:=byte(s[i])-71;
          '0'..'9':a:=byte(s[i])+4;
          '+':a:=62;
          '/':a:=63;
          //'=':;
          else raise Exception.Create('Authorization: invalid base64 character');
        end;
        inc(i);
        if i<=l then
         begin
          case char(s[i]) of
            'A'..'Z':b:=byte(s[i])-65;
            'a'..'z':b:=byte(s[i])-71;
            '0'..'9':b:=byte(s[i])+4;
            '+':b:=62;
            '/':b:=63;
            //'=':;
            else raise Exception.Create('Authorization: invalid base64 character');
          end;
          inc(j);
          t[j]:=AnsiChar((a shl 2) or (b shr 4));
          inc(i);
         end
        else
          b:=0;//counter warning
        if i<=l then
         begin
          case char(s[i]) of
            'A'..'Z':a:=byte(s[i])-65;
            'a'..'z':a:=byte(s[i])-71;
            '0'..'9':a:=byte(s[i])+4;
            '+':a:=62;
            '/':a:=63;
            '=':a:=$FF;
            else raise Exception.Create('Authorization: invalid base64 character');
          end;
          if a<>$FF then
           begin
            inc(j);
            t[j]:=AnsiChar((b shl 4) or (a shr 2));
           end;
          inc(i);
         end;
        if i<=l then
         begin
          case char(s[i]) of
            'A'..'Z':b:=byte(s[i])-65;
            'a'..'z':b:=byte(s[i])-71;
            '0'..'9':b:=byte(s[i])+4;
            '+':b:=62;
            '/':b:=63;
            '=':b:=$FF;
            else raise Exception.Create('Authorization: invalid base64 character');
          end;
          if b<>$FF then
           begin
            inc(j);
            t[j]:=AnsiChar((a shl 6) or b);
           end;
          inc(i);
         end;
       end;
      //SetLength(t,j);
      i:=1;
      while (i<=j) and (t[i]<>':') do inc(i);
      //if i>j then raise?
      //TODO: encoding: utf8?
      FAuthUserName:=Copy(t,1,i-1);
      FAuthPassword:=Copy(t,i+1,j-i);
     end;
    FAuthParsed:=true;
   end;
  if cs=csAuthPassword then Result:=FAuthPassword else Result:=FAuthUserName;
end;

procedure TXxmGeneralContext.FlushFinal;
begin
  //inheritants can do alternate operations on final flush
  Flush;
end;

procedure TXxmGeneralContext.FlushStream(AData: TStream; ADataSize: int64);
begin
  //inheritants can do alternate operations on sendstream+flush
  //assert AData.Size=ADataSize
  //assert AData.Position=0
  //ATTENTION: TStreamAdapter will free AData when done
  SendStream(TStreamAdapter.Create(AData,soOwned));
  if FBufferSize<>0 then Flush;
end;

{ TXxmCrossProjectIncludeCheck }

constructor TXxmCrossProjectIncludeCheck.Create(AEntry: TXxmProjectEntry;
  ANext: TXxmCrossProjectIncludeCheck);
begin
  inherited Create;
  Entry:=AEntry;
  Next:=ANext;
end;

{ TXxmBufferStore }

constructor TXxmBufferStore.Create;
begin
  inherited Create;
  FBufferSize:=0;
  InitializeCriticalSection(FLock);
end;

destructor TXxmBufferStore.Destroy;
var
  i:integer;
begin
  for i:=0 to FBufferSize-1 do //downto?
    try
      FreeAndNil(FBuffer[i]);
    except
      //silent
    end;
  DeleteCriticalSection(FLock);
  inherited;
end;

procedure TXxmBufferStore.AddBuffer(var x:TMemoryStream);
var
  i:integer;
begin
  if x<>nil then
   begin
    EnterCriticalSection(FLock);
    try
      i:=0;
      while (i<FBufferSize) and (FBuffer[i]<>nil) do inc(i);
      if i=FBufferSize then
       begin
        inc(FBufferSize,$400);//grow
        SetLength(FBuffer,FBufferSize);
       end;
      FBuffer[i]:=x;
      x.Position:=0;
    finally
      LeaveCriticalSection(FLock);
      x:=nil;
    end;
   end;
end;

procedure TXxmBufferStore.GetBuffer(var x:TMemoryStream);
var
  i:integer;
begin
  if x=nil then
   begin
    EnterCriticalSection(FLock);
    try
      i:=0;
      while (i<FBufferSize) and (FBuffer[i]=nil) do inc(i);
      if i=FBufferSize then
       begin
        x:=THeapStream.Create;//TODO: tmp file when large buffer
       end
      else
       begin
        x:=FBuffer[i];
        FBuffer[i]:=nil;
       end;
    finally
      LeaveCriticalSection(FLock);
    end;
   end;
end;

initialization
  BufferStore:=TXxmBufferStore.Create;
finalization
  BufferStore.Free;
end.
