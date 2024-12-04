unit xxmContext;

interface

uses SysUtils, Classes, xxm2, xxmSock, xxmTools, xxmPReg, xxmSChannel;

type
  TNameValues=record
    Names:array of record
      Name,Value:PUTF8Char;
    end;
    NamesIndex,NamesSize:NativeInt;
  end;

  TParamInfo=record
    Context:TObject;Index:NativeUInt;
    Origin,Name,Value,ContentType:PUTF8Char;
    PostDataPos,PostDataLen:NativeUInt;
  end;
  PParamInfo=^TParamInfo;

  TxxmContext=class(TObject)
  private
    FSocket:TTcpSocket;
    FCache:array of record
      Data:array of UTF8Char;
      Index:NativeUInt;
    end;
    FCacheIndex,FCacheSize:integer;
    FVerb,FURI,FVersion,FQueryString:PUTF8Char;
    FAllowChunked,FHeaderSent:boolean;
    FHeaders:array of record
      Name,Value:PUTF8Char;
    end;
    FHeadersIndexIn,FHeadersIndexOut,FHeadersSize:NativeUInt;
    FStatusCode:word;
    FStatusText:PUTF8Char;
    FRedirectPrefix,FSessionID:UTF8String;
    FPostData:TStream;
    FPostTempFile:string;
    FProjectName,FFragmentName:UTF8String;
    FProjectEntry:TProjectEntry;
    FPageClass:UTF8String;
    FPage:CxxmFragment;
    FIncludeCheck:TObject;
    FIncludeDepth:cardinal;
    FSingleFileSent:string;
    FProgressCallback:CxxmProgress;
    FProgressRequestID,FProgressReportStep:NativeUInt;
    FChunked,FAuthParsed:boolean;
    FAuthUserName,FAuthPassword:UTF8String;
    FAutoEncoding:TxxmAutoEncoding;
    FCookie:TKeyValues;
    FParamsParsed:boolean;
    FParams:array of TParamInfo;
    FParamsIndex,FParamsSize:NativeUInt;
    FCredNTLM,FCredNego:TCredHandle;
    FCtxt:TCtxtHandle;
  protected
    FProjectData:pointer;
    function GetRequestHeader(Name:PUTF8Char):PUTF8Char;
    procedure SetResponseHeader(Name,Value:PUTF8Char;
      AllowDuplicates:boolean=false);
    function GetResponseHeader(Name:PUTF8Char):PUTF8Char;
    function Store(const Data:UTF8String):PUTF8Char;

    procedure SendHeader;
    procedure Flush;
    procedure AuthSChannel(const Package:UTF8String;var Cred:TCredHandle);
    function AuthParse(const Scheme:UTF8String):UTF8String;
    function AuthValue(cs:TXxmContextString):UTF8String;
    procedure AuthSet(const Name,Pwd:UTF8String);
    procedure ParseParams;
    function GetCookie(Name:PUTF8Char):PUTF8Char;
    function GetParam(Name:PUTF8Char):PParamInfo;
    function GetParamCount:integer;
    function GetParamByIdx(Index:NativeUInt):PParamInfo;
  public
    constructor Create;

    procedure Clear;
    procedure Bind(Socket:TTcpSocket);

    procedure HandleRequest(Sender:TObject);
    procedure Include(Address:PUTF8Char;const Values:array of Variant;
      const Objects:array of pointer);

    procedure SendStream(s:TStream);

    procedure Redirect(URL:PUTF8Char;Relative:boolean);
    function ContextString(Value:integer):PUTF8Char;

    property Socket:TTcpSocket read FSocket;
  end;

  TxxmContextPool=class(TObject)
  public
    function GetContext:TxxmContext;
    procedure Recycle(Context:TxxmContext);
  end;

  TStreamNozzle=class(TObject)
  private
    FOwner: TxxmContext;
    FSource: TStream;
    FSourceAtEnd: boolean;
    FData: array of UTF8Char;
    FSize, FIndex, FDone: NativeUInt;
    FProgress: CxxmProgress;
    FRequestID, FReportStep: NativeUInt;
    function Ensure(EnsureSize: NativeUInt): boolean;
    procedure Flush;
    procedure SkipWhiteSpace;
  public
    constructor Create(Owner: TxxmContext; Progress: CxxmProgress;
      RequestID, ReportStep: NativeUInt);
    destructor Destroy; override;
    procedure CheckBoundary(var Boundary: UTF8String);
    procedure GetHeader(var Params: TKeyValues);
    function GetString(const Boundary: UTF8String): PUTF8Char;
    procedure GetData(Boundary, FieldName, FileName: PUTF8Char;
      var Pos, Len: NativeUInt);
    function MultiPartDone: boolean;
  end;

  EXxmError=class(Exception);
  EXxmConnectionLost=class(EXxmError);
  EXxmHeaderParseTimeExceeded=class(EXxmError);
  EXxmMaximumHeaderLines=class(EXxmError);
  EXxmHeaderAlreadySent=class(EXxmError);
  EXxmUnknownPostMime=class(EXxmError);
  EXxmTransferError=class(EXxmError);
  EXxmPageRedirected=class(EXxmError);
  EXxmResponseHeaderOnly=class(EXxmError);
  EXxmProjectCheckFailed=class(EXxmError);
  EXxmIncludeNoFragmentHandler=class(Exception);
  EXxmIncludeOnlyOnBuild=class(Exception);
  EXxmIncludeStackFull=class(Exception);
  EXxmIncludeFragmentNotFound=class(Exception);
  EXxmIncludeCrossProjectDisabled=class(Exception);

var
  ContextPool:TxxmContextPool;
  SelfVersion,SessionCookie:UTF8String;

//forward
procedure Context_SendHTML(Context:PxxmContext;HTML:PUTF8Char); stdcall;

implementation

uses Windows;

const
  //TODO: from configuration
  HTTPMaxHeaderLines=$400;//1KiB
  HTTPMaxHeaderParseTimeMS=10000;
  CacheDataSize=$20000;
  CacheSizeGrowStep=8;
  PostDataThreshold=$200000;//2MiB
  MaxIncludeDepth=64;

  UTF8ByteOrderMark:array[0..2] of UTF8Char=(#$EF,#$BB,#$BF);
  UTF16ByteOrderMark:array[0..1] of UTF8Char=(#$FF,#$FE);

type
  TCacheData=array[0..CacheDataSize-1] of UTF8Char;
  PCacheData=^TCacheData;

function IntToStr8(x:integer):UTF8String;
var
  i,j:integer;
  c:UTF8Char;
begin
  if x<0 then raise Exception.Create('IntToStr8: Negative values not supported');
  Result:='00000000000';
  i:=0;
  while (x<>0) do
   begin
    inc(i);
    Result[i]:=UTF8Char($30 or (x mod 10));
    x:=x div 10;
   end;
  if i=0 then i:=1;
  SetLength(Result,i);
  j:=1;
  while (j<i) do
   begin
    c:=Result[j];
    Result[j]:=Result[i];
    Result[i]:=c;
    inc(j);
    dec(i);
   end;
end;

function StrToInt8(p:PUTF8Char):integer;
begin
  if p^='-' then raise Exception.Create('StrToInt8: Negative values not supported');
  Result:=0;
  while (p^<>#0) and (p^ in ['0'..'9']) do
   begin
    Result:=Result*10+(byte(p^) and $F);
    inc(p);
   end;
end;

procedure GetSelfVersion;
const
  dSize=$1000;
var
  d:array[0..dSize-1] of byte;
  verblock:PVSFIXEDFILEINFO;
  verlen:cardinal;
  p:PUTF8Char;
  h1,h2:THandle;
begin
  h1:=FindResource(HInstance,pointer(1),RT_VERSION);
  if h1=0 then SelfVersion:='[no version data]' else
   begin
    h2:=LoadResource(HInstance,h1);
    if h2=0 then SelfVersion:='[verion load failed]' else
     begin
      //odd, a copy is required to avoid access violation in version.dll
      Move(LockResource(h2)^,d[0],SizeofResource(HInstance,h1));
      UnlockResource(h2);
      FreeResource(h2);
      //
      if VerQueryValueA(@d[0],'\',pointer(verblock),verlen) then
        SelfVersion:=UTF8String(
          IntToStr(HiWord(verblock.dwFileVersionMS))+'.'+
          IntToStr(LoWord(verblock.dwFileVersionMS))+'.'+
          IntToStr(HiWord(verblock.dwFileVersionLS))+'.'+
          IntToStr(LoWord(verblock.dwFileVersionLS)))
      else
        SelfVersion:='v???';
      if VerQueryValueA(@d[0],'\StringFileInfo\040904E4\FileDescription',pointer(p),verlen) then
        SelfVersion:=UTF8String(p)+' '+SelfVersion;
     end;
   end;
end;

type
  TxxmBufferStore=class(TObject)
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

var
  BufferStore:TxxmBufferStore;

{ TxxmContextPool }

function TxxmContextPool.GetContext: TxxmContext;
begin
  //...
  Result:=TxxmContext.Create;
end;

procedure TxxmContextPool.Recycle(Context: TxxmContext);
begin
  //Context.Clear;
  Context.Free;
  //TODO
end;

{ TxxmContext }

constructor TxxmContext.Create;
begin
  inherited Create;
  FSocket:=nil;
  FCacheSize:=0;
  FHeadersSize:=0;
  FPostTempFile:='';
  FParamsSize:=0;

  FCredNTLM.dwUpper:=nil;
  FCredNTLM.dwLower:=nil;
  FCredNego.dwUpper:=nil;
  FCredNego.dwLower:=nil;
  FCtxt.dwUpper:=nil;
  FCtxt.dwLower:=nil;

  Clear;
end;

procedure TxxmContext.Clear;
begin
  try
    FreeAndNil(FSocket);
  except
    FSocket:=nil;
  end;

  if (FCtxt.dwLower<>nil) or (FCtxt.dwUpper<>nil) then
    DeleteSecurityContext(@FCtxt);
  if (FCredNTLM.dwLower<>nil) or (FCredNTLM.dwUpper<>nil) then
    FreeCredentialsHandle(@FCredNTLM);
  if (FCredNego.dwLower<>nil) or (FCredNego.dwUpper<>nil) then
    FreeCredentialsHandle(@FCredNego);

  //TODO BufferStore.AddBuffer(FContentBuffer);

  //TODO: EndRequest?
  try
    if FPostTempFile<>'' then
     begin
      DeleteFile(PChar(FPostTempFile));
      FPostTempFile:='';
     end;
  except
    //silent
  end;
  FreeAndNil(FPostData);

  if FCacheSize=0 then
   begin
    FCacheSize:=CacheSizeGrowStep;
    SetLength(FCache,FCacheSize);
   end;

  FCacheIndex:=1;
  SetLength(FCache[0].Data,CacheDataSize);
  FCache[0].Index:=0;
  FVerb:=nil;
  FURI:=nil;
  FVersion:=nil;
  FQueryString:=nil;
  FAllowChunked:=false;
  FHeaderSent:=false;
  FHeadersIndexIn:=0;
  FHeadersIndexOut:=0;
  FStatusCode:=200;
  FStatusText:='OK';
  FSessionID:='';
  FRedirectPrefix:='';
  FPostData:=nil;
  FProjectName:='';
  FFragmentName:='';
  FProjectEntry:=nil;
  FPageClass:='';
  FPage:=nil;
  FIncludeCheck:=nil;
  FIncludeDepth:=0;
  FSingleFileSent:='';
  FProgressCallback:=nil;
  FChunked:=false;
  FAuthParsed:=false;
  FAuthUserName:='';
  FAuthPassword:='';
  FAutoEncoding:=aeUtf8;//default (setting?)
  FCookie.Data:='';
  FParamsParsed:=false;
  FParamsSize:=0;
  FProjectData:=nil;//TODO: free by project?
end;

procedure TxxmContext.Bind(Socket: TTcpSocket);
begin
  //if FSocket<>nil then raise?
  FSocket:=Socket;
end;

procedure TxxmContext.HandleRequest(Sender: TObject);
var
  tc:cardinal;
  Data:PCacheData;
  dIndex,dLine,dEnd,dNext,d1,d2,dURI1,dURI2,n:integer;
  p:PUTF8Char;
  s:UTF8String;
  ds:TStream;

  x,y,z:UTF8String;
  fh:THandle;
  fd:TByHandleFileInformation;
  fs:int64;
  st:TSystemTime;

begin
  //TODO: if FSocket.Secure then FSocket.Negotiate;
  tc:=GetTickCount;
  //assert FCacheIndex=1 (see Clear)
  Data:=@FCache[0].Data[0];
  dIndex:=0;
  dLine:=0;
  dURI1:=0;
  dURI2:=0;
  dNext:=0;
  try
    repeat
      //out of data? read more
      if dLine>=dIndex then
       begin
        if dIndex=CacheDataSize then
          raise EXxmMaximumHeaderLines.Create('Maximum header size exceeded');
        n:=FSocket.ReceiveBuf(Data[dIndex],CacheDataSize-dIndex);
        if n<=0 then
         begin
          FSocket.Disconnect;
          raise EXxmConnectionLost.Create('Connection Lost');
         end
        else
          inc(dIndex,n);
        if cardinal(GetTickCount-tc)>HTTPMaxHeaderParseTimeMS then
         begin
          FSocket.Disconnect;
          raise ExxmHeaderParseTimeExceeded.Create('Header parse time limit exceeded');
         end;
       end;
      dEnd:=dLine;
      while (dEnd<dIndex) and (Data[dEnd]<>#13) and (Data[dEnd]<>#10) do inc(dEnd);
      if (dEnd<dIndex) and ((Data[dEnd]=#13) or (Data[dEnd]=#10)) then
       begin
        d1:=dLine;
        dNext:=dEnd+1;
        if (dNext<dIndex) and (Data[dEnd]=#13) and (Data[dNext]=#10) then inc(dNext);
        if FVerb=nil then
         begin
          while (d1<dEnd) and (Data[d1]>' ') do inc(d1);
          FVerb:=@Data[dLine];
          Data[d1]:=#0;
          inc(d1);//' '
          d2:=d1;
          while (d2<dEnd) and (Data[d2]>' ') do inc(d2);
          FURI:=@Data[d1];
          dURI1:=d1;
          dURI2:=d2;
          Data[d2]:=#0;
          inc(d2);//' '
          FVersion:=@Data[d2];
          Data[dEnd]:=#0;
          FAllowChunked:=FVersion='HTTP/1.1';
         end
        else
        if dLine<dEnd then
         begin

          if FHeadersIndexIn=HTTPMaxHeaderLines then
            raise EXxmMaximumHeaderLines.Create('Maximum header lines exceeded');

          while (d1<dEnd) and (Data[d1]<>':') do inc(d1);
          Data[d1]:=#0;
          inc(d1);
          if (d1<dEnd) and (Data[d1]=' ') then inc(d1);
          Data[dEnd]:=#0;

          if FHeadersIndexIn=FHeadersSize then
           begin
            inc(FHeadersSize,$20);//growstep
            SetLength(FHeaders,FHeadersSize);
           end;
          FHeaders[FHeadersIndexIn].Name:=@Data[dLine];
          FHeaders[FHeadersIndexIn].Value:=@Data[d1];
          inc(FHeadersIndexIn);

         end
        else
          dEnd:=dNext;//end loop
        dLine:=dNext;
       end;
    until dEnd=dNext;
    FCache[0].Index:=dLine;
    FHeadersIndexOut:=FHeadersIndexIn;

    //buffer:

    //TODO: streaming requests: keep reading requests while response is streaming out

    XxmProjectRegistry.CheckRegistry;//TODO: behind the scenes thread?

    d1:=dURI1;
    //if FData[d1]<>'/' then //raise 400'Bad Request'
    inc(d1);
    d2:=d1;
    while (d2<dURI2) and not(Data[d2] in ['/','?','&','$','#']) do inc(d2);
    n:=d2-d1;
    SetLength(FProjectName,n);
    Move(Data[d1],FProjectName[1],n);
    if (d2<=dURI2) and (Data[d2]='/') then inc(d2);

    p:=XxmProjectRegistry.GetProjectName(GetRequestHeader('Host'),PUTF8Char(FProjectName));
    if FProjectName='' then
     begin
      s:='/'+UTF8String(p)+'/'+UTF8String(PUTF8Char(@Data[d2]));
      Redirect(PUTF8Char(s),true);
     end;
    d1:=d2;
    FPageClass:='['+UTF8String(p)+']';
    FRedirectPrefix:='/'+FProjectName;
    FProjectEntry:=XxmProjectRegistry.GetProjectEntry(p);
    if FProjectEntry=nil then
      if UTF8CmpI(PUTF8Char(FProjectName),'favicon.ico') then
       begin
        n:=Length(XxmProjectRegistry.FavIcon);
        SetResponseHeader('Content-Length',Store(IntToStr8(n)));
        SetResponseHeader('Content-Type','image/x-icon');
        FAutoEncoding:=aeContentDefined;
        SendHeader;
        if FSocket.SendBuf(XxmProjectRegistry.FavIcon[0],n)<>n then
          raise EXxmTransferError.Create(SysErrorMessage(GetLastError));
        raise EXxmPageRedirected.Create('favicon.ico');//skip the rest
       end
      else
        raise EXxmProjectNotFound.Create('xxm Project not found "'+string(FProjectName)+'"')
    else
      if (d2>dURI1) and (Data[d2-1]<>'/') then
       begin
        s:=FURI;
        n:=d2-dURI1;
        s:=Copy(s,1,n)+'/'+Copy(s,n+1,Length(s)-n);
        Redirect(PUTF8Char(s),true);
       end;

    d2:=d1;
    while (d2<dURI2) and not(Data[d2] in ['?','&','$','#']) do inc(d2);
    n:=d2-d1;
    SetLength(FFragmentName,n);
    Move(Data[d1],FFragmentName[1],n);
    if d2<dURI2 then FQueryString:=@Data[d2+1]; //else FQueryString:=nil;//see Clear

    //SetResponseHeader('Server','xxm');//TODO: SelfVersion

    p:=GetRequestHeader('Content-Length');
    if p<>nil then
     begin
      n:=StrToInt8(p);
      if n<PostDataThreshold then
        ds:=THeapStream.Create
      else
       begin
        SetLength(FPostTempFile,$400);
        SetLength(FPostTempFile,GetTempPath($400,PChar(FPostTempFile)));
        FPostTempFile:=FPostTempFile+'xxm_'+
          IntToHex(integer(Self),8)+'_'+IntToHex(GetTickCount,8)+'.dat';
        ds:=TFileStream.Create(FPostTempFile,fmCreate);
       end;
      ds.Size:=n;
      ds.Position:=0;
      FPostData:=THandlerReadStreamAdapter.Create(FSocket,n,ds,@Data[dLine],dIndex-dLine);
     end;

    if FVerb='TRACE' then
     begin
      FStatusCode:=501;
      FStatusText:='Not Implemented';
      x:='<h1>Not Implemented</h1>';
      FSocket.SendBuf(x[1],Length(x));
      //SendStr();
      //Flush;
     end
    else
     begin
      if FVerb='HEAD' then
       begin
        SetResponseHeader('Content-Length','0');
       end
      else
      if FVerb='OPTIONS' then
       begin
        SetResponseHeader('Allow','OPTIONS, GET, HEAD, POST');
        SetResponseHeader('Public','OPTIONS, GET, HEAD, POST');
        SetResponseHeader('Content-Length','0');
       end;

      //clear buffer just in case
      //if FContentBuffer<>nil then FContentBuffer.Position:=0;

      //LoadPage;
      if @XxmProjectCheckHandler<>nil then
        if not(XxmProjectCheckHandler(FProjectEntry,Self,FProjectName)) then
         begin
          FProjectEntry:=nil;
          raise EXxmProjectCheckFailed.Create(string(FProjectName));
         end;
      FProjectEntry.OpenContext;
      try
        if FProjectEntry.Negotiate then AuthSChannel('Negotiate',FCredNego) else
          if FProjectEntry.NTLM then AuthSChannel('NTLM',FCredNTLM);

        //TODO SetBufferSize(FProjectEntry.BufferSize);
        FPage:=FProjectEntry.xxmPage(FProjectEntry.Project,Self,@FFragmentName[1]);

        if @FPage=nil then
         begin
          //find a file
          FPageClass:='['+FProjectName+']GetFilePath';
          FProjectEntry.GetFilePath(FFragmentName,FSingleFileSent,x);
          fh:=CreateFileW(PWideChar(FSingleFileSent),
            GENERIC_READ,FILE_SHARE_READ,nil,OPEN_EXISTING,
            FILE_ATTRIBUTE_NORMAL or FILE_FLAG_SEQUENTIAL_SCAN,0);
          if (fh<>INVALID_HANDLE_VALUE) then
           begin
            //TODO: GetRequestHeader('Range')
            try
              if GetFileInformationByHandle(fh,fd) then
               begin
                FileTimeToSystemTime(fd.ftLastWriteTime,st);
                y:=UTF8String(RFC822DateGMT(SystemTimeToDateTime(st)));
                fs:=fd.nFileSizeHigh shl 32 or fd.nFileSizeLow;
               end
              else
               begin
                y:='';
                fs:=0;
               end;
              FAutoEncoding:=aeContentDefined;
              SetResponseHeader('Content-Type',Store(x));
              //TODO: Cache-Control max-age (and others?), other 'If-'s?
              if (y<>'') and UTF8CmpI(GetRequestHeader('If-Modified-Since'),@y[1]) then
               begin
                FStatusCode:=304;
                FStatusText:='Not Modified';
                SendHeader;
                CloseHandle(fh);
               end
              else
               begin
                //send the file
                if y<>'' then SetResponseHeader('Last-Modified',Store(y));
                if fs<>0 then SetResponseHeader('Content-Length',Store(IntToStr8(fs)));
                FAutoEncoding:=aeContentDefined;

                SendHeader;

                ds:=TOwningHandleStream.Create(fh);//does CloseHandle(fh) when done
{//TODO
                if fs>SpoolingThreshold then
                 begin
                  ds.Seek(0,soFromEnd);//used by SpoolingConnections.Add
                  SpoolingConnections.Add(Self,AData,true);
                 end
                else
}
                 begin
                  SendStream(ds);
                  //if FBufferSize<>0 then Flush;
                 end;

               end;
            except //not finally!
              CloseHandle(fh);
              raise;
            end;
           end
          else
           begin
            FPageClass:='['+FProjectName+']404:'+FFragmentName;
            FPage:=FProjectEntry.xxmPage(FProjectEntry.Project,Self,'404.xxm');
            if @FPage=nil then
             begin
              FStatusCode:=404;
              FStatusText:='File not found';
              x:=HTMLEncode(FFragmentName);
              y:=UTF8String(UTF8ByteOrderMark)+
                '<html><head><title>File not found: '+x+'</title></head>'
                +#13#10'<body style="font-family:sans-serif;background-color:white;color:black;margin:0em;">'
                +#13#10'<h1 style="background-color:red;color:white;margin:0em;padding:0.1em;">'
                  +HTMLEncode(FProjectName)+'</h1>'
                +#13#10'<p style="margin:0.1em;">File not found.<br />'+x+'<br /><b>'
                  +HTMLEncode(UTF8Encode(FSingleFileSent))+'</b><br />'
                  +HTMLEncode(ContextString(csURL))+'</p>'
                +#13#10'<p style="background-color:red;color:white;font-size:0.8em;margin:0em;padding:0.2em;text-align:right;">'
                +#13#10'<a href="http://yoy.be/xxm/" style="color:white;">'
                  +HTMLEncode(SelfVersion)+'</a></p></body></html>'
                ;
              SetResponseHeader('Content-Length',Store(IntToStr8(Length(y))));
              SetResponseHeader('Content-Type','text/html; charset="utf-8"');
              FAutoEncoding:=aeContentDefined;//since included in y here
              SendHeader;
              FSocket.SendBuf(y[1],Length(y));
             end
            else
             begin
              FPage(Self,[FFragmentName,FSingleFileSent,x],[]);
              //TODO: if FBufferSize<>0 then FlushFinal;
             end;
           end;

         end
        else
         begin
          //build the page
          FPageClass:=FFragmentName;
          SetResponseHeader('Content-Type','text/html');//default

          //build page
          FPage(Self,[],[]);

{//TODO
          //close page
          if State in [ctHeaderNotSent..ctResponding] then
           begin
            if State<>ctResponding then
             begin
              if FBufferSize=0 then i:=0 else i:=FContentBuffer.Position;
              if (i=0) and (FStatusCode=200) then
               begin
                FStatusCode:=204;
                FStatusText:='No Content';
               end;
              if (FStatusCode<>304) and not(FChunked) then //if State<>ctHeaderOnly then
                SetResponseHeader('Content-Length',Store(IntToStr8(i)));
              if i=0 then
               begin
                SendHeader;
                //State:=ctHeaderOnly;
               end;
             end;
            FlushFinal;
           end;
}
         end;

      finally
        FProjectEntry.CloseContext;
      end;
      FProjectEntry.ClearContext(Self);

     end;

  except
    on EXxmPageRedirected do Flush;
    on EXxmResponseHeaderOnly do Flush;
    on EXxmProjectCheckFailed do ;//assert output done
    on EXxmConnectionLost do ;
    on e:Exception do
     begin
      if not(FProjectEntry.HandleException(Self,FPageClass,
        UTF8Encode(e.ClassName),UTF8Encode(e.Message))) then
       begin
        FStatusCode:=500;
        FStatusText:='Internal Server Error';
        if not FHeaderSent then SendHeader;
        x:=HTMLEncode(FPageClass);
        y:=
          '<html><head><title>Error: '+x+'</title></head>'
          +#13#10'<body style="font-family:sans-serif;background-color:white;color:black;margin:0em;">'
          +#13#10'<h1 style="background-color:red;color:white;margin:0em;padding:0.1em;">'+x+'</h1>'
          +#13#10'<p style="margin:0.1em;">An error occurred while rendering this page.<br />'
            +HTMLEncode(ContextString(csURL))+' <i>'+HTMLEncode(UTF8String(e.ClassName))+'</i><br />'
          +#13#10'<b>'+HTMLEncode(UTF8String(e.Message))+'</b><br />'
          +#13#10'QueryString: '+HTMLEncode(ContextString(csQueryString))+'<br />'
          +#13#10'Post data: '
          ;
        try
          if FPostData=nil then
            y:=y+'none'
          else
            y:=y+IntToStr8(FPostData.Size)+' bytes';
        except
          y:=y+'unknown';
        end;
        y:=y+'</p>'
          +#13#10'<p style="background-color:red;color:white;font-size:0.8em;margin:0em;padding:0.2em;">'
          +#13#10'<a href="http://yoy.be/xxm/" style="float:right;color:white;">'+HTMLEncode(SelfVersion)+'</a>'
          ;
        z:=ContextString(csURL);
        x:=ContextString(csReferer);
        if (x<>'') and (x<>z) then y:=y
          +#13#10'<a href="'+HTMLEncode(x)+'" style="color:white;">back</a>';
        if UTF8CmpI(FVerb,'GET') then y:=y
          +#13#10'<a href="'+HTMLEncode(z)+'" style="color:white;">refresh</a>';
        y:=y
          +#13#10'&nbsp;</p></body></html>'
          ;
        Context_SendHTML(Self,PUTF8Char(y));//FSocket.SendBuf(y[1],Length(y));//attention FAutoEncoding could be set here!
       end;
      //TODO: FlushFinal;
      FSocket.Disconnect;
     end;
  end;

  //TODO: prevent empty request? (if not FHeaderSent then bad request?
  //TODO: endRequest in all except cases?
  FSocket.Disconnect;

end;

function TxxmContext.ContextString(Value: integer): PUTF8Char;
var
  x:UTF8String;
begin
  case Value of

    csVersion:
      Result:=PUTF8Char(SelfVersion);
    csProjectName:
      Result:=PUTF8Char(FProjectName);
    csURL:
     begin
      x:=GetRequestHeader('Host');
      if x='' then x:='localhost';//?
      {//TODO:
      if (FSocket.Port<>0) and (FSocket.Port<>80) then //TODO 443 with https
        x:=x+':'+IntToStr8(FSocket.Port);
      }
      x:='http://'+x+UTF8String(FURI);//TODO: https?
      Result:=Store(x);//TODO: keep FURL?
     end;
    csLocalURL:
      Result:=PUTF8Char(FFragmentName);
    csVerb:
      Result:=FVerb;
    csExtraInfo:
      Result:='';//reserved
    csUserAgent:
      Result:=GetRequestHeader('User-Agent');
    csQueryString:
      Result:=FQueryString;//TODO: if FParamsParsed then reconstruct?
    csPostMimeType:
      Result:=GetRequestHeader('Content-Type');
    csReferer:
      Result:=GetRequestHeader('Referer');
    csLanguage:
      Result:=GetRequestHeader('Accept-Language');
    csAcceptedMimeTypes:
      Result:=GetRequestHeader('Accept');
    csRemoteAddress:
      Result:=Store(UTF8Encode(FSocket.Address));
    csRemoteHost:
      Result:=Store(UTF8Encode(FSocket.HostName));
    csAuthUser,csAuthPassword:
      Result:=Store(AuthValue(Value));

    else raise EXxmError.Create('Unknown ContextString '+IntToStr(Value));
  end;
end;

procedure TxxmContext.Redirect(URL: PUTF8Char; Relative: boolean);
var
  NewURL,Res:UTF8String;
begin
  if FHeaderSent then
    raise EXxmHeaderAlreadySent.Create('Header already sent.');
  FStatusCode:=302;
  FStatusText:='Object moved';//301,'Moved Permanently'?
  //SetResponseHeader('Cache-Control','no-cache, no-store');//?
  NewURL:=URL;
  if Relative and (NewURL<>'') and (NewURL[1]<>'/') then
    NewURL:=FRedirectPrefix+NewURL;
  SetResponseHeader('Location',Store(NewURL));
  Res:='<h1>Object moved</h1><p><a href="'+
    HTMLEncode(NewURL)+'">'+HTMLEncode(NewURL)+'</a></p>'#13#10;
  SetResponseHeader('Content-Type','text/html');
  SetResponseHeader('Content-Length',Store(IntToStr8(Length(Res))));
  SendHeader;
  FSocket.SendBuf(Res[1],Length(Res));
  raise EXxmPageRedirected.Create(string(URL));
end;

function TxxmContext.GetRequestHeader(Name: PUTF8Char): PUTF8Char;
var
  i:NativeUInt;
begin
  i:=0;
  //TODO: search binary tree?
  while (i<FHeadersIndexIn) and not(UTF8CmpI(Name,FHeaders[i].Name)) do inc(i);
  if i<FHeadersIndexIn then
    Result:=FHeaders[i].Value
  else
    Result:=nil;
end;

procedure TxxmContext.SetResponseHeader(Name,Value:PUTF8Char;
  AllowDuplicates:boolean);
var
  i:NativeUInt;
begin
  if FHeaderSent then
    raise EXxmHeaderAlreadySent.Create('Header already sent.');

  //callers are responsible to have the memory of Name and Value
  //be available for the duration of the request/response lifetime

  if AllowDuplicates then
    i:=FHeadersIndexOut
  else
   begin
    //find by name
    i:=FHeadersIndexIn;
    while (i<FHeadersIndexOut) and not(UTF8CmpI(FHeaders[i].Name,Name)) do inc(i);
   end;
  if i=FHeadersIndexOut then
   begin
    //add new
    if FHeadersIndexOut=FHeadersSize then
     begin
      inc(FHeadersSize,$20);//growstep
      SetLength(FHeaders,FHeadersSize);
     end;
    FHeaders[FHeadersIndexOut].Name:=Name;
    inc(FHeadersIndexOut);
   end;
  FHeaders[i].Value:=Value;
end;

function TxxmContext.GetResponseHeader(Name:PUTF8Char):PUTF8Char;
var
  i:NativeUInt;
begin
  //read-only! use SetResponseHeader to modify/overwrite
  i:=FHeadersIndexIn;
  while (i<FHeadersIndexOut) and not(UTF8CmpI(FHeaders[i].Name,Name)) do inc(i);
  if i<FHeadersIndexOut then
    Result:=FHeaders[i].Value
  else
    Result:=nil;
end;

function TxxmContext.GetCookie(Name: PUTF8Char): PUTF8Char;
begin
  if FCookie.Data='' then
    FCookie.SplitHeaderValue(GetRequestHeader('Cookie'),false);
  Result:=FCookie[Name];
end;

function TxxmContext.Store(const Data:UTF8String):PUTF8Char;
var
  i,l:NativeUInt;
begin
  l:=Length(Data)+1;//include #0
  if l>CacheDataSize then
    raise EXxmError.Create('Store: Data too large for cache chunks');
  //assert FCacheIndex<>0 (see clear)
  i:=FCacheIndex-1;
  if FCache[i].Index+l>CacheDataSize then
   begin
    //TODO: check previous FCache[]?
    if FCacheIndex=FCacheSize then
     begin
      inc(FCacheSize,CacheSizeGrowStep);
      SetLength(FCache,FCacheSize);
     end;
    i:=FCacheIndex;
    SetLength(FCache[i].Data,CacheDataSize);
    FCache[i].Index:=0;
    inc(FCacheIndex);
   end;
  Result:=@FCache[i].Data[FCache[i].Index];
  Move(Data[1],Result^,l);
  inc(FCache[i].Index,l);
end;

procedure TxxmContext.SendHeader;
var
  s:UTF8String;
  i:NativeUInt;
  n:NativeInt;
  p:PUTF8Char;
const
  AutoEncodingCharset:array[TXxmAutoEncoding] of UTF8String=(
    '',//aeContentDefined
    '; charset="utf-8"',
    '; charset="utf-16"',
    '; charset="iso-8859-15"'
  );
begin
  if FHeaderSent then
    raise EXxmHeaderAlreadySent.Create('Header already sent.');

  //auto-encoding?
  p:=GetResponseHeader('Content-Type');
  if p=nil then
    SetResponseHeader('Content-Type',Store('text/html'//default
      +AutoEncodingCharset[FAutoEncoding]),false)
  else
    if FAutoEncoding<>aeContentDefined then //and not ';' in p^?
      SetResponseHeader('Content-Type',Store(UTF8String(p)
        +AutoEncodingCharset[FAutoEncoding]),false);

  //TODO: calculate total length beforehand
  s:=UTF8String(FVersion)+' '+IntToStr8(FStatusCode)+' '+UTF8String(FStatusText)+#13#10;
  i:=FHeadersIndexIn;
  while i<FHeadersIndexOut do
   begin
    //TODO: sanitize Name, Value values
    s:=s+UTF8String(FHeaders[i].Name)+': '+UTF8String(FHeaders[i].Value)+#13#10;
    inc(i);
   end;
  s:=s+#13#10;

  {
  //TODO
        if (FBufferSize<>0) and AllowChunked and
          (GetResponseHeader('Content-Length')='') and
          (GetResponseHeader('Transfer-Encoding')='') then
         begin
          FChunked:=true;
          AddResponseHeader('Transfer-Encoding','chunked');
         end;
}


  //auto-encoding: byte-order-mark?
  if (FVerb<>'HEAD') and (FVerb<>'OPTIONS') then //see also below
    case FAutoEncoding of
      aeUtf8:s:=s+UTF8String(UTF8ByteOrderMark);
      aeUtf16:s:=s+UTF8String(UTF16ByteOrderMark);
    end;

  FHeaderSent:=true;
  n:=FSocket.SendBuf(s[1],Length(s));
  if n<=0 then
   begin
    FSocket.Disconnect;
    raise EXxmTransferError.Create('Connection Lost');
   end;

  if (FVerb='HEAD') or (FVerb='OPTIONS') then
    raise EXxmResponseHeaderOnly.Create(string(FVerb));
end;

procedure TxxmContext.SendStream(s: TStream);
const
  dSize=$10000;
  hex:array[0..15] of AnsiChar='0123456789ABCDEF';
var
  d:array[0..dSize-1] of byte;
  i,k,l:integer;
begin
  if not(FHeaderSent) and (GetResponseHeader('Content-Type')=nil) then
   begin
    SetResponseHeader('Content-Type','application/octet-stream');
    FAutoEncoding:=aeContentDefined;
   end;
  if not(FHeaderSent) then SendHeader;
  Flush;
  if FChunked then
    repeat
      l:=dSize-12;
      l:=s.Read(d[10],l);
      if l<>0 then
       begin
        d[8]:=13;//CR
        d[9]:=10;//LF
        i:=8;
        k:=l;
        repeat
          dec(i);
          d[i]:=byte(hex[k and $F]);
          k:=k shr 4;
        until k=0;
        d[l+10]:=13;//CR
        d[l+11]:=10;//LF
        l:=l+12-i;
        if FSocket.SendBuf(d[i],l)<>l then
          raise EXxmTransferError.Create(SysErrorMessage(GetLastError));
       end;
    until l=0
  else
    repeat
      l:=s.Read(d[1],dSize);
      if l<>0 then
        if FSocket.SendBuf(d[1],l)<>l then
          raise EXxmTransferError.Create(SysErrorMessage(GetLastError));
    until l=0;
end;

type
  TxxmCrossProjectIncludeCheck=class(TObject)
  public
    Entry:TProjectEntry;
    Next:TxxmCrossProjectIncludeCheck;
    constructor Create(AEntry: TProjectEntry;
      ANext: TxxmCrossProjectIncludeCheck);
  end;

procedure TxxmContext.Include(Address: PUTF8Char;
  const Values: array of Variant; const Objects: array of pointer);
var
  p,pb:CxxmFragment;
  pa,pc,pn:UTF8String;
  pe:TProjectEntry;
  px:TXxmCrossProjectIncludeCheck;
  i,j,l:integer;
begin
  if @FPage=nil then
    raise EXxmIncludeOnlyOnBuild.Create('Include only allowed when building a page');
  if FIncludeDepth=MaxIncludeDepth then
    raise EXxmIncludeStackFull.Create('Maximum level of includes exceeded');
  pa:=Address;
  pe:=FProjectEntry;
  pn:=FProjectName;
  pb:=FPage;
  pc:=FPageClass;
  inc(FIncludeDepth);
  try
    if Copy(pa,1,4)='xxm:' then
      if pe.AllowInclude then
       begin
        //cross-project include
        l:=Length(Address);
        i:=5;
        if (i<=l) and (pa[i]='/') then inc(i);
        if (i<=l) and (pa[i]='/') then inc(i);
        j:=i;
        while (j<=l) and not(pa[j] in ['/','?','&','$','#']) do inc(j);
        FProjectName:=Copy(pa,i,j-i);
        if (j<=l) and (pa[j]='/') then inc(j);
        FProjectEntry:=XxmProjectRegistry.GetProjectEntry(PUtf8Char(FProjectName));
        //XxmProjectCheckHandler but check for recurring PE's to avoid deadlock
        if @XxmProjectCheckHandler<>nil then
         begin
          px:=FIncludeCheck as TXxmCrossProjectIncludeCheck;
          while (px<>nil) and (px.Entry<>FProjectEntry) do px:=px.Next;
          if px=nil then
            if not(XxmProjectCheckHandler(FProjectEntry,Self,FProjectName)) then
              raise EXxmProjectCheckFailed.Create(string(FProjectName));
          //if px<>nil then raise? just let the request complete
         end;
        if @FProjectEntry.xxmFragment=nil then
          raise EXxmIncludeNoFragmentHandler.Create('Project "'+string(FProjectName)+
            '" doesn''t provide a fragment handler');
        p:=FProjectEntry.xxmFragment(FProjectEntry.Project,Self,PUTF8Char(
          Copy(Address,j,l-j+1)));//TODO: RelativePath
        if @p=nil then
          raise EXxmIncludeFragmentNotFound.Create(
            'Include fragment not found "'+string(pa)+'"');
        FPage:=p;
        px:=TXxmCrossProjectIncludeCheck.Create(pe,
          FIncludeCheck as TxxmCrossProjectIncludeCheck);
        try
          FIncludeCheck:=px;
          FProjectEntry.OpenContext;
          try
            FPageClass:=FProjectName+':'+pa+' < '+pc;
            p(Self,Values,Objects);
          finally
            FProjectEntry.CloseContext;
          end;
        finally
          FIncludeCheck:=px.Next;
          px.Free;
        end;
       end
      else
        raise EXxmIncludeCrossProjectDisabled.Create(
          'Cross-project includes disabled')
    else
     begin
      //FPage.Project?
      pn:='';
      if @FProjectEntry.xxmFragment=nil then
        raise EXxmIncludeNoFragmentHandler.Create('Nu fragment handler is available');
      p:=FProjectEntry.xxmFragment(FProjectEntry.Project,Self,PUTF8Char(pa));//TODO: RelativePath
      if @p=nil then
        raise EXxmIncludeFragmentNotFound.Create(
          'Include fragment not found "'+string(pa)+'"');
      FPage:=p;
      FPageClass:=pa+' < '+pc;
      p(Self,Values,Objects);
     end;
    FPageClass:=pc; //not in finally: preserve on exception
  finally
    dec(FIncludeDepth);
    FProjectEntry:=pe;
    FPage:=pb;
  end;
end;

procedure TxxmContext.Flush;
begin
  //TODO:
end;

procedure TxxmContext.AuthSChannel(const Package:UTF8String;var Cred:TCredHandle);
var
  s,t:UTF8String;
  p:PCtxtHandle;
  r,f:cardinal;
  d1,d2:TSecBufferDesc;
  d:array of TSecBuffer;
  n:TSecPkgContextNames;
begin
  s:=AuthParse('NTLM');
  if s='' then
   begin
    FStatusCode:=401;
    FStatusText:='Unauthorized';
    SetResponseHeader('Connection','keep-alive');//TODO: keep in session
    SetResponseHeader('WWW-Authenticate',Store(Package));
    s:='<h1>Authorization required</h1>';
    FSocket.SendBuf(s[1],Length(s));
    raise EXxmPageRedirected.Create('401');
   end
  else
   begin
    if Cred.dwLower=nil then
      if AcquireCredentialsHandle(nil,PAnsiChar(Package),SECPKG_CRED_INBOUND,
        nil,nil,nil,nil,@Cred,nil)<>0 then RaiseLastOSError;

    SetLength(d,3);
    SetLength(t,$10000);

    d1.ulVersion:=SECBUFFER_VERSION;
    d1.cBuffers:=2;
    d1.pBuffers:=@d[0];

    d[0].cbBuffer:=Length(s);
    d[0].BufferType:=SECBUFFER_TOKEN;
    d[0].pvBuffer:=PUTF8Char(s);

    d[1].cbBuffer:=0;
    d[1].BufferType:=SECBUFFER_EMPTY;
    d[1].pvBuffer:=nil;

    d2.ulVersion:=SECBUFFER_VERSION;
    d2.cBuffers:=1;
    d2.pBuffers:=@d[2];

    d[2].cbBuffer:=$10000;;
    d[2].BufferType:=SECBUFFER_TOKEN;
    d[2].pvBuffer:=PUTF8Char(t);

    if (FCtxt.dwLower=nil) and (FCtxt.dwUpper=nil) then
      p:=nil
    else
      p:=@FCtxt;
    r:=AcceptSecurityContext(@Cred,p,@d1,
      ASC_REQ_REPLAY_DETECT or ASC_REQ_SEQUENCE_DETECT,SECURITY_NATIVE_DREP,
      @FCtxt,@d2,@f,nil);

    if r=SEC_E_OK then
     begin
      r:=QueryContextAttributes(@FCtxt,SECPKG_ATTR_NAMES,@n);
      if r=0 then
        AuthSet(n.sUserName,'')
      else
        AuthSet('???'+UTF8String(SysErrorMessage(r)),'');//raise?
      DeleteSecurityContext(@FCtxt);
      FCtxt.dwLower:=nil;
      FCtxt.dwUpper:=nil;
     end
    else
    if r=SEC_I_CONTINUE_NEEDED then
     begin
      SetLength(t,d[2].cbBuffer);
      FStatusCode:=401;
      FStatusText:='Unauthorized';
      SetResponseHeader('Connection','keep-alive');
      SetResponseHeader('WWW-Authenticate',Store(Package+' '+Base64Encode(t)));
      s:='<h1>Authorization required</h1>';
      FSocket.SendBuf(s[1],Length(s));
      raise EXxmPageRedirected.Create('401.1');
     end
    else
      raise EXxmError.Create(SysErrorMessage(r));
   end;
end;

function TxxmContext.AuthParse(const Scheme: UTF8String): UTF8String;
var
  s:UTF8String;
  i,j,l:integer;
  a,b:byte;
begin
  //Base64Decode see http://www.faqs.org/rfcs/rfc2045.html #6.8
  s:=GetRequestHeader('Authorization');
  l:=Length(s);
  if l=0 then
    Result:=''
  else
   begin
    i:=Length(Scheme);
    if (l<i) or (Copy(s,1,i+1)<>Scheme+' ') then
      raise EXxmError.Create('Unexpected authorization method');
    inc(i,2);
    j:=0;
    SetLength(Result,l*3 div 4);
    while i<=l do
     begin
      case s[i] of
        'A'..'Z':a:=byte(s[i])-65;
        'a'..'z':a:=byte(s[i])-71;
        '0'..'9':a:=byte(s[i])+4;
        '+':a:=62;
        '/':a:=63;
        //'=':;
        else raise EXxmError.Create('Authorization: invalid base64 character');
      end;
      inc(i);
      if i<=l then
       begin
        case s[i] of
          'A'..'Z':b:=byte(s[i])-65;
          'a'..'z':b:=byte(s[i])-71;
          '0'..'9':b:=byte(s[i])+4;
          '+':b:=62;
          '/':b:=63;
          //'=':;
          else raise EXxmError.Create('Authorization: invalid base64 character');
        end;
        inc(j);
        Result[j]:=UTF8Char((a shl 2) or (b shr 4));
        inc(i);
       end
      else
        b:=0;//counter warning
      if i<=l then
       begin
        case s[i] of
          'A'..'Z':a:=byte(s[i])-65;
          'a'..'z':a:=byte(s[i])-71;
          '0'..'9':a:=byte(s[i])+4;
          '+':a:=62;
          '/':a:=63;
          '=':a:=$FF;
          else raise EXxmError.Create('Authorization: invalid base64 character');
        end;
        if a<>$FF then
         begin
          inc(j);
          Result[j]:=UTF8Char((b shl 4) or (a shr 2));
         end;
        inc(i);
       end;
      if i<=l then
       begin
        case s[i] of
          'A'..'Z':b:=byte(s[i])-65;
          'a'..'z':b:=byte(s[i])-71;
          '0'..'9':b:=byte(s[i])+4;
          '+':b:=62;
          '/':b:=63;
          '=':b:=$FF;
          else raise EXxmError.Create('Authorization: invalid base64 character');
        end;
        if b<>$FF then
         begin
          inc(j);
          Result[j]:=UTF8Char((a shl 6) or b);
         end;
        inc(i);
       end;
     end;
    SetLength(Result,j);
   end;
end;

function TxxmContext.AuthValue(cs: TXxmContextString): UTF8String;
var
  s:UTF8String;
  i,l:integer;
begin
  if not FAuthParsed then
   begin
    s:=AuthParse('Basic');
    l:=Length(s);
    if l=0 then
     begin
      FAuthUserName:='';
      FAuthPassword:='';
     end
    else
     begin
      i:=1;
      while (i<=l) and (s[i]<>':') do inc(i);
      //if i>l then raise?
      FAuthUserName:=Copy(s,1,i-1);
      FAuthPassword:=Copy(s,i+1,l-i);
     end;
    FAuthParsed:=true;
   end;
  if cs=csAuthPassword then Result:=FAuthPassword else Result:=FAuthUserName;
end;

procedure TxxmContext.AuthSet(const Name, Pwd: UTF8String);
begin
  FAuthParsed:=true;//done by inheritant
  FAuthUserName:=Name;
  FAuthPassword:=Pwd;
end;

procedure TxxmContext.ParseParams;
  function AddPar(Origin,Name,Value:PUTF8Char):NativeInt;
  begin
    if FParamsIndex=FParamsSize then
     begin
      inc(FParamsSize,$100);//grow step
      SetLength(FParams,FParamsSize);
     end;
    Result:=FParamsIndex;
    FParams[FParamsIndex].Context:=Self;
    FParams[FParamsIndex].Index:=FParamsIndex;
    FParams[FParamsIndex].Origin:=Origin;
    FParams[FParamsIndex].Name:=Name;
    FParams[FParamsIndex].Value:=Value;
    FParams[FParamsIndex].ContentType:=nil;
    FParams[FParamsIndex].PostDataPos:=0;
    FParams[FParamsIndex].PostDataLen:=0;
    inc(FParamsIndex);
  end;
var
  p,q,pn,pv,pt:PUTF8Char;
  h,v:TKeyValues;
  pb,s:UTF8String;
  i,l:integer;
  sn:TStreamNozzle;
begin
  //assert not FParamsParsed
  //assert FParamsIndex=0

  //query string
  p:=Store(FQueryString);//work on a copy, keep original
  if p<>nil then
    while p^<>#0 do
     begin
      pn:=p;
      while (p^<>#0) and (p^<>'=') do inc(p);
      if p^<>#0 then
       begin
        p^:=#0;
        inc(p);
       end;
      pv:=p;
      while (p^<>#0) and (p^<>'&') do inc(p);
      if p^<>#0 then
       begin
        p^:=#0;
        inc(p);
       end;
      AddPar('GET',pn,pv);//TODO:URLDecode!!!
     end;

  if FPostData<>nil then
   begin
    FPostData.Seek(0,soFromBeginning);
    q:=h.SplitHeaderValue(GetRequestHeader('Content-Type'),true);
    if q=nil then //redirect in response to POST request, but StgMed prevails! drop it
      FreeAndNil(FPostData)
    else
    if q='application/x-www-form-urlencoded' then
     begin
      //read into string
      //TODO: encoding??
      i:=0;
      repeat
        l:=$1000;
        SetLength(s,i+l);
        l:=FPostData.Read(s[i+1],l);
        inc(i,l);
        //TODO: if DataProgressAgent<>nil then DataProgressAgent.ReportProgress('','',p);
      until l=0;
      SetLength(s,i);

      p:=Store(s);//work on a copy, keep original
      while p^<>#0 do
       begin
        pn:=p;
        while (p^<>#0) and (p^<>'=') do inc(p);
        if p^<>#0 then
         begin
          p^:=#0;
          inc(p);
         end;
        pv:=p;
        while (p^<>#0) and (p^<>'&') do inc(p);
        if p^<>#0 then
         begin
          p^:=#0;
          inc(p);
         end;
        AddPar('POST',pn,pv);//TODO:URLDecode!!!
       end;

     end
    else
    if q='multipart/form-data' then
     begin
      pb:=h['boundary'];
      if pb='' then
        raise EXxmError.Create('Unable to get multipart/form-data boundary');

      sn:=TStreamNozzle.Create(Self,
        FProgressCallback,FProgressRequestID,FProgressReportStep);
      try
        //initialization, find first boundary
        sn.CheckBoundary(pb);

        while not(sn.MultiPartDone) do
         begin

          pn:=nil;
          pv:=nil;
          pt:=nil;
          h.KeysIndex:=0;
          sn.GetHeader(h);
          for i:=0 to h.KeysIndex-1 do
           begin
            if UTF8CmpI(h.Keys[i].Key,'Content-Disposition') then
             begin
              v.SplitHeaderValue(h.Keys[i].Value,true);//assert results 'form-data'
              pn:=v['name'];
              pv:=v['filename'];
             end
            else
            if UTF8CmpI(h.Keys[i].Key,'Content-Type') then
              pt:=h.Keys[i].Value
            else
              ;//raise EXxmError.Create('Unknown multipart header "'+h.Keys[i].Key+'"');
            //TODO: transfer encoding?
           end;

          if pt=nil then
            AddPar('POST',pn,sn.GetString(pb))
          else
           begin
            i:=AddPar('FILE',pn,pv);
            FParams[i].ContentType:=pt;
            sn.GetData(pn,pv,pt,FParams[i].PostDataPos,FParams[i].PostDataLen);
           end;

         end;
      finally
        sn.Free;
      end;

     end
    else
      raise EXxmUnknownPostMime.Create('Unsupported Post Mime type "'+string(q)+'"');

    FPostData.Seek(0,soFromBeginning);
   end;
  FParamsParsed:=true;
end;

function TxxmContext.GetParam(Name: PUTF8Char): PParamInfo;
var
  i:NativeUInt;
begin
  if not FParamsParsed then ParseParams;
  i:=0;
  while (i<FParamsIndex) and not(UTF8CmpI(Name,FParams[i].Name)) do inc(i);
  if i<FParamsIndex then
    Result:=@FParams[i]
  else
    Result:=nil;
end;

function TxxmContext.GetParamCount: integer;
begin
  if not FParamsParsed then ParseParams;
  Result:=FParamsIndex;
end;

function TxxmContext.GetParamByIdx(Index: NativeUInt): PParamInfo;
begin
  if not FParamsParsed then ParseParams;
  if {(Index>=0) and} (Index<FParamsIndex) then
    Result:=@FParams[Index]
  else
    raise ERangeError.Create('GetParamByIdx: index out of range');
end;

{ xxm2 implementation }

function Context_URL(Context:PxxmContext):PUTF8Char; stdcall;
begin
  Result:=TxxmContext(Context).ContextString(csURL);
end;

function Context_SessionID(Context:PxxmContext):PUTF8Char; stdcall;
var
  c:TxxmContext absolute Context;
begin
  //c:=TxxmContext(Context);//see absolute above
  if c.FSessionID='' then
   begin
    c.FSessionID:=c.GetCookie(PUTF8Char(SessionCookie));
    if c.FSessionID='' then
     begin
      c.FSessionID:=UTF8String(Format('%.6x%.6x%.6x%.6x%.6x%.6x',
        [Random($1000000),Random($1000000)
        ,Random($1000000),Random($1000000)
        ,Random($1000000),Random($1000000)
        ]));
      c.SetResponseHeader('Set-Cookie',c.Store(SessionCookie+'='+c.FSessionID+
        '; Path=/; SameSite=Lax'),true);//expiry?
     end;
   end;
  Result:=PUTF8Char(c.FSessionID);
end;

function Context_ContextString(Context:PxxmContext;Value:TxxmContextString):PUTF8Char; stdcall;
begin
  Result:=TxxmContext(Context).ContextString(Value);
end;

function Context_BufferSize(Context:PxxmContext):NativeUInt; stdcall;
begin
  //TODO Result:=TxxmContext(Context).BufferSize;
  Result:=0;
end;

procedure Context_Set_BufferSize(Context:PxxmContext;Value:NativeUInt); stdcall;
begin
  //TODO TxxmContext(Context).BufferSize:=Value;
end;

function Context_Connected(Context:PxxmContext):boolean; stdcall;
begin
  Result:=TxxmContext(Context).Socket.Connected;
end;

procedure Context_Set_Status(Context:PxxmContext;Status:word;Text:PUTF8Char); stdcall;
var
  c:TxxmContext absolute Context;
begin
  if c.FHeaderSent then
    raise EXxmHeaderAlreadySent.Create('Header already sent.');
  c.FStatusCode:=Status;
  c.FStatusText:=Text;
end;

procedure Context_Redirect(Context:PxxmContext;RedirectURL:PUTF8Char;
  Relative:boolean); stdcall;
begin
  TxxmContext(Context).Redirect(RedirectURL,Relative);
end;

function Context_Cookie(Context:PxxmContext;Name:PUTF8Char):PUTF8Char; stdcall;
begin
  Result:=TxxmContext(Context).GetCookie(Name);
end;

procedure Context_Set_Cookie(Context:PxxmContext;Name,Value:PUTF8Char); stdcall;
var
  c:TxxmContext absolute Context;
begin
  //TODO: validate/sanitize name,value
  c.SetResponseHeader('Set-Cookie',c.Store(UTF8String(Name)+'='+UTF8String(Value)),true);
end;

procedure Context_Set_CookieEx(Context:PxxmContext;Name,Value:PUTF8Char;
  KeepSeconds:NativeUInt;Comment,Domain,Path:PUTF8Char;Secure,HttpOnly:boolean); stdcall;

  function CheckColon(p:PUTF8Char):UTF8String;
  var
    q:PUTF8Char;
  begin
    q:=p;
    while (q^<>#0) and (q^<>';') do inc(q);
    if q^=';' then
      Result:='"'+UTF8String(p)+'"'
    else
      Result:=p;
  end;

var
  x:UTF8String;
  c:TxxmContext absolute Context;
begin
  //TODO: sanitize name,value?
  x:=UTF8String(Name)+'='+CheckColon(Value);
  //'; Version=1';
  if Comment<>'' then
    x:=x+'; Comment='+CheckColon(Comment);
  if Domain<>'' then
    x:=x+'; Domain='+CheckColon(Domain);
  if Path<>'' then
    x:=x+'; Path='+CheckColon(Path);
  if KeepSeconds<>0 then
    x:=x+'; Max-Age='+IntToStr8(KeepSeconds)+
      '; Expires='+UTF8String(RFC822DateGMT(Now+KeepSeconds/86400));
  if Secure then
    x:=x+'; Secure';
  if HttpOnly then
    x:=x+'; HttpOnly';
  x:=x+'; SameSite=Lax';//?
  c.SetResponseHeader('Set-Cookie',c.Store(x),true);
end;

function Context_ContentType(Context:PxxmContext):PUTF8Char; stdcall;
begin
  Result:=TxxmContext(Context).GetResponseHeader('Content-Type');
end;

procedure Context_Set_ContentType(Context:PxxmContext;ContentType:PUTF8Char); stdcall;
var
  c:TxxmContext absolute Context;
begin
  if c.FHeaderSent then
    raise EXxmHeaderAlreadySent.Create('Header already sent.');
  c.SetResponseHeader('Content-Type',c.Store(ContentType));
  c.FAutoEncoding:=aeContentDefined;
end;

function Context_AutoEncoding(Context:PxxmContext):TxxmAutoEncoding; stdcall;
begin
  Result:=TxxmContext(Context).FAutoEncoding;
end;

procedure Context_Set_AutoEncoding(Context:PxxmContext;AutoEncoding:TxxmAutoEncoding); stdcall;
var
  c:TxxmContext absolute Context;
begin
  if c.FHeaderSent then
    raise EXxmHeaderAlreadySent.Create('Header already sent.');
  c.FAutoEncoding:=AutoEncoding;
end;

procedure Context_DispositionAttach(Context:PxxmContext;FileName:PUTF8Char); stdcall;
var
  fn:UTF8String;
  i:integer;
  c:TxxmContext absolute Context;
begin
  fn:=FileName;
  for i:=1 to Length(fn) do
    if fn[i] in ['\','/',':','*','?','"','<','>','|'] then fn[i]:='_';
  c.SetResponseHeader('Content-Disposition',c.Store('attachment; filename="'+fn+'"'));
end;

procedure Context_Send(Context:PxxmContext;Data:PUTF8Char); stdcall;
var
  c:TxxmContext absolute Context;
  x:UTF8String;
  w:WideString;
  l:integer;
begin
  if not c.FHeaderSent then c.SendHeader;
  x:=HTMLEncode(Data);
  if c.FAutoEncoding=aeUtf16 then
   begin
    w:=UTF8ToWideString(x);
    l:=Length(w)*2;
    if c.Socket.SendBuf(w[1],l)<>l then
      raise EXxmTransferError.Create(SysErrorMessage(GetLastError));
   end
  else
   begin
    l:=Length(x);
    if c.Socket.SendBuf(x[1],l)<>l then
      raise EXxmTransferError.Create(SysErrorMessage(GetLastError));
   end;
end;

procedure Context_SendHTML(Context:PxxmContext;HTML:PUTF8Char); stdcall;
var
  c:TxxmContext absolute Context;
  p:PUTF8Char;
  w:WideString;
  l:integer;
begin
  if not c.FHeaderSent then c.SendHeader;
  if c.FAutoEncoding=aeUtf16 then
   begin
    w:=UTF8ToWideString(HTML);
    l:=Length(w)*2;
    if c.Socket.SendBuf(w[1],l)<>l then
      raise EXxmTransferError.Create(SysErrorMessage(GetLastError));
   end
  else
   begin
    p:=HTML;
    while p^<>#0 do inc(p);
    l:=NativeUInt(p)-NativeUInt(HTML);
    if c.Socket.SendBuf(HTML^,l)<>l then
      raise EXxmTransferError.Create(SysErrorMessage(GetLastError));
   end;
end;

procedure Context_SendFile(Context:PxxmContext;FilePath:PUTF8Char); stdcall;
var
  f:TFileStream;
begin
  //TODO: memory map?
  //TODO: cache?
  //TODO: spooled delivery?
  f:=TFileStream.Create(string(FilePath),fmOpenRead or fmShareDenyWrite);
  try
    TxxmContext(Context).SendStream(f);
  finally
    f.Free;
  end;
end;

procedure Context_SendStream(Context:PxxmContext;Stream:TObject); stdcall;
begin
  TxxmContext(Context).SendStream(Stream as TStream);
end;

procedure Context_Flush(Context:PxxmContext); stdcall;
begin
  TxxmContext(Context).Flush;
end;

function Context_Parameter(Context:PxxmContext;Name:PUTF8Char):PxxmParameter; stdcall;
begin
  Result:=TxxmContext(Context).GetParam(Name);
end;

function Context_ParameterCount(Context:PxxmContext):NativeUInt; stdcall;
begin
  Result:=TxxmContext(Context).GetParamCount;
end;

function Context_ParameterByIdx(Context:PxxmContext;Index:NativeUInt):PxxmParameter; stdcall;
begin
  Result:=TxxmContext(Context).GetParamByIdx(Index);
end;

procedure Context_Add_Parameter(Context:PxxmContext;Origin,Name,Value:PUTF8Char); stdcall;
begin
  //TODO
end;

function Context_RequestHeader(Context:PxxmContext;Name:PUTF8Char):PUTF8Char; stdcall;
begin
  Result:=TxxmContext(Context).GetRequestHeader(Name);
end;

function Context_RequestHeaderCount(Context:PxxmContext):NativeUInt; stdcall;
begin
  Result:=TxxmContext(Context).FHeadersIndexIn;
end;

procedure Context_RequestHeaderByIdx(Context:PxxmContext;Index:NativeUInt;
  var Name,Value:PUTF8Char); stdcall;
var
  c:TxxmContext absolute Context;
begin
  if {(Index>=0) and} (Index<c.FHeadersIndexIn) then
   begin
    Name:=c.FHeaders[Index].Name;
    Value:=c.FHeaders[Index].Value;
   end
  else
    raise ERangeError.Create('RequestHeaderByIdx: index out of range');
end;

function Context_ResponseHeader(Context:PxxmContext;Name:PUTF8Char):PUTF8Char; stdcall;
begin
  Result:=TxxmContext(Context).GetResponseHeader(Name);
end;

function Context_ResponseHeaderCount(Context:PxxmContext):NativeUInt; stdcall;
var
  c:TxxmContext absolute Context;
begin
  Result:=c.FHeadersIndexOut-c.FHeadersIndexIn;
end;

procedure Context_ResponseHeaderByIdx(Context:PxxmContext;Index:NativeUInt;
  var Name,Value:PUTF8Char); stdcall;
var
  c:TxxmContext absolute Context;
  l:NativeUInt;
begin
  l:=c.FHeadersIndexOut-c.FHeadersIndexIn;
  if {(Index>=0) and} (Index<l) then
   begin
    Name:=@c.FHeaders[c.FHeadersIndexIn+Index].Name;
    Value:=@c.FHeaders[c.FHeadersIndexIn+Index].Value;
   end
  else
    raise ERangeError.Create('ResponseHeaderByIdx: index out of range');
end;

procedure Context_Set_ResponseHeader(Context:PxxmContext;Name,Value:PUTF8Char); stdcall;
begin
  TxxmContext(Context).SetResponseHeader(Name,Value);
end;

function Context_Data(Context:PxxmContext):pointer; stdcall;
begin
  Result:=TxxmContext(Context).FProjectData;
end;

procedure Context_Set_Data(Context:PxxmContext;Data:pointer); stdcall;
begin
  TxxmContext(Context).FProjectData:=Data;
end;

procedure Context_Include(Context:PxxmContext;Address:PUTF8Char;
  const Values:array of Variant;const Objects:array of pointer); stdcall;
begin
  TxxmContext(Context).Include(Address,Values,Objects);
end;

function Context_PostData(Context:PxxmContext):TObject; stdcall;
begin
  Result:=TxxmContext(Context).FPostData;
end;

procedure Context_Set_ProgressCallback(Context:PxxmContext;Callback:CxxmProgress;
  RequestID,Flags,Step:NativeUInt); stdcall;
var
  c:TxxmContext absolute Context;
begin
  if @c.FProgressCallback<>nil then
    raise EXxmError.Create('Only one Context_Set_ProgressCallback per request allowed');
  if c.FParamsParsed then
    raise EXxmError.Create('Call Context_Set_ProgressCallback before accessing parameters');
  if Flags<>0 then
    raise Exception.Create('Context_Set_ProgressCallback Flags not implemented');
  c.FProgressCallback:=Callback;
  c.FProgressRequestID:=RequestID;
  c.FProgressReportStep:=Step;
end;


function Parameter_Origin(Parameter:PxxmParameter):PUTF8Char; stdcall; //'GET','POST','FILE'...
var
  p:PParamInfo absolute Parameter;
begin
  if Parameter=nil then
    Result:='NONE' //default value (raise?)
  else
   begin
    if p<>@(p.Context as TxxmContext).FParams[p.Index] then
      raise EInvalidPointer.Create('Invalid parameter');
    Result:=p.Origin;
   end;
end;

function Parameter_Name(Parameter:PxxmParameter):PUTF8Char; stdcall;
var
  p:PParamInfo absolute Parameter;
begin
  //if Parameter=nil then raise?
  if p<>@(p.Context as TxxmContext).FParams[p.Index] then
    raise EInvalidPointer.Create('Invalid parameter');
  Result:=p.Name;
end;

function Parameter_Value(Parameter:PxxmParameter):PUTF8Char; stdcall;
var
  p:PParamInfo absolute Parameter;
begin
  if Parameter=nil then
    Result:=nil //default value
  else
   begin
    if p<>@(p.Context as TxxmContext).FParams[p.Index] then
      raise EInvalidPointer.Create('Invalid parameter');
    Result:=p.Value;
   end;
end;

function Parameter_AsInteger(Parameter:PxxmParameter):NativeInt; stdcall;
var
  s:string;
begin
  if Parameter=nil then
    Result:=0 //default value
  else
   begin
    s:=Trim(string(PParamInfo(Parameter).Value));
    if s='' then
      Result:=0
    else
      if not NativeInt.TryParse(s,Result) then
        raise EConvertError.Create('Parameter value "'+
          string(PParamInfo(Parameter).Name)+'" is not numeric');
   end;
end;

function Parameter_NextBySameName(Parameter:PxxmParameter):PxxmParameter; stdcall;
var
  p:PParamInfo absolute Parameter;
  c:TxxmContext;
  i:NativeUInt;
begin
  //if Parameter=nil then raise?
  c:=p.Context as TxxmContext;
  i:=p.index;
  while (i<c.FParamsIndex) and not(UTF8CmpI(p.Name,c.FParams[i].Name)) do inc(i);
  if i<c.FParamsIndex then
    Result:=@c.FParams[i]
  else
    Result:=nil;
end;

function Parameter_ContentType(Parameter:PxxmParameter):PUTF8Char; stdcall;
var
  p:PParamInfo absolute Parameter;
begin
  if p<>@(p.Context as TxxmContext).FParams[p.Index] then
    raise EInvalidPointer.Create('Invalid parameter');
  if p.Origin<>'FILE' then
    raise EXxmError.Create('Parameter is not a FILE parameter');
  //if p.Origin<>'FILE' then raise?
  Result:=p.ContentType;
end;

function Parameter_SaveToFile(Parameter:PxxmParameter;FilePath:PUTF8Char):NativeUInt; stdcall;
var
  p:PParamInfo absolute Parameter;
  d:TStream;
  f:TFileStream;
begin
  //if Parameter=nil then raise?
  if p<>@(p.Context as TxxmContext).FParams[p.Index] then
    raise EInvalidPointer.Create('Invalid parameter');
  if p.Origin<>'FILE' then
    raise EXxmError.Create('Parameter is not a FILE parameter');
  d:=(p.Context as TxxmContext).FPostData;
  f:=TFileStream.Create(UTF8ToString(FilePath),fmCreate);
  try
    d.Position:=p.PostDataPos;
    Result:=f.CopyFrom(d,p.PostDataLen);//TODO: buffersize from config?
  finally
    f.Free;
  end;
end;

function Parameter_SaveToStream(Parameter:PxxmParameter;Stream:TObject):NativeUInt; stdcall;
var
  p:PParamInfo absolute Parameter;
  d:TStream;
begin
  if p<>@(p.Context as TxxmContext).FParams[p.Index] then
    raise EInvalidPointer.Create('Invalid parameter');
  if p.Origin<>'FILE' then
    raise EXxmError.Create('Parameter is not a FILE parameter');
  d:=(p.Context as TxxmContext).FPostData;
  d.Position:=p.PostDataPos;
  Result:=(Stream as TStream).CopyFrom(d,p.PostDataLen);//TODO: buffersize from config?
end;

procedure CompatibilityGuard; stdcall;
begin
  raise Exception.Create('This call is not yet available in this version');
end;

procedure SetupXxm2;
var
  xxmHttp:Pxxm2;
  l,x:DWORD;
  p0,p1:pointer;
begin
  //l:=SizeOf(Txxm2);//TODO: get page size?
  l:=$400;
  xxmHttp:=VirtualAlloc(nil,l,MEM_COMMIT,PAGE_READWRITE);
  //if xxmHttp=nil then raise?

  xxmHttp.Context_APILevel:=XxmAPILevel;
  xxmHttp.Context_URL:=@Context_URL;
  xxmHttp.Context_SessionID:=@Context_SessionID;

  xxmHttp.Context_ContextString:=@Context_ContextString;
  xxmHttp.Context_BufferSize:=@Context_BufferSize;
  xxmHttp.Context_Set_BufferSize:=@Context_Set_BufferSize;
  xxmHttp.Context_Connected:=@Context_Connected;

  xxmHttp.Context_Set_Status:=@Context_Set_Status;
  xxmHttp.Context_Redirect:=@Context_Redirect;
  xxmHttp.Context_Cookie:=@Context_Cookie;
  xxmHttp.Context_Set_Cookie:=@Context_Set_Cookie;
  xxmHttp.Context_Set_CookieEx:=@Context_Set_CookieEx;

  xxmHttp.Context_ContentType:=@Context_ContentType;
  xxmHttp.Context_Set_ContentType:=@Context_Set_ContentType;
  xxmHttp.Context_AutoEncoding:=@Context_AutoEncoding;
  xxmHttp.Context_Set_AutoEncoding:=@Context_Set_AutoEncoding;
  xxmHttp.Context_DispositionAttach:=@Context_DispositionAttach;

  xxmHttp.Context_Send:=@Context_Send;
  xxmHttp.Context_SendHTML:=@Context_SendHTML;
  xxmHttp.Context_SendFile:=@Context_SendFile;
  xxmHttp.Context_SendStream:=@Context_SendStream;
  xxmHttp.Context_Flush:=@Context_Flush;

  xxmHttp.Context_Parameter:=@Context_Parameter;
  xxmHttp.Context_ParameterCount:=@Context_ParameterCount;
  xxmHttp.Context_ParameterByIdx:=@Context_ParameterByIdx;
  xxmHttp.Context_Add_Parameter:=@Context_Add_Parameter;

  xxmHttp.Context_RequestHeader:=@Context_RequestHeader;
  xxmHttp.Context_RequestHeaderCount:=@Context_RequestHeaderCount;
  xxmHttp.Context_RequestHeaderByIdx:=@Context_RequestHeaderByIdx;
  xxmHttp.Context_ResponseHeader:=@Context_ResponseHeader;
  xxmHttp.Context_ResponseHeaderCount:=@Context_ResponseHeaderCount;
  xxmHttp.Context_ResponseHeaderByIdx:=@Context_ResponseHeaderByIdx;
  xxmHttp.Context_Set_ResponseHeader:=@Context_Set_ResponseHeader;

  xxmHttp.Context_Data:=@Context_Data;
  xxmHttp.Context_Set_Data:=@Context_Set_Data;

  xxmHttp.Context_Include:=@Context_Include;
  xxmHttp.Context_PostData:=@Context_PostData;
  xxmHttp.Context_Set_ProgressCallback:=@Context_Set_ProgressCallback;

  xxmHttp.Parameter_Origin:=@Parameter_Origin; //'GET','POST','FILE'...
  xxmHttp.Parameter_Name:=@Parameter_Name;
  xxmHttp.Parameter_Value:=@Parameter_Value;
  xxmHttp.Parameter_AsInteger:=@Parameter_AsInteger;
  xxmHttp.Parameter_NextBySameName:=@Parameter_NextBySameName;

  xxmHttp.Parameter_ContentType:=@Parameter_ContentType;
  xxmHttp.Parameter_SaveToFile:=@Parameter_SaveToFile;
  xxmHttp.Parameter_SaveToStream:=@Parameter_SaveToStream;

  p0:=xxmHttp;
  p1:=xxmHttp;
  inc(NativeInt(p0),SizeOf(Txxm2));
  inc(NativeInt(p1),l);
  while NativeInt(p0)<NativeInt(p1) do
   begin
    pointer(p0^):=@CompatibilityGuard;
    inc(NativeInt(p0),SizeOf(pointer));
   end;

  VirtualProtect(xxmHttp,l,PAGE_READONLY,@x);
  //if not then RaiseLastOSError?

  xxmPReg.Xxm2:=xxmHttp;
end;

{ TStreamNozzle }

constructor TStreamNozzle.Create(Owner: TxxmContext; Progress: CxxmProgress;
  RequestID, ReportStep: NativeUInt);
begin
  inherited Create;
  FOwner:=Owner;
  FSource:=Owner.FPostData;
  FSize:=0;
  FIndex:=0;
  FDone:=0;
  FSourceAtEnd:=false;
  FProgress:=FProgress;
  FRequestID:=RequestID;
  FReportStep:=ReportStep;
end;

destructor TStreamNozzle.Destroy;
begin
  //
  inherited;
end;

function TStreamNozzle.Ensure(EnsureSize: NativeUInt): boolean;
var
  i:NativeUInt;
const
  GrowStep=$10000;
begin
  //assert EnsureSize<=GrowStep
  if FIndex+EnsureSize>FSize then
   begin
    if FSourceAtEnd then Result:=false else
     begin
      i:=GrowStep;
      SetLength(FData,FSize+i);
      i:=FSource.Read(FData[FSize],i);
      inc(FSize,i);
      if i=0 then FSourceAtEnd:=true;
      Result:=FIndex+EnsureSize<=FSize;
      //TODO: if FDataAgent<>nil then FDataAgent.ReportProgress('','',Done+Size);
     end;
   end
  else
    Result:=true;
end;

procedure TStreamNozzle.Flush;
const
  FlushThreshold=$1000;
var
  l:integer;
begin
  if FIndex>FlushThreshold then
   begin
    l:=FSize-FIndex;
    Move(FData[FIndex],FData[0],l);
    SetLength(FData,l);
    FSize:=l;
    inc(FDone,FIndex);
    FIndex:=0;
   end;
end;

procedure TStreamNozzle.SkipWhiteSpace;
begin
  while Ensure(1) and (FData[FIndex] in [#0..#31]) do inc(FIndex);
end;

procedure TStreamNozzle.CheckBoundary(var Boundary: UTF8String);
var
  i,l:integer;
begin
  l:=Length(Boundary);
  Ensure(l+5);
  //assert FData[0]='-' and FData[1]='-'
  FIndex:=2;
  i:=0;
  while (i<>l) and (FData[FIndex]=Boundary[i+1]) do
   begin
    inc(i);
    inc(FIndex);
   end;
  if i<>l then
    raise EXxmError.Create('Multipart data does not start with boundary');
  SkipWhiteSpace;
  Boundary:=#13#10'--'+Boundary;
end;

procedure TStreamNozzle.GetHeader(var Params: TKeyValues);
const
  pGrowStep=$10;
var
  done,b:boolean;
begin
  done:=false;
  repeat
    if Ensure(2) and (FData[FIndex]=#13) and (FData[FIndex+1]=#10) then
     begin
      inc(FIndex,2);
      done:=true;
     end
    else
     begin
      if Params.KeysIndex=Params.KeysSize then
       begin
        inc(Params.KeysSize,$10);//grow step
        SetLength(Params.Keys,Params.KeysSize);
       end;
      Params.Keys[Params.KeysIndex].Key:=@FData[FIndex];
      while Ensure(1) and (FData[FIndex]<>':') do inc(FIndex);
      FData[FIndex]:=#0;
      inc(FIndex);
      while Ensure(1) and (FData[FIndex] in [#1..#32]) do inc(FIndex);
      Params.Keys[Params.KeysIndex].Value:=@FData[FIndex];
      b:=false;
      while Ensure(1) and not(b and (FData[FIndex]=#10)) do
       begin
        b:=FData[FIndex]=#13;
        inc(FIndex);
       end;
      FData[FIndex-1]:=#0;
      inc(FIndex);
      inc(Params.KeysIndex);
     end;
  until done;
  Flush;
end;

function TStreamNozzle.GetString(const Boundary: UTF8String): PUTF8Char;
var
  l,p,q:NativeUInt;
begin
  l:=Length(Boundary);
  p:=0;
  q:=FIndex;
  while (p<>l) and Ensure(l) do
   begin
    p:=0;
    while (p<l) and (FData[p+FIndex]=Boundary[p+1]) do inc(p);
    if p<>l then inc(FIndex);
   end;
  FData[FIndex]:=#0;
  Result:=@FData[q];
  //SetLength(Result,FIndex-q);
  //Move(PAnsiChar(@FData[q])^,Result^,Index-q);
  inc(FIndex,l);
  SkipWhiteSpace;
  Flush;
end;

procedure TStreamNozzle.GetData(Boundary, FieldName, FileName: PUTF8Char;
  var Pos, Len: NativeUInt);
var
  l,p,x,s:NativeUInt;
begin
  Pos:=FDone+FIndex-1;
  l:=Length(Boundary);
  p:=0;
  if (FReportStep=0) or (@FProgress=nil) then
   begin
    //short loop
    while Ensure(l) and (p<>l) do
     begin
      Flush;//depends on flush threshold
      p:=0;
      while (p<l) and (FData[p+FIndex]=Boundary[p+1]) do inc(p);
      if p<>l then inc(FIndex);
     end;
   end
  else
   begin
    //full loop
    x:=FReportStep;
    s:=0;
    while Ensure(l) and (p<>l) do
     begin
      Flush;//depends on flush threshold
      p:=0;
      while (p<l) and (FData[p+FIndex]=Boundary[p+1]) do inc(p);
      if p<>l then
       begin
        inc(FIndex);
        inc(s);
        dec(x);
        if x=0 then
         begin
          FProgress(FOwner,FieldName,FileName,FRequestID,s);
          x:=FReportStep;
         end
        else
          inc(x);
       end;
      FProgress(FOwner,FieldName,FileName,FRequestID,s);
     end;
   end;
  Len:=FDone+FIndex-(Pos+1);
  //skip boundary
  inc(FIndex,l);
  SkipWhiteSpace;
  Flush;
end;

function TStreamNozzle.MultiPartDone: boolean;
begin
  //assert just matched boundary
  Result:=not(Ensure(2)) or ((FData[FIndex]='-') and (FData[FIndex+1]='-'));
end;

{ TxxmCrossProjectIncludeCheck }

constructor TxxmCrossProjectIncludeCheck.Create(AEntry: TProjectEntry;
  ANext: TxxmCrossProjectIncludeCheck);
begin
  inherited Create;
  Entry:=AEntry;
  Next:=ANext;
end;

{ TxxmBufferStore }

constructor TxxmBufferStore.Create;
begin
  inherited Create;
  FBufferSize:=0;
  InitializeCriticalSection(FLock);
end;

destructor TxxmBufferStore.Destroy;
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

procedure TxxmBufferStore.AddBuffer(var x: TMemoryStream);
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

procedure TxxmBufferStore.GetBuffer(var x: TMemoryStream);
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
  GetSelfVersion;
  Randomize;
  SessionCookie:=UTF8String('xxm'+Format('%.6x%.6x%.6x%.6x',[
    Random($1000000),Random($1000000),Random($1000000),Random($1000000)]));
  ContextPool:=TxxmContextPool.Create;
  BufferStore:=TXxmBufferStore.Create;
  SetupXxm2;

finalization
  ContextPool.Free;
  BufferStore.Free;

end.
