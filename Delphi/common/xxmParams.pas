unit xxmParams;

interface

uses xxm, Classes, SysUtils, ActiveX, xxmHeaders;

type
  TXxmReqPars=class(TObject)
  private
    FParams: array of IXxmParameter;
    FParamsSize,FParamsCount: integer;
    FFilled: boolean;
  public
    DataProgressAgent, FileProgressAgent: IxxmUploadProgressAgent;
    FileProgressStep: integer;
    constructor Create;
    destructor Destroy; override;
    function Fill(Context: IXxmContext; PostData: TStream): boolean;
    function Get(const Key:WideString):IXxmParameter;
    function GetNext(Par:IXxmParameter):IXxmParameter;
    function GetItem(Key:integer):IXxmParameter;
    function Count:integer;
    procedure Add(Par:IXxmParameter);
    property Filled: boolean read FFilled;
  end;

  TXxmReqPar=class(TInterfacedObject, IXxmParameter)
  private
    FOwner:TXxmReqPars;
    FName,FValue:WideString;
    FDummy:boolean;
  protected
    function GetName:WideString;
    function GetValue:WideString;
    function AsInteger:integer;
    function NextBySameName:IXxmParameter;
  public
    constructor Create(Owner:TXxmReqPars;const Name,Value:WideString);
    property Name:WideString read GetName;
    property Value:WideString read GetValue;
  end;

  TXxmReqParGet=class(TXxmReqPar, IXxmParameterGet)
  end;

  TXxmReqParPost=class(TXxmReqPar, IXxmParameterPost)
  end;

  TXxmReqParPostFile=class(TXxmReqParPost, IXxmParameterPostFile)
  private
    FStream:TStream;
    FPos,FLen:integer;
    FMimeType:WideString;
  protected
    function GetSize: Integer;
    function GetMimeType: WideString;
  public
    constructor Create(Owner:TXxmReqPars;const Name:WideString;
      const Origin,MimeType:AnsiString; Stream:TStream;Pos,Len:integer);
    property Size:integer read GetSize;
    property MimeType:WideString read GetMimeType;
    procedure SaveToFile(FilePath: AnsiString);
    function SaveToStream(Stream: IStream):integer;
  end;

  TXxmContextStringPar=class(TInterfacedObject, IXxmParameter)
  private
    FName,FValue:WideString;
  protected
    function GetName:WideString;
    function GetValue:WideString;
    function AsInteger:integer;
    function NextBySameName:IXxmParameter;
  public
    constructor Create(const Name,Value:WideString);
    property Name:WideString read GetName;
    property Value:WideString read GetValue;
  end;

  EXxmUnknownPostMime=class(Exception);

var
  SelfVersion:AnsiString;

const
  MimeFormUrlEncoded='application/x-www-form-urlencoded';
  MimeFormData='multipart/form-data';

implementation

uses Windows, xxmParUtils, ComObj;

const //resourcestring??
  SXxmUnknownPostMime='Unsupported Post Mime type "__"';

procedure GetSelfVersion;
const
  dSize=$1000;
var
  d:array[0..dSize-1] of byte;
  verblock:PVSFIXEDFILEINFO;
  verlen:cardinal;
  p:PAnsiChar;
  r:TResourceStream;
begin
  //odd, a copy is required to avoid access violation in version.dll<
  r:=TResourceStream.CreateFromID(HInstance,1,RT_VERSION);
  try
    r.Read(d[0],dSize);
  finally
    r.Free;
  end;
  if VerQueryValueA(@d[0],'\',pointer(verblock),verlen) then
    SelfVersion:=
      IntToStr(HiWord(verblock.dwFileVersionMS))+'.'+
      IntToStr(LoWord(verblock.dwFileVersionMS))+'.'+
      IntToStr(HiWord(verblock.dwFileVersionLS))+'.'+
      IntToStr(LoWord(verblock.dwFileVersionLS))
  else
    SelfVersion:='v???';
  if VerQueryValueA(@d[0],'\StringFileInfo\040904E4\FileDescription',pointer(p),verlen) then
    SelfVersion:=p+' '+SelfVersion;
end;

{ TXxmReqPars }

constructor TXxmReqPars.Create;
begin
  inherited Create;
  FParamsSize:=0;
  FParamsCount:=0;
  FFilled:=false;
  //Fill(Context,PostData);
  DataProgressAgent:=nil;
  FileProgressAgent:=nil;
  FileProgressStep:=0;
end;

destructor TXxmReqPars.Destroy;
var
  i:integer;
begin
  DataProgressAgent:=nil;
  FileProgressAgent:=nil;
  for i:=0 to FParamsCount-1 do
    try
      FParams[i]._Release;
      FParams[i]:=nil;
    except
      pointer(FParams[i]):=nil;//silent
    end;
  inherited;
end;

function TXxmReqPars.Fill(Context: IXxmContext; PostData: TStream): boolean;
var
  i,p,q,r,l:integer;
  ps:TStream;
  pm,pn,pb,pd,pf,px:AnsiString;
  pa,pax:TParamIndexes;
  sn:TStreamNozzle;
begin
  Result:=false;//return wether to free PostData
  pd:=UTF8Encode(Context.ContextString(csQueryString));
  //TODO: revert &#[0-9]+?;
  l:=Length(pd);
  r:=1;
  while r<=l do
   begin
    p:=r;
    q:=r;
    while (q<=l) and (pd[q]<>'=') do inc(q);
    r:=q+1;
    while (r<=l) and (pd[r]<>'&') do inc(r);
    Add(TXxmReqParGet.Create(Self,
      UTF8ToWideString(Copy(pd,p,q-p)),
      URLDecode(AnsiString(UTF8ToWideString(Copy(pd,q+1,r-q-1))))));
    inc(r);
   end;

  ps:=PostData;
  if ps<>nil then
   begin
    ps.Seek(0,soFromBeginning);
    pm:=Context.ContextString(csPostMimeType);
    pn:=SplitHeaderValue(pm,1,Length(pm),pa);//lower?

    //pm='' with redirect in response to POST request, but StgMed prevails! drop it
    if pm='' then Result:=true else
    if pn=MimeFormUrlEncoded then
     begin
      //read into string
      //TODO: encoding??
      p:=0;
      q:=$400;
      repeat
        SetLength(pd,p+q);
        q:=ps.Read(pd[p+1],q);
        inc(p,q);
        if DataProgressAgent<>nil then DataProgressAgent.ReportProgress('','',p);
      until q=0;
      SetLength(pd,p);

      //TODO: revert &#[0-9]+?;
      l:=Length(pd);
      r:=1;
      while r<=l do
       begin
        p:=r;
        q:=r;
        while (q<=l) and (pd[q]<>'=') do inc(q);
        r:=q+1;
        while (r<=l) and (pd[r]<>'&') do inc(r);
        Add(TXxmReqParPost.Create(Self,
          URLDecode(Copy(pd,p,q-p)),
          URLDecode(Copy(pd,q+1,r-q-1))));
        inc(r);
       end;

     end
    else
    if pn=MimeFormData then
     begin
      pb:=GetParamValue(pm,pa,'boundary');
      if pb='' then raise Exception.Create('unable to get boundary from header for multipart/form-data');

      sn:=TStreamNozzle.Create(ps,DataProgressAgent,FileProgressAgent,FileProgressStep);
      try
        //initialization, find first boundary
        sn.CheckBoundary(pb);

        while not(sn.MultiPartDone) do
         begin

          pm:='';
          pf:='';
          pd:=sn.GetHeader(pa);
          for i:=0 to Length(pa)-1 do
           begin
            pn:=LowerCase(Copy(pd,pa[i].NameStart,pa[i].NameLength));
            if pn='content-disposition' then
             begin
              pn:=SplitHeaderValue(pd,pa[i].ValueStart,pa[i].ValueLength,pax);
              //assert pn='form-data'
              px:=GetParamValue(pd,pax,'name');
              pf:=GetParamValue(pd,pax,'filename');
             end
            else
            if pn='content-type' then pm:=Copy(pd,pa[i].ValueStart,pa[i].ValueLength)
            else
              ;//raise Exception.Create('Unknown multipart header "'+pn+'"');
           end;

          //TODO: transfer encoding?

          if pm='' then Add(TXxmReqParPost.Create(Self,px,sn.GetString(pb))) else
           begin
            sn.GetData(pb,px,pf,p,q);
            Add(TXxmReqParPostFile.Create(Self,px,pf,pm,ps,p,q));
           end;

         end;

      finally
        sn.Free;
      end;

     end
    else
      raise EXxmUnknownPostMime.Create(StringReplace(
        SXxmUnknownPostMime,'__',pm,[]));

    ps.Seek(0,soFromBeginning);
   end;
  FFilled:=true;
  DataProgressAgent:=nil;
  FileProgressAgent:=nil;
end;

procedure TXxmReqPars.Add(Par: IXxmParameter);
const
  GrowStep=$100;
begin
  if FParamsCount=FParamsSize then
   begin
    inc(FParamsSize,GrowStep);
    SetLength(FParams,FParamsSize);
   end;
  FParams[FParamsCount]:=Par;
  Par._AddRef;
  inc(FParamsCount);
end;

function TXxmReqPars.Get(const Key: WideString): IXxmParameter;
var
  i:integer;
  p:TXxmReqPar;
begin
  i:=0;
  //case sensitive?
  while (i<FParamsCount) and (FParams[i].Name<>Key) do inc(i);
  if (i<FParamsCount) then Result:=FParams[i] else
   begin
    p:=TXxmReqPar.Create(Self,Key,'');
    p.FDummy:=true;
    Result:=p;
   end;
end;

function TXxmReqPars.GetNext(Par: IXxmParameter): IXxmParameter;
var
  i:integer;
  Key:WideString;
begin
  i:=0;
  while (i<FParamsCount) and (FParams[i]<>Par) do inc(i);
  if (i<FParamsCount) then
   begin
    Key:=FParams[i].Name;//lower?
    inc(i);
    while (i<FParamsCount) and (FParams[i].Name<>Key) do inc(i);
    if (i<FParamsCount) then Result:=FParams[i] else Result:=nil;
   end
  else
    Result:=nil;
end;

function TXxmReqPars.GetItem(Key: integer): IXxmParameter;
begin
  Result:=FParams[Key];
end;

function TXxmReqPars.Count: integer;
begin
  Result:=FParamsCount;
end;

{ TXxmReqPar }

constructor TXxmReqPar.Create(Owner: TXxmReqPars; const Name, Value: WideString);
begin
  inherited Create;
  FOwner:=Owner;
  FName:=Name;
  FValue:=Value;
  FDummy:=false;
end;

function TXxmReqPar.AsInteger: integer;
begin
  if FDummy then Result:=0 else Result:=StrToIntDef(FValue,0);
end;

function TXxmReqPar.GetName: WideString;
begin
  Result:=FName;
end;

function TXxmReqPar.GetValue: WideString;
begin
  Result:=FValue;
end;

function TXxmReqPar.NextBySameName: IXxmParameter;
begin
  Result:=FOwner.GetNext(Self);
end;

{ TXxmReqParPostFile }

constructor TXxmReqParPostFile.Create(Owner: TXxmReqPars; const Name: WideString;
  const Origin, MimeType: AnsiString; Stream: TStream; Pos, Len: integer);
begin
  inherited Create(Owner,Name,Origin);
  FMimeType:=MimeType;
  FStream:=Stream;
  FPos:=Pos;
  FLen:=Len;
end;

function TXxmReqParPostFile.GetMimeType: WideString;
begin
  Result:=FMimeType;
end;

function TXxmReqParPostFile.GetSize: Integer;
begin
  Result:=FLen;
end;

procedure TXxmReqParPostFile.SaveToFile(FilePath: AnsiString);
begin
  SaveToStream(TStreamAdapter.Create(TFileStream.Create(FilePath,fmCreate),soOwned));
end;

function TXxmReqParPostFile.SaveToStream(Stream: IStream): integer;
const
  xSize=$10000;
var
  x:array[0..xSize-1] of byte;
  c,l,r:integer;
begin
  //TODO: encoding??!!
  FStream.Position:=FPos;
  //Result:=Stream.CopyFrom(FStream,FLen);
  l:=FLen;
  while l>0 do
   begin
    c:=xSize;
    if c>l then c:=l;
    c:=FStream.Read(x[0],c);
    OleCheck(Stream.Write(@x[0],c,@r));
    if r<>c then raise Exception.Create('[TXxmReqParPostFile.SaveToStream]Stream Write Error');
    dec(l,c);
   end;
  Result:=FLen;
end;

{ TXxmContextStringPar }

constructor TXxmContextStringPar.Create(const Name, Value: WideString);
begin
  inherited Create;
  FName:=Name;
  FValue:=Value;
end;

function TXxmContextStringPar.GetName: WideString;
begin
  Result:=FName;
end;

function TXxmContextStringPar.GetValue: WideString;
begin
  Result:=FValue;
end;

function TXxmContextStringPar.AsInteger: integer;
begin
  Result:=StrToIntDef(FValue,0);
end;

function TXxmContextStringPar.NextBySameName: IXxmParameter;
begin
  Result:=nil;//context strings are unique
end;

initialization
  GetSelfVersion;

end.
