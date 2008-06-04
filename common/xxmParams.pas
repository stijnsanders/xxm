unit xxmParams;

interface

uses xxm, Classes, SysUtils;

type
  TXxmReqPar=class;

  TXxmReqPars=class(TObject)
  private
    FParams:array of TXxmReqPar;
    procedure Fill(Context: IXxmContext);
    procedure Add(Par:TXxmReqPar);
  public
    PostDataOnRedirect:boolean;
    constructor Create(Context: IXxmContext);
    destructor Destroy; override;
    function Get(Key:WideString):TXxmReqPar;
    function GetNext(Par:TXxmReqPar):TXxmReqPar;
    function GetItem(Key:integer):TXxmReqPar;
    function Count:integer;
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
    constructor Create(Owner:TXxmReqPars;Name,Value:WideString);
    property Name:WideString read GetName;
    property Value:WideString read GetValue;
  end;

  TXxmReqParGet=class(TXxmReqPar, IXxmParameterGet)
  end;

  TXxmReqParPost=class(TXxmReqPar, IXxmParameterPost)
    //TODO: store header?
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
    constructor Create(Owner:TXxmReqPars;Name:WideString;
      Origin,MimeType:string; Stream:TStream;Pos,Len:integer);
    property Size:integer read GetSize;
    property MimeType:WideString read GetMimeType;
    procedure SaveToFile(FilePath: String);
    function SaveToStream(Stream: TStream):integer;
  end;

  EXxmUnknownPostMime=class(Exception);

var
  SelfVersion:string;

const
  MimeFormUrlEncoded='application/x-www-form-urlencoded';

implementation

uses Windows, xxmParUtils;

const //resourcestring??
  SXxmUnknownPostMime='Unsupported Post Mime type "__"';

procedure GetSelfVersion;
var
  verblock:PVSFIXEDFILEINFO;
  verlen:cardinal;
  p:PChar;
  r:TResourceStream;
  m:TMemoryStream;
begin
  m:=TMemoryStream.Create;
  try
    //odd, a copy is required to avoid access violation in version.dll<
    r:=TResourceStream.CreateFromID(HInstance,1,RT_VERSION);
    try
      r.SaveToStream(m);
    finally
      r.Free;
    end;
    m.Position:=0;
    if VerQueryValue(m.Memory,'\',pointer(verblock),verlen) then
      SelfVersion:=
        IntToStr(HiWord(verblock.dwFileVersionMS))+'.'+
        IntToStr(LoWord(verblock.dwFileVersionMS))+'.'+
        IntToStr(HiWord(verblock.dwFileVersionLS))+'.'+
        IntToStr(LoWord(verblock.dwFileVersionLS))
    else
      SelfVersion:='v???';
    if VerQueryValue(m.Memory,'\StringFileInfo\040904E4\FileDescription',pointer(p),verlen) then
      SelfVersion:=p+' '+SelfVersion;
  finally
    m.Free;
  end;
end;

{ TXxmReqPars }

constructor TXxmReqPars.Create;
begin
  inherited Create;
  //
  PostDataOnRedirect:=false;
  Fill(Context);
end;

destructor TXxmReqPars.Destroy;
var
  i:integer;
begin
  for i:=0 to Length(FParams)-1 do FParams[i]._Release;
  inherited;
end;

procedure TXxmReqPars.Fill(Context: IXxmContext);
var
  i,p,q,r,l:integer;
  ps:TStream;
  pm,pn,pd,pb,ph,pf,px:string;
  pa,pax:TParamIndexes;
  sn:TStreamNozzle;
begin
  pd:=UTF8Encode(Context.ContextString(csQueryString));
  //TODO: revert &#[0-9]+?;
  l:=Length(pd);
  r:=1;
  while r<=l do
   begin
    p:=r;
    q:=r;
    while (q<=l) and not(pd[q]='=') do inc(q);
    r:=q+1;
    while (r<=l) and not(pd[r]='&') do inc(r);
    Add(TXxmReqParGet.Create(Self,
      UTF8Decode(Copy(pd,p,q-p)),
      URLDecode(UTF8Decode(Copy(pd,q+1,r-q-1)))));
    inc(r);
   end;

  ps:=Context.PostData;
  if not(ps=nil) then
   begin
    ps.Seek(0,soFromBeginning);
    pm:=Context.ContextString(csPostMimeType);
    pn:=SplitHeaderValue(pm,pa);//lower?

    //pm='' with redirect in response to POST request, but StgMed prevails! dorp it
    if pm='' then PostDataOnRedirect:=true else
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
      until not(q=$400);
      SetLength(pd,p);

      //TODO: revert &#[0-9]+?;
      l:=Length(pd);
      r:=1;
      while r<=l do
       begin
        p:=r;
        q:=r;
        while (q<=l) and not(pd[q]='=') do inc(q);
        r:=q+1;
        while (r<=l) and not(pd[r]='&') do inc(r);
        Add(TXxmReqParPost.Create(Self,
          UTF8Decode(URLDecode(Copy(pd,p,q-p))),
          UTF8Decode(URLDecode(Copy(pd,q+1,r-q-1)))));
        inc(r);
       end;

     end
    else
    if pn='multipart/form-data' then
     begin
      pb:=GetParamValue(pm,pa,'boundary');
      if pb='' then raise Exception.Create('unable to get boundary from header for multipart/form-data');

      sn:=TStreamNozzle.Create(ps);
      try
        //initialization, find first boundary
        sn.CheckBoundary(pb);

        while not(sn.MultiPartDone) do
         begin

          pm:='';
          pf:='';
          ph:=sn.GetHeader(pa);
          for i:=0 to Length(pa)-1 do
           begin
            pn:=LowerCase(Copy(ph,pa[i].NameStart,pa[i].NameLength));
            if pn='content-disposition' then
             begin
              pd:=Copy(ph,pa[i].ValueStart,pa[i].ValueLength);
              pn:=SplitHeaderValue(pd,pax);
              //assert pn='form-data'
              px:=GetParamValue(pd,pax,'name');
              pf:=GetParamValue(pd,pax,'filename');
             end
            else
            if pn='content-type' then pm:=Copy(ph,pa[i].ValueStart,pa[i].ValueLength)
            else
              ;//raise Exception.Create('Unknown multipart header "'+pn+'"');
           end;

          //TODO: transfer encoding?
          //TODO: store header (defined in xxm.pas?)

          if pm='' then Add(TXxmReqParPost.Create(Self,px,sn.GetString(pb))) else
           begin
            sn.GetData(pb,p,q);
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
end;

procedure TXxmReqPars.Add(Par: TXxmReqPar);
var
  i:integer;
begin
  i:=Length(FParams);
  SetLength(FParams,i+1);
  FParams[i]:=Par;
  Par._AddRef;
end;

function TXxmReqPars.Get(Key: WideString): TXxmReqPar;
var
  i:integer;
begin
  i:=0;
  //case sensitive?
  while (i<Length(FParams)) and not(FParams[i].Name=Key) do inc(i);
  if (i<Length(FParams)) then Result:=FParams[i] else
   begin
    //TODO: setting: nil or create empty?
    //Result:=nil;
    Result:=TXxmReqPar.Create(Self,Key,'');
    Result.FDummy:=true;
    //default value from setting?
   end;
end;

function TXxmReqPars.GetNext(Par: TXxmReqPar): TXxmReqPar;
var
  i:integer;
  Key:WideString;
begin
  i:=0;
  while (i<Length(FParams)) and not(FParams[i]=Par) do inc(i);
  if (i<Length(FParams)) then
   begin
    Key:=FParams[i].Name;//lower?
    inc(i);
    while (i<Length(FParams)) and not(FParams[i].Name=Key) do inc(i);
    if (i<Length(FParams)) then Result:=FParams[i] else Result:=nil;
   end
  else
    Result:=nil;
end;

function TXxmReqPars.GetItem(Key: integer): TXxmReqPar;
begin
  Result:=FParams[Key];
end;

function TXxmReqPars.Count: integer;
begin
  Result:=Length(FParams);
end;

{ TXxmReqPar }

constructor TXxmReqPar.Create(Owner: TXxmReqPars; Name, Value: WideString);
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

constructor TXxmReqParPostFile.Create(Owner: TXxmReqPars; Name: WideString;
  Origin, MimeType: string; Stream: TStream; Pos, Len: integer);
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

procedure TXxmReqParPostFile.SaveToFile(FilePath: String);
var
  f:TFileStream;
begin
  f:=TFileStream.Create(FilePath,fmCreate);
  try
    SaveToStream(f);
  finally
    f.Free;
  end;
end;

function TXxmReqParPostFile.SaveToStream(Stream: TStream): integer;
const
  xSize=$10000;
var
  x:array[0..xSize-1] of byte;
  c,l:integer;
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
    Stream.Write(x[0],c);
    dec(l,c);
   end;
  Result:=FLen;
end;

initialization
  GetSelfVersion;

end.
