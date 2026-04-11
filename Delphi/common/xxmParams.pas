unit xxmParams;

interface

uses xxm, Classes, SysUtils, ActiveX, xxmHeaders;

type
  TXxmReqPars=class(TObject)
  private
    FRoot, FFirst, FLast, FFound: pointer;
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
    procedure Clear;
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
    constructor Create(Owner: TXxmReqPars; const Name, Origin,
      MimeType: WideString; Stream: TStream; Pos, Len: integer);
    property Size:integer read GetSize;
    property MimeType:WideString read GetMimeType;
    procedure SaveToFile(const FilePath: AnsiString);
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
  SelfVersion:string;

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
        SelfVersion:=
          IntToStr(HiWord(verblock.dwFileVersionMS))+'.'+
          IntToStr(LoWord(verblock.dwFileVersionMS))+'.'+
          IntToStr(HiWord(verblock.dwFileVersionLS))+'.'+
          IntToStr(LoWord(verblock.dwFileVersionLS))
      else
        SelfVersion:='v???';
      if VerQueryValueA(@d[0],'\StringFileInfo\040904E4\FileDescription',pointer(p),verlen) then
        SelfVersion:=string(p)+' '+SelfVersion;
     end;
   end;
end;

{ key value store }

const
  KeyValueNodeHashBits = 8;//bits
  KeyValueNodeHashMask = (1 shl KeyValueNodeHashBits)-1;//$F
  KeyValueItemMaxChain = 8;

type
  PPKeyValueNode = ^PKeyValueNode;
  PKeyValueNode = ^TKeyValueNode;
  TKeyValueNode = array[0..KeyValueNodeHashMask] of pointer;

//strange! 'old' Delphi has NativeUInt=0..1; ??!!
{$IF not(Declared(NativeUInt)) or (SizeOf(cardinal)=SizeOf(pointer))}
  NativeUInt = cardinal;
{$IFEND}

  PKeyValueItem = ^TKeyValueItem;
  TKeyValueItem = record
    Hash: NativeUInt;
    Param: IXxmParameter;
    More, Next: PKeyValueItem;
  end;

function NewNode(var nn:PKeyValueNode):PKeyValueNode;
const
  ll=SizeOf(TKeyValueNode);
begin
  New(nn);//GetMem(nn,ll);
  FillChar(nn^,ll,0);
  Result:=nn;
end;

function NewItem(Hash:NativeUInt;var pp:PKeyValueItem):pointer;
const
  ll=SizeOf(TKeyValueItem);
begin
  New(pp);//GetMem(pp,ll);
  FillChar(pp^,ll,0);
  Result:=pointer(NativeUInt(pp) or 1);
  //assert Hash=kvsHash(Key)
  pp.Hash:=Hash;
  //pp.Key:=Key;
end;

function IsItem(p:pointer;var pp:PKeyValueItem):boolean; //inline;
begin
  Result:=(NativeUInt(p) and 1)<>0;
  if Result then pp:=PKeyValueItem(NativeUInt(p) xor 1);
end;

function AsItem(p:PKeyValueItem):pointer; //inline;
begin
  Result:=pointer(NativeUInt(p) or 1);
end;

function kvsHash(const Key: WideString): NativeUInt;
const
  seed = $11BB9955;
var
  i,l:NativeUInt;
begin
  Result:=seed;
  l:=Length(Key);
  for i:=1 to l do
    Result:=(Result shl 1) + (word(Key[i]) * $0901);
  inc(Result,l);
end;

function LookUpItem(Root: pointer; const Name: WideString;
  var FoundLast: pointer): PKeyValueItem;
var
  h,h0,h1:NativeUInt;
  n:PKeyValueNode;
begin
  h:=kvsHash(Name);
  if (FoundLast<>nil) and (PKeyValueItem(FoundLast).Hash=h)
    and (PKeyValueItem(FoundLast).Param.Name=Name) then
   begin
    Result:=FoundLast;
    Exit;
   end;
  h1:=h;
  n:=Root;
  Result:=nil;
  while n<>nil do
    if IsItem(n,Result) then
      n:=nil //end loop
    else
     begin
      h0:=h1 and KeyValueNodeHashMask;
      h1:=h1 shr KeyValueNodeHashBits;
      n:=n[h0];
     end;
  while (Result<>nil) and not((Result.Hash=h) and (Result.Param<>nil)
    and (Result.Param.Name=Name)) do Result:=Result.More;
  if Result<>nil then FoundLast:=Result;
end;

function StoreItem(var Root: pointer; const Name: WideString;
  var First, Last: pointer): PKeyValueItem;
const
  KeyValueNodeMaxLevel=(SizeOf(NativeUInt)*8+KeyValueNodeHashBits-1)
    div KeyValueNodeHashBits;
var
  r:PPKeyValueNode;
  h,h0,h1,c,l:NativeUInt;
  n:PKeyValueNode;
  p,p1,p2:PKeyValueItem;
begin
  h:=kvsHash(Name);
  h1:=h;
  l:=0;
  c:=0;
  r:=@Root;
  p:=nil;
  while p=nil do
    if r^=nil then
      r^:=NewItem(h,p)
    else
      if IsItem(r^,p) then
       begin
        p1:=p;
        while (p<>nil) and not((p.Hash=h) and (p.Param=nil)) do
         begin
          p:=p.More;
          inc(c);
         end;
        if p=nil then
          if (c>=KeyValueItemMaxChain) and (l<KeyValueNodeMaxLevel) then
           begin
            //convert to node
            n:=NewNode(r^);
            h0:=h1 and KeyValueNodeHashMask;
            n[h0]:=NewItem(h,p);
            p2:=p1;
            while p2<>nil do
             begin
              p1:=p2;
              p2:=p2.More;
              p1.More:=nil;
              h0:=(p1.Hash shr (KeyValueNodeHashBits*l)) and KeyValueNodeHashMask;
              if n[h0]<>nil then IsItem(n[h0],p1.More);//assume true since set in this loop
              n[h0]:=AsItem(p1);
             end;
           end
          else
           begin
            //add to chain
            r^:=NewItem(h,p);
            p.More:=p1;
           end;
       end
      else
       begin
        h0:=h1 and KeyValueNodeHashMask;
        h1:=h1 shr KeyValueNodeHashBits;
        inc(l);
        r:=@r^[h0];
       end;
  //p.Param:=//done by caller
  p.Next:=nil;
  if First=nil then
   begin
    First:=p;
    Last:=p;
   end
  else
   begin
    PKeyValueItem(Last).Next:=p;
    Last:=p;
   end;
  Result:=p;
end;

{ TXxmReqPars }

constructor TXxmReqPars.Create;
begin
  inherited Create;
  FRoot:=nil;
  FFirst:=nil;
  FLast:=nil;
  FFound:=nil;
  FFilled:=false;
  //Fill(Context,PostData);
  DataProgressAgent:=nil;
  FileProgressAgent:=nil;
  FileProgressStep:=0;
end;

destructor TXxmReqPars.Destroy;
   procedure ClearNode(n:PKeyValueNode);
   var
     i:NativeUInt;
     p,q:PKeyValueItem;
   begin
     //assert n<>nil
     if IsItem(n,p) then
       while p<>nil do
        begin
         q:=p;
         p:=p.More;
         try
           //q.Key:='';
           q.Param:=nil;
         except
           //silent
         end;
         Dispose(q);//FreeMem(q);
        end
     else
      begin
       for i:=0 to KeyValueNodeHashMask do
         if n[i]<>nil then
           ClearNode(n[i]);
       Dispose(n);//FreeMem(n);
      end;
   end;
begin
  DataProgressAgent:=nil;
  FileProgressAgent:=nil;
  if FRoot<>nil then ClearNode(FRoot);
  inherited;
end;

function TXxmReqPars.Fill(Context: IXxmContext; PostData: TStream): boolean;
var
  i,p,q,r,l:integer;
  ps:TStream;
  pm,pd,pf,px:AnsiString;
  pa,pax:TParamIndexes;
  pn:string;
  sn:TStreamNozzle;
begin
  Result:=false;//return wether to free PostData
  pa.ParsIndex:=0;
  pa.ParsSize:=0;
  pax.ParsIndex:=0;
  pax.ParsSize:=0;
  pd:=UTF8Encode(Context.ContextString(csQueryString));
  //TODO: revert &#[0-9]+?;
  l:=Length(pd);
  r:=1;
  while r<=l do
   begin
    p:=r;
    q:=r;
    while (q<=l) and (pd[q]<>'=') and (pd[q]<>'&') do inc(q);
    r:=q;
    if (q<=l) and (pd[q]='=') then inc(r);
    while (r<=l) and (pd[r]<>'&') do inc(r);
    Add(TXxmReqParGet.Create(Self,
      UTF8Decode(Copy(pd,p,q-p)),
      URLDecode(AnsiString(UTF8Decode(Copy(pd,q+1,r-q-1))))));
    inc(r);
   end;

  ps:=PostData;
  if ps<>nil then
   begin
    ps.Seek(0,soFromBeginning);
    pm:=AnsiString(Context.ContextString(csPostMimeType));
    pn:=string(SplitHeaderValue(pm,1,Length(pm),pa));//lower?

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
        if DataProgressAgent<>nil then
          try
            DataProgressAgent.ReportProgress('','',p);
          except
            pointer(DataProgressAgent):=nil;
          end;
      until q=0;
      SetLength(pd,p);

      //TODO: revert &#[0-9]+?;
      l:=Length(pd);
      r:=1;
      while r<=l do
       begin
        p:=r;
        q:=r;
        while (q<=l) and (pd[q]<>'=') and (pd[q]<>'&') do inc(q);
        r:=q;
        if (q<=l) and (pd[q]='=') then inc(r);
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
      sn:=TStreamNozzle.Create(ps,GetParamValue(pm,pa,'boundary'),
        DataProgressAgent,FileProgressAgent,FileProgressStep);
      try
        repeat
          pm:='';
          pf:='';
          pd:=sn.GetHeader(pa);
          for i:=0 to pa.ParsIndex-1 do
            if pa.Pars[i].NameIndex=KnownHeaderIndex(khContentDisposition) then
             begin
              SplitHeaderValue(pd,
                pa.Pars[i].ValueStart,pa.Pars[i].ValueLength,pax);
              //assert ='form-data'
              px:=GetParamValue(pd,pax,'name');
              pf:=GetParamValue(pd,pax,'filename');
             end
            else
            if pa.Pars[i].NameIndex=KnownHeaderIndex(khContentType)  then
              pm:=Copy(pd,pa.Pars[i].ValueStart,pa.Pars[i].ValueLength)
            else
              ;//raise Exception.Create('Unknown multipart header "'+pn+'"');
          //TODO: transfer encoding?
          if pm='' then
            Add(TXxmReqParPost.Create(Self,WideString(px),
              UTF8Decode(sn.GetString)))
          else
           begin
            sn.GetData(px,pf,p,q);
            Add(TXxmReqParPostFile.Create(Self,WideString(px),
              UTF8Decode(pf),//TODO: encoding from header?
              WideString(pm),ps,p,q));
           end;
        until sn.MultiPartDone;
      finally
        sn.Free;
      end;

     end
    else
      raise EXxmUnknownPostMime.Create(StringReplace(
        SXxmUnknownPostMime,'__',string(pm),[]));

    ps.Seek(0,soFromBeginning);
   end;
  FFilled:=true;
  try
    DataProgressAgent:=nil;
  except
    //silent
    pointer(DataProgressAgent):=nil;
  end;
  try
    FileProgressAgent:=nil;
  except
    //silent
    pointer(FileProgressAgent):=nil;
  end;
end;

procedure TXxmReqPars.Add(Par: IXxmParameter);
begin
  StoreItem(FRoot,Par.Name,FFirst,FLast).Param:=Par;
end;

function TXxmReqPars.Get(const Key: WideString): IXxmParameter;
var
  n:PKeyValueItem;
  p:TXxmReqPar;
begin
  n:=LookUpItem(FRoot,Key,FFound);
  if (n=nil) or (n.Param=nil) then
   begin
    p:=TXxmReqPar.Create(Self,Key,'');
    p.FDummy:=true;
    Result:=p;
   end
  else
    Result:=n.Param;
end;

function TXxmReqPars.GetNext(Par: IXxmParameter): IXxmParameter;
var
  Key:WideString;
  n:PKeyValueItem;
begin
  Key:=Par.Name;
  n:=LookUpItem(FRoot,Key,FFound);
  while (n<>nil) and (n.Param<>Par) do n:=n.More;
  if (n<>nil) then n:=n.More;
  while (n<>nil) and (n.Param.Name<>Key) do n:=n.More;
  if n=nil then
    Result:=nil
  else
    Result:=n.Param;
end;

function TXxmReqPars.GetItem(Key: integer): IXxmParameter;
var
  i:integer;
  n:PKeyValueItem;
begin
  i:=Key;
  if i<0 then
    raise ERangeError.Create('(TXxmReqPars.GetItem: Out of range');
  n:=FFirst;
  while (i<>0) and (n<>nil) do
   begin
    n:=n.Next;
    dec(i);
   end;
  if n=nil then
    raise ERangeError.Create('(TXxmReqPars.GetItem: Out of range');
  Result:=n.Param;
end;

function TXxmReqPars.Count: integer;
var
  n:PKeyValueItem;
begin
  Result:=0;
  n:=FFirst;
  while n<>nil do
   begin
    inc(Result);
    n:=n.Next;
   end;
end;

procedure TXxmReqPars.Clear;
var
  p,q:PKeyValueItem;
begin
  try
    p:=FFirst;
    while p<>nil do
     begin
      q:=p.Next;
      p.Next:=nil;
      try
        p.Param:=nil;//assert p.Param._Release=0
      except
        //silent
        pointer(p.Param):=nil;
      end;
      p:=q;
     end;
  except
    //silent
  end;
  FFirst:=nil;
  FLast:=nil;
  FFound:=nil;
  FFilled:=false;
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

constructor TXxmReqParPostFile.Create(Owner: TXxmReqPars; const Name,
  Origin, MimeType: WideString; Stream: TStream; Pos, Len: integer);
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

procedure TXxmReqParPostFile.SaveToFile(const FilePath: AnsiString);
var
  fn:string;
  i:integer;
begin
  //this is strange! string(FilePath) results in ''??!!
  SetLength(fn,Length(FilePath));
  for i:=1 to Length(FilePath) do fn[i]:=char(FilePath[i]);
  SaveToStream(TStreamAdapter.Create(TFileStream.Create(fn,fmCreate),soOwned));
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
