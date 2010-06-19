unit xxmGeckoStreams;

interface

uses nsXPCOM, SysUtils, Classes, xxmParUtils;

type
  //TODO: nsIInputStream impl for channel output?

  TxxmGeckoUploadStream=class(TStream)
  private
    FUploadStream:nsIInputStream;
    FData:TStream;
    FDataPos,FDataSize:Int64;
    FDataFile:AnsiString;
  protected
    function GetSize: Int64; override;
    procedure SetSize(const NewSize: Int64); overload; override;
  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; overload; override;
  public
    constructor Create(UploadStream:nsIInputStream);
    destructor Destroy; override;
    procedure ParseHeader(Headers:TResponseHeaders);
    property InputStream:nsIInputStream read FUploadStream;
  end;

  EXxmPostDataReadOnly=class(EInvalidOperation);

implementation

uses
  Windows, xxmGeckoInterfaces;

resourcestring
  SXxmPostDataReadOnly='Post-data is read-only';

const
  SwitchToFileTreshold=$100000;//TODO: setting?

{ TxxmGeckoUploadStream }

constructor TxxmGeckoUploadStream.Create(UploadStream: nsIInputStream);
begin
  inherited Create;
  FUploadStream:=UploadStream;
  //FHeaderSize:=0;
  FData:=TMemoryStream.Create;
  FDataPos:=0;
  FDataSize:=0;
  //TODO: check header Content-length, if larger than SwitchToFileTreshold start with TFileStream
  FDataFile:='';
end;

destructor TxxmGeckoUploadStream.Destroy;
begin
  FUploadStream:=nil;
  FData.Free;
  if FDataFile<>'' then SysUtils.DeleteFile(FDataFile);
  inherited;
end;

function TxxmGeckoUploadStream.GetSize: Int64;
begin
  //(FUploadStream as nsISeekableStream) works, but seek doesn't
  Result:=FDataSize;
end;

procedure TxxmGeckoUploadStream.ParseHeader(Headers: TResponseHeaders);
var
  s:AnsiString;
  i,j,l:integer;
  c:cardinal;
begin
  //TODO: use ReadSegment? use smoother way than read 1 by 1?
  //assert position=0
  repeat
    l:=0;
    c:=1;
    while (c=1) and not((l>=2) and (s[l-1]=#13) and (s[l]=#10)) do
     begin
      inc(l);
      SetLength(s,l);
      c:=FUploadStream.Read(PAnsiChar(@s[l]),1);
      //inc(FHeaderSize,c);
     end;
    //TODO: concat next line that starts with whitespace
    if l<2 then l:=0 else dec(l,2);
    if l<>0 then
     begin
      i:=1;
      while (i<=l) and (s[i]<>':') do inc(i);
      j:=i+1;
      while (j<=l) and (s[j] in [' ',#9]) do inc(j);
      Headers[Copy(s,1,i-1)]:=Copy(s,j,l-j+1);
     end;
  until l=0;
end;

function TxxmGeckoUploadStream.Read(var Buffer; Count: Integer): Longint;
var
  c:integer;
  f:TFileStream;
begin
  //FUploadStream.ReadSegments?
  Result:=0;
  if FDataPos=FDataSize then c:=Count else
   begin
    if FDataPos+Count>FDataSize then c:=FDataPos+Count-FDataSize else c:=Count;
    Result:=FData.Read(Buffer,c);
    //assert c=Result
    inc(FDataPos,c);
    c:=Count-c;
   end;
  if c<>0 then
   begin
    //assert FDataPos=FDataLen
    inc(Result,FUploadStream.Read(PAnsiChar(@Buffer),c));
    //assert c=Result
    if (FDataFile='') and (FDataSize+Result>=SwitchToFileTreshold) then
     begin
      SetLength(FDataFile,$400);
      SetLength(FDataFile,GetTempPathA($400,PAnsiChar(FDataFile)));//TODO: setting
      FDataFile:=FDataFile+'xxm_'+IntToStr(GetCurrentProcessId)+'_'+IntToStr(GetCurrentThreadId)+'.dat';
      f:=TFileStream.Create(FDataFile,fmCreate);
      f.Write((FData as TMemoryStream).Memory^,FDataSize);
      FData.Free;
      FData:=f;
     end;
    FData.Write(Buffer,Result);
    inc(FDataSize,Result);
    FDataPos:=FDataSize;
   end;
end;

function TxxmGeckoUploadStream.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
begin
  //(FUploadStream as nsISeekableStream) works, but seek doesn't
  FDataPos:=FData.Seek(Offset,Origin);
  Result:=FDataPos;
end;

procedure TxxmGeckoUploadStream.SetSize(const NewSize: Int64);
begin
  raise EXxmPostDataReadOnly.Create(SXxmPostDataReadOnly);
end;

function TxxmGeckoUploadStream.Write(const Buffer; Count: Integer): Longint;
begin
  raise EXxmPostDataReadOnly.Create(SXxmPostDataReadOnly);
end;

end.
