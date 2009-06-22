unit xxmGeckoStreams;

interface

uses nsXPCOM, Classes, xxmParUtils;

type
  //TODO: nsIInputStream impl for channel output?

  TUploadStream=class(TStream)
  private
    FUploadStream:nsIInputStream;
    FHeaderSize:int64;
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

implementation

uses
  xxmGeckoInterfaces, SysUtils, Debug1;

{ TUploadStream }

constructor TUploadStream.Create(UploadStream: nsIInputStream);
begin
  inherited Create;
  FUploadStream:=UploadStream;
  FHeaderSize:=0;
end;

destructor TUploadStream.Destroy;
begin
  FUploadStream:=nil;
  inherited;
end;

function TUploadStream.GetSize: Int64;
var
  x:int64;
  s:nsISeekableStream;
begin
  s:=(FUploadStream as nsISeekableStream);
  x:=s.tell;
  s.seek(NS_SEEK_END,0);
  Result:=s.tell-FHeaderSize;
  s.seek(NS_SEEK_SET,x);
end;

procedure TUploadStream.ParseHeader(Headers: TResponseHeaders);
var
  s:string;
  i,j,l:integer;
  c:cardinal;
begin
Debug('>UploadStream.ParseHeader');
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
      inc(FHeaderSize,c);
     end;
    //TODO: concat next line that starts with whitespace
    if l<2 then l:=0 else dec(l,2);
    if not(l=0) then
     begin
      i:=1;
      while (i<=l) and not(s[i]=':') do inc(i);
      j:=i+1;
      while (j<=l) and (s[j] in [' ',#9]) do inc(j);
      Headers[Copy(s,1,i-1)]:=Copy(s,j,l-j+1);
     end;
  until l=0;
Debug('<UploadStream.ParseHeader');
end;

function TUploadStream.Read(var Buffer; Count: Integer): Longint;
begin
  //FUploadStream.ReadSegments?
  Result:=FUploadStream.Read(PAnsiChar(@Buffer),Count);
Debug('UploadStream.Read(,'+IntToStr(Count)+')='+IntToStr(Result));
end;

function TUploadStream.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
var
  whence:cardinal;
  x:int64;
  s:nsISeekableStream;
begin
  s:=(FUploadStream as nsISeekableStream);
  case Origin of
    soBeginning:whence:=NS_SEEK_SET;
    soCurrent:whence:=NS_SEEK_CUR;
    soEnd:whence:=NS_SEEK_END;
    else
      raise Exception.Create('Seek: unknown origin');
  end;
  if Origin=soBeginning then x:=FHeaderSize+Offset else x:=Offset;
  s.seek(whence,x);
  Result:=s.tell-FHeaderSize;
end;

procedure TUploadStream.SetSize(const NewSize: Int64);
begin
  raise EInvalidOperation.Create('SetSize on read-only stream not supported.');
end;

function TUploadStream.Write(const Buffer; Count: Integer): Longint;
begin
  raise EInvalidOperation.Create('Write on read-only stream not supported.');
end;

end.
