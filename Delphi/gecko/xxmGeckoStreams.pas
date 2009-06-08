unit xxmGeckoStreams;

interface

uses Windows, nsXPCOM, nsTypes, Classes, xxmGeckoInterfaces;

type
  TxxmInputStream=class(TInterfacedObject,
    nsIInputStream)
    //nsISeekableStream?
  private
    FTotalSize,FOutputSize,FExportSize:int64;
    FData:TMemoryStream;
    FLock:TRTLCriticalSection;
  protected
    //nsIInputStream
    function Available: Cardinal; safecall;
    procedure Close; safecall;
    function IsNonBlocking: LongBool; safecall;
    function Read(aBuf: PAnsiChar; aCount: Cardinal): Cardinal; safecall;
    function ReadSegments(aWriter: nsWriteSegmentFun; aClosure: Pointer;
      aCount: Cardinal): Cardinal; safecall;
{
//exclude seekable stream interface, to allow collapsing datastream when empty
    //nsISeekableStream
    procedure seek(whence:PRUint32;offset:PRUint64);
    function tell:PRUint64;
    procedure setEOF();
}
  public
    ReportPending:boolean;
    ReportSize: cardinal;
    constructor Create;
    destructor Destroy; override;
    procedure Lock;
    procedure Unlock;
    function Write(const Buffer; Count: Longint): Longint;//use within lock!
    property TotalSize: int64 read FTotalSize;
  end;

implementation

uses SysUtils, Debug1;

{ TxxmInputStream }

constructor TxxmInputStream.Create;
begin
  inherited Create;
  FData:=TMemoryStream.Create;
  InitializeCriticalSection(FLock);
  ReportPending:=false;
  FOutputSize:=0;
  ReportSize:=0;
  FTotalSize:=0;
  FExportSize:=0;
  Debug('TxxmInputStream.Create');
end;

destructor TxxmInputStream.Destroy;
begin
Debug('TxxmInputStream.Destroy');
  //assert not ReportPending
  FData.Free;
  DeleteCriticalSection(FLock);
  inherited;
Debug('TxxmInputStream.Destroy done');
end;

function TxxmInputStream.Available: Cardinal;
begin
Debug('TxxmInputStream.Available');
  Result:=FData.Size-FData.Position;
end;

procedure TxxmInputStream.Close;
begin
Debug('TxxmInputStream.Close');
  raise Exception.Create('Closing stream not supported.');
end;

function TxxmInputStream.IsNonBlocking: LongBool;
begin
Debug('TxxmInputStream.IsNonBlocking');
  Result:=true;//????
end;

function TxxmInputStream.Read(aBuf: PAnsiChar; aCount: Cardinal): Cardinal;
begin
Debug('TxxmInputStream.Read(,'+IntToStr(aCount));
  Lock;
  try
    //assert aCount is size of data reported
    FData.Position:=FExportSize;
    Result:=FData.Read(aBuf^,aCount);
    //assert Result=aCount
    ReportPending:=false;
    inc(FExportSize,Result);
    if FExportSize=FOutputSize then
     begin
      FOutputSize:=0; //don't set size, this saves on allocations
      FExportSize:=0;
     end;
  finally
    Unlock;
  end;
  //TODO: report data here when not ReportSize=0?
Debug('TxxmInputStream.Read()'+IntToStr(Result));
end;

function TxxmInputStream.ReadSegments(aWriter: nsWriteSegmentFun;
  aClosure: Pointer; aCount: Cardinal): Cardinal;
var
  p:pointer;
begin
Debug('TxxmInputStream.ReadSegments(,'+IntToStr(aCount));
  Lock;
  try
    //assert aCount is size of data reported
    p:=FData.Memory;
    inc(integer(p),FExportSize);
    aWriter(Self,aClosure,p,0,aCount,Result);
    //assert Result=aCount
    ReportPending:=false;
    inc(FExportSize,Result);
    if FExportSize=FOutputSize then
     begin
      //don't set size, saves on allocations
      FOutputSize:=0;
      FExportSize:=0;
     end;
  finally
    Unlock;
  end;
  //TODO: report data here when not ReportSize=0?
end;

{
procedure TxxmInputStream.seek(whence: PRUint32; offset: PRUint64);
begin
  case whence of
    NS_SEEK_SET:FData.Position:=offset;
    NS_SEEK_CUR:FData.Position:=FData.Position+offset;
    NS_SEEK_END:FData.Position:=FData.Size+offset;
  end;
end;

procedure TxxmInputStream.setEOF;
begin
  FData.Position:=FData.Size;
end;

function TxxmInputStream.tell: PRUint64;
begin
  Result:=FData.Position;
end;
}

procedure TxxmInputStream.Lock;
begin
  EnterCriticalSection(FLock);
end;

procedure TxxmInputStream.Unlock;
begin
  LeaveCriticalSection(FLock);
end;

function TxxmInputStream.Write(const Buffer; Count: Integer): Longint;
begin
  //assert between Lock/Unlock try/finally calls!
  FData.Position:=FOutputSize;
  Result:=FData.Write(Buffer,Count);
  //Result=Count
  inc(FOutputSize,Result);
  inc(FTotalSize,Result);
  inc(ReportSize,Result);
end;

end.
