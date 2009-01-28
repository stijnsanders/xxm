unit xxmGeckoStreams;

interface

uses nsXPCOM, nsTypes, Classes, xxmGeckoInterfaces;

type
  TxxmInputStream=class(TInterfacedObject,
    nsIInputStream,
    nsISeekableStream)
  private
    FData:TMemoryStream;
  protected
    //nsIInputStream
    function Available: Cardinal; safecall;
    procedure Close; safecall;
    function IsNonBlocking: LongBool; safecall;
    function Read(aBuf: PAnsiChar; aCount: Cardinal): Cardinal; safecall;
    function ReadSegments(aWriter: nsWriteSegmentFun; aClosure: Pointer;
      aCount: Cardinal): Cardinal; safecall;
    //nsISeekableStream
    procedure seek(whence:PRUint32;offset:PRUint64);
    function tell:PRUint64;
    procedure setEOF();
  public
    constructor Create;
    destructor Destroy; override;
    property Data:TMemoryStream read FData;
  end;

implementation

uses SysUtils, Debug1;

{ TxxmInputStream }

constructor TxxmInputStream.Create;
begin
  inherited Create;
  FData:=TMemoryStream.Create;
  Debug('TxxmInputStream.Create');
end;

destructor TxxmInputStream.Destroy;
begin
Debug('TxxmInputStream.Destroy');
  FData.Free;
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
  Result:=false;//????
end;

function TxxmInputStream.Read(aBuf: PAnsiChar; aCount: Cardinal): Cardinal;
begin
Debug('TxxmInputStream.Read(,'+IntToStr(aCount));
  Result:=FData.Read(aBuf^,aCount);
  //Result:=ReadSegments(NS_CopySegmentToBuffer,aBuf,aCount);
Debug('TxxmInputStream.Read()'+IntToStr(Result));
end;

function TxxmInputStream.ReadSegments(aWriter: nsWriteSegmentFun;
  aClosure: Pointer; aCount: Cardinal): Cardinal;
begin
Debug('TxxmInputStream.ReadSegments(,'+IntToStr(aCount));
  //TODO
  //aWriter(Self,aClosure,aFromSegment,aToOffset,aCount,Result);
  raise Exception.Create('ReadSegments not implemented');
end;

procedure TxxmInputStream.seek(whence: PRUint32; offset: PRUint64);
begin
Debug('TxxmInputStream.seek');
  case whence of
    NS_SEEK_SET:FData.Position:=offset;
    NS_SEEK_CUR:FData.Position:=FData.Position+offset;
    NS_SEEK_END:FData.Position:=FData.Size+offset;
  end;
end;

procedure TxxmInputStream.setEOF;
begin
Debug('TxxmInputStream.setEOF');
  FData.Position:=FData.Size;
end;

function TxxmInputStream.tell: PRUint64;
begin
Debug('TxxmInputStream.tell');
  Result:=FData.Position;
end;

end.
