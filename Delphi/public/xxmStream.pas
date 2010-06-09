unit xxmStream;

interface

uses xxm, Classes;

type
  TxxmOutputStream=class(TStream)
  private
    FContext:IXxmContext;
    FForwardStream:TStream;
  protected
    procedure SetSize(const NewSize: Int64); override;
  public
    constructor Create(Context:IXxmContext);
    destructor Destroy; override;

    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
      override;
    function Write(const Buffer; Count: Integer): Integer; override;
    function Read(var Buffer; Count: Integer): Integer; override;
  end;

implementation

uses SysUtils;

type
  TxxmForwardStream=class(TCustomMemoryStream)
  public
    function Write(const Buffer; Count: Integer): Integer; override;
  end;

{ TxxmOutputStream }

constructor TxxmOutputStream.Create(Context: IXxmContext);
begin
  inherited Create;
  FContext:=Context;
  FForwardStream:=TxxmForwardStream.Create;
end;

destructor TxxmOutputStream.Destroy;
begin
  FForwardStream.Free;
  FContext:=nil;
  inherited;
end;

function TxxmOutputStream.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
begin
  raise Exception.Create('[TxxmOutputStream]Seek not supported');
end;

procedure TxxmOutputStream.SetSize(const NewSize: Int64);
begin
  raise Exception.Create('[TxxmOutputStream]SetSize not supported');
end;

function TxxmOutputStream.Read(var Buffer; Count: Integer): Integer;
begin
  raise Exception.Create('[TxxmOutputStream]Read not supported');
end;

function TxxmOutputStream.Write(const Buffer; Count: Integer): Integer;
begin
  //TODO: thread-safe, locking?
  FForwardStream.Position:=0;
  (FForwardStream as TxxmForwardStream).SetPointer(@Buffer,Count);
  FContext.SendStream(FForwardStream);
  Result:=Count;
end;

{ TxxmForwardStream }

function TxxmForwardStream.Write(const Buffer; Count: Integer): Integer;
begin
  raise Exception.Create('[TxxmForwardStream]Write not supported');
end;

end.
