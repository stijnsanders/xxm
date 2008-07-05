unit xxmStream;

interface

uses xxm, Classes;

type
  TxxmOutputStream=class(TCustomMemoryStream)
  private
    FWriting:boolean;
    FContext:IXxmContext;
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

{ TxxmOutputStream }

constructor TxxmOutputStream.Create(Context: IXxmContext);
begin
  inherited Create;
  FContext:=Context;
  FWriting:=false;
end;

destructor TxxmOutputStream.Destroy;
begin
  FContext:=nil;
  inherited;
end;

function TxxmOutputStream.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
begin
  if FWriting then
    Result:=inherited Seek(Offset,Origin)
  else
    raise Exception.Create('[TxxmOutputStream]Seek not supported');
end;

procedure TxxmOutputStream.SetSize(const NewSize: Int64);
begin
  raise Exception.Create('[TxxmOutputStream]SetSize not supported');
end;

function TxxmOutputStream.Read(var Buffer; Count: Integer): Integer;
begin
  if FWriting then
    Result:=inherited Read(Buffer,Count)
  else
    raise Exception.Create('[TxxmOutputStream]Read not supported');
end;

function TxxmOutputStream.Write(const Buffer; Count: Integer): Integer;
begin
  //TODO: thread-safe, locking?
  FWriting:=true;
  try
    Position:=0;
    SetPointer(@Buffer,Count);
    FContext.SendStream(Self);
    Result:=Count;
  finally
    FWriting:=false;
  end;
end;

end.
