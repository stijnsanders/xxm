unit xxmp;

{
  xxm Project Unit
  demo: 07 Resources

Please note this xxmp.pas was first created from the xxmp.pas template,
but has been modified to check incoming requests for files that are stored in the  module's resources.
}

interface

uses xxm;

type
  TXxmdemo=class(TXxmProject)
  public
    function LoadPage(Context: IXxmContext; Address: WideString): IXxmFragment; override;
    function LoadFragment(Context: IXxmContext; Address, RelativeTo: WideString): IXxmFragment; override;
    procedure UnloadFragment(Fragment: IXxmFragment); override;
  end;

  TxxmFileFromRes=class(TxxmPage)
  private
    FResName:AnsiString;
  public
    constructor Create(AProject: TXxmProject; AResName: AnsiString);
    procedure Build(const Context: IXxmContext; const Caller: IXxmFragment;
      const Values: array of OleVariant; const Objects: array of TObject); override;
  end;

function XxmProjectLoad(AProjectName:WideString): IXxmProject; stdcall;

implementation

uses Windows, SysUtils, Classes, xxmFReg, xxmHeaders;

function XxmProjectLoad(AProjectName:WideString): IXxmProject;
begin
  Result:=TXxmdemo.Create(AProjectName);
end;

{ xxmFilesFromResIndex }

const
  RT_XXMRESFILE=MakeIntResource(999);
  RT_XXMRESINDEX=MakeIntResource(998);

var
  xxmFilesFromResIndex:TStringList;

procedure LoadFilesFromResIndex;
var
  r:TResourceStream;
begin
  xxmFilesFromResIndex:=TStringList.Create;
  r:=TResourceStream.Create(HInstance,'INDEX',RT_XXMRESINDEX);
  //TODO: TDecompressionStream
  try
    //into a memory stream first because TStringList.LoadFromStream calls TDecompressionStream.Size
    xxmFilesFromResIndex.LoadFromStream(r);
  finally
    r.Free;
  end;
end;

{ TXxmdemo }

function TXxmdemo.LoadPage(Context: IXxmContext; Address: WideString): IXxmFragment;
var
  rn:AnsiString;
begin
  inherited;
  //TODO: link session to request
  if Copy(Address,1,5)='res/' then
   begin
    rn:=xxmFilesFromResIndex.Values[Copy(Address,6,Length(Address)-5)];
    if rn='' then Result:=nil else Result:=TxxmFileFromRes.Create(Self,rn);
   end
  else
    Result:=XxmFragmentRegistry.GetFragment(Self,Address,'');
end;

function TXxmdemo.LoadFragment(Context: IXxmContext; Address, RelativeTo: WideString): IXxmFragment;
begin
  Result:=XxmFragmentRegistry.GetFragment(Self,Address,RelativeTo);
end;

procedure TXxmdemo.UnloadFragment(Fragment: IXxmFragment);
begin
  inherited;
  //TODO: set cache TTL, decrease ref count
  //Fragment.Free;
end;

{ TxxmFileFromRes }

constructor TxxmFileFromRes.Create(AProject: TXxmProject;
  AResName: AnsiString);
begin
  inherited Create(AProject);
  FResName:=AResName;
end;

procedure TxxmFileFromRes.Build(const Context: IXxmContext;
  const Caller: IXxmFragment; const Values: array of OleVariant;
  const Objects: array of TObject);
var
  r:TResourceStream;
begin
  r:=TResourceStream.Create(HInstance,FResName,RT_XXMRESFILE);
  //TODO: TDecompressionStream
  try
    //TODO: Context.ContentType:= (from resources index?)
    //TODO: ['Last-Modified']:=RFC822DateGMT()
    (Context as IxxmHttpHeaders).ResponseHeaders.Item['Content-Length']:=IntToStr(r.Size);
    Context.SendStream(TStreamAdapter.Create(r));
  finally
    r.Free;
  end;
end;

initialization
  IsMultiThread:=true;
  LoadFilesFromResIndex;
finalization
  xxmFilesFromResIndex.Free;
end.
