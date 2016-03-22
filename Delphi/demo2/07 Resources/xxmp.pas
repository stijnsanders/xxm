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
  private
    ResModSince:string;
  public
    procedure AfterConstruction; override;
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

  TxxmNoChange=class(TxxmPage)
  public
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

function RFC822DateGMT(dd: TDateTime): string;
const
  Days:array [1..7] of string=
    ('Sun','Mon','Tue','Wed','Thu','Fri','Sat');
  Months:array [1..12] of string=
    ('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec');
//  SignStr:array[boolean] of string=('-','+');
var
  dg:TDateTime;
  y,m,d,wd,th,tm,ts,tms:Word;
  tz:TIME_ZONE_INFORMATION;
begin
  GetTimeZoneInformation(tz);
  dg:=dd+tz.Bias/1440;
  DecodeDateFully(dg,y,m,d,wd);
  DecodeTime(dg,th,tm,ts,tms);
  FmtStr(Result, '%s, %d %s %d %.2d:%.2d:%.2d GMT',
    [Days[wd],d,Months[m],y,th,tm,ts]);
end;

{ TXxmdemo }

procedure TXxmdemo.AfterConstruction;
begin
  inherited;
  ResModSince:=RFC822DateGMT(Now);
end;

function TXxmdemo.LoadPage(Context: IXxmContext; Address: WideString): IXxmFragment;
var
  rn:AnsiString;
begin
  inherited;
  //TODO: link session to request
  if Copy(Address,1,4)='res/' then
   begin
    rn:=xxmFilesFromResIndex.Values[Copy(Address,5,Length(Address)-4)];
    if rn='' then
      Result:=nil
    else
      //TODO: LastMod from resource index?
      if (Context as IxxmHttpHeaders).RequestHeaders['If-Modified-Since']=ResModSince then
        Result:=TxxmNoChange.Create(Self)
      else
       begin
        (Context as IxxmHttpHeaders).ResponseHeaders['Last-Modified']:=ResModSince;
        Result:=TxxmFileFromRes.Create(Self,rn);
       end;
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
    (Context as IxxmHttpHeaders).ResponseHeaders.Item['Content-Length']:=IntToStr(r.Size);
    Context.SendStream(TStreamAdapter.Create(r));
  finally
    r.Free;
  end;
end;

{ TxxmNoChange }

procedure TxxmNoChange.Build(const Context: IXxmContext;
  const Caller: IXxmFragment; const Values: array of OleVariant;
  const Objects: array of TObject);
begin
  Context.SetStatus(304,'Not Modified');
  //no content
end;

initialization
  IsMultiThread:=true;
  LoadFilesFromResIndex;
finalization
  xxmFilesFromResIndex.Free;
end.
