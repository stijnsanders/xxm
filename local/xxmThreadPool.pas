unit xxmThreadPool;

interface

uses
  Windows, SysUtils, xxmLoader, Classes;

type
  TXxmPageLoaderPool=class(TObject)
  private
    FLoaders:array of TXxmPageLoader;
    FLoaderSize:integer;
    FLock:TRTLCriticalSection;
    FQueue:TXxmLocalContext;
    procedure SetSize(x:integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Queue(Context:TXxmLocalContext);//called from handler
    function Unqueue:TXxmLocalContext;//called from threads
  end;

const
  PoolMaxThreads=64;//TODO: from setting?

var
  PageLoaderPool:TXxmPageLoaderPool;

implementation

{ TXxmPageLoaderPool }

constructor TXxmPageLoaderPool.Create;
begin
  inherited Create;
  FLoaderSize:=0;
  FQueue:=nil;
  InitializeCriticalSection(FLock);
  SetSize(PoolMaxThreads);//TODO: setting
  //TODO: setting no pool
end;

destructor TXxmPageLoaderPool.Destroy;
begin
  SetSize(0);
  DeleteCriticalSection(FLock);
  {
  2007-12
  When using MemCheck, you may notice that the TXxmPageLoader's
  in the thread pool are not released properly
  When using a WaitFor, the finalization of the process takes too much time.
  Since only the exact number of threads in the pool are not released,
  this is a limited leak.
  }
  inherited;
end;

procedure TXxmPageLoaderPool.SetSize(x: integer);
var
  ThreadsClosed:boolean;
begin
  EnterCriticalSection(FLock);
  try
    ThreadsClosed:=false;
    if FLoaderSize<x then
     begin
      SetLength(FLoaders,x);
      while not(FLoaderSize=x) do
       begin
        FLoaders[FLoaderSize]:=nil;
        inc(FLoaderSize);
       end;
     end
    else
     begin
      while not(FLoaderSize=X) do
       begin
        dec(FLoaderSize);
        //FreeAndNil(FLoaders[FLoaderSize]);
        if not(FLoaders[FLoaderSize]=nil) then
         begin
          FLoaders[FLoaderSize].FreeOnTerminate:=true;
          FLoaders[FLoaderSize].Terminate;
          FLoaders[FLoaderSize].Resume;
          ThreadsClosed:=true;
          FLoaders[FLoaderSize]:=nil;
         end;
       end;
      SetLength(FLoaders,x);
      if ThreadsClosed then Sleep(50);
     end;
    //if FLoaderIndex>=FLoaderSize then FLoaderIndex:=0;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TXxmPageLoaderPool.Queue(Context:TXxmLocalContext);
var
  c:TXxmLocalContext;
  i:integer;
begin
  EnterCriticalSection(FLock);
  try
    //add to queue
    if FQueue=nil then FQueue:=Context else
     begin
      c:=FQueue;
      while not(c.Queue=nil) do c:=c.Queue;
      c.Queue:=Context;
     end;
  finally
    LeaveCriticalSection(FLock);
  end;

  //fire thread
  //TODO: see if a rotary index matters in any way
  i:=0;
  while (i<FLoaderSize) and not(FLoaders[i]=nil) and FLoaders[i].InUse do inc(i);
  if i=FLoaderSize then
   begin
    //pool full, leave on queue
   end
  else
   begin
    if FLoaders[i]=nil then
      FLoaders[i]:=TXxmPageLoader.Create //start thread
    else
      FLoaders[i].Resume; //resume on waiting unqueues
    //TODO: expire unused threads on low load
   end;
end;

function TXxmPageLoaderPool.Unqueue:TXxmLocalContext;
begin
  if FQueue=nil then Result:=nil else
   begin
    EnterCriticalSection(FLock);
    try
      Result:=FQueue;
      if not(Result=nil) then
       begin
        FQueue:=FQueue.Queue;
        Result.Queue:=nil;
       end;
    finally
      LeaveCriticalSection(FLock);
    end;
   end;
end;

initialization
  PageLoaderPool:=nil;//created on first page start, DLL may be called for URL info only
finalization
  FreeAndNil(PageLoaderPool);

end.
