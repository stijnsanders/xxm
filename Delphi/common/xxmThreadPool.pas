unit xxmThreadPool;

interface

uses
  Windows, SysUtils, xxmContext, Classes;

type
  TXxmQueueContext=class(TXxmGeneralContext)
  public
    Queue:TXxmQueueContext;
    procedure Execute; virtual; abstract;
  end;

  TXxmPageLoader=class(TThread)
  protected
    FInUse:boolean;
    FNextJobEvent:THandle;
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SignalNextJob;
    property InUse:boolean read FInUse;
  end;

  TXxmPageLoaderPool=class(TObject)
  private
    FLoaders:array of TXxmPageLoader;
    FLoaderSize:integer;
    FLock:TRTLCriticalSection;
    FQueue:TXxmQueueContext;
    procedure SetSize(x:integer);
  public
    constructor Create(PoolMaxThreads:integer);
    destructor Destroy; override;
    procedure Queue(Context:TXxmQueueContext);//called from handler
    function Unqueue:TXxmQueueContext;//called from threads
  end;

var
  PageLoaderPool:TXxmPageLoaderPool;

threadvar
  ContentBuffer: TMemoryStream;

procedure SetThreadName(const ThreadDisplayName:AnsiString);
function IsDebuggerPresent: BOOL; stdcall;

implementation

uses
  xxm, ActiveX;

function IsDebuggerPresent; external 'kernel32.dll';

procedure SetThreadName(const ThreadDisplayName:AnsiString);
var
  ThreadInfo:record
    dwType:LongWord;
    szName:PAnsiChar;
    dwThreadID:LongWord;
    dwFlags:LongWord;
  end;
begin
  if IsDebuggerPresent then
    begin
      ThreadInfo.dwType:=$1000;
      ThreadInfo.szName:=PAnsiChar(ThreadDisplayName);
      ThreadInfo.dwThreadID:=LongWord(-1);//calling thread
      ThreadInfo.dwFlags:=0;
      try
        RaiseException($406D1388,0,SizeOf(ThreadInfo) div SizeOf(LongWord),@ThreadInfo);
      except
        //
      end;
    end;
end;

{ TXxmPageLoader }

constructor TXxmPageLoader.Create;
begin
  inherited Create(false);
  //FInUse:=false;
  FNextJobEvent:=CreateEventA(nil,true,false,
    PAnsiChar('xxmLocal:PageLoader:NextJob:'+IntToHex(GetCurrentThreadId,8)));
end;

destructor TXxmPageLoader.Destroy;
begin
  CloseHandle(FNextJobEvent);
  inherited;
end;

procedure TXxmPageLoader.Execute;
var
  Context:TXxmQueueContext;
  ContextI:IXxmContext;
begin
  inherited;
  CoInitialize(nil);
  SetErrorMode(SEM_FAILCRITICALERRORS);
  ContentBuffer:=nil;
  while not(Terminated) do
   begin
    Context:=PageLoaderPool.Unqueue;
    if Context=nil then
     begin
      FInUse:=false;//used by PageLoaderPool.Queue
      SetThreadName('(xxmPageLoader)');
      ResetEvent(FNextJobEvent);
      WaitForSingleObject(FNextJobEvent,INFINITE);
      FInUse:=true;
     end
    else
     begin
      ContextI:=Context;//keep refcount up for premature terminate
      try
        SetThreadName('xxmPageLoader:'+Context.FURL);
        Context.Execute;//assert all exceptions handled!
      finally
        ContextI:=nil;
      end;
     end;
   end;
  //CoUninitialize;//? hangs thread
  if ContentBuffer<>nil then ContentBuffer.Free;
end;

procedure TXxmPageLoader.SignalNextJob;
begin
  //assert thread is waiting on FNextJobEvent
  SetEvent(FNextJobEvent);
end;

{ TXxmPageLoaderPool }

constructor TXxmPageLoaderPool.Create(PoolMaxThreads:integer);
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
      while FLoaderSize<>x do
       begin
        FLoaders[FLoaderSize]:=nil;
        inc(FLoaderSize);
       end;
     end
    else
     begin
      while FLoaderSize<>X do
       begin
        dec(FLoaderSize);
        //FreeAndNil(FLoaders[FLoaderSize]);
        if FLoaders[FLoaderSize]<>nil then
         begin
          FLoaders[FLoaderSize].FreeOnTerminate:=true;
          FLoaders[FLoaderSize].Terminate;
          FLoaders[FLoaderSize].SignalNextJob;
          ThreadsClosed:=true;
          FLoaders[FLoaderSize]:=nil;
         end;
       end;
      SetLength(FLoaders,x);
      if ThreadsClosed then Sleep(250);
     end;
    //if FLoaderIndex>=FLoaderSize then FLoaderIndex:=0;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TXxmPageLoaderPool.Queue(Context:TXxmQueueContext);
var
  c:TXxmQueueContext;
  i:integer;
begin
  EnterCriticalSection(FLock);
  try
    //add to queue
    if FQueue=nil then FQueue:=Context else
     begin
      c:=FQueue;
      while c.Queue<>nil do c:=c.Queue;
      c.Queue:=Context;
     end;
  finally
    LeaveCriticalSection(FLock);
  end;

  //fire thread
  //TODO: see if a rotary index matters in any way
  i:=0;
  while (i<FLoaderSize) and (FLoaders[i]<>nil) and FLoaders[i].InUse do inc(i);
  if i=FLoaderSize then
   begin
    //pool full, leave on queue
   end
  else
   begin
    if FLoaders[i]=nil then
      FLoaders[i]:=TXxmPageLoader.Create //start thread
    else
      FLoaders[i].SignalNextJob;
    //TODO: expire unused threads on low load
   end;
end;

function TXxmPageLoaderPool.Unqueue:TXxmQueueContext;
begin
  if FQueue=nil then Result:=nil else
   begin
    EnterCriticalSection(FLock);
    try
      Result:=FQueue;
      if Result<>nil then
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
