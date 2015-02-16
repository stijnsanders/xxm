unit xxmThreadPool;

interface

uses
  Windows, SysUtils, xxmContext, Classes, xxmPReg;

type
  TXxmQueueContext=class(TXxmGeneralContext)
  protected
    QueueIn,QueueOut:TXxmQueueContext;
    QueueSince,SuspendMax:cardinal;
    procedure Resume(ToDrop:boolean); virtual;
  public
    procedure Execute; virtual; abstract;
  end;

  TXxmEventsController=class;//forward

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
    FQueueIn,FQueueOut:TXxmQueueContext;
    FEventsCtrl:TXxmEventsController;
    procedure SetSize(x:integer);
  public
    constructor Create(PoolMaxThreads:integer);
    destructor Destroy; override;
    procedure Queue(Context:TXxmQueueContext);//called from handler
    function Unqueue:TXxmQueueContext;//called from threads
    function EventsController:TXxmEventsController;
  end;

  TXxmEventsController=class(TThread)
  private
    FLock:TRTLCriticalSection;
    FEvents:array of record
      Key:WideString;
      ProjectEntry:TXxmProjectEntry;
      Queue:TXxmQueueContext;
      CheckInterval,CheckNext:cardinal;
    end;
    FEventsIndex,FEventsSize:cardinal;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SuspendContext(Context:TXxmQueueContext;const EventKey:WideString;
      CheckIntervalMS,MaxWeightSec:cardinal);
  end;

var
  PageLoaderPool:TXxmPageLoaderPool;

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
    PAnsiChar('xxm:PageLoader:NextJob:'+IntToHex(ThreadID,8)));
end;

destructor TXxmPageLoader.Destroy;
begin
  CloseHandle(FNextJobEvent);
  inherited;
end;

procedure TXxmPageLoader.Execute;
var
  Context:TXxmQueueContext;
begin
  inherited;
  CoInitialize(nil);
  SetErrorMode(SEM_FAILCRITICALERRORS);
  while not(Terminated) do
   begin
    Context:=PageLoaderPool.Unqueue;
    if Context=nil then
     begin
      FInUse:=false;//used by PageLoaderPool.Queue
      //SetThreadName('(xxm)');
      ResetEvent(FNextJobEvent);
      WaitForSingleObject(FNextJobEvent,INFINITE);
      FInUse:=true;
     end
    else
     begin
      //Context._AddRef;//_AddRef moved to TXxmPageLoaderPool.Queue
      try
        //SetThreadName('xxm:'+Context.FURL);
        //TODO: if cardinal(GetTickCount-Context.Context.QueueSince)>?
        Context.Execute;//assert all exceptions handled!
      finally
        Context._Release;
      end;
     end;
   end;
  //CoUninitialize;//? hangs thread
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
  FQueueIn:=nil;
  FQueueOut:=nil;
  FEventsCtrl:=nil;
  InitializeCriticalSection(FLock);
  SetSize(PoolMaxThreads);//TODO: setting
  //TODO: setting no pool
end;

destructor TXxmPageLoaderPool.Destroy;
begin
  SetSize(0);
  FreeAndNil(FEventsCtrl);
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
  i:integer;
begin
  //TODO: max on queue, fire 'server busy' when full?
  if Context<>nil then
   begin
    EnterCriticalSection(FLock);
    try
      //add to queue
      Context._AddRef;//see _Release by TXxmPageLoader.Execute
      Context.QueueIn:=FQueueIn;
      Context.QueueOut:=nil;
      Context.QueueSince:=GetTickCount;
      if FQueueIn=nil then
       begin
        FQueueIn:=Context;
        FQueueOut:=Context;
       end
      else
       begin
        FQueueIn.QueueOut:=Context;
        FQueueIn:=Context;
       end;

      //fire thread
      //TODO: see if a rotary index matters in any way
      i:=0;
      while (i<FLoaderSize) and (FLoaders[i]<>nil) and FLoaders[i].InUse do inc(i);
      if i<>FLoaderSize then
        if FLoaders[i]=nil then
          FLoaders[i]:=TXxmPageLoader.Create //start thread
        else
          FLoaders[i].SignalNextJob; //so it calls Unqueue
      //TODO: expire unused threads on low load

    finally
      LeaveCriticalSection(FLock);
    end;
   end;
end;

function TXxmPageLoaderPool.Unqueue:TXxmQueueContext;
begin
  if FQueueOut=nil then Result:=nil else
   begin
    EnterCriticalSection(FLock);
    try
      Result:=FQueueOut;
      if Result<>nil then
       begin
        FQueueOut:=Result.QueueOut;
        if FQueueOut=nil then FQueueIn:=nil;
       end;
    finally
      LeaveCriticalSection(FLock);
    end;
   end;
end;

function TXxmPageLoaderPool.EventsController: TXxmEventsController;
begin
  if FEventsCtrl=nil then
   begin
    EnterCriticalSection(FLock);
    try
      if FEventsCtrl=nil then
        FEventsCtrl:=TXxmEventsController.Create;
    finally
      LeaveCriticalSection(FLock);
    end;
   end;
  Result:=FEventsCtrl;
end;

{ TXxmEventsController }

constructor TXxmEventsController.Create;
begin
  inherited Create(false);
  FEventsIndex:=0;
  FEventsSize:=0;
  InitializeCriticalSection(FLock);
end;

destructor TXxmEventsController.Destroy;
begin
  DeleteCriticalSection(FLock);
  SetLength(FEvents,0);
  inherited;
end;

procedure TXxmEventsController.SuspendContext(Context: TXxmQueueContext;
  const EventKey: WideString; CheckIntervalMS, MaxWeightSec: cardinal);
var
  pe:TXxmProjectEntry;
  i,tc:cardinal;
  c,c1:TXxmQueueContext;
begin
  EnterCriticalSection(FLock);
  try
    pe:=Context.ProjectEntry;
    tc:=GetTickCount;
    i:=0;
    while (i<FEventsIndex) and not((FEvents[i].ProjectEntry=pe)
      and (FEvents[i].Key=EventKey)) do inc(i);
    if i=FEventsIndex then
     begin
      if FEventsIndex=FEventsSize then
       begin
        inc(FEventsSize,$20);//grow
        SetLength(FEvents,FEventsSize);
        FEvents[i].Key:=EventKey;
        FEvents[i].ProjectEntry:=pe;
        FEvents[i].Queue:=Context;
        FEvents[i].CheckInterval:=CheckIntervalMS;
        FEvents[i].CheckNext:=tc+CheckIntervalMS;
        inc(FEventsIndex);
        Context.QueueIn:=nil;
       end;
     end
    else
     begin
      if CheckIntervalMS<FEvents[i].CheckInterval then
        FEvents[i].CheckInterval:=CheckIntervalMS;
      if cardinal(tc-FEvents[i].CheckNext)>FEvents[i].CheckInterval then
        FEvents[i].CheckNext:=tc+FEvents[i].CheckInterval;
      //insert sorted by SuspendMax (zeroes on tail)
      c1:=nil;
      c:=FEvents[i].Queue;
      while (c<>nil) and (((Context.SuspendMax=0) and (c.SuspendMax<>0)) or
        (integer(c.SuspendMax-Context.SuspendMax)<=0)) do
       begin
        c1:=c;
        c:=Context.QueueIn;
       end;
      Context.QueueIn:=c;
      if c1=nil then FEvents[i].Queue:=Context else c1.QueueIn:=Context;
     end;
    Context._AddRef;
    Context.QueueSince:=tc;
    if MaxWeightSec=0 then Context.SuspendMax:=0 else
     begin
      Context.SuspendMax:=tc+MaxWeightSec*1000;
      if Context.SuspendMax=0 then inc(Context.SuspendMax);
     end;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TXxmEventsController.Execute;
var
  i,j,k,x,tc:cardinal;
  pe:IXxmProjectEvents2;
  c,c1:TXxmQueueContext;
begin
  x:=0;
  i:=0;
  while not Terminated do
   begin
    if x<>0 then Sleep(x);
    x:=250;//default
    if FEventsIndex<>0 then
     begin
      EnterCriticalSection(FLock);
      try
        tc:=GetTickCount;
        j:=FEventsIndex;
        k:=0;
        while (k<FEventsIndex) and (j=FEventsIndex) do
         begin
          j:=(i+k) mod FEventsIndex;
          if (FEvents[j].Queue=nil) or (integer(tc-
            FEvents[(i+j) mod FEventsIndex].CheckNext)<0) then j:=FEventsIndex;
          inc(k);
         end;
        if j<>FEventsIndex then
         begin
          c:=FEvents[j].Queue;
          try
            //check event
            pe:=FEvents[j].ProjectEntry.Project as IXxmProjectEvents2;
            if pe.CheckEvent(FEvents[j].Key,FEvents[j].CheckInterval) then
             begin
              //resume
              FEvents[j].Queue:=nil;
              while c<>nil do
               begin
                c1:=c;
                c:=c.QueueIn;
                c1.Resume(false);
               end;
             end
            else
             begin
              //drop any past SuspendMax or that lost connection
              c1:=nil;
              while c<>nil do
                if not(c.Connected) or
                  ((c.SuspendMax<>0) and ((integer(tc-c.SuspendMax)>0))) then
                 begin
                  if c1=nil then FEvents[j].Queue:=c.QueueIn else c1.QueueIn:=c.QueueIn;
                  c.Resume(true);
                  if c1=nil then c:=FEvents[j].Queue else c:=c1.QueueIn;
                 end
                else
                 begin
                  c1:=c;
                  c:=c.QueueIn;
                 end;
             end;
          except
            //TODO: HandleException(?
            //drop all
            FEvents[j].Queue:=nil;
            while c<>nil do
             begin
              c1:=c;
              c:=c.QueueIn;
              c1.Resume(true);
             end;
          end;
          try
            pe:=nil;
          except
            pointer(pe):=nil;//silent
          end;
          if FEvents[j].Queue<>nil then
            FEvents[j].CheckNext:=GetTickCount+FEvents[j].CheckInterval;
         end;
        if i>=FEventsIndex then i:=0 else inc(i);
      finally
        LeaveCriticalSection(FLock);
      end;
     end;

    //x:=PageLoaderPool.CheckNextEvent;
   end;
end;

{ TXxmQueueContext }

procedure TXxmQueueContext.Resume(ToDrop:boolean);
begin
  //if ToDrop then Next:=ntResumeDrop else Next:=ntResume;
  PageLoaderPool.Queue(Self);
  _Release;//see _Addref by TXxmEventsController.SuspendContext
end;

initialization
  PageLoaderPool:=nil;//created on first page start, DLL may be called for URL info only
finalization
  FreeAndNil(PageLoaderPool);

end.
