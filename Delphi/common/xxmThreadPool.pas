unit xxmThreadPool;

interface

uses
  Windows, SysUtils, xxm, xxmContext, Classes, xxmPReg;

type
  TXxmQueueContext=class(TXxmGeneralContext
    ,IXxmContextSuspend
    //,IXxmSocketSuspend //only on handlers that support this!
    )
  private
    QueueIn,QueueOut:TXxmQueueContext;
    QueueSince,SuspendMax:cardinal;

    FResumeFragment, FDropFragment: WideString;
    FResumeValue, FDropValue: OleVariant;
    FStateNext: TXxmContextState;
  protected

    procedure BeginRequest; override;
    procedure EndRequest; override;

    { IXxmContextSuspend }
    procedure Suspend(const EventKey: WideString;
      CheckIntervalMS, MaxWaitTimeSec: cardinal;
      const ResumeFragment: WideString; ResumeValue: OleVariant;
      const DropFragment: WideString; DropValue: OleVariant);

    { IXxmSocketSuspend }
    procedure SuspendSocket(Handler: IXxmRawSocket); virtual;

  public
    procedure Perform;  
  end;

  TXxmEventsController=class;//forward

  TXxmPageLoader=class(TThread)
  protected
    FNextJobEvent:THandle;
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    function InUse:boolean;
    procedure SignalNextJob;
  end;

  TXxmPageLoaderPool=class(TObject)
  private
    FLoaders:array of TXxmPageLoader;
    FLoaderSize,FLoaderCount:integer;
    FLock:TRTLCriticalSection;
    FQueueIn,FQueueOut:TXxmQueueContext;
    FEventsCtrl:TXxmEventsController;
    procedure SetSize(x:integer);
  public
    constructor Create(PoolMaxThreads:integer);
    destructor Destroy; override;
    procedure Queue(Context:TXxmGeneralContext;
      SetState:TXxmContextState);//called from handler
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
      CheckInterval,CheckLast:cardinal;
    end;
    FEventsIndex,FEventsSize:cardinal;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SuspendContext(Context:TXxmGeneralContext;
      const EventKey:WideString; CheckIntervalMS,MaxWeightSec:cardinal);
  end;

  EXxmContextAlreadySuspended=class(Exception);

var
  PageLoaderPool:TXxmPageLoaderPool;

procedure SetThreadName(const ThreadDisplayName:AnsiString);
function IsDebuggerPresent: BOOL; stdcall;

implementation

uses
  ActiveX, Variants;

const //resourcestring?
  SXxmContextAlreadySuspended='Context has already been suspended';

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
  FNextJobEvent:=CreateEvent(nil,true,false,nil);
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
    try
      Context:=PageLoaderPool.Unqueue;
      if Context=nil then
       begin
        //SetThreadName('(xxm)');
        ResetEvent(FNextJobEvent);
        WaitForSingleObject(FNextJobEvent,INFINITE);
       end
      else
       begin
        //SetThreadName('xxm:'+Context.URL);
        Context.Perform;
        //assert all exceptions handled
       end;
    except
      //silent (log?)
    end;
  //CoUninitialize;//? hangs thread
end;

function TXxmPageLoader.InUse: boolean;
begin
  //thread is not in use when event non-signaled (and thread is waiting)
  Result:=WaitForSingleObject(FNextJobEvent,0)=WAIT_OBJECT_0;
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
  FLoaderCount:=0;
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
          dec(FLoaderCount);
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

procedure TXxmPageLoaderPool.Queue(Context:TXxmGeneralContext;
  SetState:TXxmContextState);
var
  i,j:integer;
  c:TXxmQueueContext;
begin
  //TODO: max on queue, fire 'server busy' when full?
  i:=0;
  if Context<>nil then
   begin
    Context.State:=SetState;
    c:=Context as TXxmQueueContext;
    EnterCriticalSection(FLock);
    try
      //add to queue
      c.QueueIn:=FQueueIn;
      c.QueueOut:=nil;
      c.QueueSince:=GetTickCount;
      if FQueueIn=nil then
       begin
        FQueueIn:=c;
        FQueueOut:=c;
       end
      else
       begin
        FQueueIn.QueueOut:=c;
        FQueueIn:=c;
       end;

      //find or fire thread
      if FLoaderCount=0 then i:=0 else
       begin
        if i=0 then j:=FLoaderCount-1 else j:=i-1;
        while (i<>j) and (FLoaders[i]<>nil)
          and FLoaders[i].InUse do
         begin
          inc(i);
          if i=FLoaderCount then i:=0;
         end;
        if (i=j) and (FLoaderCount<FLoaderSize) then i:=FLoaderCount;
       end;
      if FLoaders[i]=nil then
       begin
        FLoaders[i]:=TXxmPageLoader.Create; //start thread
        inc(FLoaderCount);
       end
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

procedure TXxmEventsController.SuspendContext(Context: TXxmGeneralContext;
  const EventKey: WideString; CheckIntervalMS, MaxWeightSec: cardinal);
var
  pe:TXxmProjectEntry;
  i,tc:cardinal;
  c,c0,c1:TXxmQueueContext;
begin
  Context.State:=ctSuspended;
  EnterCriticalSection(FLock);
  try
    c0:=Context as TXxmQueueContext;
    pe:=c0.ProjectEntry;
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
       end;
      FEvents[i].Key:=EventKey;
      FEvents[i].ProjectEntry:=pe;
      FEvents[i].Queue:=c0;
      FEvents[i].CheckInterval:=CheckIntervalMS;
      FEvents[i].CheckLast:=tc;
      inc(FEventsIndex);
      c0.QueueIn:=nil;
     end
    else
     begin
      if CheckIntervalMS<FEvents[i].CheckInterval then
        FEvents[i].CheckInterval:=CheckIntervalMS;
      if cardinal(tc-FEvents[i].CheckLast)>FEvents[i].CheckInterval then
        FEvents[i].CheckLast:=tc;
      //insert sorted by SuspendMax (zeroes on tail)
      c1:=nil;
      c:=FEvents[i].Queue;
      while (c<>nil) and (((c0.SuspendMax=0) and (c.SuspendMax<>0)) or
        (integer(c.SuspendMax)-integer(c0.SuspendMax)<=0)) do
       begin
        c1:=c;
        c:=c.QueueIn;
       end;
      c0.QueueIn:=c;
      if c1=nil then FEvents[i].Queue:=c0 else c1.QueueIn:=c0;
     end;
    c0.QueueSince:=tc;
    if MaxWeightSec=0 then c0.SuspendMax:=0 else
     begin
      c0.SuspendMax:=tc+MaxWeightSec*1000;
      if c0.SuspendMax=0 then inc(c0.SuspendMax);
     end;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TXxmEventsController.Execute;
var
  i,j,x,y,z,tc:cardinal;
  pe:IXxmProjectEvents2;
  c,c1:TXxmQueueContext;
  b:boolean;
begin
  x:=0;
  while not Terminated do
    try
      if x<>0 then Sleep(x);
      x:=250;//default
      if FEventsIndex<>0 then
       begin
        EnterCriticalSection(FLock);
        try
          tc:=GetTickCount;
          i:=0;
          j:=FEventsIndex;
          z:=0;
          while i<FEventsIndex do
           begin
            if FEvents[i].Queue<>nil then
             begin
              y:=cardinal(tc-FEvents[i].CheckLast);
              if y>FEvents[i].CheckInterval then
               begin
                if (y>z) or (j=FEventsIndex) then
                 begin
                  z:=y;
                  j:=i;
                  if FEvents[i].CheckInterval<x then x:=FEvents[i].CheckInterval;
                 end;
               end
              else
                if y<x then x:=y;
             end;
            inc(i);
           end;
          if j<>FEventsIndex then
           begin
            c:=FEvents[j].Queue;
            try
              //check event
              if FEvents[j].ProjectEntry.Project.
                QueryInterface(IXxmProjectEvents2,pe)=S_OK then
               begin
                b:=pe.CheckEvent(FEvents[j].Key,FEvents[j].CheckInterval);
                pe:=nil;
               end
              else
                b:=true;//
              if b then
               begin
                //resume
                FEvents[j].Queue:=nil;
                while c<>nil do
                 begin
                  c1:=c;
                  c:=c.QueueIn;
                  PageLoaderPool.Queue(c1,ctResuming);
                 end;
               end
              else
               begin
                //drop any past SuspendMax or that lost connection
                c1:=nil;
                while c<>nil do
                  if not(c.Connected) or
                    ((c.SuspendMax<>0) and ((integer(tc)-integer(c.SuspendMax)>0))) then
                   begin
                    if c1=nil then
                      FEvents[j].Queue:=c.QueueIn
                    else
                      c1.QueueIn:=c.QueueIn;
                      
                    PageLoaderPool.Queue(c,ctDropping);

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
                PageLoaderPool.Queue(c1,ctDropping);
               end;
            end;
            try
              pe:=nil;
            except
              pointer(pe):=nil;//silent
            end;
            if FEvents[j].Queue<>nil then
              FEvents[j].CheckLast:=GetTickCount;
           end;
        finally
          LeaveCriticalSection(FLock);
        end;
       end;
    except
      //silent (log?)
    end;
end;

{ TXxmQueueContext }

procedure TXxmQueueContext.BeginRequest;
begin
  inherited;
  FResumeFragment:='';
  FResumeValue:=Null;
  FDropFragment:='';
  FDropValue:=Null;
  FStateNext:=ctResponding;//default
end;

procedure TXxmQueueContext.EndRequest;
begin
  FResumeValue:=Null;
  FDropValue:=Null;
  inherited;
end;

procedure TXxmQueueContext.Perform;
begin
  try
    case State of
      ctHeaderNotSent:
       begin
        BeginRequest;
        HandleRequest;
       end;
      ctSpooling:
        Spool;
      ctResuming:
       begin
        State:=FStateNext;
        IncludeX(FResumeFragment,FResumeValue);
       end;
      ctDropping:
       begin
        State:=FStateNext;
        IncludeX(FDropFragment,FDropValue);
       end;
      ctSocketResume:
       begin
        State:=ctSocketDisconnect;
        (IUnknown(FResumeValue) as IXxmRawSocket).DataReady(0);
        if State=ctSocketDisconnect then
         begin
          State:=FStateNext;
          (IUnknown(FResumeValue) as IXxmRawSocket).Disconnect;
         end;
       end;
      ctSocketDisconnect:
       begin
        State:=FStateNext;
        (IUnknown(FResumeValue) as IXxmRawSocket).Disconnect;
       end;
      else
        State:=ctResponding;//unexpected! raise?
    end;
  except
    //silent
    if State>ctResponding then State:=ctResponding;
  end;
  if State in [ctHeaderNotSent..ctResponding] then
    try
      Recycle;
    except
      //silent (log?)
    end;
end;

procedure TXxmQueueContext.Suspend(const EventKey: WideString;
  CheckIntervalMS, MaxWaitTimeSec: cardinal;
  const ResumeFragment: WideString; ResumeValue: OleVariant;
  const DropFragment: WideString; DropValue: OleVariant);
begin
  if State=ctSuspended then
    raise EXxmContextAlreadySuspended.Create(SXxmContextAlreadySuspended);
  FResumeFragment:=ResumeFragment;
  FResumeValue:=ResumeValue;
  FDropFragment:=DropFragment;
  FDropValue:=DropValue;
  FStateNext:=State;
  PageLoaderPool.EventsController.SuspendContext(Self,
    EventKey,CheckIntervalMS,MaxWaitTimeSec);
end;

procedure TXxmQueueContext.SuspendSocket(Handler: IXxmRawSocket);
begin
  if State=ctSocketResume then
    raise EXxmContextAlreadySuspended.Create(SXxmContextAlreadySuspended);
  FResumeValue:=Handler;
  FStateNext:=State;
  //TODO: force inheriters to keep connection:
  //KeptConnections.Queue(Self,ctSocketResume);
end;

initialization
  PageLoaderPool:=nil;//created on first page start, DLL may be called for URL info only
finalization
  FreeAndNil(PageLoaderPool);
end.
