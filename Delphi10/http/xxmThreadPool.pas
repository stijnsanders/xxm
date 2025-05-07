unit xxmThreadPool;

interface

uses Windows, Classes, xxmContext, xxmSock;

type
  TxxmContextEvent=procedure(Sender:TObject) of object;

  TxxmPageLoader=class(TThread)
  private
    FNextJobEvent:THandle;
  protected
    procedure Execute; override;
  public
    InUse:boolean;
    constructor Create;
    destructor Destroy; override;
    procedure SignalNextJob;
  end;

  TxxmPageLoaderPool=class(TObject)
  private
    FLock:TRTLCriticalSection;
    FQueue:array of TxxmContextEvent;
    FQueueIndex,FQueueSize:integer;
    FLoader:array of TxxmPageLoader;
    FLoaderIndex,FLoaderSize:integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Queue(Event: TxxmContextEvent);
    procedure Unqueue(var Job: TxxmContextEvent);
  end;

var
  PageLoaderPool:TxxmPageLoaderPool;

implementation

uses SysUtils, ActiveX;

{ TxxmPageLoaderPool }

constructor TxxmPageLoaderPool.Create;
begin
  inherited Create;
  FQueueIndex:=0;
  FQueueSize:=0;
  FLoaderIndex:=0;
  FLoaderSize:=0;
  InitializeCriticalSection(FLock);
  //TODO: InitializeCriticalSectionAndSpinCount ?
end;

destructor TxxmPageLoaderPool.Destroy;
begin
  DeleteCriticalSection(FLock);
  inherited;
end;

procedure TxxmPageLoaderPool.Queue(Event: TxxmContextEvent);
var
  i:integer;
begin
  EnterCriticalSection(FLock);
  try
    i:=0;
    while (i<FQueueIndex) and (@FQueue[i]<>nil) do inc(i);
    if i<FQueueIndex then
     begin
      FQueue[i]:=Event;
     end
    else
     begin
      if FQueueIndex=FQueueSize then
       begin
        //TODO: max queue size
        inc(FQueueSize,$1000);//grow step
        SetLength(FQueue,FQueueSize);
       end;
      FQueue[FQueueIndex]:=Event;
      inc(FQueueIndex);
     end;
  finally
    LeaveCriticalSection(FLock);
  end;

  //find or fire thread
  i:=0;
  while (i<FLoaderIndex) and (FLoader[i].InUse) do inc(i);
  if not(i<FLoaderIndex) then
   begin
    if FLoaderIndex=FLoaderSize then
     begin
      inc(FLoaderSize,$20);//grow step
      //TODO: config limit thread count
      SetLength(FLoader,FLoaderSize);
     end;
    FLoader[i]:=TxxmPageLoader.Create;
    inc(FLoaderIndex);
   end;
  FLoader[i].SignalNextJob;
end;

procedure TxxmPageLoaderPool.Unqueue(var Job: TxxmContextEvent);
var
  i:integer;
begin
  EnterCriticalSection(FLock);
  try
    i:=0;
    while (i<FQueueIndex) and (@FQueue[i]=nil) do inc(i);
    if i<FQueueIndex then
     begin
      Job:=FQueue[i];
      FQueue[i]:=nil;
     end
    else
      Job:=nil;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

{ TxxmPageLoader }

constructor TxxmPageLoader.Create;
begin
  inherited Create(false);
  InUse:=false;
  FNextJobEvent:=CreateEvent(nil,true,false,nil);
end;

destructor TxxmPageLoader.Destroy;
begin
  CloseHandle(FNextJobEvent);
  inherited;
end;

procedure TxxmPageLoader.Execute;
var
  Job:TXxmContextEvent;
begin
  inherited;
  CoInitialize(nil);//TODO: config?
  SetErrorMode(SEM_FAILCRITICALERRORS);
  while not(Terminated) do
    try
      PageLoaderPool.Unqueue(Job);
      if @Job=nil then
       begin
        //SetThreadName('(xxm)');
        InUse:=false;//?
        ResetEvent(FNextJobEvent);
        WaitForSingleObject(FNextJobEvent,INFINITE);
       end
      else
       begin
        //SetThreadName('xxm:'+Context.URL);
        try

          Job(Self);

        finally
          InUse:=false;
        end;
        //assert all exceptions handled
       end;
    except
      //silent (log?)
    end;
  //CoUninitialize;//? hangs thread
end;

procedure TxxmPageLoader.SignalNextJob;
begin
  //assert thread is waiting on FNextJobEvent
  InUse:=true;
  SetEvent(FNextJobEvent);
end;

initialization
  PageLoaderPool:=TxxmPageLoaderPool.Create;
finalization
  PageLoaderPool.Free;
end.
