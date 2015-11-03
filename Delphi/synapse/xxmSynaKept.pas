unit xxmSynaKept;

interface

uses Windows, Classes, xxmSynaMain;

type
  TXxmKeptConnections=class(TThread)
  private
    FLock:TRTLCriticalSection;
    FQueueEvent:THandle;
    FContexts: TList;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Queue(Context:TXxmSynaContext);
  end;

implementation

uses SysUtils, blcksock, xxmThreadPool, xxmCommonUtils;

{ TXxmKeptConnections }

constructor TXxmKeptConnections.Create;
begin
  inherited Create(false);
  Priority:=tpLower;//?
  FContexts:=TList.Create;
  InitializeCriticalSection(FLock);
  FQueueEvent:=CreateEvent(nil,true,false,nil);
end;

destructor TXxmKeptConnections.Destroy;
var
  i:integer;
  x:TXxmSynaContext;
begin
  Terminate;
  SetEvent(FQueueEvent);//wake up thread
  WaitFor;
  CloseHandle(FQueueEvent);
  DeleteCriticalSection(FLock);
  for i:=0 to FContexts.Count-1 do
   begin
    x:=TXxmSynaContext(FContexts[i]);
    SafeFree(TInterfacedObject(x));
   end;
  FContexts.Free;
  inherited;
end;

procedure TXxmKeptConnections.Queue(Context: TXxmSynaContext);
const
  GrowStep=$100;
begin
  //TODO: maximum lingering connections? or close oldest on queue newer?
  EnterCriticalSection(FLock);
  try
    FContexts.Add(Context);
    Context.KeptCount:=0;
    //protect from destruction by TXxmPageLoader.Execute:
    (Context as IUnknown)._AddRef;
    SetEvent(FQueueEvent);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TXxmKeptConnections.Execute;
var
  x:TXxmSynaContext;
  xx:TTCPBlockSocket;
  r,l:TList;
  i,j:integer;
begin
  inherited;
  r:=TList.Create;
  l:=TList.Create;
  xx:=TTCPBlockSocket.Create;
  try
    while not Terminated do
     begin
      EnterCriticalSection(FLock);
      try
        i:=FContexts.Count;
        while i<>0 do
         begin
          dec(i);
          x:=TXxmSynaContext(FContexts[i]);
          inc(x.KeptCount);
          //timed out? (see also t value below: 300x100ms~=30s)
          if x.KeptCount=300 then
           begin
            if x.Next=ntResumeSocket then
             begin
              x.Next:=ntResumeDisconnect;
              PageLoaderPool.Queue(x);
              (x as IUnknown)._Release;
             end
            else
              SafeFree(TInterfacedObject(x));
            FContexts.Delete(i);
           end;
         end;
        l.Clear;
        i:=0;
        while i<FContexts.Count do
         begin
          l.Add(TXxmSynaContext(FContexts[i]).Socket);
          inc(i);
         end;
      finally
        LeaveCriticalSection(FLock);
      end;
      if i=0 then //if FContexts.Count=0 then
       begin
        ResetEvent(FQueueEvent);
        WaitForSingleObject(FQueueEvent,INFINITE);
       end
      else
       begin
        xx.GroupCanRead(l,100,r);
        EnterCriticalSection(FLock);
        try
          i:=r.Count;
          while i<>0 do
           begin
            dec(i);
            j:=l.Count;
            while (j<>0) and (l[j-1]<>r[i]) do dec(j);
            if j<>0 then
             begin
              dec(j);
              x:=TXxmSynaContext(FContexts[j]);
              FContexts.Delete(j);
              PageLoaderPool.Queue(x);
              (x as IUnknown)._Release;
             end;
           end;
        finally
          LeaveCriticalSection(FLock);
        end;
       end;
     end;
  finally
    r.Free;
    l.Free;
    xx.Free;
  end;
end;

end.
