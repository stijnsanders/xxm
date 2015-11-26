unit xxmKeptCon;

interface

uses Windows, Classes, xxmHttpMain;

type
  TXxmKeptConnections=class(TThread)
  private
    FLock:TRTLCriticalSection;
    FQueueEvent:THandle;
    FContexts:array of record
      Context:TXxmHttpContext;
      KeptCount:cardinal;
    end;
    FContextIndex,FContextSize:integer;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Queue(Context: TXxmHttpContext);
  end;

implementation

uses SysUtils, xxmSock, xxmThreadPool, xxmCommonUtils;

{ TXxmKeptConnections }

constructor TXxmKeptConnections.Create;
begin
  inherited Create(false);
  Priority:=tpLower;//?
  FContextIndex:=0;
  FContextSize:=0;
  InitializeCriticalSection(FLock);
  FQueueEvent:=CreateEvent(nil,true,false,nil);
end;

destructor TXxmKeptConnections.Destroy;
var
  i:integer;
begin
  Terminate;
  SetEvent(FQueueEvent);//wake up thread
  WaitFor;
  CloseHandle(FQueueEvent);
  DeleteCriticalSection(FLock);
  for i:=0 to FContextIndex-1 do
    if FContexts[i].Context<>nil then
      SafeFree(TInterfacedObject(FContexts[i].Context));
  inherited;
end;

procedure TXxmKeptConnections.Queue(Context: TXxmHttpContext);
const
  GrowStep=$100;
var
  i:integer;
begin
  //TODO: maximum lingering connections? or close oldest on queue newer?
  EnterCriticalSection(FLock);
  try
    i:=0;
    while (i<FContextIndex) and (FContexts[i].Context<>nil) do inc(i);
    if i=FContextIndex then
     begin
      if FContextIndex=FContextSize then
       begin
        inc(FContextSize,GrowStep);
        SetLength(FContexts,FContextSize);
       end;
      inc(FContextIndex);
     end;
    //protect from destruction by TXxmPageLoader.Execute:
    (Context as IUnknown)._AddRef;
    FContexts[i].KeptCount:=0;
    FContexts[i].Context:=Context;
    SetEvent(FQueueEvent);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TXxmKeptConnections.Execute;
var
  r,x:TFDSet;
  i,ii,j,k:integer;
  t:TTimeVal;
  h:THandle;
begin
  inherited;
  i:=0;
  while not Terminated do
    try
      EnterCriticalSection(FLock);
      try
        r.fd_count:=0;
        x.fd_count:=0;
        j:=0;
        while (j<FContextIndex) and (r.fd_count<64) do
         begin
          ii:=(i+j) mod FContextIndex;
          if FContexts[ii].Context<>nil then
           begin
            inc(FContexts[ii].KeptCount);
            //timed out? (see also t value below: 300x100ms~=30s)
            if FContexts[ii].KeptCount=300 then
              if FContexts[ii].Context.Next=ntResumeSocket then
               begin
                FContexts[ii].Context.Next:=ntResumeDisconnect;
                PageLoaderPool.Queue(FContexts[ii].Context);
                SafeClear(TInterfacedObject(FContexts[ii].Context));
               end
              else
                SafeFree(TInterfacedObject(FContexts[ii].Context))
            else
             begin
              h:=FContexts[ii].Context.Socket.Handle;
              r.fd_array[r.fd_count]:=h;
              inc(r.fd_count);
              x.fd_array[x.fd_count]:=h;
              inc(x.fd_count);
             end;
           end;
          inc(j);
         end;
      finally
        LeaveCriticalSection(FLock);
      end;
      if FContextIndex=0 then i:=0 else i:=(i+j) mod FContextIndex;
      if r.fd_count=0 then
       begin
        ResetEvent(FQueueEvent);
        WaitForSingleObject(FQueueEvent,INFINITE);
       end
      else
       begin
        t.tv_sec:=0;
        t.tv_usec:=100000;//microseconds
        if select(0,@r,nil,@x,@t)=SOCKET_ERROR then
         begin
          //TODO: raise? log? sleep?
         end
        else
         begin
          EnterCriticalSection(FLock);
          try
            //errors
            for k:=0 to x.fd_count-1 do
             begin
              j:=0;
              h:=x.fd_array[k];
              while (j<FContextIndex) and not((FContexts[j].Context<>nil)
                and (FContexts[j].Context.Socket.Handle=h)) do inc(j);
              if j<FContextIndex then
                if FContexts[j].Context.Next=ntResumeSocket then
                 begin
                  FContexts[j].Context.Next:=ntResumeDisconnect;
                  PageLoaderPool.Queue(FContexts[j].Context);
                  SafeClear(TInterfacedObject(FContexts[j].Context));
                 end
                else
                  SafeFree(TInterfacedObject(FContexts[j].Context));
             end;
            //readables
            for k:=0 to r.fd_count-1 do
             begin
              j:=0;
              h:=r.fd_array[k];
              while (j<FContextIndex) and not((FContexts[j].Context<>nil)
                and (FContexts[j].Context.Socket.Handle=h)) do inc(j);
              if j<FContextIndex then
               begin
                PageLoaderPool.Queue(FContexts[j].Context);
                SafeClear(TInterfacedObject(FContexts[j].Context));
               end;
              //else raise?
             end;
            //clean-up
            while (FContextIndex>0) and (FContexts[FContextIndex-1].Context=nil) do
              dec(FContextIndex);
          finally
            LeaveCriticalSection(FLock);
          end;
         end;
       end;
    except
      //silent (log?)
    end;
end;

end.
