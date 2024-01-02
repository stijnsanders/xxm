unit xxmKeptCon;

interface

uses Windows, Classes, xxmContext;

type
  TXxmKeptConnections=class(TThread)
  private
    FLock:TRTLCriticalSection;
    FQueueEvent:THandle;
    FContexts:array of record
      Context:TXxmGeneralContext;
      MaxKeep:cardinal;
    end;
    FContextIndex,FContextSize:integer;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Queue(Context: TXxmGeneralContext;
      SetState: TXxmContextState);
  end;

implementation

uses SysUtils, xxmSock, xxmThreadPool, xxmCommonUtils, xxmHttpCtx;

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
    FreeAndNil(FContexts[i].Context);
  inherited;
end;

procedure TXxmKeptConnections.Queue(Context: TXxmGeneralContext;
  SetState: TXxmContextState);
const
  GrowStep=$100;
var
  i:integer;
begin
  //TODO: maximum lingering connections? or close oldest on queue newer?
  if Terminated then raise EXxmShuttingDown.Create(
    'Connection Queue denied: service is shutting down');
  EnterCriticalSection(FLock);
  try
    Context.State:=SetState;
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
    //timeout (see also t value below: 300x100ms~=30s)
    if SetState=ctSocketResume then
      FContexts[i].MaxKeep:=864000 //TODO: WebSocket ping/pong (then lower this)
    else
      FContexts[i].MaxKeep:=300;
    FContexts[i].Context:=Context;
    SetEvent(FQueueEvent);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TXxmKeptConnections.Execute;
var
  r,x:TFDSet;
  i,j,k:integer;
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
        k:=0;
        while (k<FContextIndex) and (r.fd_count<64) do
         begin
          j:=(i+k) mod FContextIndex;
          inc(k);
          if FContexts[j].Context<>nil then
           begin
            dec(FContexts[j].MaxKeep);
            if (FContexts[j].MaxKeep=0) or
              ((FContexts[j].Context as TXxmHttpContext).Socket=nil) then
              try
                if FContexts[j].Context.State=ctSocketResume then
                  PageLoaderPool.Queue(FContexts[j].Context,ctSocketDisconnect)
                else
                 begin
                  FContexts[j].Context.State:=ctSocketDisconnect;
                  FContexts[j].Context.Recycle;
                 end;
              finally
                FContexts[j].Context:=nil;
              end
            else
             begin
              h:=(FContexts[j].Context as TXxmHttpContext).Socket.Handle;
              r.fd_array[r.fd_count]:=h;
              inc(r.fd_count);
              x.fd_array[x.fd_count]:=h;
              inc(x.fd_count);
             end;
           end;
         end;
      finally
        LeaveCriticalSection(FLock);
      end;
      if FContextIndex=0 then i:=0 else i:=(i+k) mod FContextIndex;
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
                and ((FContexts[j].Context as TXxmHttpContext).Socket.Handle=h))
                do inc(j);
              if j<FContextIndex then
                try
                  if FContexts[j].Context.State=ctSocketResume then
                    PageLoaderPool.Queue(FContexts[j].Context,ctSocketDisconnect)
                  else
                   begin
                    FContexts[j].Context.State:=ctSocketDisconnect;
                    FContexts[j].Context.Recycle;
                   end;
                finally
                  FContexts[j].Context:=nil;
                end;
             end;
            //readables
            for k:=0 to r.fd_count-1 do
             begin
              j:=0;
              h:=r.fd_array[k];
              while (j<FContextIndex) and not((FContexts[j].Context<>nil)
                and ((FContexts[j].Context as TXxmHttpContext).Socket.Handle=h))
                do inc(j);
              if j<FContextIndex then
                try
                  PageLoaderPool.Queue(FContexts[j].Context,
                    FContexts[j].Context.State);
                finally
                  FContexts[j].Context:=nil;
                end;
              //else raise?
             end;
            //clean-up
            while (FContextIndex>0)
              and (FContexts[FContextIndex-1].Context=nil) do
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
