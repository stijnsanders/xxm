unit xxmKeptCon;

interface

uses Windows, Classes, xxmHttpMain;

type
  TXxmKeptConnections=class(TThread)
  private
    FLock:TRTLCriticalSection;
    FQueueEvent:THandle;
    FContexts:array of TXxmHttpContext;
    FContextIndex,FContextSize:integer;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Queue(Context:TXxmHttpContext);
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
  FQueueEvent:=CreateEventA(nil,true,false,
    PAnsiChar('xxmHttp:KeepConnection:'+IntToHex(GetCurrentThreadId,8)));
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
    if FContexts[i]<>nil then SafeFree(TInterfacedObject(FContexts[i]));
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
    while (i<FContextIndex) and (FContexts[i]<>nil) do inc(i);
    if i=FContextIndex then
     begin
      if FContextIndex=FContextSize then
       begin
        inc(FContextSize,GrowStep);
        SetLength(FContexts,FContextSize);
       end;
      inc(FContextIndex);
     end;
    FContexts[i]:=Context;
    Context.KeptCount:=0;
    //protect from destruction by TXxmPageLoader.Execute:
    Context.WasKept:=true;
    (Context as IUnknown)._AddRef;
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
   begin
    EnterCriticalSection(FLock);
    try
      r.fd_count:=0;
      x.fd_count:=0;
      j:=0;
      while (j<FContextIndex) and (r.fd_count<64) do
       begin
        ii:=(i+j) mod FContextIndex;
        if FContexts[ii]<>nil then
         begin
          inc(FContexts[ii].KeptCount);
          //timed out? (see also t value below: 300x100ms~=30s)
          if FContexts[ii].KeptCount=300 then
            SafeFree(TInterfacedObject(FContexts[ii]))
          else
           begin
            h:=FContexts[ii].Socket.Handle;
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
            while (j<FContextIndex) and not((FContexts[j]<>nil)
              and (FContexts[j].Socket.Handle=h)) do inc(j);
            if j<FContextIndex then
              SafeFree(TInterfacedObject(FContexts[j]));//else raise?
           end;
          //readables
          for k:=0 to r.fd_count-1 do
           begin
            j:=0;
            h:=r.fd_array[k];
            while (j<FContextIndex) and not((FContexts[j]<>nil)
              and (FContexts[j].Socket.Handle=h)) do inc(j);
            if j<FContextIndex then
             begin
              PageLoaderPool.Queue(FContexts[j]);
              FContexts[j]:=nil;
             end;
            //else raise?
           end;
          //clean-up
          while (FContextIndex>0) and (FContexts[FContextIndex-1]=nil) do
            dec(FContextIndex);
        finally
          LeaveCriticalSection(FLock);
        end;
       end;
     end;
   end;
end;

end.
