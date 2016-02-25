unit xxmSynaKept;

interface

uses Windows, Classes, xxmContext;

type
  TXxmKeptConnections=class(TThread)
  private
    FLock:TRTLCriticalSection;
    FQueueEvent:THandle;
    FContexts:array of record
      Context:TXxmGeneralContext;
      KeptTC,MaxTC:cardinal;
    end;
    FContextIndex,FContextSize:integer;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Queue(Context:TXxmGeneralContext;
      SetState: TXxmContextState);
  end;

implementation

uses SysUtils, blcksock, xxmThreadPool, xxmCommonUtils, xxmSynaMain;

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
    if SetState=ctSocketResume then
      FContexts[i].MaxTC:=8640000 //TODO: WebSocket ping/pong (then lower this)
    else
      FContexts[i].MaxTC:=3000;
    FContexts[i].KeptTC:=GetTickCount;
    FContexts[i].Context:=Context;
    SetEvent(FQueueEvent);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TXxmKeptConnections.Execute;
var
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
      try
        EnterCriticalSection(FLock);
        try
          i:=0;
          while i<FContextIndex do
           begin
            if FContexts[i].Context<>nil then
             begin
              if cardinal(GetTickCount-FContexts[i].KeptTC)
                >FContexts[i].MaxTC then
                try
                  if FContexts[i].Context.State=ctSocketResume then
                    PageLoaderPool.Queue(FContexts[i].Context,ctSocketDisconnect)
                  else
                    FContexts[i].Context.Recycle;
                finally
                  FContexts[i].Context:=nil;
                end;
             end;
            inc(i);
           end;
          l.Clear;
          i:=0;
          while i<FContextIndex do
           begin
            if FContexts[i].Context<>nil then
              l.Add((FContexts[i].Context as TXxmSynaContext).Socket);
            inc(i);
           end;
        finally
          LeaveCriticalSection(FLock);
        end;
        if l.Count=0 then //if FContexts.Count=0 then
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
                try
                  PageLoaderPool.Queue(FContexts[j].Context,
                    FContexts[j].Context.State);
                finally
                  FContexts[j].Context:=nil;
                end;
               end;
             end;
          finally
            LeaveCriticalSection(FLock);
          end;
         end;
      except
        //silent
      end;
  finally
    r.Free;
    l.Free;
    xx.Free;
  end;
end;

end.
