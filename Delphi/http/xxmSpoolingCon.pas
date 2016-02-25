unit xxmSpoolingCon;

interface

uses Windows, Classes, xxmContext;

type
  TXxmSpoolingConnections=class(TThread)
  private
    FLock:TRTLCriticalSection;
    FAddEvent:THandle;
    FContexts:array of record
      Context:TXxmGeneralContext;
      KeptCount:cardinal;
      DataLeft:int64;
      Buffer:TStream;
      BufferFreeWhenDone:boolean;
      WasState:TXxmContextState;
    end;
    FContextIndex,FContextSize:integer;
    procedure DropContext(i:integer);
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Context:TXxmGeneralContext;
      Buffer:TStream;FreeWhenDone:boolean);
  end;


implementation

uses SysUtils, xxmSock, xxmThreadPool, xxmCommonUtils, xxmHttpMain;

{ TXxmSpoolingConnections }

constructor TXxmSpoolingConnections.Create;
begin
  inherited Create(false);
  Priority:=tpLower;//?
  FContextIndex:=0;
  FContextSize:=0;
  InitializeCriticalSection(FLock);
  FAddEvent:=CreateEvent(nil,true,false,nil);
end;

destructor TXxmSpoolingConnections.Destroy;
var
  i:integer;
begin
  Terminate;
  SetEvent(FAddEvent);//wake up thread
  WaitFor;
  CloseHandle(FAddEvent);
  DeleteCriticalSection(FLock);
  for i:=0 to FContextIndex-1 do DropContext(i);
  inherited;
end;

procedure TXxmSpoolingConnections.Add(Context:TXxmGeneralContext;
  Buffer:TStream;FreeWhenDone:boolean);
const
  GrowStep=$100;
var
  i:integer;
  s:TXxmContextState;
begin
  EnterCriticalSection(FLock);
  try
    s:=Context.State;
    Context.State:=ctSpooling;
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
    FContexts[i].Context:=Context;
    FContexts[i].KeptCount:=0;
    FContexts[i].DataLeft:=Buffer.Position;
    FContexts[i].Buffer:=Buffer;
    FContexts[i].BufferFreeWhenDone:=FreeWhenDone;
    FContexts[i].WasState:=s;
    Buffer.Position:=0;
    SetEvent(FAddEvent);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TXxmSpoolingConnections.DropContext(i:integer);
begin
  try
    FContexts[i].Context.Recycle;
    if FContexts[i].BufferFreeWhenDone then
      FreeAndNil(FContexts[i].Buffer)
    else
      BufferStore.AddBuffer(TMemoryStream(FContexts[i].Buffer));
  finally
    FContexts[i].Context:=nil;
    FContexts[i].Buffer:=nil;
  end;
end;

procedure TXxmSpoolingConnections.Execute;
const
  dSize=$10000;
var
  d:array[0..dSize-1] of byte;
  w,x:TFDSet;
  i,j,k,l:integer;
  t:TTimeVal;
  h:THandle;
begin
  inherited;
  i:=0;
  while not Terminated do
    try
      EnterCriticalSection(FLock);
      try
        w.fd_count:=0;
        x.fd_count:=0;
        k:=0;
        while (k<FContextIndex) and (w.fd_count<64) do
         begin
          j:=(i+k) mod FContextIndex;
          if FContexts[j].Context<>nil then
           begin
            inc(FContexts[j].KeptCount);
            //timed out? (see also t value below: 300x100ms~=30s)
            if (FContexts[j].KeptCount=300) or
              ((FContexts[j].Context as TXxmHttpContext).Socket=nil) then
              DropContext(j)
            else
             begin
              h:=(FContexts[j].Context as TXxmHttpContext).Socket.Handle;
              w.fd_array[w.fd_count]:=h;
              inc(w.fd_count);
              x.fd_array[x.fd_count]:=h;
              inc(x.fd_count);
             end;
           end;
          inc(k);
         end;
      finally
        LeaveCriticalSection(FLock);
      end;
      if FContextIndex=0 then i:=0 else i:=(i+k) mod FContextIndex;
      if w.fd_count=0 then
       begin
        ResetEvent(FAddEvent);
        WaitForSingleObject(FAddEvent,INFINITE);
       end
      else
       begin
        t.tv_sec:=0;
        t.tv_usec:=100000;//microseconds
        if select(0,nil,@w,@x,@t)=SOCKET_ERROR then
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
              if j<FContextIndex then DropContext(j); //else raise?
             end;
            //writables
            for k:=0 to w.fd_count-1 do
             begin
              j:=0;
              h:=w.fd_array[k];
              while (j<FContextIndex) and not((FContexts[j].Context<>nil)
                and ((FContexts[j].Context as TXxmHttpContext).Socket.Handle=h))
                do inc(j);
              if j<FContextIndex then
               begin
                //forward a chunk
                if FContexts[j].DataLeft>dSize then l:=dSize
                  else l:=FContexts[j].DataLeft;
                if l<>0 then l:=FContexts[j].Buffer.Read(d[0],l);
                if l<>0 then
                  try
                    if (FContexts[j].Context as TXxmHttpContext).Socket.
                      SendBuf(d[0],l)<>l then l:=0;
                  except
                    on ETcpSocketError do l:=0;
                  end;
                //done?
                dec(FContexts[j].DataLeft,l);
                if (l=0) or (FContexts[j].DataLeft=0) then
                  DropContext(j)
                else
                  FContexts[j].KeptCount:=0;
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

