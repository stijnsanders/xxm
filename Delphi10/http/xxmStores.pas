unit xxmStores;

interface

uses Windows, SysUtils, Classes, xxmContext, xxmThreadPool;

type
  TXxmSpoolingConnections=class(TThread)
  private
    FLock:TRTLCriticalSection;
    FAddEvent:THandle;
    FContexts:array of record
      Context:TxxmContext;
      KeptCount:cardinal;
      DataLeft:int64;
      Buffer:TStream;
      BufferFreeWhenDone,Chunked:boolean;
    end;
    FContextIndex,FContextSize:integer;
    procedure DropContext(i:integer);
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Context: TxxmContext;
      Buffer: TStream; FreeWhenDone:boolean);
  end;

  TXxmKeptConnections=class(TThread)
  private
    FLock:TRTLCriticalSection;
    FQueueEvent:THandle;
    FContexts:array of record
      Context:TxxmContext;
      Resume:TxxmContextEvent;
      MaxKeep,Reserved:cardinal;
    end;
    FContextIndex,FContextSize:integer;
    procedure DropContext(i:integer);
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Queue(Context: TxxmContext; Resume: TxxmContextEvent;
      MaxKeep: cardinal);
  end;

  EXxmShuttingDown=class(Exception);

var
  SpoolingConnections:TXxmSpoolingConnections;
  KeptConnections:TXxmKeptConnections;

implementation

uses xxmSock, WinSock2;

type
  TFDSet = record
    fd_count: cardinal;
    fd_array: array[0..63] of THandle;
  end;
  PFDSet = ^TFDSet;

  TTimeVal = record
    tv_sec: cardinal;
    tv_usec: cardinal;
  end;
  PTimeVal = ^TTimeVal;

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
  for i:=0 to FContextIndex-1 do
    try
      DropContext(i);
    except
      //ignore
    end;
  inherited;
end;

procedure TXxmSpoolingConnections.Add(Context:TxxmContext;
  Buffer:TStream;FreeWhenDone:boolean);
const
  GrowStep=$100;
var
  i:integer;
begin
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
    FContexts[i].Context:=Context;
    FContexts[i].KeptCount:=0;
    FContexts[i].DataLeft:=Buffer.Position;
    FContexts[i].Buffer:=Buffer;
    FContexts[i].BufferFreeWhenDone:=FreeWhenDone;
    FContexts[i].Chunked:=Context.Chunked;
    Buffer.Position:=0;
    SetEvent(FAddEvent);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TXxmSpoolingConnections.DropContext(i:integer);
var
  c:TxxmContext;
begin
  try
    c:=FContexts[i].Context;
    if c.Socket<>nil then
      c.Socket.Disconnect;
    if FContexts[i].BufferFreeWhenDone then
      FreeAndNil(FContexts[i].Buffer)
    else
      BufferStore.AddBuffer(TMemoryStream(FContexts[i].Buffer));
    ContextPool.Recycle(c);
  finally
    FContexts[i].Context:=nil;
    FContexts[i].Buffer:=nil;
  end;
end;

procedure TXxmSpoolingConnections.Execute;
const
  dSize=$10000;
  Chunk0:array[0..4] of AnsiChar='0'#13#10#13#10;
  hex:array[0..15] of AnsiChar='0123456789ABCDEF';
var
  d:array[0..dSize-1] of byte;
  w,x:TFDSet;
  i,j,k,l,i1,k1,l1:integer;
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
               (FContexts[j].Context.Socket=nil) then
              DropContext(j)
            else
             begin
              h:=FContexts[j].Context.Socket.Handle;
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
        if WinSock2.select(0,nil,@w,@x,@t)=SOCKET_ERROR then
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
                and (FContexts[j].Context.Socket<>nil)
                and (FContexts[j].Context.Socket.Handle=h))
                do inc(j);
              if j<FContextIndex then DropContext(j); //else raise?
             end;
            //writables
            for k:=0 to w.fd_count-1 do
             begin
              j:=0;
              h:=w.fd_array[k];
              while (j<FContextIndex) and not((FContexts[j].Context<>nil)
                and (FContexts[j].Context.Socket<>nil)
                and (FContexts[j].Context.Socket.Handle=h))
                do inc(j);
              if j<FContextIndex then
               begin
                //forward a chunk
                if FContexts[j].Chunked then
                 begin
                  l:=dSize-12;
                  if FContexts[j].DataLeft<l then l:=FContexts[j].DataLeft;
                  if l<>0 then l:=FContexts[j].Buffer.Read(d[10],l);
                  if l<>0 then
                   begin
                    d[8]:=13;//CR
                    d[9]:=10;//LR
                    i1:=8;
                    k1:=l;
                    repeat
                      dec(i1);
                      d[i1]:=byte(hex[k1 and $F]);
                      k1:=k1 shr 4;
                    until k1=0;
                    d[l+10]:=13;//CR
                    d[l+11]:=10;//LF
                    l1:=l+12-i1;
                    try
                      if (FContexts[j].Context.Socket=nil)
                        or (FContexts[j].Context.Socket.SendBuf(d[i1],l1)<>l1)
                        then l:=0;
                    except
                      on ETcpSocketError do l:=0;
                    end;
                   end;
                 end
                else
                 begin
                  l:=dSize;
                  if FContexts[j].DataLeft<dSize then l:=FContexts[j].DataLeft;
                  if l<>0 then l:=FContexts[j].Buffer.Read(d[0],l);
                  if l<>0 then
                    try
                      if (FContexts[j].Context.Socket=nil)
                        or (FContexts[j].Context.Socket.SendBuf(d[0],l)<>l)
                        then l:=0;
                    except
                      on ETcpSocketError do l:=0;
                    end;
                 end;
                //done?
                dec(FContexts[j].DataLeft,l);
                try
                  if FContexts[j].Chunked and (FContexts[j].DataLeft=0)
                    and (FContexts[j].Context.Socket<>nil) then
                    FContexts[j].Context.Socket.SendBuf(Chunk0[0],5);
                except
                  //silent
                end;
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
    try
      FreeAndNil(FContexts[i].Context);
    except
      //silent
    end;
  inherited;
end;

procedure TXxmKeptConnections.Queue(Context: TxxmContext;
  Resume: TxxmContextEvent; MaxKeep: cardinal);
var
  i:integer;
begin
  //TODO: maximum lingering connections? or close oldest on queue newer?
  if Terminated then raise EXxmShuttingDown.Create(
    'Connection Queue denied: service is shutting down');
  EnterCriticalSection(FLock);
  try
    i:=0;
    while (i<FContextIndex) and (FContexts[i].Context<>nil) do inc(i);
    if i=FContextIndex then
     begin
      if FContextIndex=FContextSize then
       begin
        inc(FContextSize,$100);//grow step
        SetLength(FContexts,FContextSize);
       end;
      inc(FContextIndex);
     end;
    //timeout (see also t value below: 300x100ms~=30s)
    FContexts[i].Context:=Context;
    FContexts[i].Resume:=Resume;
    FContexts[i].MaxKeep:=MaxKeep;
    FContexts[i].Reserved:=0;
    SetEvent(FQueueEvent);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TXxmKeptConnections.DropContext(i: integer);
var
  c:TxxmContext;
begin
  try
    c:=FContexts[i].Context;
    if c.Socket<>nil then
      c.Socket.Disconnect;
    ContextPool.Recycle(c);
  finally
    FContexts[i].Context:=nil;
    FContexts[i].Resume:=nil;
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
            if (FContexts[j].MaxKeep=0)
              or (FContexts[j].Context.Socket=nil)
              then
                DropContext(j)
            else
             begin
              h:=FContexts[j].Context.Socket.Handle;
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
        if winSock2.select(0,@r,nil,@x,@t)=SOCKET_ERROR then
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
                and (FContexts[j].Context.Socket<>nil)
                and (FContexts[j].Context.Socket.Handle=h))
                do inc(j);
              if j<FContextIndex then
                DropContext(j);
             end;
            //readables
            for k:=0 to r.fd_count-1 do
             begin
              j:=0;
              h:=r.fd_array[k];
              while (j<FContextIndex) and not((FContexts[j].Context<>nil)
                and (FContexts[j].Context.Socket<>nil)
                and (FContexts[j].Context.Socket.Handle=h))
                do inc(j);
              if j<FContextIndex then
                try
                  PageLoaderPool.Queue(FContexts[j].Resume);
                finally
                  FContexts[j].Context:=nil;
                  FContexts[j].Resume:=nil;
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

initialization
  SpoolingConnections:=TXxmSpoolingConnections.Create;
  KeptConnections:=TXxmKeptConnections.Create;
finalization
  SpoolingConnections.Free;
  KeptConnections.Free;
end.
