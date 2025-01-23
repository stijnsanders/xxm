unit xxmSession;

{

Use a copy of this unit in your xxm project to enable session data.

Extend the TXxmSession class definition with extra data to store with the session.

Add this unit to the uses clause of the project source file (xxmp.pas) and add this line to the LoadPage function of the project object:

  SetSession(Context);

}

interface

uses xxm2, ADODB_TLB;

type
  TXxmSession=class(TObject)
  private
    FSessionID: UTF8String;

  public

    //TODO: full properties?
    Connection:Connection;

    constructor Create(const SessionID: UTF8String; Context: CxxmContext);
    destructor Destroy; override;

    //CSRF protection by posting session cookie value
    function FormProtect: UTF8string;
    procedure CheckProtect(Context: CXxmContext);

    property SessionID: UTF8String read FSessionID;
  end;

procedure SetSession(Context: CXxmContext);
procedure AbandonSession;

threadvar
  Session: TXxmSession;

implementation

uses Windows, SysUtils, ActiveX, xxmData;

var
  SessionStore: array of TXxmSession;
  SessionStoreIndex, SessionStoreSize: integer;

function UTF8Cmp(const aa, bb: UTF8String): NativeInt; inline;
var
  a,b:PUTF8Char;
begin
  a:=PUTF8Char(aa);
  b:=PUTF8Char(bb);
  if a=nil then
    if b=nil then
      Result:=0
    else
      Result:=-1
  else
    if b=nil then
      Result:=1
    else
     begin
      while (a^=b^) and (a^<>#0) and (b^<>#0) do
       begin
        inc(a);
        inc(b);
       end;
      if a^=b^ then
        Result:=0
      else
        if a^<b^ then
          Result:=-1
        else
          Result:=1;
     end;
end;

procedure FindSession(const SessionID: UTF8String; var i,a: integer); inline;
var
  b,c,m:integer;
begin
  //TODO: SessionStoreLock
  i:=-1;
  a:=0;
  b:=SessionStoreIndex-1;
  while a<=b do
   begin
    c:=(a+b) div 2;
    m:=UTF8Cmp(SessionID,SessionStore[c].SessionID);
    if m<0 then
      if b=c then dec(b) else b:=c
    else
    if m>0 then
      if a=c then inc(a) else a:=c
    else
     begin
      a:=c;
      b:=a-1;//end loop
      i:=c;
     end;
   end;
end;

procedure SetSession(Context: CXxmContext);
var
  i,a,b,c: integer;
  SessionID: UTF8String;
begin
  SessionID:=Context.SessionID
    //+'|'+Context.ContextString(csRemoteAddress)//?
    +'|'+Context.ContextString(csUserAgent)
    ;//TODO: hash
  for i:=1 to Length(SessionID) do
    if SessionID[i] in ['<','>','"'] then
      SessionID[i]:='_';
  //TODO: more ways to prevent session hijacking?
  FindSession(SessionID,i,a);
  if i=-1 then
   begin
    Session:=TxxmSession.Create(SessionID,Context);
    if SessionStoreIndex=SessionStoreSize then
     begin
      inc(SessionStoreSize,32);//grow step
      SetLength(SessionStore,SessionStoreSize);
     end;
    //re-order
    c:=SessionStoreIndex;
    while c>a do
     begin
      b:=c;
      dec(b);
      SessionStore[c]:=SessionStore[b];
      c:=b;
     end;
    SessionStore[a]:=Session;
    inc(SessionStoreIndex);
   end
  else
    Session:=SessionStore[i];
  //Context.Data:=Session;//?
  //TODO: session expiry!!!
end;

//call AbandonSession to release session data (e.g. logoff)
procedure AbandonSession;
var
  i,j:integer;
begin
  //FindSession(Session.SessionID,i,a);
  i:=0;
  while (i<SessionStoreIndex) and (SessionStore[i]<>Session) do inc(i);
  if i<SessionStoreIndex then
   begin
    while i<SessionStoreIndex do
     begin
      j:=i;
      inc(j);
      SessionStore[i]:=SessionStore[j];
      i:=j;
     end;
    dec(SessionStoreIndex);
   end;
  FreeAndNil(Session);
end;

{ TxxmSession }

constructor TXxmSession.Create(const SessionID: UTF8String; Context: CXxmContext);
var
  s:string;
  i:integer;
begin
  inherited Create;
  FSessionID:=SessionID;
  //TODO: initiate expiry

  SetLength(s,1024);
  i:=GetModuleFileName(HInstance,PChar(s),1024);
  while (i<>0) and (s[i]<>PathDelim) do dec(i);
  SetLength(s,i);
  
  if QueryStore=nil then QueryStore:=TQueryStore.Create(s);

  //TODO: connection pool
  CoInitialize(nil);
  Connection:=CoConnection.Create;
  Connection.Open('File Name='+s+'demo.udl','','',0);

end;

destructor TXxmSession.Destroy;
begin
  //TODO: connection pool
  try
    Connection:=nil;
  except
    //COM may have died, dirty fix
    pointer(Connection):=nil;
  end;
  inherited;
end;

function TXxmSession.FormProtect: UTF8String;
begin
  Result:='<input type="hidden" name="XxmSessionID" value="'+FSessionID+'" />';
end;

procedure TXxmSession.CheckProtect(Context: CXxmContext);
var
  p:CXxmParameter;
begin
  if Context.ContextString(csVerb)='POST' then
   begin
    p:=Context.Parameter['XxmSessionID'];
    if not((p.Origin='POST') and (p.Value=FSessionID)) then
      raise Exception.Create('Invalid POST source detected.');
   end
  else
    raise Exception.Create('xxmSession.CheckProtect only works on POST requests.');
end;

procedure SessionStore_CleanUp;
var
  i:integer;
begin
  for i:=0 to SessionStoreIndex-1 do
    try
      FreeAndNil(SessionStore[i]);
    except
      //silent
    end;
  SessionStoreIndex:=0;
  SessionStoreSize:=0;
  SetLength(SessionStore,0);
end;

initialization
  SessionStoreIndex:=0;
  SessionStoreSize:=0;
finalization
  SessionStore_CleanUp;

end.