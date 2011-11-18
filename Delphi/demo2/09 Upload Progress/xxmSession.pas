unit xxmSession;

interface

uses xxm, Contnrs;

type
  TXxmSession=class(TObject)
  private
    FSessionID:WideString;
  public
    //TODO: support multiple concurrent uploads per session with upload identifiers
	UploadProgressPosition,UploadProgressLength:integer;

    constructor Create(Context: IXxmContext);
    property SessionID:WideString read FSessionID;
  end;

procedure SetSession(Context: IXxmContext);
procedure AbandonSession;

threadvar
  Session: TXxmSession;

implementation

uses SysUtils;

//TODO: something better than plain objectlist
var
  SessionStore:TObjectList;

procedure SetSession(Context: IXxmContext);
var
  i:integer;
  sid:WideString;
begin
  if SessionStore=nil then SessionStore:=TObjectList.Create(true);
  sid:=Context.SessionID;
  i:=0;
  while (i<SessionStore.Count) and not(TXxmSession(SessionStore[i]).SessionID=sid) do inc(i);
  //TODO: session expiry!!!
  if (i<SessionStore.Count) then Session:=TXxmSession(SessionStore[i]) else
   begin
    //as a security measure, disallow  new sessions on a first POST request
    if Context.ContextString(csVerb)='POST' then
      raise Exception.Create('Access denied.');
    Session:=TXxmSession.Create(Context);
    SessionStore.Add(Session);
   end;
end;

//call AbandonSession to release session data (e.g. logoff)
procedure AbandonSession;
begin
  SessionStore.Remove(Session);
  Session:=nil;
end;

{ TxxmSession }

constructor TXxmSession.Create(Context: IXxmContext);
begin
  inherited Create;
  FSessionID:=Context.SessionID;
  //TODO: initiate expiry

  //default values
  UploadProgressPosition:=0;
  UploadProgressLength:=0;
end;

initialization
  SessionStore:=nil;
finalization
  FreeAndNil(SessionStore);

end.
