unit xxmSession;

interface

uses Contnrs;

type
  TXxmSession=class(TObject)
  private
    FSessionID:WideString;
  public
    constructor Create(SessionID:WideString);
    property SessionID:WideString read FSessionID;
  end;

procedure SetSession(SessionID: WideString);
procedure AbandonSession;

threadvar
  Session: TXxmSession;

implementation

uses SysUtils;

//TODO: something better than plain objectlist
var
  SessionStore:TObjectList;

procedure SetSession(SessionID: WideString);
var
  i:integer;
begin
  if SessionStore=nil then SessionStore:=TObjectList.Create(true);
  i:=0;
  while (i<SessionStore.Count) and not(TXxmSession(SessionStore[i]).SessionID=SessionID) do inc(i);
  //TODO: session expiry!!!
  if (i<SessionStore.Count) then Session:=TXxmSession(SessionStore[i]) else
   begin
    Session:=TXxmSession.Create(SessionID);
    SessionStore.Add(Session);
   end;
end;

procedure AbandonSession;
begin
  SessionStore.Remove(Session);
  Session:=nil;
end;

{ TxxmSession }

constructor TXxmSession.Create(SessionID: WideString);
begin
  inherited Create;
  FSessionID:=SessionID;
  //TODO: initiate expiry
end;

initialization
  SessionStore:=nil;
finalization
  FreeAndNil(SessionStore);

end.
