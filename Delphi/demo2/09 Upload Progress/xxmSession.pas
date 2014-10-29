unit xxmSession;

interface

uses xxm, Classes;

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

var
  SessionStore:TStringList;

procedure SetSession(Context: IXxmContext);
var
  i:integer;
  sid:WideString;
begin
  if SessionStore=nil then
   begin
    SessionStore:=TStringList.Create;
    SessionStore.Sorted:=true;
    SessionStore.CaseSensitive:=true;
    //SessionStore.Duplicates:=dupError;
   end;
  sid:=Context.SessionID;
  i:=SessionStore.IndexOf(sid);
  //TODO: session expiry!!!
  if (i<>-1) then Session:=SessionStore.Objects[i] as TXxmSession else
   begin
    //as a security measure, disallow  new sessions on a first POST request
    if Context.ContextString(csVerb)='POST' then
      raise Exception.Create('Access denied.');
    Session:=TXxmSession.Create(Context);
    SessionStore.AddObject(sid,Session);
   end;
end;

//call AbandonSession to release session data (e.g. logoff)
procedure AbandonSession;
begin
  SessionStore.Delete(SessionStore.IndexOf(Session.SessionID));
  FreeAndNil(Session);
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
