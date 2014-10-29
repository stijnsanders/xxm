unit xxmSession;

{

Use a copy of this unit in your xxm project to enable session data.

Extend the TXxmSession class definition with extra data to store with the session.

Add this unit to the uses clause of the project source file (xxmp.pas) and add this line to the LoadPage function of the project object:

function TXxmSomeProject.LoadPage(Context: IXxmContext; Address: WideString): IXxmFragment;
begin
  inherited;
>>>  SetSession(Context);  <<<
  Result:=LoadFragment(Address);
end;

}

interface

uses xxm, Classes, ADODB_TLB;

type
  TXxmSession=class(TObject)
  private
    FSessionID:WideString;
  public

    //TODO: full properties?
    Connection:Connection;

    constructor Create(Context: IXxmContext);
    destructor Destroy; override;
    property SessionID:WideString read FSessionID;
  end;

procedure SetSession(Context: IXxmContext);
procedure AbandonSession;

threadvar
  Session: TXxmSession;

implementation

uses Windows, SysUtils, xxmData;

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
var
  s:AnsiString;
begin
  inherited Create;
  FSessionID:=Context.SessionID;
  //TODO: initiate expiry

  SetLength(s,1024);
  SetLength(s,GetModuleFileNameA(HInstance,PAnsiChar(s),1024));
  SetCurrentDir(ExtractFilePath(s));

  if QueryStore=nil then QueryStore:=TQueryStore.Create;

  //TODO: connection pool
  Connection:=CoConnection.Create;
  Connection.Open('File Name=demo.udl','','',0);
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

initialization
  SessionStore:=nil;
finalization
  FreeAndNil(SessionStore);

end.
