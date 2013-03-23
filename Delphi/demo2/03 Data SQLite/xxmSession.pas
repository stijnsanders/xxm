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

uses xxm, Contnrs, SQLiteData;

type
  TXxmSession=class(TObject)
  private
    FSessionID:WideString;
	function GetDbCon: TSQLiteConnection;
  public
    constructor Create(Context: IXxmContext);
    destructor Destroy; override;
    property SessionID:WideString read FSessionID;
	property DbCon: TSQLiteConnection read GetDbCon;
  end;

procedure SetSession(Context: IXxmContext);
procedure AbandonSession;

threadvar
  Session: TXxmSession;

implementation

uses Windows, SysUtils;

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

//threadvar
var //see SQLITE_CONFIG_SERIALIZED: one connection may be used over several threads! (otherwise will throw "database is locked" under load)
  ThreadDbCon:TSQLiteConnection;

{ TxxmSession }

constructor TXxmSession.Create(Context: IXxmContext);
begin
  inherited Create;
  FSessionID:=Context.SessionID;
  //TODO: initiate expiry
  //FDbCon:=TSQLiteConnection.Create(...'demo.db');
end;

destructor TXxmSession.Destroy;
begin
  //FDbCon.Free;
  inherited;
end;

function TXxmSession.GetDbCon: TSQLiteConnection;
var
  fn:string;
begin
  if ThreadDbCon=nil then 
   begin
    SetLength(fn,1024);
    SetLength(fn,GetModuleFileName(HInstance,PChar(fn),1024));
    ThreadDBCon:=TSQLiteConnection.Create(ExtractFilePath(fn)+'demo.db');//TODO from setting?
   end;
  Result:=ThreadDbCon;
end;

initialization
  SessionStore:=nil;
finalization
  FreeAndNil(SessionStore);

end.
