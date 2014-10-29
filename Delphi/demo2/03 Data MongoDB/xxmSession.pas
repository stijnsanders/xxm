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

uses xxm, Classes, mongoWire;

type
  TXxmSession=class(TObject)
  private
    FSessionID:WideString;
    function GetDbCon: TMongoWire;
  public
    constructor Create(Context: IXxmContext);
    destructor Destroy; override;
    property SessionID:WideString read FSessionID;
    property DbCon: TMongoWire read GetDbCon;
  end;

procedure SetSession(Context: IXxmContext);
procedure AbandonSession;

threadvar
  Session: TXxmSession;
  
const
  xxmDemoCollection='test.xxmdemo1';  

implementation

uses Windows, SysUtils;

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

threadvar
  ThreadDbCon:TMongoWire;

{ TxxmSession }

constructor TXxmSession.Create(Context: IXxmContext);
begin
  inherited Create;
  FSessionID:=Context.SessionID;
  //TODO: initiate expiry
  //FDbCon:=TMongoWire.Create(...
end;

destructor TXxmSession.Destroy;
begin
  //FDbCon.Free;
  inherited;
end;

function TXxmSession.GetDbCon: TMongoWire;
begin
  if ThreadDbCon=nil then
   begin
    ThreadDBCon:=TMongoWire.Create;
    ThreadDBCon.Open;//TODO: server,port from setting
   end;
  Result:=ThreadDbCon;
end;

initialization
  SessionStore:=nil;
finalization
  FreeAndNil(SessionStore);

end.
