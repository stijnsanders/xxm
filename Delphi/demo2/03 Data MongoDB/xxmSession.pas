unit xxmSession;

{

Use a copy of this unit in your xxm project to enable session data.

Extend the TXxmSession class definition with extra data to store with the session.

Add this unit to the uses clause of the project source file (xxmp.pas) and add this line to the LoadPage function of the project object:

function TXxmSomeProject.LoadPage(Context: IXxmContext; const Address: WideString): IXxmFragment;
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
    FID:WideString;
    function GetDbCon: TMongoWire;
  public
    constructor Create(const ID: WideString; Context: IXxmContext);
    destructor Destroy; override;

    //CSRF protection by posting session cookie value
    function FormProtect:WideString;
    procedure CheckProtect(Context: IXxmContext);

    property ID:WideString read FID;
    property DbCon: TMongoWire read GetDbCon;
  end;

procedure SetSession(Context: IXxmContext);
procedure AbandonSession;

threadvar
  Session: TXxmSession;

const
  xxmDemoNameSpace='xxmdemo';
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
  sid:=Context.SessionID+
    '|'+Context.ContextString(csUserAgent);//TODO: hash
  //TODO: more ways to prevent session hijacking?
  i:=SessionStore.IndexOf(sid);
  //TODO: session expiry!!!
  if (i<>-1) then Session:=SessionStore.Objects[i] as TXxmSession else
   begin
    //as a security measure, disallow  new sessions on a first POST request
    if Context.ContextString(csVerb)='POST' then
      raise Exception.Create('Access denied.');
    Session:=TXxmSession.Create(sid,Context);
    SessionStore.AddObject(sid,Session);
   end;
end;

//call AbandonSession to release session data (e.g. logoff)
procedure AbandonSession;
begin
  SessionStore.Delete(SessionStore.IndexOf(Session.ID));
  FreeAndNil(Session);
end;

threadvar
  ThreadDbCon:TMongoWire;

{ TxxmSession }

constructor TXxmSession.Create(const ID: WideString; Context: IXxmContext);
begin
  inherited Create;
  FID:=ID;
  //TODO: initiate expiry
  //FDbCon:=TMongoWire.Create(...
end;

destructor TXxmSession.Destroy;
begin
  //FDbCon.Free;
  inherited;
end;

function TXxmSession.FormProtect:WideString;
begin
  Result:='<input type="hidden" name="XxmSessionID" value="'+HTMLEncode(FID)+'" />';
end;

procedure TXxmSession.CheckProtect(Context: IXxmContext);
var
  p:IXxmParameter;
  pp:IXxmParameterPost;
begin
  if Context.ContextString(csVerb)='POST' then
   begin
    p:=Context.Parameter['XxmSessionID'];
    if not((p.QueryInterface(IxxmParameterPost,pp)=S_OK) and (p.Value=FID)) then
      raise Exception.Create('Invalid POST source detected.');
   end
  else
    raise Exception.Create('xxmSession.CheckProtect only works on POST requests.');
end;

function TXxmSession.GetDbCon: TMongoWire;
begin
  if ThreadDbCon=nil then
   begin
    ThreadDBCon:=TMongoWire.Create(xxmDemoNameSpace);
    ThreadDBCon.Open;//TODO: server,port from setting
   end;
  Result:=ThreadDbCon;
end;

initialization
  SessionStore:=nil;//see SetSession
finalization
  FreeAndNil(SessionStore);

end.
