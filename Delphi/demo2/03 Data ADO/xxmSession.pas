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

uses xxm, Classes, ADODB_TLB;

type
  TXxmSession=class(TObject)
  private
    FID:WideString;
  public

    //TODO: full properties?
    Connection:Connection;

    constructor Create(const ID: WideString; Context: IXxmContext);
    destructor Destroy; override;

    //CSRF protection by posting session cookie value
    function FormProtect:WideString;
    procedure CheckProtect(Context: IXxmContext);

    property ID:WideString read FID;
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

{ TxxmSession }

constructor TXxmSession.Create(const ID: WideString; Context: IXxmContext);
var
  s:AnsiString;
begin
  inherited Create;
  FID:=ID;
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

initialization
  SessionStore:=nil;//see SetSession
finalization
  FreeAndNil(SessionStore);

end.
