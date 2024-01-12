unit [[FragmentUnit]];

{
  --- ATTENTION! ---

  This file is re-constructed when the xxm source file changes.
  Any changes to this file will be overwritten.
  If you require changes to this file that can not be defined
  in the xxm source file, set up an alternate prototype-file.

  Prototype-file used:
  "[[ProtoFile]]"
  $Rev: 102 $ $Date: 2010-09-15 14:42:45 +0200 (wo, 15 sep 2010) $
}

interface

uses xxm;

type
  [[FragmentID]]=class(TXxmInclude)
  public
    procedure Build(const Context: IXxmContext; const Caller: IXxmFragment;
      const Values: array of OleVariant; const Objects: array of TObject); override;
  end;

implementation

uses 
  SysUtils, 
[[UsesClause]]
  xxmFReg;
  
[[FragmentDefinitions]]
{ [[FragmentID]] }

procedure [[FragmentID]].Build(const Context: IXxmContext; const Caller: IXxmFragment;
      const Values: array of OleVariant; const Objects: array of TObject);
[[FragmentHeader]]
var
  ExLogPrefix:string;
begin
  inherited;
  try
    ExLogPrefix:='ExLogPrefix:0';
[[FragmentBody]]
  except
    on e:Exception do
	 begin
	  e.Message:=e.Message+#13#10+ExLogPrefix;
	  raise;
	 end;
  end;
end;

initialization
  XxmFragmentRegistry.RegisterClass('[[FragmentAddress]]',[[FragmentID]]);
[[FragmentFooter]]

end.
