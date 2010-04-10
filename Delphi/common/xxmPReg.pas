unit xxmPReg;

interface

uses xxm;

type
  TXxmProjectEntry=class(TObject)
  protected
    FSignature:AnsiString;
    function GetModulePath:WideString; virtual; abstract;
    procedure SetSignature(const Value: AnsiString); virtual; abstract;
  public
    LastCheck:cardinal;
    procedure Release; virtual; abstract;
    property ModulePath:WideString read GetModulePath;
    property Signature:AnsiString read FSignature write SetSignature;
  end;

  TXxmAutoBuildHandler=function(pce:TXxmProjectEntry;
    Context:IXxmContext; ProjectName:WideString):boolean;

var
  XxmAutoBuildHandler:TXxmAutoBuildHandler;

implementation

end.
