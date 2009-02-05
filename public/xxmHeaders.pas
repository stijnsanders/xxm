unit xxmHeaders;

interface

type
  IxxmDictionary=interface
    ['{78786D00-0000-000D-C000-00000000000D}']
    function GetItem(Name: OleVariant): WideString;
    procedure SetItem(Name: OleVariant; const Value: WideString);
    function GetName(Idx: integer): WideString;
    procedure SetName(Idx: integer; Value: WideString);
    function GetCount:integer;
    property Item[Name:OleVariant]:WideString read GetItem write SetItem; default;
    property Name[Idx:integer]:WideString read GetName write SetName;
    property Count:integer read GetCount;
  end;

  IxxmDictionaryEx=interface(IxxmDictionary)
    ['{78786D00-0000-000E-C000-00000000000E}']
    function Complex(Name:OleVariant;out Items:IxxmDictionary):WideString;
  end;

  IxxmHttpHeaders=interface
    ['{78786D00-0000-000C-C000-00000000000C}']
    function GetRequestHeaders:IxxmDictionaryEx;
    function GetResponseHeaders:IxxmDictionaryEx;
    property RequestHeaders:IxxmDictionaryEx read GetRequestHeaders;
    property ResponseHeaders:IxxmDictionaryEx read GetResponseHeaders;
  end;

implementation

end.
