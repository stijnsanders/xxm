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

  IxxmParameterCollection=interface
    ['{78786D00-0000-000F-C000-00000000000F}']
    procedure AddParameter(Param: IUnknown);//IxxmParameter
  end;

  IxxmUploadProgressAgent=interface
    ['{78786D00-0000-0011-C000-000000000011}']
    procedure ReportProgress(const FieldName, FileName: AnsiString; Position: integer);
  end;

  IxxmUploadProgressService=interface
    ['{78786D00-0000-0012-C000-000000000012}']
    procedure AttachAgent(Agent: IxxmUploadProgressAgent; Flags, Step: integer);
  end;

const
  xxmUploadProgressAttach_PostData   = $00000001;
  xxmUploadProgressAttach_FileFields = $00000002;

implementation

end.
