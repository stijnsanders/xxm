unit WikiEngine_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// PASTLWTR : 1.2
// File generated on 23/05/2008 22:19:28 from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\Data\2007\WikiEngine\Delphi\WikiEngine\WikiEngine.tlb (1)
// LIBID: {F45E4446-FC7B-4E6E-8ACB-3C68A3CFA2A3}
// LCID: 0
// Helpfile: 
// HelpString: WikiEngine Library
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINDOWS\system32\stdole2.tlb)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
interface

uses Windows, ActiveX, Classes, Graphics, StdVCL, Variants;
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  WikiEngineMajorVersion = 1;
  WikiEngineMinorVersion = 0;

  LIBID_WikiEngine: TGUID = '{F45E4446-FC7B-4E6E-8ACB-3C68A3CFA2A3}';

  IID_IEngine: TGUID = '{592E287F-4FE9-4DD3-A25B-55C3F6E35F65}';
  CLASS_Engine: TGUID = '{A20016A7-685B-4554-A8B6-7A06E71D22FB}';
  IID_IWikiPageCheck: TGUID = '{49011DF9-7627-43D2-A249-51815DA96342}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum Setting_TableName
type
  Setting_TableName = TOleEnum;
const
  tnPage = $00000000;
  tnPageName = $00000001;
  tnPageData = $00000002;
  tnPageCreated = $00000003;
  tnPageModified = $00000004;
  tnLink = $00000005;
  tnLinkFromPage = $00000006;
  tnLinkToPage = $00000007;
  tnLock = $00000008;
  tnLockName = $00000009;
  tnLockInfo = $0000000A;
  tnLockSince = $0000000B;
  tnHistory = $0000000C;
  tnHistoryName = $0000000D;
  tnHistoryType = $0000000E;
  tnHistoryAuthor = $0000000F;
  tnHistoryData = $00000010;
  tnHistoryInfo = $00000011;
  tnHistoryCreated = $00000012;

// Constants for enum History_EntryType
type
  History_EntryType = TOleEnum;
const
  htNewPage = $00000001;
  htUpdate = $00000002;
  htDeletePage = $00000003;
  ht_Unknown = $000000FF;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IEngine = interface;
  IEngineDisp = dispinterface;
  IWikiPageCheck = interface;
  IWikiPageCheckDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  Engine = IEngine;


// *********************************************************************//
// Interface: IEngine
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {592E287F-4FE9-4DD3-A25B-55C3F6E35F65}
// *********************************************************************//
  IEngine = interface(IDispatch)
    ['{592E287F-4FE9-4DD3-A25B-55C3F6E35F65}']
    function Get_ConnectionString: WideString; safecall;
    procedure Set_ConnectionString(const Value: WideString); safecall;
    function Get_TableName(Name: Setting_TableName): WideString; safecall;
    procedure Set_TableName(Name: Setting_TableName; const Value: WideString); safecall;
    function Get___DBConnection: IUnknown; safecall;
    procedure Set___DBConnection(const Value: IUnknown); safecall;
    function Get_DBUserName: WideString; safecall;
    procedure Set_DBUserName(const Value: WideString); safecall;
    function Get_DBPassword: WideString; safecall;
    procedure Set_DBPassword(const Value: WideString); safecall;
    function Get_PageLockTime: Integer; safecall;
    procedure Set_PageLockTime(Value: Integer); safecall;
    function Render(const Data: WideString; const CurrentGroup: WideString): WideString; safecall;
    function Get_WikiParseXML: WideString; safecall;
    procedure Set_WikiParseXML(const Value: WideString); safecall;
    function Get_WikiParseXMLLoadTime: Integer; safecall;
    function Get_Author: WideString; safecall;
    procedure Set_Author(const Value: WideString); safecall;
    function Get_LastCallTime: Integer; safecall;
    function Get_SessionInfo: WideString; safecall;
    procedure Set_SessionInfo(const Value: WideString); safecall;
    function Get_Groups: WordBool; safecall;
    procedure Set_Groups(Value: WordBool); safecall;
    function GetPage(const Name: WideString; Rendered: WordBool): WideString; safecall;
    procedure SetPage(const Name: WideString; const Data: WideString); safecall;
    function CheckLock(const Name: WideString): OleVariant; safecall;
    function CheckPage(var Name: WideString; const CurrentGroup: WideString): WordBool; safecall;
    function Get_GroupDelim: WideString; safecall;
    procedure Set_GroupDelim(const Value: WideString); safecall;
    function GetModification(out Subject: WideString; out Value: WideString): WordBool; safecall;
    function Get_ModificationCount: Integer; safecall;
    procedure LockPage(const Name: WideString); safecall;
    function GetGroupByName(var Name: WideString; const DefaultGroup: WideString): WideString; safecall;
    function Get_CorrectedName: WideString; safecall;
    function Get_ModificationSubject: WideString; safecall;
    function Get_ModificationValue: WideString; safecall;
    function Get_Templates: WordBool; safecall;
    procedure Set_Templates(Value: WordBool); safecall;
    function Get_TemplateSuffix: WideString; safecall;
    procedure Set_TemplateSuffix(const Value: WideString); safecall;
    procedure UnlockPage(const Name: WideString); safecall;
    function Get_WikiPageCheck: IWikiPageCheck; safecall;
    procedure Set_WikiPageCheck(const Value: IWikiPageCheck); safecall;
    property ConnectionString: WideString read Get_ConnectionString write Set_ConnectionString;
    property TableName[Name: Setting_TableName]: WideString read Get_TableName write Set_TableName;
    property __DBConnection: IUnknown read Get___DBConnection write Set___DBConnection;
    property DBUserName: WideString read Get_DBUserName write Set_DBUserName;
    property DBPassword: WideString read Get_DBPassword write Set_DBPassword;
    property PageLockTime: Integer read Get_PageLockTime write Set_PageLockTime;
    property WikiParseXML: WideString read Get_WikiParseXML write Set_WikiParseXML;
    property WikiParseXMLLoadTime: Integer read Get_WikiParseXMLLoadTime;
    property Author: WideString read Get_Author write Set_Author;
    property LastCallTime: Integer read Get_LastCallTime;
    property SessionInfo: WideString read Get_SessionInfo write Set_SessionInfo;
    property Groups: WordBool read Get_Groups write Set_Groups;
    property GroupDelim: WideString read Get_GroupDelim write Set_GroupDelim;
    property ModificationCount: Integer read Get_ModificationCount;
    property CorrectedName: WideString read Get_CorrectedName;
    property ModificationSubject: WideString read Get_ModificationSubject;
    property ModificationValue: WideString read Get_ModificationValue;
    property Templates: WordBool read Get_Templates write Set_Templates;
    property TemplateSuffix: WideString read Get_TemplateSuffix write Set_TemplateSuffix;
    property WikiPageCheck: IWikiPageCheck read Get_WikiPageCheck write Set_WikiPageCheck;
  end;

// *********************************************************************//
// DispIntf:  IEngineDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {592E287F-4FE9-4DD3-A25B-55C3F6E35F65}
// *********************************************************************//
  IEngineDisp = dispinterface
    ['{592E287F-4FE9-4DD3-A25B-55C3F6E35F65}']
    property ConnectionString: WideString dispid 1;
    property TableName[Name: Setting_TableName]: WideString dispid 3;
    property __DBConnection: IUnknown dispid 4;
    property DBUserName: WideString dispid 5;
    property DBPassword: WideString dispid 6;
    property PageLockTime: Integer dispid 7;
    function Render(const Data: WideString; const CurrentGroup: WideString): WideString; dispid 9;
    property WikiParseXML: WideString dispid 8;
    property WikiParseXMLLoadTime: Integer readonly dispid 10;
    property Author: WideString dispid 11;
    property LastCallTime: Integer readonly dispid 12;
    property SessionInfo: WideString dispid 13;
    property Groups: WordBool dispid 14;
    function GetPage(const Name: WideString; Rendered: WordBool): WideString; dispid 17;
    procedure SetPage(const Name: WideString; const Data: WideString); dispid 18;
    function CheckLock(const Name: WideString): OleVariant; dispid 19;
    function CheckPage(var Name: WideString; const CurrentGroup: WideString): WordBool; dispid 21;
    property GroupDelim: WideString dispid 22;
    function GetModification(out Subject: WideString; out Value: WideString): WordBool; dispid 2;
    property ModificationCount: Integer readonly dispid 15;
    procedure LockPage(const Name: WideString); dispid 201;
    function GetGroupByName(var Name: WideString; const DefaultGroup: WideString): WideString; dispid 16;
    property CorrectedName: WideString readonly dispid 20;
    property ModificationSubject: WideString readonly dispid 23;
    property ModificationValue: WideString readonly dispid 24;
    property Templates: WordBool dispid 25;
    property TemplateSuffix: WideString dispid 26;
    procedure UnlockPage(const Name: WideString); dispid 27;
    property WikiPageCheck: IWikiPageCheck dispid 28;
  end;

// *********************************************************************//
// Interface: IWikiPageCheck
// Flags:     (320) Dual OleAutomation
// GUID:      {49011DF9-7627-43D2-A249-51815DA96342}
// *********************************************************************//
  IWikiPageCheck = interface(IUnknown)
    ['{49011DF9-7627-43D2-A249-51815DA96342}']
    function CheckPage(var Name: WideString; const CurrentGroup: WideString): WordBool; safecall;
  end;

// *********************************************************************//
// DispIntf:  IWikiPageCheckDisp
// Flags:     (320) Dual OleAutomation
// GUID:      {49011DF9-7627-43D2-A249-51815DA96342}
// *********************************************************************//
  IWikiPageCheckDisp = dispinterface
    ['{49011DF9-7627-43D2-A249-51815DA96342}']
    function CheckPage(var Name: WideString; const CurrentGroup: WideString): WordBool; dispid 1;
  end;

// *********************************************************************//
// The Class CoEngine provides a Create and CreateRemote method to          
// create instances of the default interface IEngine exposed by              
// the CoClass Engine. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoEngine = class
    class function Create: IEngine;
    class function CreateRemote(const MachineName: string): IEngine;
  end;

implementation

uses ComObj;

class function CoEngine.Create: IEngine;
begin
  Result := CreateComObject(CLASS_Engine) as IEngine;
end;

class function CoEngine.CreateRemote(const MachineName: string): IEngine;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_Engine) as IEngine;
end;

end.

