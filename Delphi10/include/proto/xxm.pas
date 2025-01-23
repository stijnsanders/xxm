unit [[FragmentUnit]];

{
  --- ATTENTION! ---

  This file is re-constructed when the xxm source file changes.
  Any changes to this file will be overwritten.
  If you require changes to this file that can not be defined
  in the xxm source file, set up an alternate prototype-file.

  Prototype-file used:
  "[[ProtoFile]]"
  $Rev: 529 $ $Date: 2025-01-23 23:55:26 +0100 (do, 23 jan 2025) $
}

interface

uses xxm2;

procedure build(Context: CxxmContext;
  const Values: array of Variant;
  const Objects: array of pointer); stdcall;

implementation

uses
  SysUtils,
  [[UsesClause]]
  xxmp2;
  
[[FragmentDefinitions]]
{ [[FragmentID]] }

procedure build(Context: CxxmContext;
  const Values: array of Variant;
  const Objects: array of pointer); stdcall;
[[FragmentHeader]]
begin
[[FragmentBody]]
end;

initialization
[[FragmentFooter]]
end.
