library [[ProjectName]];

{
  --- ATTENTION! ---

  This file is re-constructed when the xxm project source changes.
  Any changes to this file will be overwritten.
  If you require changes to this file that can not be defined
  in the xxm source file, set up an alternate prototype-file.

  Prototype-file used:
  "[[ProtoFile]]"
  $Rev: 522 $ $Date: 2024-12-05 23:37:31 +0100 (do, 05 dec 2024) $
}

[[ProjectSwitches]]
uses
	[[@Include]][[IncludeUnit]] in '..\[[IncludePath]][[IncludeUnit]].pas',
	[[@]][[@Fragment]][[FragmentUnit]] in '[[FragmentPath]][[FragmentUnit]].pas', {[[FragmentAddress]]}
	[[@]][[UsesClause]]
	xxmp2 in '..\xxmp2.pas';

{$E xxl}
[[ProjectHeader]]
begin
[[ProjectBody]]
end.
