library [[ProjectName]];

{
  --- ATTENTION! ---

  This file is re-constructed when the xxm source file changes.
  Any changes to this file will be overwritten.
  If you require changes to this file that can not be defined
  in the xxm source file, set up an alternate prototype-file.

  Prototype-file used:
  "[[ProtoFile]]"
  $Rev: 198 $ $Date: 2008-01-21 22:52:37 +0100 (ma, 21 jan 2008) $
}

uses
	FRegRouting in '..\FRegRouting.pas',
	[[@Include]][[IncludeUnit]] in '..\[[IncludePath]][[IncludeUnit]].pas',
	[[@]][[@Fragment]][[FragmentUnit]] in '[[FragmentPath]][[FragmentUnit]].pas', {[[FragmentAddress]]}
	[[@]][[UsesClause]]
	xxmp in '..\xxmp.pas';

{$E xxl}
[[ProjectHeader]]
exports
	XxmProjectLoad;
[[ProjectBody]]
end.
