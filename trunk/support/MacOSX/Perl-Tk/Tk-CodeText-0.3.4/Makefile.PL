use strict;
use ExtUtils::MakeMaker;

my $pm = 'CodeText';
my $ld = '$(INST_LIBDIR)';

WriteMakefile(
	NAME => "Tk::$pm",
	PREREQ_PM => {
		'Tk' => '800.024',
	},
#	PMLIBDIRS => ['Tk'],
	PM => {
		"$pm.pm" => "$ld/$pm.pm",
		"$pm/None.pm" => "$ld/$pm/None.pm",
		"$pm/McStas.pm" => "$ld/$pm/McStas.pm",
		"$pm/Template.pm" => "$ld/$pm/Template.pm",
		"$pm/Xresources.pm" => "$ld/$pm/Xresources.pm",
		"$pm.pod" => "$ld/$pm.pod",
		"RulesEditor.pm" => "$ld/RulesEditor.pm",
	},
	VERSION_FROM => "$pm.pm",
	'dist'	=> {COMPRESS => 'gzip', SUFFIX => 'gz'},
	($] >= 5.005 ?
		(ABSTRACT => "a TextUndo widget with syntax highlight capabilities",
		AUTHOR	=> 'Hans Jeuken (haje@toneel.demon.nl)') : ()),
);
