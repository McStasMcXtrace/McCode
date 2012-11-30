use strict;
use ExtUtils::MakeMaker;
use Config;

WriteMakefile(
	'NAME'			=> 'Math::Amoeba',
	'LINKTYPE'		=> '$(INST_PM)',
	'VERSION_FROM'	=> 'lib/Math/Amoeba.pm',
	'dist'  => { 
		SUFFIX 	 => 'gz', 
		COMPRESS => 'gzip -f' 
	},
	'clean'	=> {
		'FILES' => '*~'
	},
);
