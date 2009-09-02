
use ExtUtils::MakeMaker;

WriteMakefile( 
        'VERSION_FROM' => "Simple.pm",
	'NAME'	  => 'Proc::Simple',
	'dist' => { COMPRESS=>"gzip", SUFFIX=>"gz" },
	);
