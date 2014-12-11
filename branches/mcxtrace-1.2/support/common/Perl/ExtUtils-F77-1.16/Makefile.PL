
# Makefile.PL for ExtUtils::F77

use ExtUtils::MakeMaker;
# # See lib/ExtUtils/MakeMaker.pm for details of how to influence
# # the contents of the Makefile that is written.
WriteMakefile(
     'NAME'	=> 'ExtUtils::F77',
     'VERSION_FROM'	=> 'F77.pm',  
     'linkext'  => {LINKTYPE => ''},  # Only .pm files in this directory
    'dist'      => { COMPRESS=>"gzip", SUFFIX=>"gz" }
 );

