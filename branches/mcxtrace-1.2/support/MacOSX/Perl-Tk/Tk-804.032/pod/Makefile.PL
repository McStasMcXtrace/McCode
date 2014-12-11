use ExtUtils::MakeMaker;

WriteMakefile(
	NAME	=> 'Tk::pod',
	# DIR	=> [ 'pTk' ],
	PMLIBDIRS => [ 'pod', 'Menu' ],
);

sub MY::postamble {
    '
html ::
	@echo "Sorry no HTML building yet"
'
}


