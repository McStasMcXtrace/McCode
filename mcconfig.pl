#!/usr/bin/perl

use Config;

if ($Config{'osname'} eq "MSWin32") {

    print STDOUT "\nConfiguring McStas plotter on Win32\n\n";
    print STDOUT "Looking for matlab:\n";

    my $matlab=system('which.exe matlab.exe');

    print STDOUT "\nLooking for scilab:\n";
    my $scilab=system('which.exe runscilab.exe');
    
    # On Win32, matlab is preferred before scilab, which
    # lack certian functionality...
    if ($matlab == 0) {
	print STDOUT "\n\nMatlab found, configuring McStas\n";
	configure_mcstas("1");
    } elsif ($scilab ==0) {
	print STDOUT "\n\nScilab found, configuring McStas\n";
	configure_mcstas("3");
    } else {
	die "\n\nSorry, neither Matlab or Scilab found, no plotting available.\n";
    }
} else {
    print STDOUT "This perl script is only ment for use on Win32!\n";
}

sub configure_mcstas{
    my ($plotter) = @_;
    print STDERR "The plotter is $plotter";
    my $fid = open(READ,"<lib/tools/perl/mcstas_config.perl") || die "Could not open config file\n";
    my $fid2 = open(WRITE,">lib/tools/perl/mcstas_config.perl.new") || die "Could not write to new config file\n";
    while (<READ>) {
	if (/\w*PLOTTER \=\w*/) {
	    print WRITE "     PLOTTER => $plotter,\n";
	} else {
	    print WRITE;
	}
    }
    close(WRITE);
    # It should now be ok to overwrite the config file:
    system("copy lib\\tools\\perl\\mcstas_config.perl.new lib\\tools\\perl\\mcstas_config.perl");
}

