#!/usr/bin/perl

use Config;

our $scilab;
our $tcltk; 
our $matlab;
our $cc;
our $mpirun;
our $mpicc;
our $terminal;
our $plotter;

if ($Config{'osname'} eq "MSWin32") {
    my $failed;

    print STDOUT "\nConfiguring McStas on Win32\n\n";
    
    print STDOUT "Checking for C compiler: ";
    $failed=system('support\Win32\which.exe cc.exe');
    if ($failed) { 
      $failed=system('support\Win32\which.exe gcc.exe'); 
      $cc = (not $failed) ? "gcc.exe" : "no";
    } else { $cc = "cc.exe"; }
    print STDOUT "$cc\n";
    
    print STDOUT "Checking for Matlab: ";

    $failed=system('support\Win32\which.exe matlab.exe');
    $matlab = (not $failed) ? "matlab.exe" : "no";
    print STDOUT "$matlab\n";

    print STDOUT "Checking for Scilab: ";
    $failed=system('support\Win32\which.exe runscilab.exe');
    if ($failed) { 
      $failed=system('support\Win32\which.exe scilab.exe'); 
      $scilab = (not $failed) ? "scilab.exe" : "no";
    }
    if ($failed) { 
      $failed=system('support\Win32\which.exe wscilex.exe'); 
      $scilab = (not $failed) ? "wscilex.exe" : "no";
    }
    if ($failed) { 
      $failed=system('support\Win32\which.exe scilab.bat'); 
      $scilab = (not $failed) ? "scilab.bat" : "no";
    }
    if ($failed) { $scilab = "runscilab.exe"; }
    print STDOUT "$scilab\n";
    
    print STDOUT "Checking for Terminal: ";
    $failed=system('support\Win32\which.exe cmd.exe');
    if ($failed) { 
      $failed=system('support\Win32\which.exe command.com'); 
      $terminal = (not $failed) ? "command.com" : "no";
    } else { $terminal = "cmd.exe"; }
    print STDOUT "$terminal\n";
    
    print STDOUT "Checking for Tcl/Tk: ";
    $failed=system('support\Win32\which.exe wperl.exe');
    $tcltk = (not $failed) ? "wperl.exe" : "no";
    if ($failed) { 
      $failed=system('support\Win32\which.exe wish.exe'); 
      $tcltk = (not $failed) ? "wish.exe" : "no";
    }
    print STDOUT "$tcltk\n";
    
    print STDOUT "Checking for MPI compiler: ";
    $failed=system('support\Win32\which.exe mpicc.exe');
    $mpicc = (not $failed) ? "mpicc.exe" : "no";
    print STDOUT "$mpicc\n";
    
    print STDOUT "Checking for MPI run: ";
    $failed=system('support\Win32\which.exe mpirun.exe');
    $mpirun = (not $failed) ? "mpirun.exe" : "no";
    print STDOUT "$mpi\n";
    
    # On Win32, matlab is preferred before scilab, which
    # lacks certain functionality...
    if ($matlab ne "no") {
        print STDOUT "\n\nMatlab found, configuring McStas\n";
        $plotter = "Matlab";
    } elsif ($scilab ne "no") {
        print STDOUT "\n\nScilab found ($scilab), configuring McStas\n";
        $plotter = "Scilab";
    } else {
        print STDERR "\n\nSorry, neither Matlab or Scilab found, setting HTML/VRML.\n";
        $plotter = "HTML";
    }
} else {
    die "This perl script is only ment for use on Win32!\n";
}


print STDOUT "The plotter is $plotter";
my $fid = open(READ,"<lib/tools/perl/mcstas_config.perl") || die "Could not open config file\n";
my $fid2 = open(WRITE,">lib/tools/perl/mcstas_config.perl.new") || die "Could not write to new config file\n";
while (<READ>) {
    if (/\w*PLOTTER \=\w*/) {
        print WRITE "     PLOTTER => '$plotter',\n";
    } elsif (/\w*SCILAB \=\w*/) {
        print WRITE "     SCILAB => '$scilab',\n";
    } elsif (/\w*MATLAB \=\w*/) {
        print WRITE "     MATLAB => '$matlab',\n";
    } elsif (/\w*TCLTK \=\w*/) {
        print WRITE "     TCLTK => '$tcltk',\n";
    } elsif (/\w*SSH \=\w*/) {
        print WRITE "     SSH => 'no',\n";
    } elsif (/\w*BROWSER \=\w*/) {
        print WRITE "     BROWSER => 'start',\n";
    } elsif (/\w*TERMINAL \=\w*/) {
        print WRITE "     TERMINAL => '$terminal',\n";
    } elsif (/\w*MPICC \=\w*/) {
        print WRITE "     MPICC => '$mpicc',\n";
    } elsif (/\w*PGPLOT \=\w*/) {
        print WRITE "     PGPLOT => 'no',\n";
    } elsif (/\w*CC \=\w*/) {
        print WRITE "     CC => '$cc',\n";
    } elsif (/\w*CFLAGS \=\w*/) {
        print WRITE "     CFLAGS => '-O2',\n";
    } elsif (/\w*MPIRUN \=\w*/) {
        print WRITE "     MPIRUN => '$mpirun',\n";
    } elsif (/\w*PREFIX \=\w*/) {
        print WRITE "     PREFIX => 'start ',\n";
    } elsif (/\w*SUFFIX \=\w*/) {
        print WRITE "     SUFFIX => '.pl',\n";
    } elsif (/\w*BACKGROUND \=\w*/) {
        print WRITE "     BACKGROUND => '',\n";
    } elsif (/\w*EXTERNAL_EDITOR \=\w*/) {
        print WRITE "     EXTERNAL_EDITOR => 'notepad',\n";
    } elsif (/\w*EXE \=\w*/) {
        print WRITE "     EXE => 'exe',\n";
    } else {
        print WRITE;
    }
}
close(WRITE);
# It should now be ok to overwrite the config file:
system("copy lib\\tools\\perl\\mcstas_config.perl.new lib\\tools\\perl\\mcstas_config.perl");
print STDOUT "Updating lib\\tools\\perl\\mcstas_config.perl\n";
print STDOUT "Installing Tk-CodeText extension (ppm)\n";
system("ppm install support\\ppds\\Syntax-Highlight-Perl.ppd");
system("ppm install support\\ppds\\Tk-CodeText.ppd");


