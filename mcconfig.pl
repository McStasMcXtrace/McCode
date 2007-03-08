#!/usr/bin/perl

use Config;
use File::Copy;

our $scilab;
our $tcltk;
our $matlab;
our $cc;
our $mpirun;
our $mpicc;
our $terminal;
our $plotter;
our $vrmlview;

if ($Config{'osname'} eq "MSWin32") {
    my $failed;
    my $which="support\\Win32\\which.exe";
    if (not -f $which) { $which="which.exe"; }

    print STDOUT "\nConfiguring McStas on Win32\n\n";

    print STDOUT "Checking for C compiler: ";

      $failed=system("$which cc.exe");
      if ($failed) {
        $failed=system("$which gcc.exe");
        $cc = (not $failed) ? "gcc.exe" : "no";
      } else { $cc = "cc.exe"; }

    print STDOUT "$cc\n";

    print STDOUT "Checking for Matlab: ";

      $failed=system("$which matlab.exe");
      $matlab = (not $failed) ? "matlab.exe" : "no";

    print STDOUT "$matlab\n";

    print STDOUT "Checking for Scilab: ";

      $failed=system("$which runscilab.exe");
      if ($failed) {
        $failed=system("$which scilab.exe");
        $scilab = (not $failed) ? "scilab.exe" : "no";
      }
      if ($failed) {
        $failed=system("$which wscilex.exe");
        $scilab = (not $failed) ? "wscilex.exe" : "no";
      }
      if ($failed) {
        $failed=system("$which scilab.bat");
        $scilab = (not $failed) ? "scilab.bat" : "no";
      }
      if ($failed) { $scilab = "runscilab.exe"; }

    print STDOUT "$scilab\n";

    print STDOUT "Checking for VRML viewer: ";

      $failed=system("$which freewrl.exe");
      if ($failed) {
        $failed=system("$which glview.exe");
        $vrmlview = (not $failed) ? "glview.exe" : "no";
      }
      if ($failed) {
        $failed=system("$which lookat.exe");
        $vrmlview = (not $failed) ? "lookat.exe" : "no";
      }
      if ($failed) {
        $failed=system("$which openwrl.exe");
        $vrmlview = (not $failed) ? "openwrl.exe" : "no";
      }
      if ($failed) {
        $failed=system("$which Octaga.exe");
        $vrmlview = (not $failed) ? "Octaga.exe" : "no";
      }
      if ($failed) {
        $failed=system("$which explorer.exe");
        $vrmlview = (not $failed) ? "explorer.exe" : "no";
      }
      if ($failed) { $vrmlview = "start"; }

    print STDOUT "$vrmlview\n";

    print STDOUT "Checking for Terminal: ";
    $failed=system("$which cmd.exe");
    if ($failed) {
      $failed=system("$which command.com");
      $terminal = (not $failed) ? "command.com" : "no";
    } else { $terminal = "cmd.exe"; }
    print STDOUT "$terminal\n";

    print STDOUT "Checking for Tcl/Tk: ";
    $failed=system("$which wperl.exe");
    $tcltk = (not $failed) ? "wperl.exe" : "no";
    if ($failed) {
      $failed=system("$which wish.exe");
      $tcltk = (not $failed) ? "wish.exe" : "no";
    }
    print STDOUT "$tcltk\n";

    print STDOUT "Checking for MPI compiler: ";
    $failed=system("$which mpicc.exe");
    $mpicc = (not $failed) ? "mpicc.exe" : "no";
    print STDOUT "$mpicc\n";

    print STDOUT "Checking for MPI run: ";
    $failed=system("$which mpirun.exe");
    $mpirun = (not $failed) ? "mpirun.exe" : "no";
    print STDOUT "$mpirun\n";

    print STDOUT "Checking for HDFView compiler: ";
    $failed=system("$which hdfview.exe");
    $hdfview = (not $failed) ? "hdfview.exe" : "no";
    print STDOUT "$hdfview\n";


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


print STDOUT "The plotter is $plotter\n";
my $file = "mcstas_config.perl";
if (not -f $file) { $file = "lib/tools/perl/mcstas_config.perl"; }
if (not -f $file) { $file = "../lib/tools/perl/mcstas_config.perl"; }
if (not -f $file) { $file = "$ENV{'MCSTAS'}/tools/perl/mcstas_config.perl"; }
my $fid = open(READ,"<$file");
if (not $fid) { die "Could not open config file $file\n"; }

my $file2 = "$file.new";
my $fid2 = open(WRITE,">$file2") || die "Could not write to new config file $file2\n";

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
    } elsif (/\w*VRMLVIEW \=\w*/) {
        print WRITE "     VRMLVIEW => '$vrmlview',\n";
    } elsif (/\w*TERMINAL \=\w*/) {
        print WRITE "     TERMINAL => '$terminal',\n";
    } elsif (/\w*MPICC \=\w*/) {
        print WRITE "     MPICC => '$mpicc',\n";
    } elsif (/\w*PGPLOT \=\w*/) {
        print WRITE "     PGPLOT => 'yes',\n";
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
    } elsif (/\w*THREADS \=\w*/) {
        print WRITE "     THREADS => 'no',\n";
    } elsif (/\w*HDFVIEW \=\w*/) {
        print WRITE "     HDFVIEW => '$hdfview',\n";
    } elsif (/\w*PGDEV \=\w*/) {
	print WRITE "     PGDEV => '/gw',\n";
    } else {
        print WRITE;
    }
}
close(WRITE);
# It should now be ok to overwrite the config file:
copy("$file2", "$file");
print STDOUT "Updating: $file2 $file\n";
print STDOUT "Installing Tk-CodeText extension (ppm)\n";
system("ppm install support\\ppds\\Syntax-Highlight-Perl.ppd");
system("ppm install support\\ppds\\Tk-CodeText.ppd");
system("ppm install support\\ppds\\Math-Amoeba.ppd");
print STDOUT "Installing PDL and PGPLOT extensions (ppm)\n";
chdir("support/ppds");
system("ppm install PGPLOT56.PPD");
system("ppm install PDL.PPD");




