#! /usr/bin/perl
#
# Implements graphical user interface for McXtrace
#
#
#   This file is part of the McXtrace xray ray-trace simulation package
#   Copyright (C) 1997-2008, All rights reserved
#   Risoe National Laborartory, Roskilde, Denmark
#   Institut Laue Langevin, Grenoble, France
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; version 2 of the License.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program; if not, write to the Free Software
#   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

# Config module needed for various platform checks.
# PW, 20030314
use Config;
use Tk::Balloon;
use POSIX qw(_exit);

# Determine the path to the McXtrace system directory. This must be done
# in the BEGIN block so that it can be used in a "use lib" statement
# afterwards.
BEGIN {
    ENV_HEADER
}

use lib $MCSTAS::perl_dir;
use lib $MCSTAS::perl_modules;
require "mccode_config.perl";

# Overload with user's personal config
if ($ENV{"HOME"} && -e $ENV{"HOME"}."/.".$MCSTAS::mcstas_config{'MCCODE'}."/mccode_config.perl") {
  print "$0: reading local $MCSTAS::mcstas_config{'MCCODE'} configuration from " . $ENV{"HOME"}."/.".$MCSTAS::mcstas_config{'MCCODE'}."/mccode_config.perl\n";
  require $ENV{"HOME"}."/.".$MCSTAS::mcstas_config{'MCCODE'}."/mccode_config.perl";
}

use strict;
use FileHandle;
use Tk;
use Tk::TextUndo;
use Tk::ROText;
use Tk::DialogBox;
use File::Path;

require "mcfrontlib.pl";
require "mxguilib.pl";
# Requirement for mcplotlib.pl removed, will be loaded only
# if mcdisplay PGPLOT backend is used.
# PW, 20030314
# require "mcplotlib.pl";
require "mcrunlib.pl";

my $kill_when_editor_exits = 0;
my $current_sim_file;
my $current_sim_def = "";
my ($inf_instr, $inf_sim, $inf_data);
my %inf_param_map;

my ($main_window,$edit_window, $edit_control, $edit_label);
my ($status_label, $current_results_label, $cmdwin, $current_instr_label, $workdir);

my $prefix          = $MCSTAS::mcstas_config{'PREFIX'};
my $suffix          = $MCSTAS::mcstas_config{'SUFFIX'};
my $background; # Only really makes sense on Unix systems...
my $detach = 0;
if ($Config{'osname'} ne 'MSWin32') {
    $background = '&';
}

my %shortcuts;
# Gui shortcut setup, different for OS X, Unix, Win32
if ($Config{'osname'} eq 'darwin') {
    $shortcuts{'menuopen'}   = '<Meta-o>';
    $shortcuts{'menurun'}    = '<Meta-u>';
    $shortcuts{'menuplot'}   = '<Meta-p>';
    # These ones are not too obvious, Meta-C and Meta-M taken by general
    # OS bindings, Meta-s causing problems...
    $shortcuts{'menuprefs'}  = '<Meta-F1>';
    $shortcuts{'insertcomp'} = '<Meta-F2>';
    $shortcuts{'menusave'}   = '<Meta-F3>';
    # This one is implicit
    $shortcuts{'menuquit'}   = '<Meta-w>';
    $shortcuts{'cut'}        = '<Ctrl-x>';
    $shortcuts{'copy'}       = '<Ctrl-c>';
    $shortcuts{'paste'}      = '<Ctrl-v>';
} else {
    $shortcuts{'insertcomp'} = '<Alt-m>';
    $shortcuts{'menuopen'}   = '<Alt-o>';
    $shortcuts{'menurun'}    = '<Alt-u>';
    $shortcuts{'menuplot'}   = '<Alt-p>';
    $shortcuts{'menuprefs'}  = '<Alt-c>';
    $shortcuts{'menusave'}   = '<Alt-s>';
    $shortcuts{'menuquit'}   = '<Alt-q>';
    $shortcuts{'cut'}        = '<Ctrl-x>';
    $shortcuts{'copy'}       = '<Ctrl-c>';
    $shortcuts{'paste'}      = '<Ctrl-v>';
}

my $external_editor = $MCSTAS::mcstas_config{'EXTERNAL_EDITOR'};
our $quote=0; # default editor behaviour is to NOT surround strings with quotes
$MCSTAS::mcstas_config{'CFLAGS_SAVED'} = $MCSTAS::mcstas_config{'CFLAGS'};
$MCSTAS::mcstas_config{'CFLAGS'} = '' unless $MCSTAS::mcstas_config{'MCGUI_CFLAGS'};

my $compinfo;                        # Cache of parsed component definitions
my @compdefs;                        # List of available component definitions

# Our own Tk:Error function, to trap errors in TextUndo->Save(). See
# Tk documentation of Tk::Error.
my $error_override;                # Temporary override Tk::Error.
sub Tk::Error {
    my ($w, $err, @loc) = @_;
    if($error_override) {
        &$error_override($w, $err, @loc);
    } else {
        print STDERR "Tk::Error###: $err ";
        print STDERR join("\n ", @loc), "\n";
    }
}


sub ask_save_before_simulate {
    my ($w) = @_;
    if($edit_control && $edit_control->numberChanges() > 0) {
        my $ret = $w->messageBox(
          -message => "Save instrument \"$current_sim_def\" first?",
          -title => "Save file?",
          -type => 'YesNoCancel',
          -icon => 'question',
          -default => 'yes');
        menu_save($w) if lc($ret) eq "yes";
        return $ret eq "Cancel" ? 0 : 1;
    } else {
        return 1;
    }
}

sub is_erase_ok {
    my ($w) = @_;
    if($edit_control && $edit_control->numberChanges() > 0) {
        my $ret = $w->messageBox(-message => "Ok to lose changes?",
                                 -title => "Erase ok?",
                                 -type => 'okcancel',
                                 -icon => 'question',
                                 -default => 'cancel');
        # Make response all lowercase:
        $ret = lc($ret);
        return $ret eq "ok" ? 1 : 0;
    } else {
        return 1;
    }
}

sub menu_quit {
    if(is_erase_ok($main_window)) {
      # save log of session
      if($current_sim_def) {
        my $file;
        my ($inidir, $inifile);
        if($current_sim_def =~ m!^(.*)/([^/]*)$!) {
            ($inidir, $inifile) = ($1, $2);
        } else {
            ($inidir, $inifile) = ("", $current_sim_def);
        }
        $inifile =~ s/\.instr$//;
        my $date = localtime(time());
        $inifile .= "_${date}.log";
        $inifile =~ s!\ !_!g;
        $file = $inidir . $inifile;
        my $outputtext = $cmdwin->get('1.0', 'end');
        putmsg($cmdwin, "Save log file $file\n");
        open(MCLOG,">>$file");
        print MCLOG "# Log file $file generated by McXtrace/mcgui\n";
        print MCLOG "# Date: $date\n";
        print MCLOG "# Current instrument: $current_sim_def\n";
        print MCLOG "# Current results: $current_sim_file\n";
        print MCLOG "$outputtext";
        close(MCLOG);
      }
      $main_window->destroy;
    }
}

sub menu_edit_current {
    if($edit_control) {
        $edit_window->raise();
    } else {
      my $tkwin;
        if ($MCSTAS::mcstas_config{'EDITOR'} eq 0 || $MCSTAS::mcstas_config{'EDITOR'} eq 1) {
	  $tkwin = $main_window->Toplevel;
	  eval { # Try CodeText editor first 
	    setup_edit($main_window,$tkwin);
	  };
	  if ($@) { # or revert to old-school editor  if that failed.
	    printf "Starting Tk::CodeText based editor failed. Using simpler editor as fallback.\n";
	    setup_edit_1_7($main_window,$tkwin);
	  }
        } elsif ($MCSTAS::mcstas_config{'EDITOR'} eq 1 && $MCSTAS::mcstas_config{'CODETEXT'}) {
            
        } else {
            menu_spawn_editor($main_window);
        }
    }
}

sub menu_spawn_editor {
    my ($w) = @_;
    my $cmd = "$external_editor $current_sim_def";
    if ($external_editor eq "no") { return 0; }
    # Must be handled differently on Win32 vs. unix platforms...
    spawn_external($w,$cmd);
}

sub menu_spawn_internal_editor {
    my ($w) = @_;
    my $cmd = "mcgui$suffix --edit $current_sim_def";
    if ($external_editor eq "no") { return 0; }
    # Must be handled differently on Win32 vs. unix platforms...
    spawn_external($w,$cmd);
}


sub menu_spawn_mcdaemon {
    my ($w) = @_;
    my $cmd = "mcdaemon$suffix";
    spawn_external($w,$cmd);
}

sub menu_spawn_mcformatgui {
    my ($w) = @_;
    my $cmd = "mcformatgui$suffix";
    spawn_external($w,$cmd);
}


sub menu_spawn_mcplot {
    my ($w) = @_;
    my $file = $w->getOpenFile(-title => "Select simulation file", -initialdir => getcwd());
    if ($file == 0) {
	my $cmd = "$MCSTAS::mcstas_config{'PLOTCMD'}$suffix $file";
	spawn_external($w,$cmd);
    }
}

sub spawn_external {
    # Procedure to put external processes in the background
    my ($w, $cmd) = @_;
    my $pid;
    if($Config{'osname'} eq "MSWin32") {
        system("start $cmd");
    } else {
        $pid = fork();
        if(!defined($pid)) {
            $w->messageBox(-message =>
                           "Failed to spawn command \"$cmd\".",
                           -title => "Command failed",
                           -type => 'OK',
                           -icon => 'error');
            return 0;
        } elsif($pid > 0) {
            waitpid($pid, 0);
            return 1;
        } else {
            # Double fork to avoid having to wait() for the command to
            # finish (or having it become a zombie). See man perlfunc.
            unless(fork()) {
                exec("$cmd");
                # If we get here, the exec() failed.
                print STDERR "Error: exec() of command $cmd failed!\n";
                POSIX::_exit(1);        # CORE:exit needed to avoid Perl/Tk failure.
            }
            POSIX::_exit(0);                # CORE:exit needed to avoid Perl/Tk failure.
        }
    }
}

sub mcdoc_current {
    my $cmd = "$prefix $MCSTAS::mcstas_config{'DOCCMD'}$suffix $current_sim_def $background";
    if (-e $current_sim_def) {
        putmsg($cmdwin, "Opening instrument docs: $cmd\n", 'msg');
        system("$cmd");
    }
}
sub mcdoc_web {
    my $cmd = "$prefix $MCSTAS::mcstas_config{'DOCCMD'}$suffix --web $background";
    putmsg($cmdwin, "Opening Web Page: $cmd\n", 'msg');
    system("$cmd");
}

sub mcdoc_manual {
    my $cmd = "$prefix $MCSTAS::mcstas_config{'DOCCMD'}$suffix --manual $background";
    putmsg($cmdwin, "Opening User Manual: $cmd\n", 'msg');
    system("$cmd");
}

sub mcdoc_compman {
    my $cmd = "$prefix $MCSTAS::mcstas_config{'DOCCMD'}$suffix --comp $background";
    putmsg($cmdwin, "Opening Component Manual: $cmd\n", 'msg');
    system("$cmd");
}

sub mcdoc_components {
    my $cmd = "$prefix $MCSTAS::mcstas_config{'DOCCMD'}$suffix $background";
    putmsg($cmdwin, "Opening Library help: $cmd\n", 'msg');
    system("$cmd");
}

sub mcdoc_generate {
    my $cmd = "$prefix $MCSTAS::mcstas_config{'DOCCMD'}$suffix --force $background";
    putmsg($cmdwin, "Generating Library help (local): $cmd\n", 'msg');
    system("$cmd");
}

sub mcdoc_test {
    my $status;
    my $printer = sub { putmsg($cmdwin, "$_[0]\n", 'msg'); $main_window->update;};
    $status = do_test($printer, 1, $MCSTAS::mcstas_config{'PLOTTER'}, 'compatible graphics');
    if (defined $status) { putmsg($cmdwin, "$status", 'msg'); }
}

sub tools_shortcuts {
    my ($w) = @_;

    $w->fontCreate('small',
    -family=>'Helvetica',
    -weight=>'normal',
    -size=>12);
    my $msg="mcgui has platform dependent shortcut keys.\n\nOn this machine ".
	    "(OS type $Config{'osname'}) they are:\n\n".
	    "$shortcuts{'menuopen'} - Open instrument file\n".
	    "$shortcuts{'menurun'} - Run instrument\n".
	    "$shortcuts{'menuplot'} - Plot simulation data\n".
	    "$shortcuts{'menuprefs'} - Config menu\n".
	    "$shortcuts{'menuquit'} - Quit\n\n".
	    "$shortcuts{'insertcomp'} - Editor, insert comp\n".
	    "$shortcuts{'menusave'} - Editor, save instrument\n".
	    "$shortcuts{'cut'} - Editor cut\n".
	    "$shortcuts{'copy'} - Editor copy\n".
	    "$shortcuts{'paste'} - Editor paste";
    if ($MCSTAS::mcstas_config{'PLOTTER'} =~ /mcstas|mcxtrace|pgplot/i) {
     $msg .= "\n\n
    'P' - Plotter/PGPLOT export BW postscript
    'C' - Plotter/PGPLOT export color postscript
    'N' - Plotter/PGPLOT export PNG file
    'M' - Plotter/PGPLOT export PPM file
    'G' - Plotter/PGPLOT export GIF file
    'L' - Plotter/PGPLOT Toggle log10 plotting mode (data)
    'T' - Plotter/PGPLOT Toggle contour plotting mode (data)
    'Z' - Plotter/PGPLOT zoom (in Trace/3D view)
    'Q' - Plotter/PGPLOT quit";
    }

    if ($Config{'osname'} eq 'MSWin32') {
	    $w->messageBox(-message =>$msg,
		       -title => "McGUI: Shortcut keys",
		       -type => 'OK',
		       -icon => 'info');
    } else {
	    $w->messageBox(-message =>$msg,
		       -title => "McGUI: Shortcut keys",
		       -type => 'OK',
		       -font => 'small',
		       -icon => 'info');
    }
    $w->fontDelete('small');
}

sub tools_dsa {
    my ($w) = @_;
    my $msg="Press Yes to create DSA key.\n";
    my $key_exist = 0;
    if (-e "$ENV{'HOME'}/.ssh/id_dsa") {
      $msg = $msg."\nWarning! A DSA key exists!\n".
	    "By pressing Yes it will be overwritten!";
      $key_exist = 1;
    }
    my $do_dsa=$w->messageBox(-message =>$msg,
		   -title => "McGUI: Generate DSA key?",
		   -type => 'YesNoCancel',
		   -icon => 'question',
		   -default => 'yes');
    if ((lc($do_dsa) eq "no")||(lc($do_dsa) eq "cancel")) {
	    putmsg($cmdwin, "DSA key generation canceled!\n", 'msg');
      return 0;
    }
    if ($key_exist == 1) {
      system("rm -f $ENV{'HOME'}/.ssh/id_dsa $ENV{'HOME'}/.ssh/id_dsa.pub");
    }
    # create DSA key for local MPI execution.
    my $cmd = "ssh-keygen -q -t dsa -P \"\" -f $ENV{'HOME'}/.ssh/id_dsa";
    putmsg($cmdwin, "Installing DSA key for SSH: \n$cmd\n", 'msg');
    my $success=my_system($w, "Please wait while generating DSA key\n", $cmd);
    if ($success) {
      $cmd = "cat $ENV{'HOME'}/.ssh/id_dsa.pub >> $ENV{'HOME'}/.ssh/authorized_keys";
      system("$cmd");
      putmsg($cmdwin, "\nDSA key generated to $ENV{'HOME'}/.ssh/authorized_keys\n", 'msg');
    } else { putmsg($cmdwin, "\nDSA key generation FAILED!\n"); }
}

sub mcdoc_about {
  my ($w) = @_;
  my $version = `$MCSTAS::mcstas_config{'MCCODE'} --version`;
  # create a small font for Message Box
  $w->fontCreate('small',
    -family=>'Helvetica',
    -weight=>'normal',
    -size=>12);
  my $msg="This is the McXtrace Graphical User Interface. McXtrace is a tool for Monte Carlo X-ray scattering simulations. It provides a complete set of tools, components, and example instruments.\n
  This software required a significant effort to be brought to you. If you enjoy it, please use following references in your work:\n
  P. Willendrup, E. Farhi and K. Lefmann, Physica B, 350 (2004) 735.\n
  K. Lefmann and K. Nielsen, Neutron News 10, 20, (1999).\n
  $version
  Please visit <http://www.mcxtrace.org/>";
  if ($Config{'osname'} eq 'MSWin32') {
    $w->messageBox(-message =>$msg,
                                 -title => "MxGUI: About McXtrace",
                                 -type => 'OK',
                                 -icon => 'info');
  } else {
    $w->messageBox(-message =>$msg,
                                 -title => "MxGUI: About McXtrace",
                                 -type => 'OK',
                                 -font => 'small',
                                 -icon => 'info');
  }
  $w->fontDelete('small');
}


sub new_simulation_results {
    my ($w) = @_;
    my $text = $current_sim_file ? $current_sim_file : "<None>";
    $current_results_label->configure(-text => "Simulation results: $text");
}

sub new_sim_def_name {
    my ($w, $name) = @_;
    unless($current_sim_def ne "" && $name eq $current_sim_def) {
        undef($current_sim_file);
        new_simulation_results($w);
    }
    $current_sim_def = $name;
    # Strip any repeated "/" charactors (ie. "///" -> "/").
    $current_sim_def =~ s!//!/!g;
    # On NON-Win32 platforms, replace ' ' by '\ ' to ensure correct
    # handling of spaces in filenames... Unfortunately, this is a
    # more complicated matter on Win32 - has to be handled in each
    # subroutine... :(
    if (!$Config{'osname'} eq 'MSWin32') {
      $current_sim_def =~ s! !\ !g;
    }
    # Strip any redundant leading "./".
    while($current_sim_def =~ m!^\./(.*)$!) {
        $current_sim_def = $1;
    }
    # Strip any redundant "dir/../".
    # Problem: Needs to handle "/../../" correctly to work ...
#     while($current_sim_def =~ m!^[^/]+/\.\./(.*)$!) {
#         $current_sim_def = $1;
#     }
#     while($current_sim_def =~ m!^(.*)/[^/]+/\.\./(.*)$!) {
#         $current_sim_def = "$1/$2";
#     }
    $main_window->title("McXtrace: $current_sim_def");
    my $text = "Instrument file: " .
        ($current_sim_def ne "" ? $current_sim_def : "<None>");
    if ($current_sim_def ne "" && $edit_window) {
      $edit_window->title("Edit: $current_sim_def");
    }
    $current_instr_label->configure(-text => $text);
    # On Win32, doing a chdir is probably better at this point...
    if ($Config{'osname'} eq 'MSWin32') {
        chdir(dirname($current_sim_def));
    }
    putmsg($cmdwin, "$text\n", 'msg');
}

sub open_instr_def {
    my ($w, $file) = @_;
    $edit_control->Load($file) if $edit_control;
    new_sim_def_name($w, $file);
}


sub set_run_dir {
  my ($w, $file) = @_;
  my $dir = select_dir();
  set_workdir($w, $dir);
}

sub set_workdir{
  my ($w, $dir) = @_;
  if (!($dir eq "")) {
    if ($Config{'osname'} eq 'MSWin32') {
      $dir =~ s+/+\\+g;
    }
    $workdir->delete("1.0", "end");
    chdir($dir);
    $workdir->insert('end', $dir);
  }
}

sub menu_open {
    my ($w) = @_;
    return 0 unless(is_erase_ok($w));
    my $file = $w->getOpenFile(-defaultextension => ".instr",
                               -title => "Select instrument file", -initialdir => getcwd());
    return 0 unless $file;
    open_instr_def($w, $file);
    return 1;
}

sub menu_save {
    my ($w) = @_;
    if($current_sim_def ne "") {
        $edit_control->Save($current_sim_def);
        $edit_window->title("Edit: $current_sim_def");
        new_sim_def_name($w, $current_sim_def);
    } else {
        $error_override = sub {        # Temporary Tk::Error override
            $w->messageBox(-message => "Could not save file:\n$_[1].",
                           -title => "Save failed",
                           -type => 'OK',
                           -icon => 'error');
        };
        menu_saveas($w);
        $error_override = undef; # Reinstall default Tk::Error handler/
    }
}

sub menu_saveas {
    my ($w) = @_;
    my $file;
    if($current_sim_def) {
        my ($inidir, $inifile);
        if($current_sim_def =~ m!^(.*)/([^/]*)$!) {
            ($inidir, $inifile) = ($1, $2);
        } else {
            ($inidir, $inifile) = ("", $current_sim_def);
        }
        $file = $w->getSaveFile(-defaultextension => ".instr",
                                -title => "Select instrument file name",
                                -initialdir => $inidir,
                                -initialfile => $inifile);
    } else {
        $file = $w->getSaveFile(-defaultextension => ".instr",
                                -title => "Select instrument file name");
    }
    return 0 unless $file;
    $edit_control->FileName($file);
    new_sim_def_name($w, $file);
    menu_save($w);
    return 1;
}

sub menu_save_config {
  my ($w) = @_;

  my $initdir;

  if (-d $ENV{"HOME"}) {
    if (!(-d $ENV{"HOME"}."/.".$MCSTAS::mcstas_config{'MCCODE'})) {
      mkdir $ENV{"HOME"}."/.".$MCSTAS::mcstas_config{'MCCODE'};
    }
    $initdir = $ENV{"HOME"}."/.".$MCSTAS::mcstas_config{'MCCODE'};
  } else {
    $initdir = $MCSTAS::perl_dir
  }
  my $file = $w->getSaveFile(-defaultextension => ".perl",
                                -title => "Select preference file name",
                                -initialdir => $initdir,
                                -initialfile => "mccode_config.perl");
  if ($file) { save_config($w,$file); }
}

sub save_config {
  my ($w, $file) = @_;

  # Start by collecting header + footer from perl_dir configfile
  # for safe possible writing of this file...

  my ($HEADER, $FOOTER);
  my $found_head = 0; my $found_foot = 0;

  my $fh = new FileHandle;
  my $fid = open($fh, "<", "$MCSTAS::perl_dir/mccode_config.perl");
  while (<$fh>) {
    if (!$found_head) {
      $HEADER = $HEADER.$_;
    }
    if (/^# HEADER/) { $found_head = 1; };
    if (/^# FOOTER/) { $found_foot = 1; };
    if ($found_foot) {
      $FOOTER = $FOOTER.$_;
    }
  }

  close($fh);

  my $fid = open($fh, ">", $file);

  if(!$fid) {
    $w->messageBox(-message => "Error saving $file (permissions?)",
		   -title => "Error saving configuration",
                       -type => 'OK',
		   -icon => 'error');
    putmsg($w, "Problems saving configuration\n  $file\n", 'msg');
    return;
  } else {
    print $fh $HEADER;

    my @keys = keys %MCSTAS::mcstas_config;
    my @values = values %MCSTAS::mcstas_config;

    my ($j, $value, $key);

    print $fh "\n\%MCSTAS::mcstas_config = (\n";
    for ($j=0; $j<@keys; $j++) {
    # CFLAGS/CFLAGS_SAVED must be handled as special case
      if (!($keys[$j] eq "CFLAGS")) {
	if ($keys[$j] eq  "CFLAGS_SAVED") { $keys[$j] = "CFLAGS" };
	print $fh "\t$keys[$j] => ";
	if ($values[$j]+0 ne $values[$j]) {$values[$j] = "'$values[$j]'";}
	print $fh "$values[$j]";
	if ($j<@keys-1) {
	  print $fh ",\n";
	}
      }
    }
    print $fh "\n);\n\n";
    print $fh $FOOTER;
    close($fh);
    putmsg($w, "Configuration file\n  $file\nsaved successfully\n", 'msg');
    return;
  }
}

sub menu_new {
    my ($w) = @_;
    return 0 unless(is_erase_ok($w));
    my $file = $w->getSaveFile(-defaultextension => ".instr",
                               -title => "Select instrument file name");
    return 0 unless $file;
    $edit_control->delete("1.0", "end");
    $edit_control->FileName($file);
    new_sim_def_name($w, $file);
    return 1;
}

sub menu_undo {
    my ($w) = @_;
    if($edit_control->numberChanges() <= 0) {
        $w->messageBox(-message => "There is no further undo information.",
                       -title => "Undo not possible",
                       -type => 'OK',
                       -icon => 'error');
    } else {
        $edit_control->eventGenerate("<<Undo>>");
    }
}

sub read_sim_data {
    my ($w) = @_;
    return 0 unless $current_sim_file && -r $current_sim_file;
    my ($ii, $si, $di) = read_sim_file($current_sim_file);
    return 0 unless $ii && $si && $di;
    # Save old settings of "plot results".
    $si->{'Autoplot'} = $inf_sim->{'Autoplot'};
    $inf_instr = $ii;
    $inf_sim = $si;
    $inf_data = $di;
    my $i;
    foreach $i (keys %{$si->{'Params'}}) {
        $inf_param_map{$i} = $si->{'Params'}{$i};
    }
    $si->{'Params'} = \%inf_param_map;
    return 1;
}

sub load_sim_file {
    my ($w) = @_;
    my $file = $w->getOpenFile(-defaultextension => ".sim",
                               -title => "Select simulation file", -initialdir => getcwd());
    if($file && -r $file) {
        $current_sim_file = $file ;
        new_simulation_results($w);
    }
    read_sim_data($w);
}

sub save_disp_file {
    # Function for saving mcdisplay type output
    # PW 20030314
    my ($w,$ext) = @_;
    my $file = $w->getSaveFile(-defaultextension => $ext,
                               -title => "Select output filename", -initialdir => getcwd(), -initialfile => "mcdisplay_output.$ext");
    return $file;
}

sub putmsg {
    my ($t, $m, $tag) = @_;
    $cmdwin->insert('end', $m, $tag);
    $cmdwin->see('end');
}

sub run_dialog_create {
    my ($w, $title, $text, $cancel_cmd, $update_cmd) = @_;
    my $dlg = $w->Toplevel(-title => $title);
    # Ad-hoc "wrapping" of the tooltip text, without use of string limit + Text::Wrap
    $title =~ s/\ /\n/g;

    $dlg->transient($dlg->Parent->toplevel);
    $dlg->withdraw;
    $dlg->protocol("WM_DELETE_WINDOW" => sub { } );
    $b = $dlg->Balloon(-state => 'balloon');
    # Add labels
    my $text_label = $dlg->Label(-text => $text,
                -anchor => 'w',
                -justify => 'left')->pack(-fill => 'x');
    $b->attach($text_label, -balloonmsg => $title);
    my $bot_frame = $dlg->Frame(-relief => "raised", -bd => 1);
    $bot_frame->pack(-side => "top", -fill => "both",
                     -ipady => 3, -ipadx => 3);
    my $but = $bot_frame->Button(-text => "Cancel", -command => $cancel_cmd);
    my $buttext = "Save results\nand Stop/Abort";
    if ($text =~ /compil/i || $text =~ /DSA/i || $title =~ /compil/i || $title =~ /DSA/i ) { $buttext="Abort current Job"; }
    $b->attach($but, -balloonmsg => $buttext);
    $but->pack(-side => "left", -expand => 1, -padx => 1, -pady => 1);
    # can only update when in Single or OpenMP simulation mode (no cluster/grid)
    # and performing simulation
    if ($Config{'osname'} ne 'MSWin32' && $update_cmd && $inf_sim->{'Mode'}==0
      && $MCSTAS::mcstas_config{'CLUSTER'} <= 1 && $text !~ /compil/i && $text !~ /DSA/i
      && $title !~ /compil/i && $title !~ /DSA/i) {
      $but = $bot_frame->Button(-text => "Update", -command => $update_cmd);
      $but->pack(-side => "right");
      $b->attach($but, -balloonmsg => "Save results\nand continue");
    }
    return $dlg;
}

sub run_dialog_popup {
    my ($dlg) = @_;
    # Display the dialog box
    my $old_focus = $dlg->focusSave;
    my $old_grab = $dlg->grabSave;
    $dlg->Popup;
    $dlg->grab;
    return [$old_focus, $old_grab];
}

sub run_dialog_retract {
    my ($dlg, $oldfg) = @_;
    $dlg->grabRelease;
    $dlg->destroy;
    &{$oldfg->[0]} if $oldfg;
    &{$oldfg->[1]} if $oldfg;
}

sub run_dialog_reader {
    my ($w, $fh, $rotext, $state, $success) = @_;
    my $s;
    my $len = sysread($fh, $s, 256, 0);
    if($len) {
        putmsg($rotext, $s);
    } else {
        $w->fileevent($fh,'readable', "");
        return if $$state;
        $$state = 1;
        $$success = defined($len);
    }
}

sub run_dialog {
    my ($w, $fh, $pid, $inittext) = @_;
    # The $state variable is set when the simulation finishes.
    my ($state, $success) = (0, 0);
    # Initialize the dialog.
    my $cancel_cmd = sub {
        putmsg($cmdwin, "\nSending TERM to pid=$pid ($state)\n");
        kill "TERM", $pid unless $state; # signal 15 is SIGTERM
    };
    my $update_cmd = sub {
        putmsg($cmdwin, "\nSending USR2 to pid=$pid ($state)\n");
        kill "USR2", $pid unless $state; # signal USR2
    };
    my $text="Job";
    if ($inf_sim->{'Mode'}==1) { $text='Trace/3D View'; }
    elsif ($inf_sim->{'Mode'}==2) { $text='Parameter Optimization'; }
    if ($pid && $Config{'osname'} ne 'MSWin32') {
      $text .= " [pid $pid]";
    }
    my $dlg = run_dialog_create($w, $inittext,
                                "$text running ($current_sim_def)...",
                                $cancel_cmd, $update_cmd);
    putmsg($cmdwin, "$inittext\n", 'msg'); # Must appear before any other output
    # Set up the pipe reader callback
    my $reader = sub {
        run_dialog_reader($w, $fh, $cmdwin, \$state, \$success);
    };

    $status_label->configure(-text => "Status: Running $text");
    my $savefocus = run_dialog_popup($dlg);
    my $status;

    # The following is a little hack which allows us to upgrade perl on Win32,
    # where we have been stuck with 5.6 since 2003...
    if ($Config{'osname'} ne 'MSWin32') {
      $w->fileevent($fh, 'readable', $reader);
      do {
	$w->waitVariable(\$state);
      } until $state;
      $status = close($fh);
    } else {
      # On Win32, mcrun and other commands run by themselves in seperate
      # cmd.exe shell windows... Error messages etc. will go there.
      $status = 1;
      $success = 1;
    }
    run_dialog_retract($dlg, $savefocus);

    $status_label->configure(-text => "Status: Done");
    if(!$success || (! $status && ($? != 0 || $!))) {
        putmsg($cmdwin, "Job exited abnormally.\n");
        return undef;
    } else {
        putmsg($cmdwin, "Job finished.\n", 'msg');
        return 1;
    }
}

sub dialog_get_out_file {
    # In case of mcrunflag set, let mcrun handle the compilation
    my ($w, $file, $force, $mpi, $cflags, $mcrunflag) = @_;
    # The $state variable is set when the spawned command finishes.
    my ($state, $cmd_success);
    my $success = 0;
    my ($fh, $pid, $out_name);
    # Initialize the dialog.
    my $cancel_cmd = sub {
        putmsg($cmdwin, "Sending KILL to $pid (compile)\n", 'msg');
        kill "TERM", $pid if $pid; # signal 15 is SIGTERM
    };
    my $dlg = run_dialog_create($w, "Compiling simulation $current_sim_def",
                                "Compiling simulation\n($current_sim_def)", $cancel_cmd);
    my $printer = sub { putmsg($cmdwin, "$_[0]\n", 'msg'); };
    # Set up the pipe reader callback
    $status_label->configure(-text => "Status: Compiling simulation");
    # The dialog isn't actually popped up unless/until a command is
    # run or an error occurs.
    my $savefocus;
    my ($compile_data, $msg) = get_out_file_init($file, $force, $mpi, 1);
    if(!$compile_data) {
        &$printer("Could not compile simulation:\n$msg");
    } else {
        $state = 0;
        for(;;) {
            my ($type, $val) = get_out_file_next($compile_data, $printer, $mpi, $mcrunflag);
            if($type eq 'FINISHED') {
                $success = 1;
                $out_name = $val;
                last;
            } elsif($type eq 'RUN_CMD') {
                $success = my_system($w, "Compiling simulation $current_sim_def",
                  join(" ", @$val));
		if (defined($mcrunflag) && $mcrunflag == 1) {
		  $type = 'FINISHED';
		  $success=1;
		  &$printer("Please wait for $MCSTAS::mcstas_config{'RUNCMD'} window to exit.");
		  last;
		}
                unless($success) {
                        &$printer("** Error exit **.");
                        last;
		}
            } elsif($type eq 'ERROR') {
                &$printer("Error: $msg");
                last;
            } elsif($type eq 'CONTINUE') {
                next;
            } else {
                die "Internal: compile_dialog(): $type, $msg";
            }
        }
    }
    run_dialog_retract($dlg, $savefocus);
    my $donetype = $success ? "Done" : "Compile failed";
    $status_label->configure(-text => "Status: $donetype");
    &$printer("$donetype.") unless $success && !$savefocus;
    return $success ? $out_name : undef;
}

sub compile_instrument {
    my ($w, $force) = @_;
    return undef unless ask_save_before_simulate($w);
    my $mcrunflag;
    if ($Config{'osname'} eq 'MSWin32') {
      $mcrunflag = 1;
    }
    my $out_name = dialog_get_out_file($w, $current_sim_def, $force,
      $MCSTAS::mcstas_config{'CLUSTER'} == 2 ? 1 : 0, $MCSTAS::mcstas_config{'CLUSTER'} == 1 ? 1 : 0, $mcrunflag);
    unless($out_name && -x $out_name) {
      if ($mcrunflag == 1) {
	$w->messageBox(-message => "Compile running in seperate window.\nPlease wait for the process to finish!",
                       -title => "Notice",
                       -type => 'OK',
                       -icon => 'warning');
        return undef;
      } else {
        $w->messageBox(-message => "Could not compile simulation.",
                       -title => "Compile failed",
                       -type => 'OK',
                       -icon => 'error');
        return undef;
      }
    }
    $inf_sim->{'Forcecompile'} = 0;
    return $out_name;
}

sub menu_compile{
    my ($w) = @_;
    unless($current_sim_def) {
        $w->messageBox(-message => "No simulation definition loaded.",
                       -title => "Compilation error",
                       -type => 'OK',
                       -icon => 'error');
        return undef;
    }
    # Force recompilation.
    compile_instrument($w, 1);
    return 1;
}

sub my_system {
    my ($w, $inittext, @sysargs) = @_;
    my $fh = new FileHandle;
    my $child_pid;
    # Open calls must be handled according to
    # platform...
    # PW 20030314
    if ($Config{'osname'} eq 'MSWin32') {
      $child_pid = open($fh, "start safewrap.pl @sysargs 2>&1 |");
    } else {
      $child_pid = open($fh, "-|");
    }
    unless(defined($child_pid)) {
        $w->messageBox(-message => "Could not run $inittext.",
                       -title => "Job failed",
                       -type => 'OK',
                       -icon => 'error');
        return undef;
    }
    if($child_pid) {                # Parent
        return run_dialog($w, $fh, $child_pid, $inittext);
    } else {                        # Child
        open(STDERR, ">&STDOUT") || die "Can't dup stdout";
        # Make the child the process group leader, so that
        # we can kill off any subprocesses it may have
        # spawned when the user selects CANCEL.
        setpgrp(0,0);
        exec @sysargs if @sysargs; # The "if @sysargs" avoids a Perl warning.
        # If we get here, the exec() failed.
        print STDERR "Error: exec() of $sysargs[0] failed!\n";
        POSIX::_exit(1);        # CORE:exit needed to avoid Perl/Tk failure.
    }
}

sub menu_run_simulation {
    my ($w) = @_;
    unless($current_sim_def) {
        return undef unless menu_open($w);
    }
    my $out_name = compile_instrument($w);
    return 0 unless $out_name;
    # Attempt to avoid problem with missing "." in $PATH. Unix only.
    if (!($Config{'osname'} eq 'MSWin32')) {
        unless($out_name =~ "/") {
            $out_name = "./$out_name";
        }
    }
    my $out_info = get_sim_info($out_name);
    unless($out_info) {
        $w->messageBox(-message => "Could not run simulation.",
                       -title => "Run failed",
                       -type => 'OK',
                       -icon => 'error');
        return 0;
    }
    # In case last used path was autogenerated, strip it off before running simulation
    if ($inf_sim->{'DirAutogen'} == 1) {
      $inf_sim->{'Dir'} = '';
      $inf_sim->{'DirAutogen'} = 0;
    }
    my ($bt, $newsi) = simulation_dialog($w, $out_info, $inf_sim);

    if($bt eq 'Start') {
        my @command = ();
        # Check 'Plotter' setting
        my $plotter = $MCSTAS::mcstas_config{'PLOTTER'};

        # Check 'Mode' setting if a scan/trace/optim is
        # requested
        if ($newsi->{'Mode'} == 1) {
	    push @command, "$MCSTAS::mcstas_config{'TRACECMD'}$suffix";
            if ($plotter =~ /PGPLOT|McXtrace|Gnuplot/i) {
              push @command, "--plotter=PGPLOT";
              # Selection of PGPLOT 3-pane view from config menu only.
	      # Default is to NOT use 3-pane view.
              if ($MCSTAS::mcstas_config{'MCGUI_PGMULTI'}) {
                push @command, "--multi";
              }
            } elsif ($plotter =~ /Matlab/i && $plotter =~ /scriptfile/i) {
              push @command, "--plotter=Matlab";
              my $output_file = save_disp_file($w,'m');
              if (!$output_file) {
                putmsg($cmdwin, "Trace canceled...\n");
                return;
              }
              $output_file = "\"$output_file\"";
              push @command, "-f$output_file";

            } elsif ($plotter =~ /Matlab/i) {
              push @command, "--plotter=Matlab";
            } elsif ($plotter =~ /html|vrml/i) {
                push @command, "--plotter=VRML";
                # Make a check for # of xray histories,
                # should be made small to avoid waiting a long time for
                # mcdisplay...
                # Subtract 0 to make sure $num_histories is treated as a
                # number...
                my $num_histories = $newsi->{'Ncount'} - 0;
                if ($num_histories >=1e3) {
                    my $break = $w->messageBox(-message => "$num_histories is a very large number\nof X-ray histories when using\nVRML\nContinue ?",
                     -title => "Warning: large number",
                     -type => 'yesnocancel',
                     -icon => 'error',
                     -default => 'no');
                    # Make first char lower case - default on
                    # Win32 upper case default on Unix... (perl 5.8)
                    $break = lcfirst($break);
                    if ((lc($break) eq "no")||(lc($break) eq "cancel")) {
                        return 0;
                    }
                }

            }

            push @command, "--inspect=$newsi->{'Inspect'}" if $newsi->{'Inspect'};
            push @command, "--first=$newsi->{'First'}" if $newsi->{'First'};
            push @command, "--last=$newsi->{'Last'}" if $newsi->{'Last'};
            # push @command, "--save";
        } # end Mode=Trace mcdisplay
        elsif ($newsi->{'Mode'} == 2) { # optimize
          push @command, "$MCSTAS::mcstas_config{'prefix'}$MCSTAS::mcstas_config{'RUNCMD'}$suffix";
          if (not ($newsi->{'Last'} || $newsi->{'Inspect'} || $newsi->{'First'})) {
            putmsg($cmdwin, "Warning: No criteria/monitor selected\n
         Global optimization using all monitors\n");
            push @command, "--optim";
          } else {
            push @command, "--optim=$newsi->{'Inspect'}" if $newsi->{'Inspect'};
            push @command, "--optim=$newsi->{'First'}" if $newsi->{'First'};
            push @command, "--optim=$newsi->{'Last'}" if $newsi->{'Last'};
          }
	  push @command, "--optim-prec=$MCSTAS::mcstas_config{'PREC'}" if $MCSTAS::mcstas_config{'PREC'};
        } # end Mode=Optimize
        elsif ($newsi->{'Mode'} == 0) { # simulate
	    push @command, "$MCSTAS::mcstas_config{'prefix'}$MCSTAS::mcstas_config{'RUNCMD'}$suffix";
        } # end Mode=simulate
        push @command, "$out_name";
        my ($OutDir,$OutDirBak);
        # In the special case of --dir, we simply replace ' ' with '_'
        # on Win32 (also giving out a warning message). This is done
        # because Win32::GetShortPathName only works on directories that
        # actually exist... :(
        if (!($newsi->{'Dir'})) {
            my $tmpdir = ${out_name};
            $tmpdir =~ s/\.$MCSTAS::mcstas_config{'EXE'}$//;
            $tmpdir .= '_' . POSIX::strftime("%Y%m%d_%H%M%S", localtime);
            $newsi->{'Dir'} = $tmpdir;
	    $newsi->{'DirAutogen'} =1;
        }
        if ($newsi->{'Dir'} && !($newsi->{'Mode'})) {
          $OutDir=$newsi->{'Dir'};
          if ($Config{'osname'} eq 'MSWin32') {
            $OutDirBak = $OutDir;
            $OutDir =~ s! !_!g;
            if (! "$OutDir" == "$OutDirBak") {
              putmsg($cmdwin, "You have requested output directory \"$OutDirBak\"\n");
              putmsg($cmdwin, "For compatibility reasons, spaces are replaced by underscores.\n");
              putmsg($cmdwin, "Your output files will go to \"$OutDir\"\n");
              $newsi->{'Dir'} = $OutDir;
            }
          } else {
            $OutDir =~ s! !\ !g;
          }
        }
        # clustering methods
        if ($newsi->{'cluster'} == 2) {
          push @command, "--mpi=$MCSTAS::mcstas_config{'NODES'}";
        } elsif ($newsi->{'cluster'} == 3) {
          push @command, "--multi=$MCSTAS::mcstas_config{'NODES'}";
	  if ($MCSTAS::mcstas_config{'GRID_FORCECOMPILE'}) {
	    push @command, "-c";
	  }
        }
        if ($newsi->{'Forcecompile'} == 1) {
          if ($newsi->{'cluster'} == 3) {
            # force compile from mcrun
            push @command, "--force-compile";
            $newsi->{'Forcecompile'} = 0;
          } else { # compile from mcgui (locally)
            $MCSTAS::mcstas_config{'CLUSTER'} = $newsi->{'cluster'};
            $out_name = compile_instrument($w, 1);
          }
        }

        push @command, "--ncount=$newsi->{'Ncount'}";
        push @command, "--trace" if ($newsi->{'Mode'} eq 1);
        push @command, "--seed=$newsi->{'Seed'}" if $newsi->{'Seed'} ne "" && $newsi->{'Seed'} ne 0;
        push @command, "--dir=$OutDir" if ($newsi->{'Dir'} && !$newsi->{'Mode'}==1);
        if ($newsi->{'Force'} eq 1) {
          if (-e $OutDir) {
            rmtree($OutDir,0,1);
          }
        }
        # we now always use McStas/PGPLOT legacy format, except for HTML and NeXus
        if ($newsi->{'Mode'}!=1 && $plotter !~ /PGPLOT|McStas|Gnuplot|Matlab/i) {
          push @command, "--plotter=$plotter";
        }

       # add parameter values
        my @unset = ();
        my @multiple = ();
        if ($newsi->{'NScan'} eq '') { $newsi->{'NScan'} = 1; }
        for (@{$out_info->{'Parameters'}}) {
            if (length($newsi->{'Params'}{$_})>0) {
              # Check for comma separated values
              my @values = split(',',$newsi->{'Params'}{$_});
              my $value = $newsi->{'Params'}{$_};
              if (@values > 1) {
                  @multiple = (@multiple, $_);
                  if (($newsi->{'Mode'} == 0 && $newsi->{'NScan'} < 2)
                   || $newsi->{'Mode'} == 1) {
                   # compute mean value if range not applicable
                    my $j;
                    my $meanvalue=0;
                    for ($j=0; $j<@values; $j++) {
                        $meanvalue = $values[$j];
                    }
                    $meanvalue = $meanvalue / @values;
                    $value = $meanvalue;
                  }
              }
              push @command, "$_=$value";
            } else {
                push @unset, $_;
            }
        }
        if (@unset>0) {
            $w->messageBox(-message =>
                           "Unset parameter(s):\n\n@unset\n\nPlease fill all fields!",
                           -title => "Unset parameters!",
                           -type => 'OK',
                           -icon => 'error');
            return;
        }
        if (@multiple > 0 && (($newsi->{'Mode'} == 0 && $newsi->{'NScan'} < 2)
                   || $newsi->{'Mode'} == 1) ) {
            $w->messageBox(-message =>
                                "Scan range(s) not applicable. Mean value substituted for parameter(s):\n\n@multiple",
                                -title => "No scan here!",
                                -type => 'OK',
                                -icon => 'info');
        }
        if (@multiple eq 0 && $newsi->{'NScan'} > 1) {
            if ($newsi->{'Mode'} == 0) {
              $w->messageBox(-message =>
                      "No scan range(s) given! Performing single simulation",
                      -title => "No scan here!",
                      -type => 'OK',
                      -icon => 'info');
              $newsi->{'NScan'} = 0;
            } elsif ($newsi->{'Mode'} == 2) {
              $w->messageBox(-message =>
                      "No optimization range(s) given! ",
                      -title => "No range here!",
                      -type => 'OK',
                      -icon => 'error');
              return;
            }
        }
        if ($newsi->{'gravity'} eq 1 && !$newsi->{'Mode'}) {
            if ($newsi->{'GravityWarn'} eq 0) {
              $w->messageBox(-message =>
                      "Only use --gravitation with components that support this!",
                      -title => "BEWARE!",
                      -type => 'OK',
                      -icon => 'warning');
              $newsi->{'GravityWarn'} = 1;
            }
            push @command, "--gravitation";
        }
        if (($newsi->{'Mode'} == 0 && $newsi->{'NScan'} > 1)
         || $newsi->{'Mode'} == 2) {
          push @command, "-N$newsi->{'NScan'}";
        }

	my $tmpfile;
	if ($newsi->{'Detach'} == 1) { # Background simulations using 'at'

	    # Create temporary file
	    my $fid;
	    if ($MCSTAS::mcstas_config{'TEMP'} ne "no") {
        require File::Temp;
        ($fid, $tmpfile) = File::Temp::tempfile("McXtrace_XXXX", SUFFIX => '.sh');
        if (not defined $fid) { $tmpfile=""; }
      }
      if ($tmpfile eq "") {
        $tmpfile="McXtrace_000000.sh";
        $fid = new FileHandle "> $tmpfile";
      }
      if (not defined $fid) { die "Could not open temporary McXtrace file $tmpfile\n"; }

	    # Write to temporary file
	    print $fid "#!/bin/sh\n";
	    print $fid "#\n# This is a temporary shell script to ";
	    print $fid "run a McXtrace simulation detached\n# from the GUI";
	    print $fid "\n#\n# Will be removed shortly.\n#\n";

	    my $cmd = join(" ", @command);
	    my $date = localtime(time());
	    my $logfile = "${out_name}_${date}.log";
	    $logfile =~ s!\ !_!g;
	    print $fid "$cmd > $logfile 2>&1 ";

	    # Close; set execute mode
	    close($fid);
	    $fid = open(READ, "chmod a+x $tmpfile|");
	    close($fid);
	    $cmdwin->insert('end', "\nExecuting background job\n$cmd\n(logfile $logfile)\n\n",'');

	    # Clear @command, things to do are now in $tmpfile
	    @command = ($MCSTAS::mcstas_config{'AT'}, "-f", $tmpfile, "now");
	}
        $inf_sim->{'Mode'}     = $newsi->{'Mode'};
        my $inittext = "Running simulation '$out_name' ...\n" .
            join(" ", @command) . "\n";
        my $success = my_system $w, $inittext, @command;
        $inf_sim=$newsi;
        return unless $success;
        my $ext;
        if ($plotter =~ /PGPLOT|McStas|McXtrace|Matlab|Gnuplot/i) { $ext="sim"; }
        elsif ($plotter =~ /HTML/i)       { $ext="html"; }
        elsif ($plotter =~ /NeXus|HDF/i)  { $ext="nxs"; }
        $current_sim_file = $newsi->{'Dir'} ?
            "$newsi->{'Dir'}/mccode.$ext" :
            "mccode.$ext";
        new_simulation_results($w);
        # In case of non-PGPLOT plotter, we can not read the data from disk.
        # Instead, we simply keep $newsi information in $inf_sim
        if ($plotter !~ /PGPLOT|McStas|Matlab|Gnuplot/i) {
            read_sim_data($w);
        } else {
            $inf_sim=$newsi;
        }
        $inf_sim->{'Autoplot'} = $newsi->{'Autoplot'};
        $inf_sim->{'Mode'}     = $newsi->{'Mode'};
        $MCSTAS::mcstas_config{'CLUSTER'}  = $newsi->{'cluster'};

        if ($newsi->{'Autoplot'}) { # Is beeing set to 0 above if Win32 + trace
           plot_dialog($w, $inf_instr, $inf_sim, $inf_data,
                       $current_sim_file);
        }

	if ($newsi->{'Detach'}) { # Clean up after background simulation
	 sleep(1);
	 unlink($tmpfile)
#	    my $fid = open(READ,"rm -f $tmpfile|");
#	    close($fid);
	}

      }
  }

sub menu_plot_results {
    my ($w) = @_;
    unless($current_sim_file) {
        my $ret = load_sim_file($w);
        return 0 unless $ret && -e $current_sim_file;
    }
    plot_dialog($w, $inf_instr, $inf_sim, $inf_data, $current_sim_file);
    return 1;
}

sub menu_preferences {
    # sub for selection of mcdisplay "backend".
    # Default read from $MCSTAS::mcstas_config{'PLOTTER'}
    # PW 20030314
    # Added entry for selection of internal editor
    # PW 20040527
    my ($w) = @_;
    my $ret;
    our $MPIstuff = $MCSTAS::mcstas_config{'CLUSTER'};
    ($ret) = preferences_dialog($w); #
    $MCSTAS::mcstas_config{'CLUSTER'} = $MPIstuff;
}


sub menu_read_sim_file {
    my ($w) = @_;
    load_sim_file($w);
    menu_plot_results($w);
}


# Build the text (McXtrace metalanguage) representation of a component
# using data fillied in by the user.
sub make_comp_inst {
    my ($cdata, $r) = @_;
    my ($p, $s);
    $s = "\n";
    $s .= "COMPONENT $r->{'INSTANCE'} = $r->{'DEFINITION'}(\n";
    my @ps = ();
    my $col = "";
    for $p (@{$cdata->{'inputpar'}}) {
        my $add;
        if(defined($r->{'VALUE'}{$p}) && $r->{'VALUE'}{$p} !~ /^\s*$/) {
          if(defined($cdata->{'parhelp'}{$p}{'type'})) {
          if (($cdata->{'parhelp'}{$p}{'type'} eq "string" ||
               $cdata->{'parhelp'}{$p}{'type'} =~ /char/) &&
               $quote == 1 &&
                $r->{'VALUE'}{$p} !~ /\".*\"/ &&
                $r->{'VALUE'}{$p} !~ /\'.*\'/) {
                  # Firstly, remove existing quotes :)
                  $r->{'VALUE'}{$p} =~ s!\"!!g;
                  $r->{'VALUE'}{$p} =~ s!\'!!g;
                  # Next, add quotes...
                  $r->{'VALUE'}{$p} = "\"$r->{'VALUE'}{$p}\"";
                }
          }
          $add .= "$p = $r->{'VALUE'}{$p}";
        } elsif(defined($cdata->{'parhelp'}{$p}{'default'})) {
            next;                # Omit non-specified default parameter
        } else {
            $add.= "$p = ";
        }
        if(length($col) > 0) {
            if(length("$col, $add") > 60) {
                push @ps, $col;
                $col = $add;
            } else {
                $col = "$col, $add";
            }
        } else {
            $col = $add;
        }
    }
    push @ps, $col if length($col) > 0;
    $s .= "    " . join(",\n    ", @ps) . ")\n";
    $s .= "  AT (".  $r->{'AT'}{'x'} . ", " . $r->{'AT'}{'y'} . ", " .
        $r->{'AT'}{'z'} . ") RELATIVE " . $r->{'AT'}{'relative'} . "\n";
    $s .= "  ROTATED (" . $r->{'ROTATED'}{'x'} . ", " . $r->{'ROTATED'}{'y'} .
        ", " . $r->{'ROTATED'}{'z'} . ") RELATIVE " .
            $r->{'ROTATED'}{'relative'} . "\n"
                if($r->{'ROTATED'}{'x'} || $r->{'ROTATED'}{'y'} ||
                   $r->{'ROTATED'}{'z'} || $r->{'ROTATED'}{'relative'});
    return $s;
}

# The text for the instrument template.
my $instr_template_start = <<INSTR_FINISH;
/*******************************************************************************
*         McXtrace instrument definition URL=http://www.mcxtrace.org
*
* Instrument: test (rename also the example and DEFINE lines below)
*
* %Identification
* Written by: Your name (email)
* Date: Current Date
* Origin: Your institution
* Release: McXtrace 1.0
* Version: 0.2
* %INSTRUMENT_SITE: Institution_name_as_a_single word
*
* Instrument short description
*
* %Description
* Instrument longer description (type, elements, usage...)
*
* Example: test.instr <parameters=values>
*
* %Parameters
* Par1: [unit] Parameter1 description
*
* %Link
* A reference/HTML link for more information
*
* %End
*******************************************************************************/

/* Change name of instrument and input parameters with default values */
DEFINE INSTRUMENT test(Par1=1)

/* The DECLARE section allows us to declare variables or  small      */
/* functions in C syntax. These may be used in the whole instrument. */
DECLARE
%{
%}

/* The INITIALIZE section is executed when the simulation starts     */
/* (C code). You may use them as component parameter values.         */
INITIALIZE
%{
%}

/* Here comes the TRACE section, where the actual      */
/* instrument is defined as a sequence of components.  */
TRACE

/* The Arm() class component defines reference points and orientations  */
/* in 3D space. Every component instance must have a unique name. Here, */
/* Origin is used. This Arm() component is set to define the origin of  */
/* our global coordinate system (AT (0,0,0) ABSOLUTE). It may be used   */
/* for further RELATIVE reference, Other useful keywords are : ROTATED  */
/* EXTEND GROUP PREVIOUS. Also think about adding an xray source !    */
/* Progress_bar is an Arm displaying simulation progress.               */
COMPONENT Origin = Progress_bar()
  AT (0,0,0) ABSOLUTE
INSTR_FINISH
my $instr_template_end = <<INSTR_FINISH;

/* This section is executed when the simulation ends (C code). Other    */
/* optional sections are : SAVE                                         */
FINALLY
%{
%}
/* The END token marks the instrument definition end */
END
INSTR_FINISH

sub menu_insert_instr_template {
    if($edit_control) {
        $edit_control->insert('1.0', $instr_template_start);
        # Save the current cursor position so that we can move it to
        # before the last part of the template if necessary.
        my $currentpos = $edit_control->index('insert');
        $edit_control->insert('end', $instr_template_end);
        $edit_control->markSet('insert', $currentpos);
        if (not $current_sim_def) {
          $edit_window->title("Edit: Insert components in TRACE and save your instrument");
        }
    }
}

sub menu_insert_file {
    my ($w) = @_;
    if($edit_control) {
	my $file = $w->getOpenFile(-title => "Select file to insert", -initialdir => getcwd());
	return 0 unless $file;
	my $fid = open(FILE, "<$file");
	my $input;
	while (<FILE>) {
	    $input = "$input$_";
	}
	$edit_control->see('insert');
	$edit_control->insert('insert', $input);
	$edit_control->see('insert');
    }
}

# Allow the user to populate a given component definition in a dialog
# window, and produce a corresponding component instance.
sub menu_insert_x {
    my ($w, $path) = @_;
    my $cdata = fetch_comp_info($path, $compinfo);

    my $r = comp_instance_dialog($w, $cdata);
    return undef unless $r;
    die "No values given" unless $r;

    if($edit_control) {
        $edit_control->see('insert');
        $edit_control->insert('insert', make_comp_inst($cdata, $r));
        $edit_control->see('insert');
    }
    return 1;
}

# Choose a component definition from a list in a dialog window, and
# then allow the user to populate it in another dialog.
sub menu_insert_component {
    my ($w) = @_;

    my $comp = comp_select_dialog($w, \@compdefs, $compinfo);
    return undef unless $comp;
    return menu_insert_x($w, $comp);
}

# Directories containing component definitions.
# MOD: E. Farhi, Oct 2nd, 2001: add obsolete dir. Aug 27th, 2002: contrib
my @comp_sources =
    (["Source", ["$MCSTAS::sys_dir/sources"]],
     ["Optics", ["$MCSTAS::sys_dir/optics"]],
     ["Sample", ["$MCSTAS::sys_dir/samples"]],
     ["Monitor", ["$MCSTAS::sys_dir/monitors"]],
     ["Misc", ["$MCSTAS::sys_dir/misc"]],
     ["Contrib", ["$MCSTAS::sys_dir/contrib"]],
     ["Obsolete", ["$MCSTAS::sys_dir/obsolete"]],
     ["Other", ["$MCSTAS::sys_dir", "."]]);

# Fill out the menu for building component instances.
sub make_insert_menu {
    my ($w, $menu) = @_;
    @compdefs = ();
    my @menudefs = ();
    my ($sec,$dir);
    for $sec (@comp_sources) {
        my $sl = [$sec->[0], []];
        for $dir (@{$sec->[1]}) {
            if(opendir(DIR, $dir)) {
                my @comps = readdir(DIR);
                closedir DIR;
                next unless @comps;
                my @paths = map("$dir/$_", grep(/\.(comp|cmp|com)$/, @comps));
                @paths = sort @paths;
                push(@compdefs, @paths);
                push(@{$sl->[1]}, map([compname($_), $_], @paths));
            }
        }
        push @menudefs, $sl;
    }
    $menu->command(-label => "Instrument template",
                   -command => sub { menu_insert_instr_template($w) },
                   -underline => 0);
    $menu->command(-label => "File ...",
                   -command => sub { menu_insert_file($w) },
                   -underline => 0);
    $menu->command(-label => "Component ...",
                   -accelerator =>  $shortcuts{'insertcomp'} ,
                   -command => sub { menu_insert_component($w) },
                   -underline => 0);
    $w->bind($shortcuts{'insertcomp'} => sub { menu_insert_component($w) });
    # Now build all the menu entries for direct selection of component
    # definitions.
    my $p;
    for $p (@menudefs) {        # $p holds title and component list
        my $m2 = $menu->cascade(-label => $p->[0]);
        my $c;
        for $c (@{$p->[1]}) {        # $c holds name and path
            $m2->command(-label => "$c->[0] ...",
                         -command => sub { menu_insert_x($w, $c->[1]) });
        }
    }
    $menu->pack(-side=>'left');
}

sub setup_menu {
    my ($w) = @_;
    my $menu = $w->Frame(-relief => 'raised', -borderwidth => 2);
    $menu->pack(-fill => 'x');
    my $filemenu = $menu->Menubutton(-text => 'File', -underline => 0);
    $filemenu->command(-label => 'Open instrument ...',
                       -accelerator =>  $shortcuts{'menuopen'} ,
                       -command => [\&menu_open, $w],
                       -underline => 0);
    $w->bind( $shortcuts{'menuopen'}  => [\&menu_open, $w]);
    $filemenu->command(-label => 'Edit current/New...',
                       -underline => 0,
                       -command => \&menu_edit_current);
    $filemenu->command(-label => 'Edit current/New (detached)',
                       -underline => 0,
                       -command => sub { menu_spawn_internal_editor($w) } );
    if($external_editor) {
        my $shortname = (split " ", $external_editor)[0];
        $shortname = (split "/", $shortname)[-1];
        $filemenu->command(-label => 'Spawn editor "' . $shortname . '"',
                           -command => sub { menu_spawn_editor($w) } );
    }
    $filemenu->command(-label => 'Compile instrument',
                       -underline => 0,
                       -command => sub {menu_compile($w)});
    $filemenu->command(-label => 'Save output/Log file...',
                       -underline => 1,
                       -command => sub { setup_cmdwin_saveas($w) });
    $filemenu->command(-label => 'Clear output',
                       -underline => 1,
                       -command => sub { $cmdwin->delete("1.0", "end") });
    $filemenu->separator;
    $filemenu->command(-label => 'Preferences...',
                      -underline => 1,
                      -accelerator =>  $shortcuts{'menuprefs'} ,
                      -command => sub {menu_preferences($w);});
    $w->bind( $shortcuts{'menuprefs'}  => [\&menu_preferences, $w]);
    $filemenu->command(-label => 'Save configuration...',
                       -underline => 2,
                       -command => sub {menu_save_config($w)});
    $filemenu->separator;
    $filemenu->command(-label => 'Quit',
                       -underline => 0,
                       -accelerator =>  $shortcuts{'menuquit'} ,
                       -command => \&menu_quit);
    $w->bind( $shortcuts{'menuquit'}  => \&menu_quit);
    $filemenu->pack(-side=>'left');
    my $simmenu = $menu->Menubutton(-text => 'Simulation', -underline => 2);
    $simmenu->command(-label => 'Read old simulation ...',
                       -underline => 0,
                       -command => sub { menu_read_sim_file($w) });
    $simmenu->separator;
    $simmenu->command(-label => 'Run simulation...',
                      -underline => 1,
                      -accelerator =>  $shortcuts{'menurun'} ,
                      -command => sub {menu_run_simulation($w);});
    $w->bind( $shortcuts{'menurun'}  => [\&menu_run_simulation, $w]);
    $simmenu->command(-label => 'Plot results...',
                      -underline => 0,
                      -accelerator =>  $shortcuts{'menuplot'} ,
                      -command => sub {menu_plot_results($w);});
    $w->bind( $shortcuts{'insertcomp'}  => [\&menu_plot_results, $w]);

    $simmenu->pack(-side=>'left');

    sitemenu_build($w,$menu);

    my $toolmenu = $menu->Menubutton(-text => 'Tools', -underline => 0);

    $toolmenu->command(-label => 'Plot current results',
		       -command => sub {menu_plot_results($w);});
    $toolmenu->command(-label => 'Plot other results',
		       -command => sub {menu_spawn_mcplot($w);});
    $toolmenu->command(-label => 'Online plotting of results',
		       -command => sub {menu_spawn_mcdaemon($w);});
    $toolmenu->command(-label => 'Dataset convert/merge',
		       -command => sub {menu_spawn_mcformatgui($w,$current_sim_file);});
    $toolmenu->pack(-side=>'left');
    $toolmenu->command(-label => 'mcgui Shorcut keys',
                       -command => sub {tools_shortcuts($w)});
    # The following item for now only applies to non-Win32 systems...
    if (!($Config{'osname'} eq 'MSWin32')) {
	$toolmenu->command(-label => 'Activate MPI/grid (DSA key)',
			   -command => sub {tools_dsa($w)});
    }

    my $helpmenu = $menu->Menubutton(-text => "Help ($MCSTAS::mcstas_config{'DOCCMD'})", -underline => 0);

    $helpmenu->command(-label => 'McXtrace User manual',
                       -command => sub {mcdoc_manual()});
    $helpmenu->command(-label => 'McXtrace Component manual',
                       -command => sub {mcdoc_compman()});
    $helpmenu->command(-label => 'Component Library index',
                       -command => sub {mcdoc_components()});
    $helpmenu->separator;
    $helpmenu->command(-label => 'McXtrace web page (web)',
                       -underline => 7,
                       -command => sub {mcdoc_web()});
    $helpmenu->command(-label => 'Current instrument info',
                       -command => sub {mcdoc_current()});
    $helpmenu->separator;
    $helpmenu->command(-label => 'Test McXtrace installation',
                       -command => sub {mcdoc_test($w)});
    $helpmenu->command(-label => 'Generate component index',
                       -command => sub {mcdoc_generate()});
    $helpmenu->command(-label => 'About McXtrace',
                       -command => sub {mcdoc_about($w)});
    $helpmenu->pack(-side=>'right');
}

sub setup_cmdwin {
    my ($w) = @_;
    my $f2 = $w->Frame();
    $b = $w->Balloon(-state => 'balloon');
    $f2->pack(-fill => 'x');
    my $instr_lab = $f2->Label(-text => "Instrument file: <None>",
                               -anchor => 'w',
                               -justify => 'left',-fg => 'red');
    $instr_lab->pack(-side => 'left');
    my $instr_run = $f2->Button(-text => "Run", -fg => 'blue',
                                -command => sub { menu_run_simulation($w) });
    $instr_run->pack(-side => "right", -padx => 1, -pady => 1);
    $b->attach($instr_run, -balloonmsg => "Compile and Run current instrument");
    my $instr_but = $f2->Button(-text => "Edit/New",
                                -command => \&menu_edit_current);
    $instr_but->pack(-side => "right", -padx => 1, -pady => 1);
    $b->attach($instr_but, -balloonmsg => "Edit current instrument description\nor create a new one from a Template");
    my $f3 = $w->Frame();
    $f3->pack(-fill => 'x');
    my $res_lab = $f3->Label(-text => "Simulation results: <None>",
                             -anchor => 'w',
                             -justify => 'left');
    $res_lab->pack(-side => 'left');
    my $plot_but = $f3->Button(-text => "Plot",
                                -command => sub {menu_plot_results($w);});
    $plot_but->pack(-side => "right", -padx => 1, -pady => 1);
    $b->attach($plot_but, -balloonmsg => "Plot last simulation results");
    my $sim_but = $f3->Button(-text => "Read",
                                -command => sub { menu_read_sim_file($w) });
    $sim_but->pack(-side => "right", -padx => 1, -pady => 1);
    $b->attach($sim_but, -balloonmsg => "Open previous simulation results");
    my $f4 = $w->Frame();
    my $status_lab = $f4->Label(-text => "Status: Ok",
                                -anchor => 'w',
                                -justify => 'left');
    my $spacer = $f4->Label(-text => " in ",
                                -anchor => 'w',
                                -justify => 'left');
    my $dir_but = $f4->Button(-text => "<-- Work dir.",
    			      -command => sub { set_run_dir($w) });

    $status_lab->pack(-side => 'left');
    $spacer->pack(-side => 'left');

    my $dirbox = $f4->ROText(-relief => 'sunken', -bd => '0',
     			     -setgrid => 'true',
     			     -height => 1);
    $dir_but->pack(-side => 'right'); 
    $dirbox->pack(-expand => 'yes', -fill => 'x', -side => 'right');
  
    $workdir = $dirbox;
    set_workdir($w, getcwd());
    $f4->pack(-fill => 'x');     


    # Add the main text field, with scroll bar
    my $rotext = $w->ROText(-relief => 'sunken', -bd => '2',
                            -setgrid => 'true',
                            -height => 24, -width => 80);
    my $s = $w->Scrollbar(-command => [$rotext, 'yview']);
    $rotext->configure(-yscrollcommand =>  [$s, 'set']);
    $s->pack(-side => 'right', -fill => 'y');
    $rotext->pack(-expand => 'yes', -fill => 'both');
    $rotext->mark('set', 'insert', '0.0');
    $rotext->tagConfigure('msg', -foreground => 'blue');
    $current_instr_label   = $instr_lab;
    $current_results_label = $res_lab;
    $status_label = $status_lab;
    $cmdwin       = $rotext;


    # Insert "mcstas --version" message in window. Do it a line at the
    # time, since otherwise the tags mechanism seems to get confused.
    my $l;
    for $l (split "\n", `$MCSTAS::mcstas_config{'MCCODE'} --version`) {
        $cmdwin->insert('end', "$l\n", 'msg');
    }

    my $text="";
    if ($MCSTAS::mcstas_config{'MATLAB'} ne "no")   { $text .= "Matlab "; }
    if ($MCSTAS::mcstas_config{'PGPLOT'} ne "no")   { $text .= "PGPLOT/McXtrace "; }
    if ($MCSTAS::mcstas_config{'GNUPLOT'} ne "no")   { $text .= "Gnuplot "; }
    if ($MCSTAS::mcstas_config{'BROWSER'} ne "no")  { $text .= "HTML "; }
    if ($MCSTAS::mcstas_config{'VRMLVIEW'} ne "no") { $text .= "VRML "; }
    if ($MCSTAS::mcstas_config{'NEXUS'} ne "") { $text .= "NeXus "; }
    if ($text ne "") { $cmdwin->insert('end', "Plotters: $text\n"); }

    if ($MCSTAS::mcstas_config{'HOSTFILE'} eq "" &&
          ($MCSTAS::mcstas_config{'MPIRUN'} ne "no"
        ||  $MCSTAS::mcstas_config{'SSH'} ne "no") ) {
      $cmdwin->insert('end',
"Warning: No MPI/grid machine list. Running locally.
  Define ".$ENV{"HOME"}."/.".$MCSTAS::mcstas_config{'MCCODE'}."/hosts
  or $MCSTAS::sys_dir/tools/perl/hosts
  or use option --machines=<file>\n");
    }
    my $text_grid="Single ";
    if ($MCSTAS::mcstas_config{'MPIRUN'} ne "no")  { $text_grid .= "MPI "; }
    if ($MCSTAS::mcstas_config{'SSH'} ne "no")     { $text_grid .= "Grid "; }
    if ($text_grid ne "") { $cmdwin->insert('end', "Clustering methods: $text_grid\n"); }
    if (($MCSTAS::mcstas_config{'MPIRUN'} ne "no" || $MCSTAS::mcstas_config{'SSH'} ne "no")
        && $Config{'osname'} ne 'MSWin32' && (not -e "$ENV{'HOME'}/.ssh/id_dsa")) {
      # Suggest to create DSA key for local MPI execution.
      putmsg($cmdwin, "Your system has MPI/SSH parallelisation available. To make use of this, \n".
	     "  please go to the Tool menu and select 'Install DSA key'.\n", 'msg');
    }
    $w->protocol("WM_DELETE_WINDOW" => sub { editor_quit($w);});
}

# save command output into LOG file
sub setup_cmdwin_saveas {
  my ($w) = @_;
  my $file;
  my $date = localtime(time());
  if($current_sim_def) {
      my ($inidir, $inifile);
      if($current_sim_def =~ m!^(.*)/([^/]*)$!) {
          ($inidir, $inifile) = ($1, $2);
      } else {
          ($inidir, $inifile) = ("", $current_sim_def);
      }
      $inifile =~ s/\.instr$//;
      $inifile.= "_${date}.log";
      $inifile =~ s!\ !_!g;
      $file = $w->getSaveFile(-defaultextension => ".log",
                              -title => "Select LOG output file name",
                              -initialdir => $inidir,
                              -initialfile => $inifile);
  } else {
      $file = $w->getSaveFile(-defaultextension => ".log",
                              -title => "Select LOG output file name");
  }
  return 0 unless $file;
  my $outputtext = $cmdwin->get('1.0', 'end');
  putmsg($cmdwin, "Save log file $file\n");
  open(MCLOG,">>$file");
  print MCLOG "# Log file $file generated by McXtrace/mcgui\n";
  print MCLOG "# Date: $date\n";
  print MCLOG "# Current instrument: $current_sim_def\n";
  print MCLOG "# Current results: $current_sim_file\n";
  print MCLOG "$outputtext";
  close(MCLOG);
  return 1;
}

sub editor_quit {
    my ($w) = @_;
    if(is_erase_ok($w)) {
        $w->destroy;
        $edit_window = undef;
        $edit_control = undef;
    }
}

sub Tk::CodeText::selectionModify {
        my ($cw, $char, $mode) = @_;
        my @ranges = $cw->tagRanges('sel');
        my $charlength = length($char);
        if (@ranges >= 2) {
                my $start = $cw->index($ranges[0]);
                my $end = $cw->index($ranges[1]);
                my $firststart = $start;
                while ($cw->compare($start, "<=", $end)) {
                        if ($mode) {
                            if ($cw->get("$start linestart", "$start linestart + $charlength chars") eq $char) {
                                        $cw->delete("$start linestart", "$start linestart + $charlength chars");
                                }
                        } else {
                            $cw->insert("$start linestart", $char);
                            }
                        $start = $cw->index("$start + 1 lines");
                    }
                if (!$mode) {
                    @ranges = $cw->tagRanges('sel');
                    @ranges = ($firststart, $ranges[@ranges-1]);
                }
                $cw->tagAdd('sel', @ranges);
            }
}

sub setup_edit_1_7 {
    # BEWARE: The code in this sub is from McStas version 1.7,
    # added only for those users unable to use the CodeText
    # based highlighting editor below. Other features are
    # also missing.
    my ($mw, $w) = @_;
    # Create the editor window.
    my $e;
    # Create the editor menus.
    my $menu = $w->Frame(-relief => 'raised', -borderwidth => 2);
    $menu->pack(-fill => 'x');
    my $filemenu = $menu->Menubutton(-text => 'File', -underline => 0);
    $filemenu->command(-label => 'New instrument',
                       -command => [\&menu_new, $w],
                       -underline => 0);
    $filemenu->command(-label => 'Save instrument',
                       -accelerator =>  $shortcuts{'menusave'} ,
                       -command => [\&menu_save, $w],
                       -underline => 0);
    $w->bind( $shortcuts{'menusave'}  => [\&menu_save, $w]);
    $filemenu->command(-label => 'Save instrument as ...',
                       -underline => 16,
                       -command => sub {menu_saveas($w)});
    $filemenu->separator;
    $filemenu->command(-label => 'Close',
                       -underline => 0,
                       -accelerator =>  $shortcuts{'menuclose'} ,
                       -command => sub { editor_quit($w) } );
    $w->bind( $shortcuts{'menuclose'}  => sub { editor_quit($w) } );
    $filemenu->pack(-side=>'left');
    my $editmenu = $menu->Menubutton(-text => 'Edit', -underline => 0);
    $editmenu->command(-label => 'Undo',
                       -accelerator => 'Ctrl+Z',
                       -command => [\&menu_undo, $w], -underline => 0);
    $w->bind('<Control-z>' => [\&menu_undo, $w]);
    $editmenu->separator;
    $editmenu->command(-label => 'Cut',
                       -accelerator => $shortcuts{'cut'} ,
                       -command => sub { $e->clipboardCut(); },
                       -underline => 0);
    $editmenu->command(-label => 'Copy',
                       -accelerator =>  $shortcuts{'copy'} ,
                       -command => sub { $e->clipboardCopy(); },
                       -underline => 1);
    $editmenu->command(-label => 'Paste',
                       -accelerator =>  $shortcuts{'paste'} ,
                       -command => sub { $e->clipboardPaste(); },
                       -underline => 0);
    $editmenu->pack(-side=>'left');
    my $insert_menu = $menu->Menubutton(-text => 'Insert', -underline => 0);
    make_insert_menu($w, $insert_menu);

    # Create the editor text widget.
    $e = $w->TextUndo(-relief => 'sunken', -bd => '2', -setgrid => 'true',
                      -height => 24);
    my $s = $w->Scrollbar(-command => [$e, 'yview']);
    $e->configure(-yscrollcommand =>  [$s, 'set']);
    $s->pack(-side => 'right', -fill => 'y');
    $e->pack(-expand => 'yes', -fill => 'both');
    $e->mark('set', 'insert', '0.0');
    $e->Load($current_sim_def) if $current_sim_def && -r $current_sim_def;
    $w->protocol("WM_DELETE_WINDOW" => sub { editor_quit($w);
					     if ( $kill_when_editor_exits == 1) {
						 $mw->destroy();
					     }
					 } );
    $edit_control = $e;
    $edit_window = $w;
    if ($current_sim_def) {
      $w->title("Edit: $current_sim_def");
      if (-r $current_sim_def) {
          $e->Load($current_sim_def);
      }
    } else {
      $w->title("Edit: Start with Insert/Instrument template");
    }
}

sub setup_edit {
    my ($mw,$w) = @_;
    # Create the editor window.
    my $e;
    # Create the editor text widget.
    require Tk::CodeText;
    require Tk::CodeText::McStas;
    $e = $w->Scrolled('CodeText',-relief => 'sunken', -bd => '2', -setgrid => 'true',
                      -height => 24, wrap => 'none', -scrollbars =>'se',
                      -commentchar => '// ', -indentchar => "  ", -updatecall => \&update_line, -syntax => 'McStas');
    my $menu = $e->menu;
    $w->bind('<F5>' => [\&Tk::CodeText::selectionIndent]);
    $w->bind('<F6>' => [\&Tk::CodeText::selectionUnIndent]);
    $w->bind('<F7>' => [\&Tk::CodeText::selectionComment]);
    $w->bind('<F8>' => [\&Tk::CodeText::selectionUnComment]);
    $w->configure(-menu => $menu);
    my $insert_menu = $menu->Menubutton(-text => 'Insert',  -underline => 0, -tearoff => 0);
    # This is only done for backward compatibility - we want to use Alt+s for saving...
    my $filemenu = $menu->Menubutton(-text => 'Search', -underline => 1);
    $w->bind( $shortcuts{'menusave'}  => [\&menu_save, $w]);
    make_insert_menu($w, $insert_menu);
    my $label = $w->Label(-bd => '1', -text => 'Current line: 1');
    $e->pack(-expand => 'yes', -fill => 'both');
    $label->pack(-side => 'left', -expand => 'no', -fill => 'x');
    $e->mark('set', 'insert', '0.0');
    $w->protocol("WM_DELETE_WINDOW" => sub { editor_quit($w);
						 if ( $kill_when_editor_exits == 1) {
						     $mw->destroy();
						 }
					 } );
    $edit_control = $e;
    $edit_window = $w;
    $edit_label = $label;
    if ($current_sim_def) {
      $w->title("Edit: $current_sim_def");
      if (-r $current_sim_def) {
          $e->Load($current_sim_def);
      }
    } else {
      $w->title("Edit: Start with Insert/Instrument template");
    }
}



sub Tk::TextUndo::FileSaveAsPopup
{
 my ($w)=@_;
 menu_saveas($w);
}

sub Tk::TextUndo::FileLoadPopup
{
 my ($w)=@_;
 my $name = $w->CreateFileSelect('getOpenFile',-title => 'File Load');
 if (defined($name) && length($name)){
     open_instr_def($w, $name);
     return 1;
 }
 return 0;
}

# GUI callback function for updating line numbers etc.
sub update_line {
    if (defined($edit_control)) {
        my ($line,$col) = split(/\./,$edit_control->index('insert'));
        my ($last_line,$last_col) = split(/\./,$edit_control->index('end'));
        $last_line=$last_line-1;
        $edit_label->configure(-text => " Line: $line of $last_line total, Column: $col");
    }
}

# Check if simulation needs recompiling.
sub check_if_need_recompile {
    my ($simname) = @_;
    my $exename;
    if($simname =~ /^(.*)\.(instr|ins)$/) {
        $exename = $1;
    } else {
      $exename = "$simname.$MCSTAS::mcstas_config{'EXE'}";
    }
    return "not found" unless -f $exename;
    return "not executable" unless -x $exename;
    my @stat1 = stat($simname);
    my @stat2 = stat($exename);
    return "source is newer" unless $stat1[9] < $stat2[9];
    return "";
}

my $win = new MainWindow;

eval { # Try specified color palette...
  $win -> setPalette($MCSTAS::mcstas_config{'TKPALETTE'});
}; 
if ($@) { # or continue with system default if that failed.
  printf "Specified colorscheme '$MCSTAS::mcstas_config{'TKPALETTE'}' failed. Using system default.\n";
}

if (!($MCSTAS::mcstas_config{'TKFONT'} eq "")) { # Only try loading a font if non-empty string is defined
  eval { # Try loading specified font...
    $win->optionAdd("*font", $MCSTAS::mcstas_config{'TKFONT'});
    $win->optionAdd("*borderWidth", 1);
  };
  if ($@) { # or continue with system default if that failed
    printf "Specified font '$MCSTAS::mcstas_config{'TKFONT'}' failed. Using system default.\n";
  }
}

$main_window = $win;
setup_menu($win);
setup_cmdwin($win);

my $open_editor = 0;

if(@ARGV>0 && @ARGV<3) {
    # Check if one of the input arguments is '--open'
    # if so, start the editor of choice immediately
    my $j;
    my $filenames;
    for ($j=0; $j<@ARGV; $j++) {
        if ($ARGV[$j] eq "--help") {
          print "mxgui <instrument_file>\n";
          print "       [--help] Display command help\n";
          print "       [--open] Start with the editor opened\n";
          print "       [--edit] Edit instrument only\n";
          print "SEE ALSO: mcxtrace, mxdoc, mxplot, mxrun, mxgui\n";
          print "DOC:      Please visit http://www.mcxtrace.org\n";
          exit;
        } elsif ($ARGV[$j] eq "--open") {
            $open_editor = 1;
#            menu_edit_current($win);
        } elsif ($ARGV[$j] eq "--edit") {
	    $open_editor = 1;
	    $win->withdraw;
	    $kill_when_editor_exits = 1;
	} else {
            $filenames = "$ARGV[$j]";
        }
    }

    # Most likely, everything on the commandline is a filename... Join using
    # spaces, e.g. mcgui.pl My Documents\My Simulation.instr
    open_instr_def($win, $filenames);
    if ($open_editor == 1) {
        menu_edit_current($win);
    }
} else {
#    menu_open($win);
}


MainLoop;
