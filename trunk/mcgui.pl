#! /usr/bin/perl
#
# Implements graphical user interface for McStas
#
#
#   This file is part of the McStas neutron ray-trace simulation package
#   Copyright (C) 1997-2004, All rights reserved
#   Risoe National Laborartory, Roskilde, Denmark
#   Institut Laue Langevin, Grenoble, France
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
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
use POSIX;

# Determine the path to the McStas system directory. This must be done
# in the BEGIN block so that it can be used in a "use lib" statement
# afterwards.
BEGIN {
    if($ENV{"MCSTAS"}) {
        $MCSTAS::sys_dir = $ENV{"MCSTAS"};
    } 
    else {
      if ($Config{'osname'} eq 'MSWin32') {
        $MCSTAS::sys_dir = "c:\\mcstas\\lib";
        my $browser = $ENV{'BROWSER'};
        if (!$browser) {
            print STDERR "Your BROWSER variable is not set... Trying Win32 default\n";
            $MCSTAS::browser = "start";
        } 
      } else {
        $MCSTAS::sys_dir = "/usr/local/lib/mcstas";
        my $browser = $ENV{'BROWSER'};
        if (!$browser) {
            print STDERR "Your BROWSER variable is not set... Trying 'netscape'\n";
            $MCSTAS::browser = "netscape";
        } 
        
      }
    }
    $MCSTAS::perl_dir = "$MCSTAS::sys_dir/tools/perl"
  }
use lib $MCSTAS::perl_dir;

# Possibly, set BROWSER environment variable 
if ($MCSTAS::browser) {
    $ENV{'BROWSER'} = $MCSTAS::browser;
}

use strict;
use FileHandle;
use Tk;
use Tk::TextUndo;
use Tk::ROText;
use Tk::DialogBox;
use Tk::widgets qw(CodeText);

require "mcfrontlib.pl";
require "mcguilib.pl";
# Requirement for mcplotlib.pl removed, will be loaded only 
# if mcdisplay backend is used.
# PW, 20030314
# require "mcplotlib.pl";
require "mcrunlib.pl";

my $current_sim_file;
my ($inf_instr, $inf_sim, $inf_data);
my %inf_param_map;
my $current_sim_def;
my $main_window;
my ($edit_window, $edit_control, $edit_label);

my $external_editor;
my ($status_label, $current_results_label, $cmdwin, $current_instr_label);

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
        menu_save($w) if $ret eq "yes";
        return $ret eq "Cancel" ? 0 : 1;
    } else {
        return 1;
    }
}

sub is_erase_ok {
    my ($w) = @_;
    if($edit_control && $edit_control->numberChanges() > 0) {
        my $ret = $w->messageBox(-message => "Ok to loose changes?",
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
        $main_window->destroy;
    }
}

sub menu_edit_current {
    if($edit_control) {
        $edit_window->raise();
    } else {
        setup_edit($main_window);
    }
}

sub check_external_editor {
    $external_editor = $ENV{'VISUAL'} || $ENV{'EDITOR'};
    if ($external_editor eq "") {
      if ($Config{'osname'} eq 'MSWin32') { $external_editor = "notepad"; }
      else { $external_editor = "nedit"; }
    }
}

sub menu_spawn_editor {
    my ($w) = @_;
    my $cmd = $external_editor ? $external_editor : "vi";
    my $pid;
    # Must be handled differently on Win32 vs. unix platforms...
    if($Config{'osname'} eq "MSWin32") {
        if($current_sim_def) {
            system("$external_editor $current_sim_def");
        } else {
            system("$external_editor");
        }
    } else {
        $pid = fork();
        if(!defined($pid)) {
            $w->messageBox(-message =>
                           "Failed to spawn editor \"$external_editor\".",
                           -title => "Command failed",
                           -type => 'OK',
                           -icon => 'error');
            return 0;
        } elsif($pid > 0) {
            waitpid($pid, 0);
            return 1;
        } else {
            # Double fork to avoid having to wait() for the editor to
            # finish (or having it become a zombie). See man perlfunc.
            unless(fork()) {
                if($current_sim_def) {
                    exec($external_editor, $current_sim_def);
                } else {
                    exec($external_editor);
                }
                # If we get here, the exec() failed.
                print STDERR "Error: exec() of $external_editor failed!\n";
                POSIX::_exit(1);        # CORE:exit needed to avoid Perl/Tk failure.
            }
            POSIX::_exit(0);                # CORE:exit needed to avoid Perl/Tk failure.
        }
    }
}

sub mcdoc_web {
    my ($w) = @_;
    my $suffix='';
    my $cmd_suffix='';
    my $prefix='';
    if ($Config{'osname'} eq 'MSWin32') {
        $suffix='.pl';
        $prefix='start ';
    } else {
        $cmd_suffix=' &';
    }
    putmsg($cmdwin, "Opening Web Page: $prefix mcdoc$suffix --web $cmd_suffix\n", 'msg');
    system("$prefix mcdoc$suffix -s --web $cmd_suffix");
}

sub mcdoc_manual {
    my ($w) = @_;
    my $suffix='';
    my $cmd_suffix='';
    my $prefix='';
    if ($Config{'osname'} eq 'MSWin32') {
        $suffix='.pl';
        $prefix='start ';
    } else {
        $cmd_suffix=' &';
    }
    putmsg($cmdwin, "Opening User Manual: $prefix mcdoc$suffix --manual $cmd_suffix\n", 'msg');
    system("$prefix mcdoc$suffix -s --manual $cmd_suffix");
}

sub mcdoc_compman {
    my ($w) = @_;
    my $suffix='';
    my $cmd_suffix='';
    my $prefix='';
    if ($Config{'osname'} eq 'MSWin32') {
        $suffix='.pl';
        $prefix='start ';
    } else {
        $cmd_suffix=' &';
    }
    putmsg($cmdwin, "Opening Component Manual: $prefix mcdoc$suffix --manual $cmd_suffix\n", 'msg');
    system("$prefix mcdoc$suffix -s --comp $cmd_suffix");
}

sub mcdoc_tutorial {
    my ($w) = @_;
    my $suffix='';
    my $cmd_suffix='';
    my $prefix='';
    if ($Config{'osname'} eq 'MSWin32') {
        $suffix='.pl';
        $prefix='start ';
    } else {
        $cmd_suffix=' &';
    }
    putmsg($cmdwin, "Opening Tutorial: $prefix mcdoc$suffix --tutorial $cmd_suffix\n", 'msg');
    system("$prefix mcdoc$suffix -s --tutorial $cmd_suffix");
}

sub mcdoc_components {
    my ($w) = @_;
    my $suffix='';
    my $cmd_suffix='';
    my $prefix='';
    if ($Config{'osname'} eq 'MSWin32') {
        $suffix='.pl';
        $prefix='start ';
    } else {
        $cmd_suffix=' &';
    }
    putmsg($cmdwin, "Opening Library help: $prefix mcdoc$suffix --show $cmd_suffix\n", 'msg');
    system("$prefix mcdoc$suffix --show $cmd_suffix");
}

sub mcdoc_generate {
    my ($w) = @_;
    my $suffix='';
    my $cmd_suffix='';
    my $prefix='';
    if ($Config{'osname'} eq 'MSWin32') {
        $suffix='.pl';
        $prefix='start ';
    } else {
        $cmd_suffix=' &';
    }
    putmsg($cmdwin, "Generating Library help (local): $prefix mcdoc$suffix --force $cmd_suffix\n", 'msg');
    system("$prefix mcdoc$suffix --force $cmd_suffix");
}

sub mcdoc_test {
    my ($w) = @_;
    my $status;
    my $plotter_name;
    my $plotter = $MCSTAS::mcstas_config{'PLOTTER'};
    if ($plotter eq 1 || $plotter eq 2) { $plotter_name = "Matlab"; }
    elsif ($plotter eq 3 || $plotter eq 4) { $plotter_name = "Scilab"; }
    elsif ($plotter eq 0) { $plotter_name = "PGPLOT"; }
    if ($inf_sim->{'Binary'} == 1) { $plotter_name .= "_binary"; }
    my $printer = sub { putmsg($cmdwin, "$_[0]\n", 'msg'); $main_window->update;};
    $status = do_test($printer, 1, $plotter_name);
    if (defined $status) { putmsg($cmdwin, "$status", 'msg'); }
}

sub mcdoc_about {
  my ($w) = @_;
  my $version = `mcstas --version`;
  my $ret = $w->messageBox(-message => "This is the McStas Graphical User Interface\n
  McStas is a tool for Monte Carlo neutron scattering simulations\n
  It provides a complete set of tools, components, and example instruments\n
  $version\n
  Please visit <http://neutron.risoe.dk/mcstas/>",
                                 -title => "About McStas:McGUI",
                                 -type => 'OK',
                                 -icon => 'info',
                                 -default => 'cancel');
}


sub new_simulation_results {
    my ($w) = @_;
    my $text = $current_sim_file ? $current_sim_file : "<None>";
    $current_results_label->configure(-text => "Simulation results: $text");
}

sub new_sim_def_name {
    my ($w, $name) = @_;
    unless($current_sim_def && $name eq $current_sim_def) {
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
    $main_window->title("McStas: $current_sim_def");
    my $text = "Instrument file: " .
        ($current_sim_def ? $current_sim_def : "<None>");
    if ($current_sim_def && $edit_window) {
      $edit_window->title("Edit: $current_sim_def");
    }
    $current_instr_label->configure(-text => $text);
    # On Win32, doing a chdir is probably better at this point...
    if ($Config{'osname'} eq 'MSWin32') {
        chdir(dirname($current_sim_def));
    }
}

sub open_instr_def {
    my ($w, $file) = @_;
    $edit_control->Load($file) if $edit_control;
    new_sim_def_name($w, $file);
}

sub menu_open {
    my ($w) = @_;
    return 0 unless(is_erase_ok($w));
    my $file = $w->getOpenFile(-defaultextension => ".instr",
                               -title => "Select instrument file", -initialdir => "$ENV{'PWD'}");
    return 0 unless $file;
    open_instr_def($w, $file);
    return 1;        
}

sub menu_save {
    my ($w) = @_;
    if($current_sim_def) {
        $edit_control->Save($current_sim_def);
        $edit_window->title("Edit: $current_sim_def");
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
    $si->{'Binary'} = $inf_sim->{'Binary'};
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
                               -title => "Select simulation file", -initialdir => "$ENV{'PWD'}");
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
                               -title => "Select output filename", -initialdir => "$ENV{'PWD'}", -initialfile => "mcdisplay_output.$ext");
    return $file;
}

sub putmsg {
    my ($t, $m, $tag) = @_;
    $cmdwin->insert('end', $m, $tag);
    $cmdwin->see('end');
}

sub run_dialog_create {
    my ($w, $title, $text, $cancel_cmd) = @_;
    my $dlg = $w->Toplevel(-title => $title);
    $dlg->transient($dlg->Parent->toplevel);
    $dlg->withdraw;
    $dlg->protocol("WM_DELETE_WINDOW" => sub { } );
    # Add labels
    $dlg->Label(-text => $text,
                -anchor => 'w',
                -justify => 'left')->pack(-fill => 'x');
    my $bot_frame = $dlg->Frame(-relief => "raised", -bd => 1);
    $bot_frame->pack(-side => "top", -fill => "both",
                     -ipady => 3, -ipadx => 3);
    my $but = $bot_frame->Button(-text => "Cancel", -command => $cancel_cmd);
    $but->pack(-side => "left", -expand => 1, -padx => 1, -pady => 1);
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
        kill -15, $pid unless $state; # signal 15 is SIGTERM
    };
    my $dlg = run_dialog_create($w, "Running simulation",
                                "Simulation running ...", $cancel_cmd);
    putmsg($cmdwin, $inittext, 'msg'); # Must appear before any other output
    # Set up the pipe reader callback
    my $reader = sub {
        run_dialog_reader($w, $fh, $cmdwin, \$state, \$success);
    };
    $w->fileevent($fh, 'readable', $reader);
    $status_label->configure(-text => "Status: Running simulation");
    my $savefocus = run_dialog_popup($dlg);
    do {
        $w->waitVariable(\$state);
    } until $state;
    run_dialog_retract($dlg, $savefocus);
    my $status = close($fh);
    $status_label->configure(-text => "Status: Done");
    if(!$success || (! $status && ($? != 0 || $!))) {
        putmsg($cmdwin, "Simulation exited abnormally.\n", 'msg');
        return undef;
    } else {
        putmsg($cmdwin, "Simulation finished.\n", 'msg');
        return 1;
    }
}

sub dialog_get_out_file {
    my ($w, $file, $force) = @_;
    # The $state variable is set when the spawned command finishes.
    my ($state, $cmd_success);
    my $success = 0;
    my ($fh, $pid, $out_name);
    # Initialize the dialog.
    my $cancel_cmd = sub {
        kill -15, $pid if $pid && !$state; # signal 15 is SIGTERM
    };
    my $dlg = run_dialog_create($w, "Compiling simulation ...",
                                "Compiling simulation", $cancel_cmd);
    my $printer = sub { putmsg($cmdwin, "$_[0]\n", 'msg'); };
    # Set up the pipe reader callback
    $status_label->configure(-text => "Status: Compiling simulation");
    # The dialog isn't actually popped up unless/until a command is
    # run or an error occurs.
    my $savefocus;
    my ($compile_data, $msg) = get_out_file_init($file, $force);
    if(!$compile_data) {
        &$printer("Could not compile simulation:\n$msg");
    } else {
        $state = 0;
        for(;;) {
            my ($type, $val) = get_out_file_next($compile_data, $printer);
            if($type eq 'FINISHED') {
                $success = 1;
                $out_name = $val;
                last;
            } elsif($type eq 'RUN_CMD') {
                $savefocus = run_dialog_popup($dlg) unless $savefocus;
                my $fh = new FileHandle;
                # Open calls must be handled according to 
                # platform...
                # PW 20030314
                if ($Config{'osname'} eq 'MSWin32') {
                  $pid = open($fh, "@$val 2>&1 |");
                } else {
                  $pid = open($fh, "-|");
                }
                unless(defined($pid)) {
                    &$printer("Could not spawn command.");
                    last;
                }
                if($pid) {                # Parent
                    $state = 0; # Clear "command done" flag.
                    $cmd_success = 0;
                    my $reader = sub {
                        run_dialog_reader($w, $fh,
                                          $cmdwin, \$state, \$cmd_success);
                    };
                    $dlg->fileevent($fh, 'readable', $reader);
                    do {
                        $dlg->waitVariable(\$state);
                    } until $state;
                    my $ret = close($fh);
                    undef($pid);
                    unless($cmd_success && ($ret || ($? == 0 && ! $!))) {
                        &$printer("** Error exit **.");
                        last;
                    }
                } else {                        # Child
                    open(STDERR, ">&STDOUT") || die "Can't dup stdout";
                    # Make the child the process group leader, so that
                    # we can kill off any subprocesses it may have
                    # spawned when the user selects CANCEL.
                    setpgrp(0,0);
                    exec @$val if @$val; # The "if @$val" avoids a Perl warning.
                    # If we get here, the exec() failed.
                    print STDERR "Error: exec() of $val->[0] failed!\n";
                    POSIX::_exit(1);        # CORE:exit needed to avoid Perl/Tk failure.
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
    my $out_name = dialog_get_out_file($w, $current_sim_def, $force);
    unless($out_name && -x $out_name) {
        $w->messageBox(-message => "Could not compile simulation.",
                       -title => "Compile failed",
                       -type => 'OK',
                       -icon => 'error');
        return undef;
    }
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
    compile_instrument($w, 1);        # Force recompilation.
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
      $child_pid = open($fh, "@sysargs 2>&1 |");
    } else {
      $child_pid = open($fh, "-|");
    }
    unless(defined($child_pid)) {
        $w->messageBox(-message => "Could not run simulation.",
                       -title => "Run failed",
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
    my ($bt, $newsi) = simulation_dialog($w, $out_info, $inf_sim);
    if($bt eq 'Start') {
        my @command = ();
        my @mcplot_cmd = ();
        my $suffix='';
        # Check 'Plotter' setting
        my $plotter = $MCSTAS::mcstas_config{'PLOTTER'};
        # Check 'Trace' setting if a scan or trace is
        # requested
        if ($newsi->{'Trace'}) {
              # Here, a check is done for selected mcdisplay "backend"
            # Also, various stuff must be done differently on unix
            # type plaforms and on lovely Win32... :)
            # PW 20030314
            #
            # Check if this is Win32, call perl accordingly...
            if ($Config{'osname'} eq 'MSWin32') {
              if ($newsi->{'Trace'} eq 1 ) {
                # Win32 'start' command needed to background the process..
                push @command, "start";
                # Also, disable plotting of results after mcdisplay run...
                $newsi->{'Autoplot'}=0;
              }
              push @mcplot_cmd, "start";
              # Set $suffix to .pl
              $suffix='.pl';
            }
            if ($newsi->{'Trace'} eq 2) { # 'mcrun' mode
              push @command, "$MCSTAS::mcstas_config{'prefix'}mcrun$suffix";
              push @command, "-N$newsi->{'NScan'}" if $newsi->{'NScan'};
              push @command, "--multi" if $newsi->{'Multi'};
            } else { # 'mcrun' mode
              push @command, "$MCSTAS::mcstas_config{'prefix'}mcdisplay$suffix";
              if ($plotter eq 0) {
                push @command, "--plotter=PGPLOT";
                # Be sure to read mcplotlib.pl in this case...
                require "mcplotlib.pl";
                # Standard mcdisplay.pl with PGPLOT bindings
                # Make sure the PGPLOT server is already started. If the
                # PGPLOT server is not started, mcdisplay will start it,
                # and the server will keep the pipe to mcdisplay open
                # until the server exits, hanging mcgui.
                ensure_pgplot_xserv_started();
              }
              elsif ($plotter eq 1) {
                push @command, "-pMatlab";
              }
              elsif ($plotter eq 2) {
                push @command, "-pMatlab";
                my $output_file = save_disp_file($w,'m');
                if (!$output_file) {
                  putmsg($cmdwin, "Trace cancelled...\n");
                  return;
                }
                $output_file = "\"$output_file\"";
                push @command, "-f$output_file";
                
              }
              elsif ($plotter eq 3) {
                push @command, "-pScilab";
                # If this is Win32, make a check for # of neutron histories,
                # should be made small to avoid waiting a long time for 
                # mcdisplay...
                if ($Config{'osname'} eq "MSWin32") {
		    # Subtract 0 to make sure $num_histories is treated as a
		    # number...
                    my $num_histories = $newsi->{'Ncount'} - 0;
                    if ($num_histories >=1e3) {
                        my $break = $w->messageBox(-message => "$num_histories is a very large number of neutron histories when using Scilab on Win32.\nContinue?",
                       -title => "note",
                       -type => 'yesnocancel',
                       -icon => 'error',
                       -default => 'no');
			# Make first char lower case - default on 
			# Win32 upper case default on Unix... (perl 5.8)
			$break = lcfirst($break);
                        if (($break eq "no")||($break eq "cancel")) {
                            return 0;
                        }
                    }
                }
              }
              elsif ($plotter eq 4) {
                push @command, "-pScilab";
                my $output_file = save_disp_file($w,'sci');
                if (!$output_file) {
                  putmsg($cmdwin, "Trace cancelled...\n");
                  return;
                }
                $output_file = "\"$output_file\"";
                push @command, "-f$output_file";
                
              }
              push @command, "-i$newsi->{'Inspect'}" if $newsi->{'Inspect'};
              push @command, "--first=$newsi->{'First'}" if $newsi->{'First'};
              push @command, "--last=$newsi->{'Last'}" if $newsi->{'Last'};
              # push @command, "--save" if ($newsi->{'Trace'} eq 1);
            }
        }
        push @command, "$out_name";
        my ($OutDir,$OutDirBak);
        # In the special case of --dir, we simply replace ' ' with '_'
        # on Win32 (also giving out a warning message). This is done
        # because Win32::GetShortPathName only works on directories that
        # actually exist... :(
        if ($newsi->{'Dir'}) {
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
        
        push @command, "--ncount=$newsi->{'Ncount'}";
        push @command, "--trace" if ($newsi->{'Trace'} eq 1);
        push @command, "--seed=$newsi->{'Seed'}" if $newsi->{'Seed'};
        push @command, "--dir=$OutDir" if $newsi->{'Dir'};
  if ($inf_sim->{'Binary'} == 1) {
          push @command, "--format='Matlab_binary'" if ($plotter eq 1 || $plotter eq 2);
          push @command, "--format='Scilab_binary'" if ($plotter eq 3 || $plotter eq 4);
  } else {
    push @command, "--format=Matlab" if ($plotter eq 1 || $plotter eq 2);
          push @command, "--format=Scilab" if ($plotter eq 3 || $plotter eq 4);
  }
        my @unset = ();
        for (@{$out_info->{'Parameters'}}) {
            if (length($newsi->{'Params'}{$_})>0) {
                push @command, "$_=$newsi->{'Params'}{$_}";
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
        my $inittext = "Running simulation '$out_name' ...\n" .
            join(" ", @command) . "\n";
        my $success = my_system $w, $inittext, @command;
        $inf_sim=$newsi;
        return unless $success;
        my $ext;
        if ($plotter eq 0) {
          $ext="sim";
        } elsif ($plotter eq 1 || $plotter eq 2) {
          $ext="m";
        } elsif ($plotter eq 3 || $plotter eq 4) {
          $ext="sci";
        }
        $current_sim_file = $newsi->{'Dir'} ?
            "$newsi->{'Dir'}/mcstas.$ext" :
            "mcstas.$ext";
        new_simulation_results($w);
        # In case of non-PGPLOT plotter, we can not read the data from disk.
        # Instead, we simply keep $newsi information in $inf_sim
        if ($plotter eq 0) {
            read_sim_data($w); 
        } else {
            $inf_sim=$newsi;
        }
        $inf_sim->{'Autoplot'} = $newsi->{'Autoplot'};
  $inf_sim->{'Binary'} = $newsi->{'Binary'};
        $inf_sim->{'Trace'} = $newsi->{'Trace'};
        push @mcplot_cmd, "$MCSTAS::mcstas_config{'prefix'}mcplot$suffix";
        if ($newsi->{'Autoplot'}) { # Is beeing set to 0 above if Win32 + trace
          plot_dialog($w, $inf_instr, $inf_sim, $inf_data,
                      $current_sim_file);
        }
      }
  }

sub menu_plot_results {
    my ($w) = @_;
    unless($current_sim_file) {
        my $ret = load_sim_file($w);
        return 0 unless $ret && $current_sim_file;
    }
    plot_dialog($w, $inf_instr, $inf_sim, $inf_data, $current_sim_file);
    return 1;
}

sub menu_choose_backend {
    # sub for selection of mcdisplay "backend". 
    # Default read from $MCSTAS::mcstas_config{'PLOTTER'}
    # PW 20030314
    my ($w) = @_;
    my $plotter = $MCSTAS::mcstas_config{'PLOTTER'};
    my $ret;
    my $binary;
    my $output_file;
    ($ret, $binary, $plotter) = backend_dialog($w, $inf_sim->{'Binary'},$plotter);
    $inf_sim->{'Binary'} = $binary;
    $MCSTAS::mcstas_config{'PLOTTER'} = $plotter;
}


sub menu_read_sim_file {
    my ($w) = @_;
    load_sim_file($w); 
    menu_plot_results($w);
}

sub menu_usingmcstas {
    my ($w) = @_;
}

sub menu_about {
    my ($w) = @_;
}

# Build the text (McStas metalanguage) representation of a component
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
        my @p_splitted = split(" ", $p);
        my $length = scalar @p_splitted;
        my $p_last_word = $p_splitted[$length-1];
        if(defined($r->{'VALUE'}{$p}) && $r->{'VALUE'}{$p} !~ /^\s*$/) {
            $add .= "$p_last_word = $r->{'VALUE'}{$p}";
        } elsif(defined($cdata->{'parhelp'}{$p}{'default'})) {
            next;                # Omit non-specified default parameter
        } else {
            $add.= "$p_last_word = ";
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
*         McStas instrument definition URL=http://mcstas.risoe.dk
*
* Instrument: test (rename also the example and DEFINE lines below)
*
* %Identification
* Written by: Your name (email)
* Date: Current Date
* Origin: Your institution
* Release: McStas 1.8
* Version: 0.1
* %INSTRUMENT_SITE: Institution_name_as_a_single word
*
* Instrument short description
*
* %Description
* Instrument longer description (type, elements, usage...)
*
* Example: mcrun test.instr <parameters=values>
*
* %Parameters
* Par1: (unit) Parameter1 description
*
* %Link 
* A reference/HTML link for more information
*
* %End
*******************************************************************************/

DEFINE INSTRUMENT test(Par1=Default_Value1)

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
/* for further RELATIVE reference, and even replaced by a Progress_bar  */
/* component. Other useful keywords are : ROTATED EXTEND GROUP PREVIOUS */
/* Also think about adding a neutron source !                           */
COMPONENT Origin = Arm()
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
                push(@compdefs, @paths);
                push(@{$sl->[1]}, map([compname($_), $_], @paths));
            }
        }
        push @menudefs, $sl;
    }
    $menu->command(-label => "Instrument template",
                   -command => sub { menu_insert_instr_template($w) },
                   -underline => 0);
    $menu->command(-label => "Component ...",
                   -accelerator => 'Alt+M',
                   -command => sub { menu_insert_component($w) },
                   -underline => 0);
    $w->bind('<Alt-m>' => sub { menu_insert_component($w) });
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
                       -accelerator => 'Alt+O',
                       -command => [\&menu_open, $w],
                       -underline => 0);
    $w->bind('<Alt-o>' => [\&menu_open, $w]);
    $filemenu->command(-label => 'Edit current/New',
                       -underline => 0,
                       -command => \&menu_edit_current);
    if($external_editor) {
        my $shortname = (split " ", $external_editor)[0];
        $shortname = (split "/", $shortname)[-1];
        $filemenu->command(-label => 'Spawn editor "' . $shortname . '"',
                           -command => sub { menu_spawn_editor($w) } );
    }
    $filemenu->command(-label => 'Compile instrument',
                       -underline => 0,
                       -command => sub {menu_compile($w)});
    $filemenu->command(-label => 'Clear output',
                       -underline => 1,
                       -command => sub { $cmdwin->delete("1.0", "end") });
    $filemenu->separator;
    $filemenu->command(-label => 'Quit',
                       -underline => 0,
                       -accelerator => 'Alt+Q',
                       -command => \&menu_quit);
    $w->bind('<Alt-q>' => \&menu_quit);
    $filemenu->pack(-side=>'left');
    my $simmenu = $menu->Menubutton(-text => 'Simulation', -underline => 2);
    $simmenu->command(-label => 'Read old simulation ...',
                       -underline => 0,
                       -command => sub { menu_read_sim_file($w) });
    $simmenu->separator;
    $simmenu->command(-label => 'Run simulation ...',
                      -underline => 1,
                      -accelerator => 'Alt+U',
                      -command => sub {menu_run_simulation($w);});
    $w->bind('<Alt-u>' => [\&menu_run_simulation, $w]);
    $simmenu->command(-label => 'Plot results ...',
                      -underline => 0,
                      -accelerator => 'Alt+P',
                      -command => sub {menu_plot_results($w);});
    $w->bind('<Alt-p>' => [\&menu_plot_results, $w]);
    $simmenu->separator;
    $simmenu->command(-label => 'Choose backend (plotter)...',
                      -underline => 0,
                      -accelerator => 'Alt+C',
                      -command => sub {menu_choose_backend($w);});
    $w->bind('<Alt-c>' => [\&menu_choose_backend, $w]);
    
    $simmenu->pack(-side=>'left');
    
    sitemenu_build($w,$menu);
    
    my $helpmenu = $menu->Menubutton(-text => 'Help (McDoc)', -underline => 0);
    $helpmenu->command(-label => 'McStas web page',
                       -underline => 7,
                       -command => sub {mcdoc_web($w)});
    $helpmenu->command(-label => 'McStas User manual',
                       -command => sub {mcdoc_manual($w)});
     $helpmenu->command(-label => 'McStas Component manual',
                       -command => sub {mcdoc_compman($w)});
    $helpmenu->command(-label => 'Library doc index',
                       -command => sub {mcdoc_components($w)});
    $helpmenu->command(-label => 'Generate component index',
                       -command => sub {mcdoc_generate($w)});
    $helpmenu->command(-label => 'McStas tutorial',
                       -command => sub {mcdoc_tutorial($w)});
    $helpmenu->separator;
    $helpmenu->command(-label => 'Test McStas installation',
                       -command => sub {mcdoc_test($w)});
    $helpmenu->command(-label => 'About McStas',
                       -command => sub {mcdoc_about($w)});
    $helpmenu->pack(-side=>'right');
}

sub setup_cmdwin {
    my ($w) = @_;
    my $f2 = $w->Frame();
    $f2->pack(-fill => 'x');
    my $instr_lab = $f2->Label(-text => "Instrument file: <None>",
                               -anchor => 'w',
                               -justify => 'left');
    $instr_lab->pack(-side => 'left');
    my $instr_but = $f2->Button(-text => "Edit/New",
                                -command => \&menu_edit_current);
    $instr_but->pack(-side => "right", -padx => 1, -pady => 1);
    my $f3 = $w->Frame();
    $f3->pack(-fill => 'x');
    my $res_lab = $f3->Label(-text => "Simulation results: <None>",
                             -anchor => 'w',
                             -justify => 'left');
    $res_lab->pack(-side => 'left');
    my $sim_but = $f3->Button(-text => "Read",
                                -command => sub { menu_read_sim_file($w) });
    $sim_but->pack(-side => "right", -padx => 1, -pady => 1);
    my $status_lab = $w->Label(-text => "Status: Ok",
                                -anchor => 'w',
                                -justify => 'left');
    $status_lab->pack(-fill => 'x');
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
    $current_instr_label = $instr_lab;
    $current_results_label = $res_lab;
    $status_label = $status_lab;
    $cmdwin = $rotext;
    # Insert "mcstas --version" message in window. Do it a line at the
    # time, since otherwise the tags mechanism seems to get confused.
    my $l;
    for $l (split "\n", `mcstas --version`) {
        $cmdwin->insert('end', "$l\n", 'msg');
    }
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


sub setup_edit {
    my ($mw) = @_;
    # Create the editor window.
    my $w = $mw->Toplevel;
    my $e;
    # Create the editor text widget.
#    require Tk::CodeText::McStas;
    $e = $w->Scrolled('CodeText',-relief => 'sunken', -bd => '2', -setgrid => 'true',
                      -height => 24, wrap => 'none', -scrollbars =>'se',  
                      -commentchar => '// ', -indentchar => "  ", -updatecall => \&update_line, -syntax => 'Perl');
    my $menu = $e->menu;
    $w->bind('<F5>' => [\&Tk::CodeText::selectionIndent]);
    $w->bind('<F6>' => [\&Tk::CodeText::selectionUnIndent]);
    $w->bind('<F7>' => [\&Tk::CodeText::selectionComment]);
    $w->bind('<F8>' => [\&Tk::CodeText::selectionUnComment]);
    $w->configure(-menu => $menu);
    my $insert_menu = $menu->Menubutton(-text => 'Insert',  -underline => 0, -tearoff => 0);
    # This is only done for backward compatibility - we want to use Alt+s for saving...
    my $filemenu = $menu->Menubutton(-text => 'Search', -underline => 1);
    $w->bind('<Alt-s>' => [\&menu_save, $w]);
    make_insert_menu($w, $insert_menu);
    my $label = $w->Label(-bd => '1', -text => 'Current line: 1');
    $e->pack(-expand => 'yes', -fill => 'both');
    $label->pack(-side => 'left', -expand => 'no', -fill => 'x');
    $e->mark('set', 'insert', '0.0');
    $w->protocol("WM_DELETE_WINDOW" => sub { editor_quit($w) } );
    $edit_control = $e;
    $edit_window = $w;
    $edit_label = $label;
    if ($current_sim_def) {
      $w->title("Edit: $current_sim_def");
      if (-r $current_sim_def) {
          $e->Load($current_sim_def);
      }
    } else {
      $w->title("CodeEdit: Start with Insert/Instrument template");
    }
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
      # Once again, needed platform check - 
      # selection of executable suffix on 
      # unix vs. Win32
      # PW 20030314
      if ($Config{'osname'} eq "MSWin32") {
        $exename = "$simname.exe";
      } else {
        $exename = "$simname.out";
      }
    }
    return "not found" unless -f $exename;
    return "not executable" unless -x $exename;
    my @stat1 = stat($simname);
    my @stat2 = stat($exename);
    return "source is newer" unless $stat1[9] < $stat2[9];
    return "";
}

my $win = new MainWindow;
check_external_editor();
$main_window = $win;
setup_menu($win);
setup_cmdwin($win);

if(@ARGV) {
    # Most likely, everything on the commandline is a filename... Join using
    # spaces, e.g. mcgui.pl My Documents\My Simulation.instr
    open_instr_def($win, join(' ',@ARGV));
} else {
#    menu_open($win);
}

MainLoop;
