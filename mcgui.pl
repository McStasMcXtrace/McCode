#! /usr/bin/perl -w

# Determine the path to the McStas system directory. This must be done
# in the BEGIN block so that it can be used in a "use lib" statement
# afterwards.
BEGIN {
    if($ENV{"MCSTAS"}) {
	$MCSTAS::sys_dir = $ENV{"MCSTAS"};
    } else {
	$MCSTAS::sys_dir = "/usr/local/lib/mcstas";
    }
}
use lib $MCSTAS::sys_dir;

use strict;
use FileHandle;
use Tk;
use Tk::TextUndo;
use Tk::ROText;
use Tk::DialogBox;

require "mcfrontlib.pl";
require "mcguilib.pl";
require "mcplotlib.pl";
require "mcrunlib.pl";

my $current_sim_file;
my ($inf_instr, $inf_sim, $inf_data);
my %inf_param_map;
my $current_sim_def;
my $main_window;
my ($edit_window, $edit_control);
my $external_editor;
my ($status_label, $current_results_label, $cmdwin, $current_instr_label);

my $compinfo;			# Cache of parsed component definitions
my @compdefs;			# List of available component definitions

sub ask_save_before_simulate {
    my ($w) = @_;
    if($edit_control && $edit_control->numberChanges() > 0) {
	my $ret = $w->messageBox(
	  -message => "Save instrument \"$current_sim_def\" first?",
	  -title => "Save file?",
	  -type => 'YesNoCancel',
	  -icon => 'questhead',
	  -default => 'Yes');
	menu_save($w) if $ret eq "Yes";
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
				 -type => 'OKCancel',
				 -icon => 'questhead',
				 -default => 'Cancel');
	return $ret eq "Ok" ? 1 : 0;
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
}

sub menu_spawn_editor {
    my ($w) = @_;
    my $cmd = $external_editor ? $external_editor : "vi";
    my $pid = fork();
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
	    CORE::exit(1);	# CORE:exit needed to avoid Perl/Tk failure.
	}
	CORE::exit(0);		# CORE:exit needed to avoid Perl/Tk failure.
    }
}

sub menu_spawn_netscape {
    my ($w) = @_;
    # First try to re-use an already running netscape.
    my $stat = system("netscape", "-remote",
		      "openURL(http://neutron.risoe.dk/mcstas/,new-window)");
    return unless $stat;
    # Need to start netscape ourselves.
    my $pid = fork();
    if(!defined($pid)) {
	$w->messageBox(-message =>
		       "Failed to start Netscape.",
		       -title => "Command failed",
		       -type => 'OK',
		       -icon => 'error');
	return 0;
    } elsif($pid > 0) {
	waitpid($pid, 0);
	return 1;
    } else {
	# Double fork to avoid having to wait() for the browser to
	# finish (or having it become a zombie). See man perlfunc.
	my $pid2 = fork();
	if(!defined($pid2)) {	# fork() failed.
	    print STDERR "Error: spawn of netscape failed!\n";
	    CORE::exit(1);	# CORE:exit needed to avoid Perl/Tk failure.
	} elsif(!$pid2) {	# Child.
	    exec("netscape", "http://neutron.risoe.dk/mcstas/")
		unless $pid2;	# The "unless" avoids a perl warning.
	    # If we get here, the exec() failed.
	    print STDERR "Error: exec() of netscape failed!\n";
	    CORE::exit(1);	# CORE:exit needed to avoid Perl/Tk failure.
	} else {		# Parent.
	    CORE::exit(0);	# CORE:exit needed to avoid Perl/Tk failure.
	}
    }
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
    # Strip any redundant leading "./".
    while($current_sim_def =~ m!^\./(.*)$!) {
	$current_sim_def = $1;
    }
    # Strip any redundant "dir/../".
    # Problem: Needs to handle "/../../" correctly to work ...
#     while($current_sim_def =~ m!^[^/]+/\.\./(.*)$!) {
# 	$current_sim_def = $1;
#     }
#     while($current_sim_def =~ m!^(.*)/[^/]+/\.\./(.*)$!) {
# 	$current_sim_def = "$1/$2";
#     }
    $main_window->title("McStas: $current_sim_def");
    my $text = "Instrument file: " .
	($current_sim_def ? $current_sim_def : "<None>");
    $current_instr_label->configure(-text => $text);
}

sub open_instr_def {
    my ($w, $file) = @_;
    $edit_control->Load($file) if $edit_control;
    new_sim_def_name($w, $file);
}

sub menu_open {
    my ($w) = @_;
    return 0 unless(is_erase_ok($w));
    my $file = $w->getOpenFile(-defaultextension => "instr",
			       -title => "Select instrument file");
    return 0 unless $file;
    open_instr_def($w, $file);
    return 1;	
}

sub menu_save {
    my ($w) = @_;
    if($current_sim_def) {
	$edit_control->Save($current_sim_def);
    } else {
	menu_saveas($w);
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
	$file = $w->getSaveFile(-defaultextension => "instr",
				-title => "Select instrument file name",
				-initialdir => $inidir,
				-initialfile => $inifile);
    } else {
	$file = $w->getSaveFile(-defaultextension => "instr",
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
    my $file = $w->getSaveFile(-defaultextension => "instr",
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
    my $file = $w->getOpenFile(-defaultextension => "sim",
			       -title => "Select simulation file");
    if($file && -r $file) {
	$current_sim_file = $file ;
	new_simulation_results($w);
    }
    read_sim_data($w);
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
		$pid = open($fh, "-|");
		unless(defined($pid)) {
		    &$printer("Could not spawn command.");
		    last;
		}
		if($pid) {		# Parent
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
		} else {			# Child
		    open(STDERR, ">&STDOUT") || die "Can't dup stdout";
		    # Make the child the process group leader, so that
		    # we can kill off any subprocesses it may have
		    # spawned when the user selects CANCEL.
		    setpgrp(0,0);
		    exec @$val if @$val; # The "if @$val" avoids a Perl warning.
		    # If we get here, the exec() failed.
		    print STDERR "Error: exec() of $val->[0] failed!\n";
		    CORE::exit(1);	# CORE:exit needed to avoid Perl/Tk failure.
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
    compile_instrument($w, 1);	# Force recompilation.
    return 1;
}

sub my_system {
    my ($w, $inittext, @sysargs) = @_;
    my $fh = new FileHandle;
    my $child_pid = open($fh, "-|");
    unless(defined($child_pid)) {
	$w->messageBox(-message => "Could not run simulation.",
		       -title => "Run failed",
		       -type => 'OK',
		       -icon => 'error');
	return undef;
    }
    if($child_pid) {		# Parent
	return run_dialog($w, $fh, $child_pid, $inittext);
    } else {			# Child
	open(STDERR, ">&STDOUT") || die "Can't dup stdout";
	# Make the child the process group leader, so that
	# we can kill off any subprocesses it may have
	# spawned when the user selects CANCEL.
	setpgrp(0,0);
	exec @sysargs if @sysargs; # The "if @sysargs" avoids a Perl warning.
	# If we get here, the exec() failed.
	print STDERR "Error: exec() of $sysargs[0] failed!\n";
	CORE::exit(1);	# CORE:exit needed to avoid Perl/Tk failure.
    }
}

sub menu_run_simulation {
    my ($w) = @_;
    unless($current_sim_def) {
	return undef unless menu_open($w);
    }
    my $out_name = compile_instrument($w);
    return 0 unless $out_name;
    # Attempt to avoid problem with missing "." in $PATH.
    unless($out_name =~ "/") {
	$out_name = "./$out_name";
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
	if($newsi->{'Trace'}) {
	    push @command, "mcdisplay";
	    # Make sure the PGPLOT server is already started. If the
	    # PGPLOT server is not started, mcdisplay will start it,
	    # and the server will keep the pipe to mcdisplay open
	    # until the server exits, hanging mcgui.
	    ensure_pgplot_xserv_started();
	}
	push @command, "$out_name";
	push @command, "--ncount=$newsi->{'Ncount'}";
	push @command, "--trace" if $newsi->{'Trace'};
	push @command, "--seed=$newsi->{'Seed'}" if $newsi->{'Seed'};
	push @command, "--dir=$newsi->{'Dir'}" if $newsi->{'Dir'};
	for (@{$out_info->{'Parameters'}}) {
	    push @command, "$_=$newsi->{'Params'}{$_}";
	}
	my $inittext = "Running simulation '$out_name' ...\n" .
	    join(" ", @command) . "\n";
	my $success = my_system $w, $inittext, @command;
	return unless $success;
	$current_sim_file = $newsi->{'Dir'} ?
	    "$newsi->{'Dir'}/mcstas.sim" :
	    "mcstas.sim";
	new_simulation_results($w);
	read_sim_data($w);
	$inf_sim->{'Autoplot'} = $newsi->{'Autoplot'};
	$inf_sim->{'Trace'} = $newsi->{'Trace'};
	if($newsi->{'Autoplot'} && !$newsi->{'Trace'}) {
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
	if(defined($r->{'VALUE'}{$p}) && $r->{'VALUE'}{$p} !~ /^\s*$/) {
	    $add .= "$p = $r->{'VALUE'}{$p}";
	} elsif(defined($cdata->{'parhelp'}{$p}{'default'})) {
	    next;		# Omit non-specified default parameter
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
DEFINE INSTRUMENT test()
DECLARE
%{
%}
INITIALIZE
%{
%}
TRACE

COMPONENT a1 = Arm()
  AT (0,0,0) ABSOLUTE
INSTR_FINISH
my $instr_template_end = <<INSTR_FINISH;

FINALLY
%{
%}
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
my @comp_sources =
    (["Source", ["$MCSTAS::sys_dir/sources"]],
     ["Optics", ["$MCSTAS::sys_dir/optics"]],
     ["Sample", ["$MCSTAS::sys_dir/samples"]],
     ["Monitor", ["$MCSTAS::sys_dir/monitors"]],
     ["Misc", ["$MCSTAS::sys_dir/misc"]],
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
    for $p (@menudefs) {	# $p holds title and component list
	my $m2 = $menu->cascade(-label => $p->[0]);
	my $c;
	for $c (@{$p->[1]}) {	# $c holds name and path
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
    $filemenu->command(-label => 'Edit current',
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
    $simmenu->pack(-side=>'left');
    my $helpmenu = $menu->Menubutton(-text => 'Help', -underline => 0);
    $helpmenu->command(-label => 'McStas web page',
		       -underline => 7,
		       -command => sub {menu_spawn_netscape($w)});
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
    my $instr_but = $f2->Button(-text => "Edit",
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

sub setup_edit {
    my ($mw) = @_;
    # Create the editor window.
    my $w = $mw->Toplevel;
    my $e;
    # Create the editor menus.
    my $menu = $w->Frame(-relief => 'raised', -borderwidth => 2);
    $menu->pack(-fill => 'x');
    my $filemenu = $menu->Menubutton(-text => 'File', -underline => 0);
    $filemenu->command(-label => 'New instrument',
		       -command => [\&menu_new, $w],
		       -underline => 0);
    $filemenu->command(-label => 'Save instrument',
		       -accelerator => 'Alt+S',
		       -command => [\&menu_save, $w],
		       -underline => 0);
    $w->bind('<Alt-s>' => [\&menu_save, $w]);
    $filemenu->command(-label => 'Save instrument as ...',
		       -underline => 16,
		       -command => sub {menu_saveas($w)});
    $filemenu->separator;
    $filemenu->command(-label => 'Close',
		       -underline => 0,
		       -accelerator => 'Alt+C',
		       -command => sub { editor_quit($w) } );
    $w->bind('<Alt-c>' => sub { editor_quit($w) } );
    $filemenu->pack(-side=>'left');
    my $editmenu = $menu->Menubutton(-text => 'Edit', -underline => 0);
    $editmenu->command(-label => 'Undo',
		       -accelerator => 'Ctrl+Z',
		       -command => [\&menu_undo, $w], -underline => 0);
    $w->bind('<Control-z>' => [\&menu_undo, $w]);
    $editmenu->separator;
    $editmenu->command(-label => 'Cut',
		       -accelerator => 'Ctrl+X',
		       -command => sub { $e->clipboardCut(); },
		       -underline => 0);
    $editmenu->command(-label => 'Copy',
		       -accelerator => 'Ctrl+C',
		       -command => sub { $e->clipboardCopy(); },
		       -underline => 1);
    $editmenu->command(-label => 'Paste',
		       -accelerator => 'Ctrl+V',
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
    $w->protocol("WM_DELETE_WINDOW" => sub { editor_quit($w) } );
    $edit_control = $e;
    $edit_window = $w;
}

# Check if simulation needs recompiling.
sub check_if_need_recompile {
    my ($simname) = @_;
    my $exename;
    if($simname =~ /^(.*)\.(instr|ins)$/) {
	$exename = $1;
    } else {
	$exename = "$simname.out";
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
    open_instr_def($win, $ARGV[0]);
} else {
#    menu_open($win);
}

MainLoop;
