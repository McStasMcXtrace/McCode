#! /usr/bin/perl -w

if($ENV{"MCSTAS"}) {
    use lib $ENV{"MCSTAS"};
} else {
    use lib "/usr/local/lib/mcstas";
}

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
my $edit_control;

sub ask_save_before_simulate {
    my ($w) = @_;
    if($edit_control->numberChanges() > 0) {
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
    if($edit_control->numberChanges() > 0) {
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
    my ($w) = @_;
    if(is_erase_ok($w)) {
	$w->destroy;
    }
}

sub new_sim_def_name {
    my ($w, $name) = @_;
    undef($current_sim_file)
	unless $current_sim_def && $name eq $current_sim_def;
    $current_sim_def = $name;
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
    $w->title("Mcgui: $current_sim_def");
}

sub open_instr_def {
    my ($w, $file) = @_;
    $edit_control->Load($file);
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
    $edit_control->Save($current_sim_def);
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
    $current_sim_file = $file if $file && -r $file;
    read_sim_data($w);
}

sub myreader {
    my ($dlg, $rotext, $fh) = @_;
    my $s;
    my $len = sysread($fh, $s, 256, 0);
    if(defined($len)) {
	if($len) {
	    $rotext->insert('end', $s);
	} else {
	    $dlg->fileevent($fh,'readable', "");
	    $rotext->insert('end', "\nEOF\n");
	}
    } else {
	$dlg->fileevent($fh,'readable', "");
	$rotext->insert('end', "\nERROR: $!\n");
    }
}

sub putmsg {
    my ($t, $m, $tag) = @_;
    $t->insert('end', $m, $tag);
    $t->see('end');
}

sub run_dialog_create {
    my ($w, $title, $instr, $cancel_cmd) = @_;
    my $dlg = $w->Toplevel(-title => $title);
    $dlg->transient($dlg->Parent->toplevel);
    $dlg->withdraw;
    $dlg->protocol("WM_DELETE_WINDOW" => sub { } );
    # Add labels
    $dlg->Label(-text => $instr,
		-anchor => 'w',
		-justify => 'left')->pack(-fill => 'x');
    my $status_lab = $dlg->Label(-text => "Status: initializing",
				 -anchor => 'w',
				 -justify => 'left');
    $status_lab->pack(-fill => 'x');
    # Add the main text field, with scroll bar
    my $rotext = $dlg->ROText(-relief => 'sunken', -bd => '2',
			      -setgrid => 'true',
			      -height => 30, -width => 80);
    my $s = $dlg->Scrollbar(-command => [$rotext, 'yview']);
    $rotext->configure(-yscrollcommand =>  [$s, 'set']);
    $s->pack(-side => 'right', -fill => 'y');
    $rotext->pack(-expand => 'yes', -fill => 'both');
    $rotext->mark('set', 'insert', '0.0');
    $rotext->tagConfigure('msg', -foreground => 'blue');
    # Add buttons, in a frame at the bottom
    my $bot_frame = $dlg->Frame(-relief => "raised", -bd => 1);
    $bot_frame->pack(-side => "top", -fill => "both",
		     -ipady => 3, -ipadx => 3);
    my $but = $bot_frame->Button(-text => "Cancel", -command => $cancel_cmd);
    $but->pack(-side => "left", -expand => 1, -padx => 1, -pady => 1);
    return ($dlg, $status_lab,  $rotext, $but);
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
    my ($dlg, $fh, $status_lab, $but, $rotext, $state, $success) = @_;
    my $s;
    my $len = sysread($fh, $s, 256, 0);
    if($len) {
	putmsg($rotext, $s);
    } else {
	$dlg->fileevent($fh,'readable', "");
	return if $$state & 1;
	$$state |= 1;
	if(!defined($len)) {
	    putmsg($rotext, "Simulation exited abnormally.\n", 'msg');
	    $$success = undef;
	} else {
	    putmsg($rotext, "Simulation finished.\n", 'msg');
	    $$success = 1;
	}
	$status_lab->configure(-text => "Status: done");
	$but->configure(-text => "Ok",
			-command => sub { $$state |= 2; } );
	$dlg->bind('<Return>' => sub { $but->flash; $$state |= 2; } );
    }
}

sub run_dialog {
    my ($w, $fh, $pid, $inittext) = @_;
    # The $state variable gets bit 0 set when the simulation finishes
    # and bit 1 set when the cancel/ok button is presssed. Thus we
    # exit when it gets to the value 3.
    my ($state, $success) = (0, 0);
    # Initialize the dialog.
    my $cancel_cmd = sub {
	kill 'SIGTERM', $pid unless $state & 1;
	$state |= 2;
    };
    my ($dlg, $status_lab, $rotext, $but) =
	run_dialog_create($w, "Running simulation ...",
			  "Instrument: $current_sim_def", $cancel_cmd);
    putmsg($rotext, $inittext, 'msg'); # Must appear before any other output
    # Set up the pipe reader callback
    my $reader = sub {
	run_dialog_reader($dlg, $fh, $status_lab, $but, $rotext,
			  \$state, \$success);
    };
    $dlg->fileevent($fh, 'readable', $reader);
    $status_lab->configure(-text => "Status: simulation running");
    my $savefocus = run_dialog_popup($dlg);
    do {
        $dlg->waitVariable(\$state);
    } until $state == 3;
    run_dialog_retract($dlg, $savefocus);
    return $success;
}

sub compile_dialog_reader {
    my ($dlg, $fh, $status_lab, $but, $rotext, $state, $success) = @_;
    my $s;
    my $len = sysread($fh, $s, 256, 0);
    if($len) {
	putmsg($rotext, $s);
    } else {
	$dlg->fileevent($fh,'readable', "");
	return if $$state & 1;
	$$state |= 1;
	$$success = defined($len) ? 1 : 0;
    }
}

sub dialog_get_out_file {
    my ($w, $file, $force) = @_;
    # The $state variable gets bit 0 set when the spawned command
    # finishes and bit 1 set when the cancel/ok button is presssed.
    my ($state, $cmd_success);
    my $success = 0;
    my ($fh, $pid, $out_name);
    # Initialize the dialog.
    my $cancel_cmd = sub {
	kill -15, $pid if $pid && !($state & 1); # signal 15 is SIGTERM
	$state |= 2;
    };
    my ($dlg, $status_lab, $rotext, $but) =
	run_dialog_create($w, "Compiling simulation ...",
			  "Instrument: $current_sim_def", $cancel_cmd);
    my $printer = sub { putmsg($rotext, "$_[0]\n", 'msg'); };
    # Set up the pipe reader callback
    $status_lab->configure(-text => "Status: compiling simulation");
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
		    $state &= ~1; # Clear "command done" flag.
		    $cmd_success = 0;
		    my $reader = sub {
			compile_dialog_reader($dlg, $fh, $status_lab, $but,
					      $rotext, \$state, \$cmd_success);
		    };
		    $dlg->fileevent($fh, 'readable', $reader);
		    do {
			$dlg->waitVariable(\$state);
		    } until $state & 1;
		    my $ret = close($fh);
		    undef($pid);
		    unless($cmd_success && defined($ret) && $? == 0) {
			&$printer("** Error exit **.");
			last;
		    }
		} else {			# Child
		    open(STDERR, ">&STDOUT") || die "Can't dup stdout";
		    # Make the child the process group leader, so that
		    # we can kill off any subprocesses it may have
		    # spawned when the user selects CANCEL.
		    setpgrp(0,0);
		    exec @$val;
		}
	    } elsif($type eq 'ERROR') {
		$savefocus = run_dialog_popup($dlg) unless $savefocus;
		&$printer("Error: $msg");
		last;
	    } elsif($type eq 'CONTINUE') {
		next;
	    } else {
		die "Internal: compile_dialog(): $type, $msg";
	    }
	}
    }
    if($savefocus) {
	my $donetype = $success ? "Done" : "Compile failed";
	$status_lab->configure(-text => "Status: $donetype");
	&$printer("$donetype.");
	$state &= ~2;		# Wait for "OK" even if user selected cancel.
	unless($state & 2) {
	    $but->configure(-text => "Ok", -command => sub { $state |= 2; } );
	    $dlg->bind('<Return>' => sub { $but->flash; $state |= 2; } );
	    do {
		$dlg->waitVariable(\$state);
	    } until $state & 2;
	}
    }
    run_dialog_retract($dlg, $savefocus);
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
    compile_instrument($w, 1);	# Force recompilation.
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
	my $ret1 = run_dialog($w, $fh, $child_pid, $inittext);
	my $ret2 = close($fh);
	return $ret1 && defined($ret2) && ($? == 0);
    } else {			# Child
	open(STDERR, ">&STDOUT") || die "Can't dup stdout";
	exec @sysargs;
    }
}

sub menu_run_simulation {
    my ($w) = @_;
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
	read_sim_data($w);
	$inf_sim->{'Autoplot'} = $newsi->{'Autoplot'};
	$inf_sim->{'Trace'} = $newsi->{'Trace'};
	if($newsi->{'Autoplot'} && !$newsi->{'Trace'}) {
	    plot_dialog($w, $inf_instr, $inf_sim, $inf_data);
	}
    }
}

sub menu_plot_results {
    my ($w) = @_;
    unless($current_sim_file) {
	my $ret = load_sim_file($w);
	return 0 unless $ret && $current_sim_file;
    }
    plot_dialog($w, $inf_instr, $inf_sim, $inf_data);
    return 1;
}

sub menu_usingmcstas {
    my ($w) = @_;
}

sub menu_about {
    my ($w) = @_;
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
    $filemenu->command(-label => 'Save instrument',
		       -accelerator => 'Alt+S',
		       -command => [\&menu_save, $w],
		       -underline => 0);
    $w->bind('<Alt-s>' => [\&menu_save, $w]);
    $filemenu->command(-label => 'Save instrument as ...',
		       -underline => 16,
		       -command => sub {menu_saveas($w)});
    $filemenu->separator;
    $filemenu->command(-label => 'Compile instrument',
		       -underline => 0,
		       -command => sub {menu_compile($w)});
    $filemenu->separator;
    $filemenu->command(-label => 'Quit',
		       -underline => 0,
		       -accelerator => 'Alt+Q',
		       -command => [\&menu_quit, $w]);
    $w->bind('<Alt-q>' => [\&menu_quit, $w]);
    $filemenu->pack(-side=>'left');
    my $editmenu = $menu->Menubutton(-text => 'Edit', -underline => 0);
    $editmenu->command(-label => 'Undo',
		       -accelerator => 'Ctrl+Z',
		       -command => [\&menu_undo, $w], -underline => 0);
    $w->bind('<Control-z>' => [\&menu_undo, $w]);
    $editmenu->pack(-side=>'left');
    my $simmenu = $menu->Menubutton(-text => 'Run', -underline => 0);
    $simmenu->command(-label => 'Read old simulation ...',
		       -underline => 0,
		       -command => sub {load_sim_file($w); 
					menu_plot_results($w);});
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
    $helpmenu->command(-label => 'Using McGui',
		       -underline => 6,
		       -command => sub {menu_usingmcstas($w)});
    $helpmenu->command(-label => 'About',
		       -underline => 0,
		       -command => sub {menu_about($w)});
    $helpmenu->pack(-side=>'right');
}

sub setup_edit {
    my ($w) = @_;
    my $e = $w->TextUndo(-relief => 'sunken', -bd => '2', -setgrid => 'true',
		      -height => 30);
    my $s = $w->Scrollbar(-command => [$e, 'yview']);
    $e->configure(-yscrollcommand =>  [$s, 'set']);
    $s->pack(-side => 'right', -fill => 'y');
    $e->pack(-expand => 'yes', -fill => 'both');
#    $e->insert('0.0', "");
    $e->mark('set', 'insert', '0.0');
    $edit_control = $e;
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
setup_menu($win);
setup_edit($win);

MainLoop;
