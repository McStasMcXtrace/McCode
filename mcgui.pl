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

require "mcfrontlib.pl";
require "mcguilib.pl";
require "mcplotlib.pl";
require "mcrunlib.pl";

my $current_sim_file;
my ($inf_instr, $inf_sim, $inf_data);
my %inf_param_map;
my $current_sim_def;
my $edit_control;

sub is_erase_ok {
    my ($w) = @_;
    if($edit_control->numberChanges() > 0) {
	my $ret = $w->messageBox(-message => "Ok to loose changes?'.",
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

sub read_file {
    my ($f) = @_;
    return undef unless $f && -r $f;
    my $H = new FileHandle;
    open($H, $f) || return undef;
    local $/ = undef;		# Read whole file in one go
    my $content = <$H>;
    close($H);
    return $content;
}

sub open_instr_def {
    my ($w, $file) = @_;
    $edit_control->Load($file);
    $current_sim_def = $file;
}

sub menu_open {
    my ($w) = @_;
    return 0 unless(is_erase_ok($w));
    my $file = $w->getOpenFile(-defaultextension => "instr",
			       -title => "Select instrument file");
    return 0 unless $file;
    open_instr_def($w, $file);
    undef($current_sim_file);
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
	$file = $w->getSaveFile(-defaultextension => "instr",
				-title => "Select instrument file name",
				-initialfile => $current_sim_def);
    } else {
	$file = $w->getSaveFile(-defaultextension => "instr",
				-title => "Select instrument file name");
    }
    return 0 unless $file;
    $edit_control->FileName($file);
    undef($current_sim_file) unless $file eq $current_sim_def;
    $current_sim_def = $file;
    menu_save($w);
    return 1;
}

sub menu_undo {
    my ($w) = @_;
    $edit_control->eventGenerate("<<Undo>>");
}

sub read_sim_data {
    my ($w) = @_;
    return 0 unless $current_sim_file && -r $current_sim_file;
    my ($ii, $si, $di) = read_sim_file($current_sim_file);
    return 0 unless $ii && $si && $di;
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

sub menu_run_simulation {
    my ($w) = @_;
    my $out_name = get_out_file($current_sim_def);
    unless($out_name && -x $out_name) {
	$w->messageBox(-message => "Could not compile simulation.",
		       -title => "Run failed",
		       -type => 'OK',
		       -icon => 'error');
	return 0;
    }
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
	push @command, "mcdisplay" if $newsi->{'Trace'};
	push @command, "$out_name";
	push @command, "--ncount=$newsi->{'Ncount'}";
	push @command, "--trace" if $newsi->{'Trace'};
	push @command, "--seed=$newsi->{'Seed'}" if $newsi->{'Seed'};
	push @command, "--dir=$newsi->{'Dir'}" if $newsi->{'Dir'};
	for (keys %{$newsi->{'Params'}}) {
	    push @command, "$_=$newsi->{'Params'}{$_}";
	}
	print "Running simulation '$out_name' ...\n";
	print join(" ", @command), "\n";
	my @retval = system @command;
	$current_sim_file = $newsi->{'Dir'} ?
	    "$newsi->{'Dir'}/mcstas.sim" :
	    "mcstas.sim";
	read_sim_data($w);
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
    $editmenu->pack(-side=>'left');
    my $simmenu = $menu->Menubutton(-text => 'Run', -underline => 0);
    $simmenu->command(-label => 'Read old simulation ...',
		       -underline => 0,
		       -command => sub {load_sim_file($w); 
					menu_plot_results($w);});
    $simmenu->separator;
    $simmenu->command(-label => 'Run simulation ...',
		      -underline => 0,
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
