#! /usr/bin/perl -w

use strict;
use Tk;

my $SIM_TYPE;

sub menu_quit {
    my ($w) = @_;
    $w->destroy;
}

sub menu_open {
}

sub menu_save {
}

sub setup_menu {
    my ($w) = @_;
    my $menu = $w->Frame(-relief => 'raised', -borderwidth => 2);
    $menu->pack(-fill => 'x');
    my $filemenu = $menu->Menubutton(-text => 'File', -underline => 0);
    $filemenu->command(-label => 'Open ...',
		       -accelerator => 'Alt+O',
		       -command => \&menu_open,
		       -underline => 0);
    $w->bind('<Alt-o>' => \&menu_open);
    $filemenu->command(-label => 'Save',
		       -accelerator => 'Alt+S',
		       -command => \&menu_open,
		       -underline => 0);
    $w->bind('<Alt-s>' => \&menu_open);
    $filemenu->command(-label => 'Save as ...',
		       -underline => 5,
		       -command => sub {});
    $filemenu->command(-label => 'Quit',
		       -underline => 0,
		       -accelerator => 'Alt+Q',
		       -command => \&menu_quit);
    $w->bind('<Alt-q>' => \&menu_quit);
    $filemenu->pack(-side=>'left');
    my $simmenu = $menu->Menubutton(-text => 'Run', -underline => 0);
    $simmenu->command(-label => 'Read parameters ...',
		       -underline => 0,
		       -command => sub {});
    $simmenu->separator;
    $SIM_TYPE = 'sim';
    $simmenu->radiobutton(-label => 'Simulate',
			  -underline => 0,
			  -variable => \$SIM_TYPE,
			  -value => 'sim',
			  -state => 'active');
    $simmenu->radiobutton(-label => 'Trace',
			  -underline => 0,
			  -variable => \$SIM_TYPE,
			  -value => 'trace');
    $simmenu->pack(-side=>'left');
    my $helpmenu = $menu->Menubutton(-text => 'Help', -underline => 0);
    $helpmenu->command(-label => 'Using McGui',
		       -underline => 6,
		       -command => sub {});
    $helpmenu->command(-label => 'About',
		       -underline => 0,
		       -command => sub {});
    $helpmenu->pack(-side=>'right');
}

sub setup_edit {
    my ($w) = @_;
    my $e = $w->Text(-relief => 'sunken', -bd => '2', -setgrid => 'true',
		      -height => 30);
    my $s = $w->Scrollbar(-command => [$e, 'yview']);
    $e->configure(-yscrollcommand =>  [$s, 'set']);
    $s->pack(-side => 'right', -fill => 'y');
    $e->pack(-expand => 'yes', -fill => 'both');
    $e->insert('0.0', "");
    $e->mark('set', 'insert', '0.0');
}


my $win = new MainWindow;
setup_menu($win);
setup_edit($win);

MainLoop;
