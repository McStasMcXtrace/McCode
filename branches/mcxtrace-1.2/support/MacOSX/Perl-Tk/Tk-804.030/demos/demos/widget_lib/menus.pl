# menus.pl

use subs qw/menus_error/;
use vars qw/$TOP/;

sub menus {

    # This demonstration script creates a window with a bunch of menus
    # and cascaded menus using a menubar.  A <<MenuSelect>> virtual event
    # tracks the active menu item.

    my ($demo) = @_;
    $TOP = $MW->WidgetDemo(
        -name     => $demo,
        -text     => ['', -wraplength => '5i'],	
        -title    => 'Menu Demonstration',
        -iconname => 'menus',
    );

    my $ws = $TOP->windowingsystem;

    my $text = ($ws eq 'classic' or $ws eq 'aqua') ?
        'This window contains a menubar with cascaded menus.  You can invoke entries with an accelerator by typing Command+x, where "x" is the character next to the command key symbol. The rightmost menu can be torn off into a palette by dragging outside of its bounds and releasing the mouse.' :
        'This window contains a menubar with cascaded menus.  You can post a menu from the keyboard by typing Alt+x, where "x" is the character underlined on the menu.  You can then traverse among the menus using the arrow keys.  When a menu is posted, you can invoke the current entry by typing space, or you can invoke any entry by typing its underlined character.  If a menu entry has an accelerator, you can invoke the entry without posting the menu just by typing the accelerator. The rightmost menu can be torn off into a palette by selecting the first item in the menu.';

    $TOP->configure(-text => $text);

    my $toplevel = $TOP->toplevel; # get $TOP's Toplevel widget reference
    my $menubar = $toplevel->Menu(-type => 'menubar');
    $toplevel->configure(-menu => $menubar);

    my $modifier;
    if ( $ws eq 'classic' or $ws eq 'aqua') {
	$modifier = 'Command';
    } elsif ($Tk::platform eq 'windows') {
	$modifier = 'Control';
    } else {
	$modifier = 'Meta';
    }
 
    my $f = $menubar->cascade(-label => '~File', -tearoff => 0);
    $f->command(-label => 'Open ...',    -command => [\&menus_error, 'Open'],
	        -image => $toplevel->Getimage("openfile"), -compound => "left");
    $f->command(-label => 'New',         -command => [\&menus_error, 'New'],
	        -image => $toplevel->Getimage("file"), -compound => "left");
    $f->command(-label => 'Save',        -command => [\&menus_error, 'Save']);
    $f->command(-label => 'Save As ...', -command => [\&menus_error, 'Save As']);
    $f->separator;
    $f->command(-label => 'Setup ...',   -command => [\&menus_error, 'Setup']);
    $f->command(-label => 'Print ...',   -command => [\&menus_error, 'Print']);
    $f->separator;
    $f->command(-label => 'Quit',        -command => [$TOP => 'bell']);

    my $b = $menubar->cascade(-label => '~Basic', -tearoff => 0);
    $b->command(-label => 'Long entry that does nothing');
    my $label;
    foreach $label (qw/A B C D E F/) {
	$b->command(
             -label => "Print letter \"$label\"",
             -underline => 14,
	     -accelerator => "$modifier+$label",
             -command => sub {print "$label\n"},
        );
	$TOP->bind("<$modifier-${label}>" => sub {print "$label\n"});
    }
    my $c = $menubar->cascade(-label => '~Cascades', -tearoff => 0);
    $c->command(
        -label       => 'Print hello',
        -command     => sub {print "Hello\n"},
	-accelerator => "$modifier+H",
        -underline   => 6,
    );
    $TOP->bind("<$modifier-h>" => sub {print "Hello\n"});
    $c->command(
        -label       => 'Print goodbye',
        -command     => sub {print "Goodbye\n"},
	-accelerator => "$modifier+G",
        -underline   => 6,
    );
    $TOP->bind("<$modifier-g>" => sub {print "Goodbye\n"});
    my $cc = $c->cascade(-label => '~Check buttons', -tearoff => 0);

    $cc->checkbutton(-label => 'Oil checked', -variable => \$OIL);
    $cc->checkbutton(-label => 'Transmission checked', -variable => \$TRANS);
    $cc->checkbutton(-label => 'Brakes checked', -variable => \$BRAKES);
    $cc->checkbutton(-label => 'Lights checked', -variable => \$LIGHTS);
    $cc->separator;
    $cc->command(
        -label => 'See current values',
	-command => [\&see_vars, $MW, [
                                       ['oil',     \$OIL],
                                       ['trans',   \$TRANS],
                                       ['brakes',  \$BRAKES],
                                       ['lights',  \$LIGHTS],
                                      ],
                    ],
    );
    my $cc_menu = $cc->cget(-menu);
    $cc_menu->invoke(1);
    $cc_menu->invoke(3);

    my $rc = $c->cascade(-label => '~Radio buttons', -tearoff => 0);

    foreach $label (qw/10 14 18 24 32/) {
	$rc->radiobutton(
            -label    => "$label point",
            -variable => \$POINT_SIZE,
            -value    => $label,
        );
    }
    $rc->separator;
    foreach $label (qw/Roman Bold Italic/) {
	$rc->radiobutton(
            -label    => $label,
            -variable => \$FONT_STYLE,
            -value    => $label,
        );
    }
    $rc->separator;
    $rc->command(
        -label => 'See current values',
	-command => [\&see_vars, $MW, [
                                      ['point size', \$POINT_SIZE],
                                      ['font style', \$FONT_STYLE],
                                     ],
                    ],
    );
    my $rc_menu = $rc->cget(-menu);
    $rc_menu->invoke(1);
    $rc_menu->invoke(7);

    my $i = $menubar->cascade(-label => '~Icons', -tearoff => 0);
    $i->command(
        -bitmap => '@'.Tk->findINC('demos/images/pattern'),
	-command => sub {
	    $TOP->messageBox(
			     -title => 'Bitmap Menu Entry', 
			     -message => 'The menu entry you invoked displays a bitmap rather than a text string.  Other than this, it is just like any other menu entry.', 
			     -type => 'ok'),
	    },
	-hidemargin => 1,
    );
    foreach $label (qw/info questhead error/) {
	$i->command(
            -bitmap  => $label,
            -command => sub {print "You invoked the \"$label\" bitmap\n"},
            -hidemargin => 1,
        );
    }
    $i->cget(-menu)->entryconfigure(2, -columnbreak => 1);

    my $m = $menubar->cascade(-label => '~More', -tearoff => 0);
    foreach $label ('An entry', 'Another entry', 'Does nothing',
		    'Does almost nothing', 'Make life meaningful') {
	$m->command(
            -label   => $label,
	    -command => sub {print "You invoked \"$label\"\n"},
        );
    }

    my $k = $menubar->cascade(-label => 'C~olors');
    foreach $label (qw/red orange yellow green blue/) {
	$k->command(
            -label      => $label,
            -background => $label,
	    -command => sub {print "You invoked \"$label\"\n"},
        );
    }

    my $status_bar;
    $TOP->Label(
        qw/-relief sunken -borderwidth 1 -anchor w/,
        -font => 'Helvetica 10', -textvariable => \$status_bar)->
	pack(qw/-padx 2 -pady 2 -expand yes -fill both/);
    $menubar->bind('<<MenuSelect>>' => sub {
	$status_bar = undef;
	$status_bar = $_[0]->entrycget('active', -label);
	$TOP->idletasks;
    });

} # end menus

sub menus_error {

    # Generate a background error, which may even be displayed in a window if
    # using ErrorDialog.

    my($msg) = @_;

    $msg = "This is just a demo: no action has been defined for \"$msg\".";
    $TOP->BackTrace($msg);

} # end menus_error


1;
