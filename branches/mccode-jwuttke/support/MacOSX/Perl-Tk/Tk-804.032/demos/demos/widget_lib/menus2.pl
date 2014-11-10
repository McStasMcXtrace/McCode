# menus2.pl

use subs qw/menus_error2/;
use vars qw/$TOP/;

sub menus2 {

    # This demonstration script creates a window with a bunch of menus
    # and cascaded menus, but uses the unique Perl/Tk -menuitems way.
    # A <<MenuSelect>> virtual event tracks the active menu item.

    my ($demo) = @_;
    $TOP = $MW->WidgetDemo(
        -name     => $demo,
        -text     => ['', -wraplength => '5i'],			   
        -title    => 'Menuitems Demonstration',
        -iconname => 'menus2',
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
    
    my $f = $menubar->Cascade(qw/-label ~File -tearoff 0 -menuitems/ =>
        [
         [Button => 'Open ...',    -command => [\&menus_error2, 'Open'],
	  			   -image   => $toplevel->Getimage("openfile"),
	  			   -compound=> "left",
	 ],
	 [Button => 'New',         -command => [\&menus_error2, 'New'],
	  			   -image   => $toplevel->Getimage("file"),
	  			   -compound=> "left",
	 ],
	 [Button => 'Save',        -command => [\&menus_error2, 'Save']],
	 [Button => 'Save As ...', -command => [\&menus_error2, 'Save As']],
	 [Separator => ''],
	 [Button => 'Setup ...',   -command => [\&menus_error2, 'Setup']],
	 [Button => 'Print ...',   -command => [\&menus_error2, 'Print']],
	 [Separator => ''],
	 [Button => 'Quit',        -command => [$TOP => 'bell']],
	],
    );

    my $b = $menubar->Cascade(qw/-label ~Basic -tearoff 0 -menuitems/ =>
        [
	 [Button => 'Long entry that does nothing'],
	  map (
	       [Button       => "Print letter \"~$_\"",
	        -command     => [sub {print "$_[0]\n"}, $_],
	        -accelerator => "Meta+$_" ],
	       ('a' .. 'g')
	  ),
        ],
    );

    my $menu_cb = '~Check buttons';
    my $menu_rb = '~Radio buttons';

    my $c = $menubar->Cascade(qw/-label ~Cascades -tearoff 0 -menuitems/ =>
        [
	 [Button => 'Print ~hello',   -command => sub {print "Hello\n"},
	  -accelerator => 'Control+a'],
	 [Button => 'Print ~goodbye', -command => sub {print "Goodbye\n"},
	  -accelerator => 'Control+b'],
	 [Cascade => $menu_cb, -tearoff => 0, -menuitems =>
	  [
	   [Checkbutton => 'Oil checked',          -variable => \$OIL],
	   [Checkbutton => 'Transmission checked', -variable => \$TRANS],
	   [Checkbutton => 'Brakes checked',       -variable => \$BRAKES],
	   [Checkbutton => 'Lights checked',       -variable => \$LIGHTS],
	   [Separator => ''],
	   [Button => 'See current values', -command =>
	    [\&see_vars, $TOP, [
				['oil',     \$OIL],
				['trans',   \$TRANS],
				['brakes',  \$BRAKES],
				['lights',  \$LIGHTS],
				],
            ], # end see_vars
	   ], # end button
	  ], # end checkbutton menuitems
	 ], # end checkbuttons cascade
	 [Cascade => $menu_rb, -tearoff => 0, -menuitems =>
	  [
	   map (
		[Radiobutton => "$_ point", -variable => \$POINT_SIZE,
		 -value => $_,
		 ],
		(qw/10 14 18 24 32/),
		),
	   [Separator => ''],
	   map (
		[Radiobutton => "$_", -variable => \$FONT_STYLE,
		 -value => $_,
		 ],
		(qw/Roman Bold Italic/),
		),
	   [Separator => ''],
	   [Button => 'See current values', -command =>
	    [\&see_vars, $TOP, [
				['point size', \$POINT_SIZE],
				['font style', \$FONT_STYLE],
				],
	     ], # end see_vars
	    ], # end button
	   ], # end radiobutton menuitems
	  ], # end radiobuttons cascade
         ],
    );

    $TOP->bind('<Control-a>' => sub {print "Hello\n"});
    $TOP->bind('<Control-b>' => sub {print "Goodbye\n"});

    # Fetch the Cascades menu, and from that get the checkbutton and
    # radiobutton cascade menus and invoke a few menu items.

    my $cm = $c->cget(-menu);
    $menu_cb = substr $menu_cb, 1;
    my $cc = $cm->entrycget($menu_cb, -menu);
    $cc->invoke(1);
    $cc->invoke(3);
    $menu_rb = substr $menu_rb, 1;
    my $cr = $cm->entrycget($menu_rb, -menu);
    $cr->invoke(1);
    $cr->invoke(7);

    my $i = $menubar->Cascade(qw/-label ~Icons -tearoff 0 -menuitems/ =>
        [
	 [Button   => '', -bitmap => '@'.Tk->findINC('demos/images/pattern'),
	  -command => sub {
	      $TOP->messageBox(
			       -title => 'Bitmap Menu Entry', 
			       -message => 'The menu entry you invoked displays a bitmap rather than a text string.  Other than this, it is just like any other menu entry.', 
			       -type => 'ok'),
	      },
	  ],
	 map (
	      [Button  => '', -bitmap => $_,
	      -command =>
	       [sub {print "You invoked the \"$_[0]\" bitmap\n"}, $_]],
	      (qw/info questhead error/),
	      ),
        ],
    );
    my $im = $i->cget(-menu);
    $im->entryconfigure(2, -columnbreak => 1);

    my $m = $menubar->Cascade(qw/-label ~More -tearoff 0 -menuitems/ =>
        [
	 map (
	      [Button   => $_,
	       -command =>
	       [sub {print "You invoked \"$_[0]\"\n"}, $_]],
	      ('An entry', 'Another entry', 'Does nothing',
	       'Does almost nothing', 'Make life meaningful'),
	      ),
        ],
    );

    my $k = $menubar->cascade(qw/-label C~olors -tearoff 1 -menuitems/ =>
        [
	 map (
	      [Button      => $_,
	       -background => $_,
	       -command    =>
	       [sub {print "You invoked \"$_[0]\"\n"}, $_]],
	      (qw/red orange yellow green blue/),
	      ),
        ],
    );

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

} # end menus2

sub menus_error2 {


    # Generate a background error, which may even be displayed in a window if
    # using ErrorDialog.

    my($msg) = @_;

    $msg = "This is just a demo: no action has been defined for \"$msg\".";
    $TOP->BackTrace($msg);

} # end menus_error


1;
