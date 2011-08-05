# menbut.pl

use vars qw/$TOP/;

sub menbut {
    my($demo) = @_;
    $TOP = $MW->WidgetDemo(
        -name             => $demo,
        -text             => '',
        -title            => 'Menubutton Demo',
        -iconname         => 'Menubutton',
    );

    my @menubuttons;
    foreach (qw/below right left above/) {
	my $pos = ucfirst;
	my $menubutton = $TOP->Menubutton(qw/-underline 0 -relief raised/,
					  -text => $pos, -direction => $_);
	push @menubuttons, $menubutton;
	my $menu = $menubutton->menu(qw/-tearoff 0/);
	$menubutton->configure(-menu => $menu);
	$menubutton->command(-label => "$pos menu: first item", -command =>
        sub {print "You selected the first item from the $pos menu.\n"});
        $menubutton->command(-label => "$pos menu: second item", -command =>
           sub {print "You selected the second item from the $pos menu.\n"});
    }
    $menubuttons[0]->grid(qw/-row 0 -column 1 -sticky n/);
    $menubuttons[3]->grid(qw/-row 2 -column 1 -sticky n/);
    $menubuttons[1]->grid(qw/-row 1 -column 0 -sticky w/);
    $menubuttons[2]->grid(qw/-row 1 -column 2 -sticky e/);

    my $body = $TOP->Frame;
    $body->grid(qw/-row 1 -column 1 -sticky news/);
    $body->Label(qw/-wraplength 300 -justify left/, -font => 'Helvetica 14',
	        -text => 'This is a demonstration of menubuttons. The "Below" menubutton pops its menu below the button; the "Right" button pops to the right, etc. There are two option menus directly below this text; one is just a standard menu and the other is a 16-color palette.')->pack(qw/-side top -padx 25
						        -pady 25/);
    $bbutt = $body->Frame->pack(qw/-padx 25 -pady 25/);
    $bbutt->Optionmenu(-options => [qw/one two three/])->pack(qw/-side left
						        -padx 25 -pady 25/);

    my $palette;
    my(@colors) = qw/Black red4 DarkGreen  NavyBlue gray75 Red Green Blue
        gray50 Yellow Cyan Magenta White Brown  DarkSeaGreen  DarkViolet/;

    my $colors =  native_optionmenu(
        $bbutt,
        \$palette,
        [sub {print "args=@_.\n"}, 'First'],
        @colors,
    );
    $colors->pack(qw/-side left -padx 25 -pady 25/);

    my $menu = $colors->cget(-menu);
    my $topborder    = 'gray50';
    my $bottomborder = 'gray75';

    foreach my $i (0 .. $#colors) {

        # Create a 16 pixel x 16 pixel solid color swatch.
        # Add a black ring around the currently selected item.

        my $color = $menu->entrycget($i, -label);
        my $p = $TOP->Photo(qw/-width 16 -height 16/);
        $p->put($topborder,    qw/-to  0  0 16  1/);
        $p->put($topborder,    qw/-to  0  1  1 16/);
        $p->put($bottomborder, qw/-to  1 15 16 16/);
        $p->put($bottomborder, qw/-to 15  1 16 15/);
        $p->put($color,        qw/-to  1  1 15 15/);

        my $r = $TOP->Photo(qw/-width 16 -height 16/);
        $r->put(qw/black          -to  0  0 16  2/);
        $r->put(qw/black          -to  0  2  2 16/);
        $r->put(qw/black          -to  2 14 16 16/);
        $r->put(qw/black          -to 14  2 16 14/);
        $r->put($color       , qw/-to  2  2 14 14/);

        $menu->entryconfigure($i, -columnbreak => 1) unless $i % 4;
        $menu->entryconfigure($i,
            -image       => $p,
            -hidemargin  => 1,
            -selectimage => $r,
        );

    } # forend all colors

    $menu->configure(-tearoff => 1);

} # end menbut

sub native_optionmenu {

    my($parent, $varref, $command, @optionvals) = @_;

    $$varref = $optionvals[0];

    my $mb = $parent->Menubutton(
        -textvariable       => $varref,
        -indicatoron        => 1,
        -relief             => 'raised',
        -borderwidth        => 2,
        -highlightthickness => 2,
        -anchor             => 'c',
        -direction          => 'flush',
    );
    my $menu = $mb->Menu(-tearoff => 0);
    $mb->configure(-menu => $menu);

    my $callback = ref($command) =~ /CODE/ ? [$command] : $command;

    foreach (@optionvals) {
        $menu->radiobutton(
            -label     => $_,
            -variable  => $varref,
            -command   => [@$callback, $_],
        );
    }

   $mb;

} # end native_optionmenu

1;
