# A N-puzzle implemented via the Grid geometry manager.
#
# This program is described in the Perl/Tk column from Volume 1, Issue 4 of
# The Perl Journal (http://tpj.com/tpj), and is included in the Perl/Tk
# distribution with permission.  It has been modified slightly to conform
# to the widget demo standard.

#!/usr/local/bin/perl -w
#
# puz - demonstrate the Grid geometry manager by implementing an n-puzzle.
#
# Stephen O. Lidie, Lehigh University Computing Center, lusol@Lehigh.EDU
# 96/08/11.
#
# Copyright (C) 1996 - 1998 Stephen O. Lidie. All rights reserved.
#
# This program is free software; you can redistribute it and/or modify it under
# the same terms as Perl itself.

require 5.002;
use Tk;
use Tk::Dialog;
use strict;
use subs qw(beep create_puz create_ui puz_fini move_piece new_puz randomly xy);

my $CAMEL;			# Perl/Tk Xcamel.gif Photo image
my $CAMEL_HEIGHT;		# Xcamel height
my $CAMEL_WIDTH;		# Xcamel width
my (@LEVELS) = (9, 16, 36, 64);	# possible puzzle piece counts
my $MW = MainWindow->new;	# program's main window
my @ORDER;			# random puzzle piece ordinals
my $PIECES = $LEVELS[1];	# total puzzle piece count
my $OLD_PIECES = -1;		# previous puzzle piece count
my $PF;			# puzzle Frame
my @PUZ;			# puzzle piece information
my $SIDE;			# pieces per side of puzzle
my $SPACE;			# shortcut to puzzle space piece
my $SPACE_IMAGE;		# space piece image

create_ui;
create_puz;

sub beep {$MW->bell}

sub create_puz {

    return if $PIECES == $OLD_PIECES;

    # Create all the puzzle pieces - buttons with images - and arrange them
    # in a rectangular grid.  @PUZ is a list of button widget references which
    # represent the puzzle pieces.
    #
    # The actual ordering is controlled by @ORDER, a list of list of two:
    #
    # $ORDER[$i]->[0] = puzzle piece ordinal
    # $ORDER[$i]->[1] = random number used to shuffle the puzzle ordinals
    #
    # If the puzzle frame $PF exists, we've been here before, which means that
    # all images and widgets associated with the previous puzzle need
    # destroying, plugging a potential memory leak.  It's important to note
    # that an image must be explicity deleted - it doesn't magically go away
    # if a widget, which just happens to use it, is destroyed.  So, loop
    # through all the puzzle pieces and delete their images, then destroy the
    # puzzle's master frame $PF, destroying all child widgets.  Now, this
    # scheme isn't particulary efficient, but it is simple; ideally, we'd like
    # to create these images only once and reuse them as required.

    if (Exists $PF) {
	my $image;
	foreach (@PUZ) {
	    $image = $_->cget(-image);
	    $image = $SPACE_IMAGE if not defined $image;
	    $image->delete;
	}
	$PF->destroy;
    }

    $PF = $MW->Frame->grid;	# create the puzzle frame grid master
    $OLD_PIECES = $PIECES;
    $#PUZ = $#ORDER = $PIECES - 1;
    $SIDE = sqrt $PIECES;

    my($i, $o, $c, $r, $w, $h, $x, $y, $but, $gif);

    foreach (0..$#ORDER) {$ORDER[$_] = [$_, undef]}

    for($i = 0; $i <= $#PUZ; $i++) {
	$o = $ORDER[$i]->[0];
	($c, $r) = xy $o;	# puzzle ordinal to column/row
	$w = $CAMEL_WIDTH  / $SIDE;
	$h = $CAMEL_HEIGHT / $SIDE;
	$x = $c * $w;		# x/column pixel offset
	$y = $r * $h;		# y/row    pixel offset
	$gif = $PF->Photo;	# new, empty, GIF image
	$gif->copy($CAMEL, -from => $x, $y, $x+$w, $y+$h);
	$but = $PF->Button(-image              => $gif,
			   -relief             => 'flat',
			   -borderwidth        => 0,
			   -command            => \&beep,
			   -highlightthickness => 0,
			   );
	$PUZ[$o] = $but;
	($c, $r) = xy $i;
	$but->grid(-column => $c, -row => $r, -sticky => 'nsew');
	if ($o == 0) {
	    $SPACE_IMAGE = $gif;
	    $SPACE = $but;
	}
    } # forend all puzzle pieces

} # end create_puz

sub create_ui {

    # Create a color Photo image of the Xcamel puzzle.

    $CAMEL = $MW->Photo(-file => "$WIDTRIB/lib/npuz/Xcamel.npuz");
    $CAMEL_WIDTH  = $CAMEL->image('width');
    $CAMEL_HEIGHT = $CAMEL->image('height');

    # Create the menubar.

    my $mf = $MW->Frame(-bg => 'blue')->grid(-sticky => 'ew');
    $mf->gridColumnconfigure(1, -weight => 1);

    my $mbf = $mf->Menubutton(-text => 'File', -relief => 'raised');
    $mbf->command(-label => 'New Puzzle', -command => \&new_puz);
    $mbf->separator;
    $mbf->command(-label => 'Quit', -command => [$MW => 'bell']);

    my $mbp = $mf->Menubutton(-text => 'Prefs', -relief => 'raised');
    my $pieces = 'Pieces';
    $mbp->cascade(-label => $pieces);
    my $mbpm = $mbp->cget(-menu);
    my $mbpmp = $mbpm->Menu;
    $mbp->entryconfigure($pieces, -menu => $mbpmp);
    foreach (@LEVELS) {
	$mbpmp->radiobutton(-label    => $_,
			    -variable => \$PIECES,
			    -value    => $_,
			    -command  => \&create_puz,
			    );
    }

    my $mbq = $mf->Menubutton(-text => 'Help', -relief => 'raised');
    my $about = $MW->Dialog(-text => <<"END"
npuz Version 1.0\n
Select \"File/New Puzzle\", then click around the red \"space\" to rearrange the pieces and solve the puzzle!\n\nThis program is described in the Perl/Tk column from Volume 1, Issue 4 of The Perl Journal (http://tpj.com/tpj), and is included in the Perl/Tk distribution with permission.
END
    );
    $about->configure(-wraplength => '6i');
    $mbq->command(-label => 'About', -command => [$about => 'Show']);

    $mbf->grid(-row => 0, -column => 0, -sticky => 'w');
    $mbp->grid(-row => 0, -column => 1, -sticky => 'w');
    $mbq->grid(-row => 0, -column => 2, -sticky => 'e');

} # end create_ui

sub puz_fini {

    # Return true iff all puzzle pieces are in order.

    my($i, $c, $r, %info);
    for($i = 0; $i <= $#PUZ; $i++) {
	($c, $r) = xy $i;
	%info = $PUZ[$i]->gridInfo;
	return 0 if $c != $info{-column} or $r != $info{-row};
    }
    return 1;

} # end puz_fini

sub move_piece {

    my($piece) = @_;

    my(%info, $c, $r, $sc, $sr);
    %info = $piece->gridInfo; ($c, $r)   = @info{-column,-row};
    %info = $SPACE->gridInfo; ($sc, $sr) = @info{-column,-row};
    if ( ($sr == $r and ($sc == $c-1 or $sc == $c+1)) or
	 ($sc == $c and ($sr == $r-1 or $sr == $r+1)) ) {
	$SPACE->grid(-column => $c,  -row => $r);
	$piece->grid(-column => $sc, -row => $sr);
    }
    if (puz_fini) {
	my $color = ($SPACE->configure(-activebackground))[3];
	$SPACE->configure(-image            => $SPACE_IMAGE,
			  -activebackground => $color,
			  -background       => $color,
			  -relief           => 'flat',
			  );
	foreach (@PUZ) {$_->configure(-command => \&beep)}
    }

} # end move_piece

sub new_puz {

    srand time;
    foreach (0..$#ORDER) {$ORDER[$_]->[1] = rand $#ORDER}
    my @order = sort randomly @ORDER;
    #@order = @ORDER; # here's how I solve the puzzle (;
    my($i, $o, $c, $r, $but);

    for($i = 0; $i <= $#PUZ; $i++) {
	$o = $order[$i]->[0];
	$but = $PUZ[$o];
	if ($o == 0) {
	    $but->configure(-background       => 'red',
			    -relief           => 'sunken',
			    -image            => undef,
			    -activebackground => 'red',
			    );
	} else {
	    $but->configure(-command => [\&move_piece, $but]);
	}
	($c, $r)   = xy $i;
	$but->grid(-column => $c, -row => $r, -sticky => 'nsew');
    }

} # end new_puz

sub randomly {$a->[1] <=> $b->[1]} # randomize order of puzzle pieces

sub xy {my($n) = @_; ($n % $SIDE, int $n / $SIDE)} # ordinal to X/Y
