# items.pl

use subs qw/items_button_press items_drag items_enter items_leave items_mark
	    items_start_drag items_stroke items_under_area/;
use vars qw/$TOP/;

sub items {

    # Create a top-level window containing a canvas that displays the various
    # item types and allows them to be selected and moved.

    my($demo) = @_;
    $TOP = $MW->WidgetDemo(
        -name     => $demo,
        -text     => ["This window contains a canvas widget with examples of the various kinds of items supported by canvases.  The following operations are supported:\n  Button-1 drag:\tmoves item under pointer.\n  Button-2 drag:\trepositions view.\n  Button-3 drag:\tstrokes out area.\n Ctrl+f:\t\tdisplays items under area.", qw/-wraplength 5i/],
        -title    => 'Canvas Item Demonstration',
        -iconname => 'items',
    );

    my $c = $TOP->Scrolled(qw/Canvas -width 15c -height 10c -relief sunken
			   -borderwidth 2 -scrollbars se -scrollregion/ =>
			   [qw/0c 0c 30c 24c/]);
    $c->pack(qw/-expand yes -fill both/);

    my %iinfo = ();		# item information hash
    $iinfo{areaX1} = 0;
    $iinfo{areaY1} = 0;
    $iinfo{areaX2} = 0;
    $iinfo{areaY2} = 0;
    $iinfo{restore_cmd} = '';


    if ($Tk::VERSION cmp '800.015') {
	# Display a 3x3 rectangular grid (800.016 or greater).
	$c->createGrid(qw/0c  0c 10c 8c -width 2 -lines 1/);
	$c->createGrid(qw/0c  0c 5c  4c -lines 1 -dash ./);
	$c->createGrid(qw/0c  0c 10m 8m -width 1/);
    } else {
	# Display a 3x3 rectangular grid.
	$c->createRectangle(qw/ 0c  0c 30c 24c -width 2/);
	$c->createLine     (qw/ 0c  8c 30c  8c -width 2/);
	$c->createLine     (qw/ 0c 16c 30c 16c -width 2/);
	$c->createLine     (qw/10c  0c 10c 24c -width 2/);
	$c->createLine     (qw/20c  0c 20c 24c -width 2/);
    }

    my $font1 = '-*-Helvetica-Medium-R-Normal--*-120-*-*-*-*-*-*';
    my $font2 = '-*-Helvetica-Bold-R-Normal--*-240-*-*-*-*-*-*';
    my($blue, $red, $bisque, $green);
    if ($TOP->depth > 1) {
	$blue = 'DeepSkyBlue3';
	$red = 'red';
	$bisque = 'bisque3';
	$green = 'SeaGreen3';
    } else {
	$blue = 'black';
	$red = 'black';
	$bisque = 'black';
	$green = 'black';
    }

    # Set up demos within each of the areas of the grid.

    $c->createText(qw/5c .2c -text Lines -anchor n/);
    $c->createLine(qw/1c 1c 3c 1c 1c 4c 3c 4c -width 2m/, -fill => $blue,
		   qw/-cap butt -join miter -tags item/);
    $c->createLine(qw/4.67c 1c 4.67c 4c -arrow last -activedash - -tags item/);
    $c->createLine(qw/6.33c 1c 6.33c 4c -arrow both -activedash . -tags item/);
    $c->createLine(qw/5c 6c 9c 6c 9c 1c 8c 1c 8c 4.8c 8.8c 4.8c 8.8c 1.2c
                   8.2c 1.2c 8.2c 4.6c 8.6c 4.6c 8.6c 1.4c 8.4c 1.4c
	           8.4c 4.4c -width 3 -tags item -fill/ => $red);
    $c->createLine(qw/1c 5c 7c 5c 7c 7c 9c 7c -width .5c/,
	           -stipple => '@'.Tk->findINC('demos/images/grey.25'),
	           qw/-arrow both -tags item -arrowshape/ => [15, 15, 7]);
    $c->createLine(qw/1c 7c 1.75c 5.8c 2.5c 7c 3.25c 5.8c 4c 7c -width .5c
	           -cap round -join round -tags item/);

    $c->createText(qw/15c .2c -anchor n -text/ => 'Curves (smoothed lines)');
    $c->createLine(qw/11c 4c 11.5c 1c 13.5c 1c 14c 4c -smooth on/,
	           -fill =>$blue, qw/-tags item/);
    $c->createLine(qw/15.5c 1c 19.5c 1.5c 15.5c 4.5c 19.5c 4c -smooth on
	           -arrow both -width 3 -tags item/);
    $c->createLine(qw/12c 6c 13.5c 4.5c 16.5c 7.5c 18c 6c 16.5c 4.5c 13.5c
	           7.5c 12c 6c -smooth on -width 3m -cap round -tags item/,
	           -stipple => '@'.Tk->findINC('demos/images/grey.25'),
	           -fill => $red);

    $c->createText(qw/25c .2c -text Polygons -anchor n/);
    $c->createPolygon(qw/21c 1.0c 22.5c 1.75c 24c 1.0c 23.25c 2.5c 24c 4.0c
                      22.5c 3.25c 21c 4.0c 21.75c 2.5c -tags item/,
	              -fill => $green);
    $c->createPolygon(qw/25c 4c 25c 4c 25c 1c 26c 1c 27c 4c 28c 1c 29c 1c
	              29c 4c 29c 4c -smooth on -tags item/, -fill => $red);
    $c->createPolygon(qw/22c 4.5c 25c 4.5c 25c 6.75c 28c 6.75c 28c 5.25c 24c
	              5.25c 24c 6.0c 26c 6c 26c 7.5c 22c 7.5c -tags item/,
	              -stipple => '@'.Tk->findINC('demos/images/grey.25'));

    $c->createText(qw/5c 8.2c -text Rectangles -anchor n/);
    $c->createRectangle(qw/1c 9.5c 4c 12.5c/, -outline => $red,
	                qw/-width 3m -tags item/);
    $c->createRectangle(qw/0.5c 13.5c 4.5c 15.5c/, -fill => $green,
	                qw/-tags item/);
    $c->createRectangle(qw/6c 10c 9c 15c -tags item/, -outline => undef,
	                -stipple => '@'.Tk->findINC('demos/images/grey.25'),
	                -fill => $blue);

    $c->createText(qw/15c 8.2c -text Ovals -anchor n/);
    $c->createOval(qw/11c 9.5c 14c 12.5c/, -outline => $red,
	           qw/-width 3m -tags item/);
    $c->createOval(qw/10.5c 13.5c 14.5c 15.5c/, -fill => $green,
	           qw/-tags item/);
    $c->createOval(qw/16c 10c 19c 15c -tags item/, -outline => undef,
	           -stipple => '@'.Tk->findINC('demos/images/grey.25'),
	           -fill => $blue);

    $c->createText(qw/25c 8.2c -text Text -anchor n/);
    $c->createRectangle(qw/22.4c 8.9c 22.6c 9.1c/);
    $c->createText(qw/22.5c 9c -anchor n -width 4c/, -font => $font1,
	           -text => 'A short string of text, word-wrapped, justified left, and anchored north (at the top).  The rectangles show the anchor points for each piece of text.', qw/-tags item/);
    $c->createRectangle(qw/25.4c 10.9c 25.6c 11.1c/);
    $c->createText(qw/25.5c 11c -anchor w/, -font => $font1, -fill => $blue,
	           -text => "Several lines,\n each centered\n" .
	           "individually,\nand all anchored\nat the left edge.",
	           qw/-justify center -tags item/);
    $c->createRectangle(qw/24.9c 13.9c 25.1c 14.1c/);
    $c->createText(qw/25c 14c -anchor c/, -font => $font2, -fill => $red,
	           -stipple => 'gray50',
	           -text => 'Stippled characters', qw/-tags item/);

    $c->createText(qw/5c 16.2c -text Arcs -anchor n/);
    $c->createArc(qw/0.5c 17c 7c 20c/, -fill => $green, qw/-outline black/,
	          -stipple => '@'.Tk->findINC('demos/images/grey.25'),
	          qw/-start 45 -extent 270 -style pieslice -tags item/);
    $c->createArc(qw/6.5c 17c 9.5c 20c -width 4m -style arc/, -fill => $blue,
	          qw/-start -135 -extent 270 -tags item/);
    $c->createArc(qw/0.5c 20c 9.5c 24c -width 4m -style pieslice/,
	          -fill => undef, -outline => $red,
	          qw/-start 225 -extent -90 -tags item/);
    $c->createArc(qw/5.5c 20.5c 9.5c 23.5c -width 4m -style chord/,
	          -fill => $blue, -outline => undef,
	          qw/-start 45 -extent 270  -tags item/);

    $c->createText(qw/15c 16.2c -text Bitmaps -anchor n/);
    $c->createBitmap(qw/13c 20c -bitmap/ =>
                     '@'.Tk->findINC('demos/images/face'), qw/-tags item/);
    $c->createBitmap(qw/17c 18.5c/,
	             -bitmap => '@'.Tk->findINC('demos/images/noletters'),
	             qw/-tags item/);
    $c->createBitmap(qw/17c 21.5c/,
	             -bitmap => '@'.Tk->findINC('demos/images/letters'),
	             qw/-tags item/);

    $c->createText(qw/25c 16.2c -text Windows -anchor n/);
    my $c_button = $c->Button(-text => 'Press Me',
        -command => [\&items_button_press, $c, $red],
    );
    $c->createWindow(qw/21c 18c/, -window => $c_button,
	             qw/-anchor nw -tags item/);
    my $c_entry = $c->Entry(-width => '20', -relief => 'sunken',
                            -validate => 'all',
                            -validatecommand => sub {$n++ ? 1 : 0},
                            -invalidcommand => sub {$TOP->bell});
    $c_entry->insert('end' => 'Edit this text');
    $c->createWindow(qw/21c 21c/, -window => $c_entry,
	             qw/-anchor nw -tags item/);
    my $c_scale = $c->Scale(qw/-from 0 -to 100 -length 6c -sliderlength .4c
			    -width .5c -tickinterval 0/);
    $c->createWindow(qw/28.5c 17.5c/, -window => $c_scale,
	             qw/-anchor n -tags item/);
    $c->createText(qw/21c 17.9c -text Button: -anchor sw/);
    $c->createText(qw/21c 20.9c -text Entry: -anchor sw/);
    $c->createText(qw/28.5c 17.4c -text Scale: -anchor s/);

    # Set up event bindings for canvas.

    $c->bind('item', '<Any-Enter>' => [\&items_enter, \%iinfo]);
    $c->bind('item', '<Any-Leave>' => [\&items_leave, \%iinfo]);

    # Get real canvas widget reference to apply bind() commands to:  the
    # Canvas widget is a subwidget of the Scrolled composite widget.  To
    # reference the X event structure, either use the XEvent() method or
    # read the specially localized variable $Tk::event.  We'll use XEvent
    # first, and the variable from then on.

    $c->CanvasBind('<<Copy>>',sub { print "Do Copy\n" });

    $c->CanvasBind('<1>' => sub {
	my($c) = @_;
        my $e = $c->XEvent;
	items_start_drag $c, $e->x, $e->y, \%iinfo;
    });
    $c->CanvasBind('<B1-Motion>' =>
        sub {items_drag shift, $Tk::event->x, $Tk::event->y, \%iinfo});
    $c->CanvasBind('<2>' =>
        sub {shift->scan('mark', $Tk::event->x, $Tk::event->y)});
    $c->CanvasBind('<B2-Motion>' =>
         sub {shift->scan('dragto', $Tk::event->x, $Tk::event->y)});
    $c->CanvasBind('<3>' =>
         sub {items_mark shift, $Tk::event->x, $Tk::event->y, \%iinfo});
    $c->CanvasBind('<B3-Motion>' =>
         sub {items_stroke shift, $Tk::event->x, $Tk::event->y, \%iinfo});
    $c->CanvasBind('<Control-f>' => [sub {
	my($c, $iinfo) = @_;
        my $e = $c->XEvent;
	items_under_area $c, $iinfo;
    }, \%iinfo]);
    $c->CanvasBind('<Any-Enter>' => sub {$_[0]->CanvasFocus});

} # end items

# Utility procedures for highlighting the item under the pointer:

sub items_button_press {

    # Procedure that's invoked when the button embedded in the canvas
    # is invoked.

    my($w, $color) = @_;

    my $i = $w->createText(qw/25c 18.1c -anchor n/, -text => 'Ouch!!',
		       -fill => $color);
    $w->after(500, sub { $w->delete($i) });

} # end items_button_press

sub items_drag {

    my($c, $x, $y, $iinfo) = @_;

    $x = $c->canvasx($x);
    $y = $c->canvasy($y);
    $c->move('current', $x-$iinfo->{lastX}, $y-$iinfo->{lastY});
    $iinfo->{lastX} = $x;
    $iinfo->{lastY} = $y;

} # end items_drag

sub items_enter {

    my($c, $iinfo) = @_;

    $iinfo->{restore_cmd} = '';

    if ($TOP->depth == 1) {
	$iinfo->{restore_cmd} = '';
	return;
    }
    my $type = $c->type('current');
    if ($type eq 'window') {
	$iinfo->{restore_cmd} = '';
	return;
    }

    if ($type eq 'bitmap') {
	my $bg = ($c->itemconfigure(qw/current -background/))[4];
	if (defined $bg) {
	    $iinfo->{restore_cmd} = "\$c->itemconfigure('current',
                -background => '$bg');";
	} else {
	    $iinfo->{restore_cmd} = "\$c->itemconfigure('current',
                -background => undef);";
	}
	$c->itemconfigure(qw/current -background SteelBlue2/);
	return;
    }
    my $fill = ($c->itemconfigure(qw/current -fill/))[4];
    my $stipple = ($c->itemconfigure(qw/current -stipple/))[4];
    if (defined $stipple) {
	$iinfo->{restore_cmd} = "\$c->itemconfigure('current',
            -stipple => '$stipple')";
	$c->itemconfigure(qw/current -stipple /,'');
    } elsif (($type eq 'rectangle' or $type eq 'oval' or $type eq 'arc')
	    and not defined $fill) {
	my $outline = ($c->itemconfigure(qw/current -outline/))[4];
	$iinfo->{restore_cmd} = "\$c->itemconfigure('current',
            -outline => '$outline')";
	$c->itemconfigure(qw/current -outline SteelBlue2/);
    } else {
	$iinfo->{restore_cmd} = "\$c->itemconfigure('current',
            -fill => '$fill')";
	$c->itemconfigure(qw/current -fill SteelBlue2/);
    }

} # end items_enter

sub items_leave {

    my($c, $iinfo) = @_;

    eval $iinfo->{restore_cmd};

} # end items_leave

sub items_mark {

    my($c, $x, $y, $iinfo) = @_;

    $iinfo->{areaX1} = $c->canvasx($x);
    $iinfo->{areaY1} = $c->canvasy($y);
    $c->delete('area');

} # end items_mark

sub items_start_drag {

    my($c, $x, $y, $iinfo) = @_;

    $iinfo->{lastX} = $c->canvasx($x);
    $iinfo->{lastY} = $c->canvasy($y);

} # end items_start_drag

sub items_stroke {

    my($c, $x, $y, $iinfo) = @_;

    $x = $c->canvasx($x);
    $y = $c->canvasy($y);
    if (($iinfo->{areaX1} != $x) and ($iinfo->{areaY1} != $y)) {
	$c->delete('area');
	$c->addtag('area', 'withtag', $c->create('rectangle',
	    $iinfo->{areaX1}, $iinfo->{areaY1}, $x, $y, -outline => 'black'));
	$iinfo->{areaX2} = $x;
	$iinfo->{areaY2} = $y;
    }

} # end items_stroke

sub items_under_area {

    my($c, $iinfo) = @_;

    my $area = $c->find('withtag', 'area');
    my @items  = ();
    my $i;
    foreach $i ($c->find('enclosed', $iinfo->{areaX1},
            $iinfo->{areaY1}, $iinfo->{areaX2}, $iinfo->{areaY2})) {
	my @tags = $c->gettags($i);
	if (defined($tags[0]) and grep $_ eq 'item', @tags) {
	    push @items, $i;
	}
    }
    @items = 'None' unless @items;
    print STDOUT 'Items enclosed by area:  ', join(' ', @items), ".\n";
    @items = ();
    foreach $i ($c->find('overlapping', $iinfo->{areaX1}, $iinfo->{areaY1},
            $iinfo->{areaX2}, $iinfo->{areaY2})) {
	my @tags = $c->gettags($i);
	if (defined($tags[0]) and grep $_ eq 'item', @tags) {
	    push @items, $i;
	}
    }
    @items = 'None' unless @items;
    print STDOUT 'Items overlapping area:  ', join(' ', @items), ".\n";

} # end items_under_area

1;
