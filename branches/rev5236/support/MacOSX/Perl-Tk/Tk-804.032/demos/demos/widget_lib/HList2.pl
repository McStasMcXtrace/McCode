# HList and ItemStyle, multicolumn listbox with individual cell styles.
# -*- perl -*-

#
# $Id: $
# Author: Slaven Rezic
#
# Copyright (C) 1999 Slaven Rezic. All rights reserved.
# This program is free software; you can redistribute it and/or
# modify it under the same terms as Perl itself.
#
# Mail: eserte@cs.tu-berlin.de
# WWW:  http://user.cs.tu-berlin.de/~eserte/
#

use Tk::HList;
use Tk::ItemStyle;

sub HList2 {
    my($demo) = @_;
    my $TOP = $MW->WidgetDemo(
        -name => $demo,
        -text => 'HList and ItemStyle, multicolumn listbox with individual cell styles.',
	-geometry_manager => 'grid',
    );

    my $h = $TOP->Scrolled
      (qw/HList
       -header 1
       -columns 4
       -width 50
       -height 20/
      )->grid(qw/-sticky nsew/);

    for (0 .. 3) {
	$h->header('create', $_, -text => 'Column ' . $_);
    }

    my @img;
    foreach ('Xcamel.gif', 'anim.gif', 'icon.gif', 'Camel.xpm') {
	push @img, $TOP->Photo(-file => Tk->findINC($_)),
    }

    my(@fonts) = ('-*-Helvetica-Medium-R-Normal--*-180-*-*-*-*-*-*',
		  '-*-Courier-Medium-R-Normal--*-180-*-*-*-*-*-*',
		  '-*-times-medium-r-normal--*-240-*-*-*-*-*-*',
		  '-Adobe-Courier-Bold-O-Normal--*-120-*-*-*-*-*-*',
		  'fixed',
		 );

    my(@colors) = qw(red green blue yellow red cyan black);

    my $rnd_font = sub {
	$fonts[rand($#fonts+1)];
    };
    my $rnd_color = sub {
	$colors[rand($#colors+1)];
    };
    my $rnd_image = sub {
	my $yn = int(rand(2));
	if ($yn) {
	    $img[rand($#img+1)];
	} else {
	    undef;
	}
    };
    my $rnd_window = sub {
	my $yn = int(rand(10));
	if ($yn == 3) {
	    ('Button', 'Entry')[rand(2)];
	} else {
	    undef;
	}
    };

    for my $y (0 .. 20) {
	my $e = $h->addchild("");
	for my $col (0 .. 3) {
	    my $window = $rnd_window->();
	    my $image = $rnd_image->();
	    my $fg = $rnd_color->();
	    my $bg = $rnd_color->();
	    if ($bg eq $fg) { $fg = 'white' }

	    my $style_type = ($window ? 'window' :
			      ($image ? 'imagetext' : 'text'));
	    my $btn;
	    my $style = $h->ItemStyle($style_type);
	    if ($style_type eq 'window') {
		$style->configure(-pady => 0, -padx => 0, -anchor => "nw");
		if ($window eq 'Button') {
		    $btn = $h->Button
		      (-text => 'Click me!',
		       -command => sub {
			   $btn->configure(-activeforeground => $rnd_color->());
		       },
		      );
		} else {
		    $btn = $h->Entry;
		}
	    } else {
		$style->configure(-foreground => $fg,
				  -background => $bg,
				  -font       => $rnd_font->(),
				 );
	    }
	    $h->itemCreate
	      ($e, $col,
	       -itemtype => $style_type,
	       -style => $style,
	       ($style_type eq 'imagetext'
		? (-image => $image) : ()
	       ),
	       ($style_type eq 'window'
		? (-widget => $btn) : (-text => 'Cell ' . $y . '/' . $col)
	       ),
	      );
	}
    }
}

1;

__END__
