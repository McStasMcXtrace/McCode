# Predefined cursors.
# -*- perl -*-

#
# $Id: $
# Author: Slaven Rezic
#
# Copyright (C) 2006,2008 Slaven Rezic. All rights reserved.
# This program is free software; you can redistribute it and/or
# modify it under the same terms as Perl itself.
#
# Mail: slaven@rezic.de
# WWW:  http://www.rezic.de/eserte/
#

use vars qw/$TOP/;

sub cursor {
    my($demo) = @_;
    $TOP = $MW->WidgetDemo(
        -name             => $demo,
        -text             => <<'EOF',
This window displays the names of Tk's built-in
resp. predefined X11 cursors. Click or move on
the names to see the cursor shape.
EOF
	-geometry_manager => 'grid',
        -title            => 'Predefined cursors',
        -iconname         => 'Predefined cursors',
    );

    my $fh;
 TRY_CURSORFONTH: {
	for my $cursorfonth (Tk->findINC("X11/cursorfont.h"),
			     "/usr/X11R6/include/X11/cursorfont.h",
			     "/usr/include/X11/cursorfont.h",
			    ) {
	    last TRY_CURSORFONTH if (open $fh, $cursorfonth);
	}
	$TOP->Label(-text => "Sorry. I can't find X11/cursorfont.h on this system.")->grid;
	return;
    }

    while(<$fh>) {
	chomp;
	if (/XC_(\S+)/) {
	    my $cursorname = $1;
	    next if $cursorname eq 'num_glyphs';
	    push @cursors, $cursorname;
	}
    }

    $lb = $TOP->Scrolled("Listbox", -scrollbars => "ose", -selectmode => "browse")->grid(-sticky => "ns");
    $lb->insert("end", @cursors);
    $lb->bind("<Motion>", sub {
		  my($inx) = $lb->nearest($lb->Subwidget("scrolled")->XEvent->y);
		  $lb->configure(-cursor => $cursors[$inx]);
	      });
    $lb->bind("<<ListboxSelect>>", sub {
		  my($inx) = $lb->curselection;
		  $lb->configure(-cursor => $cursors[$inx]);
	      });
}

__END__
