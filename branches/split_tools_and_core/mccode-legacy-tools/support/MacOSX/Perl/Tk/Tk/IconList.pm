# -*- perl -*-
#
# tkfbox.tcl --
#
#       Implements the "TK" standard file selection dialog box. This
#       dialog box is used on the Unix platforms whenever the tk_strictMotif
#       flag is not set.
#
#       The "TK" standard file selection dialog box is similar to the
#       file selection dialog box on Win95(TM). The user can navigate
#       the directories by clicking on the folder icons or by
#       selectinf the "Directory" option menu. The user can select
#       files by clicking on the file icons or by entering a filename
#       in the "Filename:" entry.
#
# Copyright (c) 1994-1996 Sun Microsystems, Inc.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# Translated to perk/Tk and modified by Slaven Rezic <slaven@rezic.de>.
#

#----------------------------------------------------------------------
#
#                     I C O N   L I S T
#
# This is a pseudo-widget that implements the icon list inside the
# tkFDialog dialog box.
#
#----------------------------------------------------------------------
# tkIconList --
#
#       Creates an IconList widget.
#

package Tk::IconList;
require Tk::Frame;

use vars qw($VERSION);
$VERSION = '4.007'; # $Id: //depot/Tkutf8/Tk/IconList.pm#7 $

use Tk qw(Ev);
use strict;
use Carp;

use base 'Tk::Frame';

Construct Tk::Widget 'IconList';

# tkIconList_Create --
#
#       Creates an IconList widget by assembling a canvas widget and a
#       scrollbar widget. Sets all the bindings necessary for the IconList's
#       operations.
#
sub Populate {
    my($w, $args) = @_;
    $w->SUPER::Populate($args);

    my $sbar = $w->Component('Scrollbar' => 'sbar',
			     -orient => 'horizontal',
			     -highlightthickness => 0,
			     -takefocus => 0,
			    );
    # make sure that the size does not exceed handhelds' dimensions
    my($sw,$sh) = ($w->screenwidth, $w->screenheight);
    my $canvas = $w->Component('Canvas' => 'canvas',
			       -bd => 2,
			       -relief => 'sunken',
			       -width  => ($sw > 420 ? 400 : $sw-20),
			       -height => ($sh > 160 ? 120 : $sh-40),
			       -takefocus => 1,
			      );
    $sbar->pack(-side => 'bottom', -fill => 'x', -padx => 2);
    $canvas->pack(-expand => 'yes', -fill => 'both');
    $sbar->configure(-command => ['xview', $canvas]);
    $canvas->configure(-xscrollcommand => ['set', $sbar]);

    # Initializes the max icon/text width and height and other variables
    $w->{'maxIW'} = 1;
    $w->{'maxIH'} = 1;
    $w->{'maxTW'} = 1;
    $w->{'maxTH'} = 1;
    $w->{'numItems'} = 0;
#XXX curItem never used    delete $w->{'curItem'};
    $w->{'noScroll'} = 1;
    $w->{'selection'} = [];
    $w->{'index,anchor'} = '';

    # Creates the event bindings.
    $canvas->Tk::bind('<Configure>', sub { $w->Arrange } );
    $canvas->Tk::bind('<1>', [$w,'Btn1',Ev('x'),Ev('y')]);
    $canvas->Tk::bind('<B1-Motion>', [$w,'Motion1',Ev('x'),Ev('y')]);
    $canvas->Tk::bind('<Control-B1-Motion>', 'NoOp');
    $canvas->Tk::bind('<Shift-B1-Motion>', 'NoOp');
    $canvas->Tk::bind('<Control-1>', [$w,'CtrlBtn1',Ev('x'),Ev('y')]);
    $canvas->Tk::bind('<Shift-1>', [$w,'ShiftBtn1',Ev('x'),Ev('y')]);
    $canvas->Tk::bind('<Double-ButtonRelease-1>', [$w,'Double1',Ev('x'),Ev('y')]);
    $canvas->Tk::bind('<Control-Double-ButtonRelease-1>', 'NoOp');
    $canvas->Tk::bind('<Shift-Double-ButtonRelease-1>', 'NoOp');
    $canvas->Tk::bind('<ButtonRelease-1>', [$w,'CancelRepeat']);
    $canvas->Tk::bind('<B1-Leave>', [$w,'Leave1',Ev('x'),Ev('y')]);
    $canvas->Tk::bind('<B1-Enter>', [$w,'CancelRepeat']);
    $canvas->Tk::bind('<Up>',       [$w,'UpDown',   -1]);
    $canvas->Tk::bind('<Down>',     [$w,'UpDown',    1]);
    $canvas->Tk::bind('<Left>',     [$w,'LeftRight',-1]);
    $canvas->Tk::bind('<Right>',    [$w,'LeftRight', 1]);
    $canvas->Tk::bind('<Return>',   [$w,'ReturnKey']);
    $canvas->Tk::bind('<KeyPress>', [$w,'KeyPress',Ev('A')]);
    $canvas->Tk::bind('<Control-KeyPress>', 'NoOp');
    $canvas->Tk::bind('<Alt-KeyPress>', 'NoOp');
    $canvas->Tk::bind('<Meta-KeyPress>', 'NoOp');
#XXX bad....
#    $canvas->Tk::bind('<FocusIn>', sub { $w->FocusIn });
#    $canvas->Tk::bind('<FocusOut>', sub { $w->FocusOut });

    # additional bindings not in tkfbox.tcl
    $canvas->Tk::bind('<2>',['scan','mark',Ev('x'),Ev('y')]);
    $canvas->Tk::bind('<B2-Motion>',['scan','dragto',Ev('x'),Ev('y')]);
    # Remove the standard Canvas bindings
    $canvas->bindtags([$canvas, $canvas->toplevel, 'all']);
    # ... and define some again
    $canvas->Tk::bind('<Home>', ['xview','moveto',0]);
    $canvas->Tk::bind('<End>',  ['xview','moveto',1]);

    $w->ConfigSpecs(-browsecmd =>
		    ['METHOD', 'browseCommand', 'BrowseCommand', undef],
		    -command =>
		    ['CALLBACK', 'command', 'Command', undef],
		    -font =>
		    ['PASSIVE', 'font', 'Font', undef],
		    -foreground =>
		    ['PASSIVE', 'foreground', 'Foreground', undef],
		    -fg => '-foreground',
		    -multiple =>
		    ['PASSIVE', 'multiple', 'Multiple', 0],
		    -selectmode =>
		    ['PASSIVE', 'selectMode', 'SelectMode', 'browse'],
		    -selectbackground =>
		    ['PASSIVE', 'selectBackground', 'Foreground', '#a0a0ff'],
		   );

    $w;
}

# compatibility for old -browsecmd options
sub browsecmd {
    my $w = shift;
    if (@_) {
	$w->{Configure}{'-browsecmd'} = $_[0];
	$w->bind('<<ListboxSelect>>' => $_[0]);
    }
    $w->{Configure}{'-browsecmd'};
}

sub Index {
    my($w, $i) = @_;
    if (!$w->{'list'}) { $w->{'list'} = [] }
    if ($i =~ /^-?[0-9]+$/) {
	if ($i < 0) {
	    $i = 0;
	}
	if ($i > @{ $w->{'list'} }) {
	    $i = @{ $w->{'list'} } - 1;
	}
	return $i;
    } elsif ($i eq 'active') {
	return $w->{'index,active'};
    } elsif ($i eq 'anchor') {
	return $w->{'index,anchor'};
    } elsif ($i eq 'end') {
	return @{ $w->{'list'} };
    } elsif ($i =~ /@(-?[0-9]+),(-?[0-9]+)/) {
	my($x, $y) = ($1, $2);
	my $canvas = $w->Subwidget('canvas');
	my $item = $canvas->find('closest', $x, $y);
	if (defined $item) {
	    return $canvas->itemcget($item, '-tags')->[1];
	} else {
	    return "";
	}
    } else {
	croak "Unrecognized Index parameter `$i', use active, anchor, end, \@x,y, or x";
    }
}

sub Selection {
    my($w, $op, @args) = @_;
    if ($op eq 'anchor') {
	if (@args == 1) {
	    $w->{'index,anchor'} = $w->Index($args[0]);
	} else {
	    return $w->{'index,anchor'};
	}
    } elsif ($op eq 'clear') {
	my($first, $last);
	if (@args == 2) {
	    ($first, $last) = @args;
	} elsif (@args == 1) {
	    $first = $last = $args[0];
	} else {
	    croak "wrong # args: should be Selection('clear', first, ?last?)"
	}
	$first = $w->Index($first);
	$last  = $w->Index($last);
	if ($first > $last) {
	    ($first, $last) = ($last, $first);
	}
	my $ind = 0;
	for my $item (@{ $w->{'selection'} }) {
	    if ($item >= $first) {
		$first = $ind;
		last;
	    }
	    $ind++; # XXX seems to be missing in the Tcl version
	}
	$ind = @{ $w->{'selection'} } - 1;
	for(; $ind >= 0; $ind--) {
	    my $item = $w->{'selection'}->[$ind];
	    if ($item <= $last) {
		$last = $ind;
		last;
	    }
	}
	if ($first > $last) {
	    return;
	}
	splice @{ $w->{'selection'} }, $first, $last-$first+1;
	$w->event('generate', '<<ListboxSelect>>');
	$w->DrawSelection;
    } elsif ($op eq 'includes') {
	my $index;
	for (@{ $w->{'selection'} }) {
	    if ($args[0] eq $_) {
		return 1;
	    }
	}
	return 0;
    } elsif ($op eq 'set') {
	my($first, $last);
	if (@args == 2) {
	    ($first, $last) = @args;
	} elsif (@args == 1) {
	    $first = $last = $args[0];
	} else {
	    croak "wrong # args: should be Selection('set', first, ?last?)";
	}

	$first = $w->Index($first);
	$last  = $w->Index($last);
	if ($first > $last) {
	    ($first, $last) = ($last, $first);
	}
	for(my $i = $first; $i <= $last; $i++) {
	    push @{ $w->{'selection'} }, $i;
	}
	# lsort -integer -unique
	my %sel = map { ($_ => 1) } @{ $w->{'selection'} };
	@{ $w->{'selection'} } = sort { $a <=> $b } keys %sel;
	$w->event('generate', '<<ListboxSelect>>');
	$w->DrawSelection;
    } else {
	croak "Unrecognized Selection parameter `$op', use anchor, clear, includes, or set";
    }
}

# XXX why lower case 's' here and upper in DrawSelection?
sub Curselection {
    my $w = shift;
    @{ $w->{'selection'} };
}

sub DrawSelection {
    my $w = shift;
    my $canvas = $w->Subwidget('canvas');
    $canvas->delete('selection');
    my $selBg = $w->cget('-selectbackground');
    for my $item (@{ $w->{'selection'} }) {
	my $rTag = $w->{'list'}->[$item][2];
	my($iTag, $tTag, $text, $serial) = @{ $w->{'itemList'}{$rTag} };
	my @bbox = $canvas->bbox($tTag);
	# XXX don't hardcode colors
	$canvas->createRectangle
	    (@bbox, -fill => $selBg, -outline => $selBg, -tags => 'selection');
    }
    $canvas->lower('selection');
}

# Returns the selected item
#
sub Get {
    my($w, $item) = @_;
    my $rTag = $w->{'list'}->[$item][2];
    my($iTag, $tTag, $text, $serial) = @{ $w->{'itemList'}{$rTag} };
    $text;
}


# tkIconList_AutoScan --
#
# This procedure is invoked when the mouse leaves an entry window
# with button 1 down.  It scrolls the window up, down, left, or
# right, depending on where the mouse left the window, and reschedules
# itself as an "after" command so that the window continues to scroll until
# the mouse moves back into the window or the mouse button is released.
#
# Arguments:
# w -           The IconList window.
#
sub AutoScan {
    my $w = shift;
    return unless ($w->exists);
    return if ($w->{'noScroll'});
    my($x, $y);
    $x = $Tk::x;
    $y = $Tk::y;
    my $canvas = $w->Subwidget('canvas');
    if ($x >= $canvas->width) {
	$canvas->xview('scroll', 1, 'units');
    } elsif ($x < 0) {
	$canvas->xview('scroll', -1, 'units');
    } elsif ($y >= $canvas->height) {
	# do nothing
    } elsif ($y < 0) {
	# do nothing
    } else {
	return;
    }
    $w->Motion1($x, $y);
    $w->RepeatId($w->after(50, ['AutoScan', $w]));
}

# Deletes all the items inside the canvas subwidget and reset the IconList's
# state.
#
sub DeleteAll {
    my $w = shift;
    my $canvas = $w->Subwidget('canvas');
    $canvas->delete('all');
    delete $w->{'selected'};
    delete $w->{'rect'};
    delete $w->{'list'};
    delete $w->{'itemList'};
    $w->{'maxIW'} = 1;
    $w->{'maxIH'} = 1;
    $w->{'maxTW'} = 1;
    $w->{'maxTH'} = 1;
    $w->{'numItems'} = 0;
#XXX curItem never used    delete $w->{'curItem'};
    $w->{'noScroll'} = 1;
    $w->{'selection'} = [];
    $w->{'index,anchor'} = '';
    $w->Subwidget('sbar')->set(0.0, 1.0);
    $canvas->xview('moveto', 0);
}

# Adds an icon into the IconList with the designated image and items
#
sub Add {
    my($w, $image, @items) = @_;
    my $canvas = $w->Subwidget('canvas');
    my $font = $w->cget(-font);
    my $fg   = $w->cget(-foreground);
    foreach my $text (@items) {
	my $iTag = $canvas->createImage
	    (0, 0, -image => $image, -anchor => 'nw',
	     -tags => ['icon', $w->{numItems}, 'item'.$w->{numItems}],
	    );
	my $tTag = $canvas->createText
	    (0, 0, -text => $text, -anchor => 'nw',
	     (defined $fg   ? (-fill => $fg)   : ()),
	     (defined $font ? (-font => $font) : ()),
	     -tags => ['text', $w->{numItems}, 'item'.$w->{numItems}],
	    );
	my $rTag = $canvas->createRectangle
	    (0, 0, 0, 0,
	     -fill => undef,
	     -outline => undef,
	     -tags => ['rect', $w->{numItems}, 'item'.$w->{numItems}],
	    );
	my(@b) = $canvas->bbox($iTag);
	my $iW = $b[2] - $b[0];
	my $iH = $b[3] - $b[1];
	$w->{'maxIW'} = $iW if ($w->{'maxIW'} < $iW);
	$w->{'maxIH'} = $iH if ($w->{'maxIH'} < $iH);
	@b = $canvas->bbox($tTag);
	my $tW = $b[2] - $b[0];
	my $tH = $b[3] - $b[1];
	$w->{'maxTW'} = $tW if ($w->{'maxTW'} < $tW);
	$w->{'maxTH'} = $tH if ($w->{'maxTH'} < $tH);
	push @{ $w->{'list'} }, [$iTag, $tTag, $rTag, $iW, $iH, $tW, $tH,
				 $w->{'numItems'}];
	$w->{'itemList'}{$rTag} = [$iTag, $tTag, $text, $w->{'numItems'}];
	$w->{'textList'}{$w->{'numItems'}} = lc($text);
	++$w->{'numItems'};
    }
}

# Places the icons in a column-major arrangement.
#
sub Arrange {
    my $w = shift;
    my $canvas = $w->Subwidget('canvas');
    my $sbar   = $w->Subwidget('sbar');
    unless (exists $w->{'list'}) {
	if (defined $canvas && Tk::Exists($canvas)) {
	    $w->{'noScroll'} = 1;
	    $sbar->configure(-command => sub { });
	}
	return;
    }

    my $W = $canvas->width;
    my $H = $canvas->height;
    my $pad = $canvas->cget(-highlightthickness) + $canvas->cget(-bd);
    $pad = 2 if ($pad < 2);
    $W -= $pad*2;
    $H -= $pad*2;
    my $dx = $w->{'maxIW'} + $w->{'maxTW'} + 8;
    my $dy;
    if ($w->{'maxTH'} > $w->{'maxIH'}) {
	$dy = $w->{'maxTH'};
    } else {
	$dy = $w->{'maxIH'};
    }
    $dy += 2;
    my $shift = $w->{'maxIW'} + 4;
    my $x = $pad * 2;
    my $y = $pad;
    my $usedColumn = 0;
    foreach my $sublist (@{ $w->{'list'} }) {
	$usedColumn = 1;
	my($iTag, $tTag, $rTag, $iW, $iH, $tW, $tH) = @$sublist;
	my $i_dy = ($dy - $iH) / 2;
	my $t_dy = ($dy - $tH) / 2;
	$canvas->coords($iTag, $x, $y + $i_dy);
	$canvas->coords($tTag, $x + $shift, $y + $t_dy);
	$canvas->coords($rTag, $x, $y, $x + $dx, $y + $dy);
	$y += $dy;
	if ($y + $dy > $H) {
	    $y = $pad;
	    $x += $dx;
	    $usedColumn = 0;
	}
    }
    my $sW;
    if ($usedColumn) {
	$sW = $x + $dx;
    } else {
	$sW = $x;
    }
    if ($sW < $W) {
	$canvas->configure(-scrollregion => [$pad, $pad, $sW, $H]);
	$sbar->configure(-command => sub { });
	$canvas->xview(moveto => 0);
	$w->{'noScroll'} = 1;
    } else {
	$canvas->configure(-scrollregion => [$pad, $pad, $sW, $H]);
	$sbar->configure(-command => ['xview', $canvas]);
	$w->{'noScroll'} = 0;
    }
    $w->{'itemsPerColumn'} = int(($H - $pad) / $dy);
    $w->{'itemsPerColumn'} = 1 if ($w->{'itemsPerColumn'} < 1);
#XXX    $w->Select($w->{'list'}[$w->{'curItem'}][2], 0)
#      if (exists $w->{'curItem'});
    $w->DrawSelection; # missing in Tcl XXX
}

# Gets called when the user invokes the IconList (usually by double-clicking
# or pressing the Return key).
#
sub Invoke {
    my $w = shift;
    $w->Callback(-command => $w->{'selected'}) if (@{ $w->{'selection'} });
}

# tkIconList_See --
#
#       If the item is not (completely) visible, scroll the canvas so that
#       it becomes visible.
sub See {
    my($w, $rTag) = @_;
    return if ($w->{'noScroll'});
    return if ($rTag < 0 || $rTag >= @{ $w->{'list'} });
    my $canvas = $w->Subwidget('canvas');
    my(@sRegion) = @{ $canvas->cget('-scrollregion') };
    return unless (@sRegion);
    my(@bbox) = $canvas->bbox('item'.$rTag);
    my $pad = $canvas->cget(-highlightthickness) + $canvas->cget(-bd);
    my $x1 = $bbox[0];
    my $x2 = $bbox[2];
    $x1 -= $pad * 2;
    $x2 -= $pad;
    my $cW = $canvas->width - $pad * 2;
    my $scrollW = $sRegion[2] - $sRegion[0] + 1;
    my $dispX = int(($canvas->xview)[0] * $scrollW);
    my $oldDispX = $dispX;
    # check if out of the right edge
    $dispX = $x2 - $cW if ($x2 - $dispX >= $cW);
    # check if out of the left edge
    $dispX = $x1 if ($x1 - $dispX < 0);
    if ($oldDispX != $dispX) {
	my $fraction = $dispX / $scrollW;
	$canvas->xview('moveto', $fraction);
    }
}

sub Btn1 {
    my($w, $x, $y) = @_;

    my $canvas = $w->Subwidget('canvas');
    $canvas->CanvasFocus;
    $x = int($canvas->canvasx($x));
    $y = int($canvas->canvasy($y));
    my $i = $w->Index('@'.$x.','.$y);
    return if ($i eq '');
    $w->Selection('clear', 0, 'end');
    $w->Selection('set', $i);
    $w->Selection('anchor', $i);
}

sub CtrlBtn1 {
    my($w, $x, $y) = @_;

    if ($w->cget(-multiple)) {
	my $canvas = $w->Subwidget('canvas');
	$canvas->CanvasFocus;
	my $x = int($canvas->canvasx($x));
	my $y = int($canvas->canvasy($y));
	my $i = $w->Index('@'.$x.','.$y);
	return if ($i eq '');
	if ($w->Selection('includes', $i)) {
	    $w->Selection('clear', $i);
	} else {
	    $w->Selection('set', $i);
	    $w->Selection('anchor', $i);
	}
    }
}

sub ShiftBtn1 {
    my($w, $x, $y) = @_;

    if ($w->cget(-multiple)) {
    my $canvas = $w->Subwidget('canvas');
	$canvas->CanvasFocus;
	my $x = int($canvas->canvasx($x));
	my $y = int($canvas->canvasy($y));
	my $i = $w->Index('@'.$x.','.$y);
	return if ($i eq '');
	my $a = $w->Index('anchor');
	if ($a eq '') {
	    $a = $i;
	}
	$w->Selection('clear', 0, 'end');
	$w->Selection('set', $a, $i);
    }
}

# Gets called on button-1 motions
#
sub Motion1 {
    my($w, $x, $y) = @_;
    $Tk::x = $x;
    $Tk::y = $y;
    my $canvas = $w->Subwidget('canvas');
    $canvas->CanvasFocus;
    $x = int($canvas->canvasx($x));
    $y = int($canvas->canvasy($y));
    my $i = $w->Index('@'.$x.','.$y);
    return if ($i eq '');
    $w->Selection('clear', 0, 'end');
    $w->Selection('set', $i);
}

sub Double1 {
    my($w, $x, $y) = @_;
    $w->Invoke if (@{ $w->{'selection'} });
}

sub ReturnKey {
    my $w = shift;
    $w->Invoke;
}

sub Leave1 {
    my($w, $x, $y) = @_;
    $Tk::x = $x;
    $Tk::y = $y;
    $w->AutoScan;
}

sub FocusIn {
    my $w = shift;
    return unless (exists $w->{'list'});
    if (@{ $w->{'selection'} }) {
	$w->DrawSelection;
    }
}

sub FocusOut {
    my $w = shift;
    $w->Selection('clear', 0, 'end');
}

# tkIconList_UpDown --
#
# Moves the active element up or down by one element
#
# Arguments:
# w -           The IconList widget.
# amount -      +1 to move down one item, -1 to move back one item.
#
sub UpDown {
    my($w, $amount) = @_;
    return unless (exists $w->{'list'});
    my $i;
    my(@curr) = $w->Curselection;
    if (!@curr) {
	$i = 0;
    } else {
	$i = $w->Index('anchor');
	return if ($i eq '');
	$i += $amount;
    }
    $w->Selection('clear', 0, 'end');
    $w->Selection('set', $i);
    $w->Selection('anchor', $i);
    $w->See($i);
}

# tkIconList_LeftRight --
#
# Moves the active element left or right by one column
#
# Arguments:
# w -           The IconList widget.
# amount -      +1 to move right one column, -1 to move left one column.
#
sub LeftRight {
    my($w, $amount) = @_;
    return unless (exists $w->{'list'});
    my $i;
    my(@curr) = $w->Curselection;
    if (!@curr) {
	$i = 0;
    } else {
	$i = $w->Index('anchor');
	return if ($i eq '');
	$i += $amount*$w->{'itemsPerColumn'};
    }
    $w->Selection('clear', 0, 'end');
    $w->Selection('set', $i);
    $w->Selection('anchor', $i);
    $w->See($i);
}

#----------------------------------------------------------------------
#               Accelerator key bindings
#----------------------------------------------------------------------
# tkIconList_KeyPress --
#
#       Gets called when user enters an arbitrary key in the listbox.
#
sub KeyPress {
    my($w, $key) = @_;
    $w->{'_ILAccel'} .= $key;
    $w->Goto($w->{'_ILAccel'});
    eval {
	$w->afterCancel($w->{'_ILAccel_afterid'});
    };
    $w->{'_ILAccel_afterid'} = $w->after(500, ['Reset', $w]);
}

sub Goto {
    my($w, $text) = @_;
    return unless (exists $w->{'list'});
    return if (not defined $text or $text eq '');
#XXX curItem never used    my $start = (!exists $w->{'curItem'} ? 0 : $w->{'curItem'});
    my $start = 0;
    $text = lc($text);
    my $theIndex = -1;
    my $less = 0;
    my $len = length($text);
    my $i = $start;
    # Search forward until we find a filename whose prefix is an exact match
    # with $text
    while (1) {
	my $sub = substr($w->{'textList'}{$i}, 0, $len);
	if ($text eq $sub) {
	    $theIndex = $i;
	    last;
	}
	++$i;
	$i = 0 if ($i == $w->{'numItems'});
	last if ($i == $start);
    }
    if ($theIndex > -1) {
	$w->Selection(qw(clear 0 end));
	$w->Selection('set', $theIndex);
	$w->Selection('anchor', $theIndex);
	$w->See($theIndex);
    }
}

sub Reset {
    my $w = shift;
    undef $w->{'_ILAccel'};
}

1;
