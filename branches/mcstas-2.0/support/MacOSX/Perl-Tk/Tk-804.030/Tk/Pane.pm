# Tk::Pane.pm
#
# Copyright (c) 1997-1998 Graham Barr <gbarr@pobox.com>. All rights reserved.
# This program is free software; you can redistribute it and/or
# modify it under the same terms as Perl itself.

package Tk::Pane;

use vars qw($VERSION);
$VERSION = '4.007'; # $Id: //depot/Tkutf8/Tk/Pane.pm#7 $

use Tk;
use Tk::Widget;
use Tk::Derived;
use Tk::Frame;

use strict;

use base qw(Tk::Derived Tk::Frame);

Construct Tk::Widget 'Pane';

use Tk::Submethods(
  grid => [qw/bbox columnconfigure location propagate rowconfigure size slaves/],
  pack => [qw/propagate slaves/]
);

sub ClassInit {
    my ($class,$mw) = @_;
    $mw->bind($class,'<Configure>',['QueueLayout',4]);
    $mw->bind($class,'<FocusIn>',  'NoOp');
    return $class;
}

sub Populate {
    my $pan = shift;

    my $frame    = $pan->Component(Frame => "frame");

    $pan->afterIdle(['Manage',$pan,$frame]);
    $pan->afterIdle(['QueueLayout',$pan,1]);

    $pan->Delegates(
	DEFAULT => $frame,
	# FIXME
	# These are a hack to avoid an existing bug in Tk::Widget::DelegateFor
	# which has been reported and should be fixed in the next Tk release
	see	=> $pan,
	xview	=> $pan,
	yview	=> $pan,
    );

    $pan->ConfigSpecs(
	DEFAULT		=> [$frame],
	-sticky		=> [PASSIVE	=> undef, undef, undef],
	-gridded	=> [PASSIVE	=> undef, undef, undef],
	-xscrollcommand => [CALLBACK	=> undef, undef, undef],
	-yscrollcommand => [CALLBACK	=> undef, undef, undef],
    );


    $pan;
}


sub grid {
    my $w = shift;
    $w = $w->Subwidget('frame')
	if (@_ && $_[0] =~ /^(?: bbox
				|columnconfigure
				|location
				|propagate
				|rowconfigure
				|size
				|slaves)$/x);
    $w->SUPER::grid(@_);
}

sub slave {
    my $w = shift;
    $w->Subwidget('frame');
}

sub pack {
    my $w = shift;
    $w = $w->Subwidget('frame')
	if (@_ && $_[0] =~ /^(?:propagate|slaves)$/x);
    $w->SUPER::pack(@_);
}

sub QueueLayout {
    shift if ref $_[1];
    my($m,$why) = @_;
    $m->afterIdle(['Layout',$m]) unless ($m->{LayoutPending});
    $m->{LayoutPending} |= $why;
}

sub AdjustXY {
    my($w,$Wref,$X,$st,$scrl,$getx) = @_;
    my $W = $$Wref;

    if($w >= $W) {
	my $v = 0;
	if($getx) {
	    $v |= 1 if $st =~ /[Ww]/;
	    $v |= 2 if $st =~ /[Ee]/;
	}
	else {
	    $v |= 1 if $st =~ /[Nn]/;
	    $v |= 2 if $st =~ /[Ss]/;
	}

	if($v == 0) {
	    $X = int(($w - $W) / 2);
	}
	elsif($v == 1) {
	    $X = 0;
	}
	elsif($v == 2) {
	    $X = int($w - $W);
	}
	else {
	    $X = 0;
	    $$Wref = $w;
	}
	$scrl->Call(0,1)
	    if $scrl;
    }
    elsif($scrl) {
	$X = 0
	    if $X > 0;
	$X = $w - $W
	    if(($X + $W) < $w);
	$scrl->Call(-$X / $W,(-$X + $w) / $W);
    }
    else {
	$X = 0;
	$$Wref = $w;
    }

    return $X;
}

sub Layout {
    my $pan = shift;
    my $why = $pan->{LayoutPending};

    my $slv = $pan->Subwidget('frame');

    return unless $slv;

    my $H = $slv->ReqHeight;
    my $W = $slv->ReqWidth;
    my $X = $slv->x;
    my $Y = $slv->y;
    my $w = $pan->width;
    my $h = $pan->height;
    my $yscrl = $pan->{Configure}{'-yscrollcommand'};
    my $xscrl = $pan->{Configure}{'-xscrollcommand'};

    $yscrl = undef
	if(defined($yscrl) && UNIVERSAL::isa($yscrl, 'SCALAR') && !defined($$yscrl));
    $xscrl = undef
	if(defined($xscrl) && UNIVERSAL::isa($xscrl, 'SCALAR') && !defined($$xscrl));

    if($why & 1) {
	$h = $pan->{Configure}{'-height'} || 0
	    unless($h > 1);
	$w = $pan->{Configure}{'-width'} || 0
	    unless($w > 1);

	$h = $H
	    unless($h > 1 || defined($yscrl));
	$w = $W
	    unless($w > 1 || defined($xscrl));

	$w = 100 if $w <= 1;
	$h = 100 if $h <= 1;

	$pan->GeometryRequest($w,$h);
    }

    my $st = $pan->{Configure}{'-sticky'} || '';

    $pan->{LayoutPending} = 0;

    $slv->MoveResizeWindow(
	AdjustXY($w,\$W,$X,$st,$xscrl,1),
	AdjustXY($h,\$H,$Y,$st,$yscrl,0),
	$W,$H
    );
}

sub SlaveGeometryRequest {
    my ($m,$s) = @_;
    $m->QueueLayout(1);
}

sub LostSlave {
    my($m,$s) = @_;
    $m->{Slave} = undef;
}

sub Manage {
    my $m = shift;
    my $s = shift;

    $m->{Slave} = $s;
    $m->ManageGeometry($s);
    $s->MapWindow;
    $m->QueueLayout(2);
}

sub xview {
    my $pan = shift;

    unless(@_) {
    	my $scrl = $pan->{Configure}{'-xscrollcommand'};
	return (0,1) unless $scrl;
	my $slv = $pan->Subwidget('frame');
	my $sw  = $slv->ReqWidth;
	my $ldx = $pan->rootx - $slv->rootx;
	my $rdx = $ldx + $pan->width;
	$ldx = $ldx <= 0   ? 0 : $ldx / $sw;
	$rdx = $rdx >= $sw ? 1 : $rdx / $sw;
	return( $ldx , $rdx);
    }
    elsif(@_ == 1) {
	my $widget = shift;
	my $slv = $pan->Subwidget('frame');
	xyview(1,$pan,
		moveto => ($widget->rootx - $slv->rootx) / $slv->ReqWidth);
    }
    else {
	xyview(1,$pan,@_);
    }
}

sub yview {
    my $pan = shift;

    unless(@_) {
	my $scrl = $pan->{Configure}{'-yscrollcommand'};
	return (0,1) unless $scrl;
	my $slv = $pan->Subwidget('frame');
	my $sh  = $slv->ReqHeight;
	my $tdy = $pan->rooty - $slv->rooty;
	my $bdy = $tdy + $pan->height;
	$tdy = $tdy <= 0   ? 0 : $tdy / $sh;
	$bdy = $bdy >= $sh ? 1 : $bdy / $sh;
	return( $tdy, $bdy);
    }
    elsif(@_ == 1) {
	my $widget = shift;
	my $slv = $pan->Subwidget('frame');
	xyview(0,$pan,
		moveto => ($widget->rooty - $slv->rooty) / $slv->ReqHeight);
    }
    else {
	xyview(0,$pan,@_);
    }
}

sub xyview {
    my($horz,$pan,$cmd,$val,$mul) = @_;
    my $slv = $pan->Subwidget('frame');
    return unless $slv;

    my($XY,$WH,$wh,$scrl,@a);

    if($horz) {
	$XY   = $slv->x;
	$WH   = $slv->ReqWidth;
	$wh   = $pan->width;
	$scrl = $pan->{Configure}{'-xscrollcommand'};
    }
    else {
	$XY   = $slv->y;
	$WH   = $slv->ReqHeight;
	$wh   = $pan->height;
	$scrl = $pan->{Configure}{'-yscrollcommand'};
    }

    $scrl = undef
	if(UNIVERSAL::isa($scrl, 'SCALAR') && !defined($$scrl));

    if($WH < $wh) {
	$scrl->Call(0,1);
	return;
    }

    if($cmd eq 'scroll') {
	my $dxy = 0;

	my $gridded = $pan->{Configure}{'-gridded'} || '';
	my $do_gridded = ($gridded eq 'both'
				|| (!$horz == ($gridded ne 'x'))) ? 1 : 0;

	if($do_gridded && $mul eq 'pages') {
	    my $ch = ($slv->children)[0];
	    if(defined($ch) && $ch->manager eq 'grid') {
		@a = $horz
			? (1-$XY,int($slv->width / 2))
			: (int($slv->height / 2),1-$XY);
		my $rc = ($slv->gridLocation(@a))[$horz ? 0 : 1];
		my $mrc = ($slv->gridSize)[$horz ? 0 : 1];
		$rc += $val;
		$rc = 0 if $rc < 0;
		$rc = $mrc if $rc > $mrc;
		my $gsl;
		while($rc >= 0 && $rc < $mrc) {
		    $gsl = ($slv->gridSlaves(-row => $rc))[0];
		    last
			if defined $gsl;
		    $rc += $val;
		}
		if(defined $gsl) {
		    @a = $horz ? ($rc,0) : (0,$rc);
		    $XY = 0 - ($slv->gridBbox(@a))[$horz ? 0 : 1];
		}
		else {
		    $XY = $val > 0 ? $wh - $WH : 0;
		}
		$dxy = $val; $val = 0;
	    }
	}
	$dxy = $mul eq 'pages' ? ($horz ? $pan->width : $pan->height) : 10
	    unless $dxy;
	$XY -= $dxy * $val;
    }
    elsif($cmd eq 'moveto') {
	$XY = -int($WH * $val);
    }

    $XY = $wh - $WH
	if($XY < ($wh - $WH));
    $XY = 0
	if $XY > 0;

    @a = $horz
	? ( $XY, $slv->y)
	: ($slv->x, $XY);

    $slv->MoveWindow(@a);

    $scrl->Call(-$XY / $WH,(-$XY + $wh) / $WH);
}

sub see {
    my $pan = shift;
    my $widget = shift;
    my %opt = @_;
    my $slv = $pan->Subwidget('frame');

    my $anchor = defined $opt{'-anchor'} ? $opt{'-anchor'} : "";

    if($pan->{Configure}{'-yscrollcommand'}) {
	my $yanchor = lc(($anchor =~ /([NnSs]?)/)[0] || "");
	my $pty = $pan->rooty;
	my $ph  = $pan->height;
	my $pby = $pty + $ph;
	my $ty  = $widget->rooty;
	my $wh  = $widget->height;
	my $by  = $ty + $wh;
	my $h   = $slv->ReqHeight;

	if($yanchor eq 'n' || ($yanchor ne 's' && ($wh >= $h || $ty < $pty))) {
	    my $y = $ty - $slv->rooty;
	    $pan->yview(moveto => $y / $h);
	}
	elsif($yanchor eq 's' || $by > $pby) {
	    my $y = $by - $ph - $slv->rooty;
	    $pan->yview(moveto => $y / $h);
	}
    }

    if($pan->{Configure}{'-xscrollcommand'}) {
	my $xanchor = lc(($anchor =~ /([WwEe]?)/)[0] || "");
	my $ptx = $pan->rootx;
	my $pw  = $pan->width;
	my $pbx = $ptx + $pw;
	my $tx  = $widget->rootx;
	my $ww  = $widget->width;
	my $bx  = $tx + $ww;
	my $w   = $slv->ReqWidth;

	if($xanchor eq 'w' || ( $xanchor ne 'e' && ($ww >= $w || $tx < $ptx))) {
	    my $x = $tx - $slv->rootx;
	    $pan->xview(moveto => $x / $w);
	}
	elsif($xanchor eq 'e' || $bx > $pbx) {
	    my $x = $bx - $pw - $slv->rootx;
	    $pan->xview(moveto => $x / $w);
	}
    }
}

1;

__END__

=head1 NAME

Tk::Pane - A window panner

=for category Derived Widgets

=head1 SYNOPSIS

    use Tk::Pane;

    $pane = $mw->Scrolled("Pane", Name => 'fred',
	-scrollbars => 'soe',
	-sticky => 'we',
	-gridded => 'y'
    );

    $pane->Frame;

    $pane->pack;

=head1 DESCRIPTION

B<Tk::Pane> provides a scrollable frame widget. Once created it can be
treated as a frame, except it is scrollable.

=head1 OPTIONS

=over 4

=item B<-gridded> =E<gt> I<direction>

Specifies if the top and left edges of the pane should snap to a
grid column. This option is only useful if the widgets in the pane
are managed by the I<grid> geometry manager. Possible values are
B<x>, B<y> and B<xy>.

=item B<-sticky> =E<gt> I<style>

If Pane is larger than its requested dimensions, this option may be used to
position (or stretch) the slave within its cavity. I<Style> is a string that
contains zero or more of the characters n, s, e or w. The string can optionally
contains spaces or commas, but they are ignored. Each letter refers to a side
(north, south, east, or west) that the slave will "stick" to. If both n and s
(or e and w) are specified, the slave will be stretched to fill the entire
height (or width) of its cavity.

=back

=head1 METHODS

=over 4

=item I<$pane>-E<gt>B<see>(I<$widget> ?,I<options>?)

Adjusts the view so that I<$widget> is visable. Aditional parameters in
I<options-value> pairs can be passed, each I<option-value> pair must be
one of the following

=over 8

=item B<-anchor> =E<gt> I<anchor>

Specifies how to make the widget visable. If not given then as much of
the widget as possible is made visable.

Possible values are B<n>, B<s>, B<w>, B<e>, B<nw>, B<ne>, B<sw> and B<se>.
This will cause an edge on the widget to be aligned with the corresponding
edge on the pane. for example B<nw> will cause the top left of the widget
to be placed at the top left of the pane. B<s> will cause the bottom of the
widget to be placed at the bottom of the pane, and as much of the widget
as possible made visable in the x direction.

=back

=item I<$pane>-E<gt>B<xview>

Returns a list containing two elements, both of which are real fractions
between 0 and 1. The first element gives the position of  the left of the
window, relative to the Pane as a whole (0.5 means it is halfway through the
Pane, for example). The second element gives the position of the right of the
window, relative to the Pane as a whole.

=item I<$pane>-E<gt>B<xview>(I<$widget>)

Adjusts the view in the window so that I<widget> is displayed at the left of
the window.

=item I<$pane>-E<gt>B<xview>(B<moveto> =E<gt> I<fraction>)

Adjusts the view in the window so that I<fraction> of the total width of the
Pane is off-screen to the left. fraction must be a fraction between 0 and 1.

=item I<$pane>-E<gt>B<xview>(B<scroll> =E<gt> I<number>, I<what>)

This command shifts the view in the window left or right according to I<number>
and I<what>. I<Number> must be an integer. I<What> must be either B<units> or
B<pages> or an abbreviation of one of these. If I<what> is B<units>, the view
adjusts left or right by I<number>*10 screen units on the display; if it is
B<pages> then the view adjusts by number screenfuls. If number is negative then
widgets farther to the left become visible; if it is positive then widgets
farther to the right become visible.

=item I<$pane>-E<gt>B<yview>

Returns a list containing two elements, both of which are real fractions
between 0 and 1. The first element gives the position of  the top of the
window, relative to the Pane as a whole (0.5 means it is halfway through the
Pane, for example). The second element gives the position of the bottom of the
window, relative to the Pane as a whole.

=item I<$pane>-E<gt>B<yview>(I<$widget>)

Adjusts the view in the window so that I<widget> is displayed at the top of the
window.

=item I<$pane>-E<gt>B<yview>(B<moveto> =E<gt> I<fraction>)

Adjusts the view in the window so that I<fraction> of the total width of the
Pane is off-screen to the top. fraction must be a fraction between 0 and 1.

=item I<$pane>-E<gt>B<yview>(B<scroll> =E<gt> I<number>, I<what>)

This command shifts the view in the window up or down according to I<number>
and I<what>. I<Number> must be an integer. I<What> must be either B<units> or
B<pages> or an abbreviation of one of these. If I<what> is B<units>, the view
adjusts up or down by I<number>*10 screen units on the display; if it is
B<pages> then the view adjusts by number screenfuls. If number is negative then
widgets farther up become visible; if it is positive then widgets farther down
become visible.

=back

=head1 AUTHOR

Graham Barr E<lt>F<gbarr@pobox.com>E<gt>

=head1 COPYRIGHT

Copyright (c) 1997-1998 Graham Barr. All rights reserved.
This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut
