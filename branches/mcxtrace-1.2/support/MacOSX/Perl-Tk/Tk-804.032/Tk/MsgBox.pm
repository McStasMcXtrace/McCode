# -*- perl -*-

# msgbox.tcl --
#
#	Implements messageboxes for platforms that do not have native
#	messagebox support.
#
# RCS: @(#) $Id: msgbox.tcl,v 1.30 2006/01/25 18:22:04 dgp Exp $
#
# Copyright (c) 1994-1997 Sun Microsystems, Inc.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#

# Translated to Perl/Tk by Slaven Rezic

package Tk::MsgBox;

use Tk qw(Ev);

use strict;
use vars qw($VERSION);
$VERSION = '4.002';

use base qw(Tk::Toplevel);
Construct Tk::Widget 'MsgBox';

sub import {
    if (defined $_[1] and $_[1] eq 'as_default') {
	local $^W = 0;
	package Tk;
	*tk_messageBox = sub {
	    Tk::MsgBox::_tk_messageBox(@_);
	};
    }
}

use vars qw(%image);

sub ClassInit {
    my($class, $mw) = @_;
    $class->SUPER::ClassInit($mw);

    $image{b1}{$mw} = $mw->Bitmap(-foreground => 'black',
				  -data => "#define b1_width 32\n#define b1_height 32
static unsigned char q1_bits[] = {
   0x00, 0xf8, 0x1f, 0x00, 0x00, 0x07, 0xe0, 0x00, 0xc0, 0x00, 0x00, 0x03,
   0x20, 0x00, 0x00, 0x04, 0x10, 0x00, 0x00, 0x08, 0x08, 0x00, 0x00, 0x10,
   0x04, 0x00, 0x00, 0x20, 0x02, 0x00, 0x00, 0x40, 0x02, 0x00, 0x00, 0x40,
   0x01, 0x00, 0x00, 0x80, 0x01, 0x00, 0x00, 0x80, 0x01, 0x00, 0x00, 0x80,
   0x01, 0x00, 0x00, 0x80, 0x01, 0x00, 0x00, 0x80, 0x01, 0x00, 0x00, 0x80,
   0x01, 0x00, 0x00, 0x80, 0x02, 0x00, 0x00, 0x40, 0x02, 0x00, 0x00, 0x40,
   0x04, 0x00, 0x00, 0x20, 0x08, 0x00, 0x00, 0x10, 0x10, 0x00, 0x00, 0x08,
   0x60, 0x00, 0x00, 0x04, 0x80, 0x03, 0x80, 0x03, 0x00, 0x0c, 0x78, 0x00,
   0x00, 0x30, 0x04, 0x00, 0x00, 0x40, 0x04, 0x00, 0x00, 0x40, 0x04, 0x00,
   0x00, 0x80, 0x04, 0x00, 0x00, 0x00, 0x05, 0x00, 0x00, 0x00, 0x06, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};");

    $image{b2}{$mw} = $mw->Bitmap(-foreground => 'white',
				  -data => "#define b2_width 32\n#define b2_height 32
static unsigned char b2_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0xf8, 0x1f, 0x00, 0x00, 0xff, 0xff, 0x00,
   0xc0, 0xff, 0xff, 0x03, 0xe0, 0xff, 0xff, 0x07, 0xf0, 0xff, 0xff, 0x0f,
   0xf8, 0xff, 0xff, 0x1f, 0xfc, 0xff, 0xff, 0x3f, 0xfc, 0xff, 0xff, 0x3f,
   0xfe, 0xff, 0xff, 0x7f, 0xfe, 0xff, 0xff, 0x7f, 0xfe, 0xff, 0xff, 0x7f,
   0xfe, 0xff, 0xff, 0x7f, 0xfe, 0xff, 0xff, 0x7f, 0xfe, 0xff, 0xff, 0x7f,
   0xfe, 0xff, 0xff, 0x7f, 0xfc, 0xff, 0xff, 0x3f, 0xfc, 0xff, 0xff, 0x3f,
   0xf8, 0xff, 0xff, 0x1f, 0xf0, 0xff, 0xff, 0x0f, 0xe0, 0xff, 0xff, 0x07,
   0x80, 0xff, 0xff, 0x03, 0x00, 0xfc, 0x7f, 0x00, 0x00, 0xf0, 0x07, 0x00,
   0x00, 0xc0, 0x03, 0x00, 0x00, 0x80, 0x03, 0x00, 0x00, 0x80, 0x03, 0x00,
   0x00, 0x00, 0x03, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};");

    $image{'q'}{$mw} = $mw->Bitmap(-foreground => 'blue',
				   -data => "#define q_width 32\n#define q_height 32
static unsigned char q_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xe0, 0x07, 0x00,
   0x00, 0x10, 0x0f, 0x00, 0x00, 0x18, 0x1e, 0x00, 0x00, 0x38, 0x1e, 0x00,
   0x00, 0x38, 0x1e, 0x00, 0x00, 0x10, 0x0f, 0x00, 0x00, 0x80, 0x07, 0x00,
   0x00, 0xc0, 0x01, 0x00, 0x00, 0xc0, 0x00, 0x00, 0x00, 0xc0, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0xc0, 0x00, 0x00, 0x00, 0xe0, 0x01, 0x00,
   0x00, 0xe0, 0x01, 0x00, 0x00, 0xc0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};");

    $image{'i'}{$mw} = $mw->Bitmap(-foreground => 'blue',
				     -data => "#define i_width 32\n#define i_height 32
static unsigned char i_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0xe0, 0x01, 0x00, 0x00, 0xf0, 0x03, 0x00, 0x00, 0xf0, 0x03, 0x00,
   0x00, 0xe0, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0xf8, 0x03, 0x00, 0x00, 0xf0, 0x03, 0x00, 0x00, 0xe0, 0x03, 0x00,
   0x00, 0xe0, 0x03, 0x00, 0x00, 0xe0, 0x03, 0x00, 0x00, 0xe0, 0x03, 0x00,
   0x00, 0xe0, 0x03, 0x00, 0x00, 0xe0, 0x03, 0x00, 0x00, 0xf0, 0x07, 0x00,
   0x00, 0xf8, 0x0f, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};");

    $image{'w1'}{$mw} = $mw->Bitmap(-foreground => 'black',
				      -data => "#define w1_width 32\n#define w1_height 32
static unsigned char w1_bits[] = {
   0x00, 0x80, 0x01, 0x00, 0x00, 0x40, 0x02, 0x00, 0x00, 0x20, 0x04, 0x00,
   0x00, 0x10, 0x04, 0x00, 0x00, 0x10, 0x08, 0x00, 0x00, 0x08, 0x08, 0x00,
   0x00, 0x08, 0x10, 0x00, 0x00, 0x04, 0x10, 0x00, 0x00, 0x04, 0x20, 0x00,
   0x00, 0x02, 0x20, 0x00, 0x00, 0x02, 0x40, 0x00, 0x00, 0x01, 0x40, 0x00,
   0x00, 0x01, 0x80, 0x00, 0x80, 0x00, 0x80, 0x00, 0x80, 0x00, 0x00, 0x01,
   0x40, 0x00, 0x00, 0x01, 0x40, 0x00, 0x00, 0x02, 0x20, 0x00, 0x00, 0x02,
   0x20, 0x00, 0x00, 0x04, 0x10, 0x00, 0x00, 0x04, 0x10, 0x00, 0x00, 0x08,
   0x08, 0x00, 0x00, 0x08, 0x08, 0x00, 0x00, 0x10, 0x04, 0x00, 0x00, 0x10,
   0x04, 0x00, 0x00, 0x20, 0x02, 0x00, 0x00, 0x20, 0x01, 0x00, 0x00, 0x40,
   0x01, 0x00, 0x00, 0x40, 0x01, 0x00, 0x00, 0x40, 0x02, 0x00, 0x00, 0x20,
   0xfc, 0xff, 0xff, 0x1f, 0x00, 0x00, 0x00, 0x00};");

    $image{'w2'}{$mw} = $mw->Bitmap(-foreground => 'yellow',
				      -data => "#define w2_width 32\n#define w2_height 32
static unsigned char w2_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x01, 0x00, 0x00, 0xc0, 0x03, 0x00,
   0x00, 0xe0, 0x03, 0x00, 0x00, 0xe0, 0x07, 0x00, 0x00, 0xf0, 0x07, 0x00,
   0x00, 0xf0, 0x0f, 0x00, 0x00, 0xf8, 0x0f, 0x00, 0x00, 0xf8, 0x1f, 0x00,
   0x00, 0xfc, 0x1f, 0x00, 0x00, 0xfc, 0x3f, 0x00, 0x00, 0xfe, 0x3f, 0x00,
   0x00, 0xfe, 0x7f, 0x00, 0x00, 0xff, 0x7f, 0x00, 0x00, 0xff, 0xff, 0x00,
   0x80, 0xff, 0xff, 0x00, 0x80, 0xff, 0xff, 0x01, 0xc0, 0xff, 0xff, 0x01,
   0xc0, 0xff, 0xff, 0x03, 0xe0, 0xff, 0xff, 0x03, 0xe0, 0xff, 0xff, 0x07,
   0xf0, 0xff, 0xff, 0x07, 0xf0, 0xff, 0xff, 0x0f, 0xf8, 0xff, 0xff, 0x0f,
   0xf8, 0xff, 0xff, 0x1f, 0xfc, 0xff, 0xff, 0x1f, 0xfe, 0xff, 0xff, 0x3f,
   0xfe, 0xff, 0xff, 0x3f, 0xfe, 0xff, 0xff, 0x3f, 0xfc, 0xff, 0xff, 0x1f,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};");

    $image{'w3'}{$mw} = $mw->Bitmap(-foreground => 'black',
				    -data => "#define w3_width 32\n#define w3_height 32
static unsigned char w3_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0xc0, 0x03, 0x00, 0x00, 0xe0, 0x07, 0x00, 0x00, 0xe0, 0x07, 0x00,
   0x00, 0xe0, 0x07, 0x00, 0x00, 0xe0, 0x07, 0x00, 0x00, 0xe0, 0x07, 0x00,
   0x00, 0xc0, 0x03, 0x00, 0x00, 0xc0, 0x03, 0x00, 0x00, 0xc0, 0x03, 0x00,
   0x00, 0x80, 0x01, 0x00, 0x00, 0x80, 0x01, 0x00, 0x00, 0x80, 0x01, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x01, 0x00, 0x00, 0xc0, 0x03, 0x00,
   0x00, 0xc0, 0x03, 0x00, 0x00, 0x80, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};");

    $class;
}

# ::tk::MessageBox --
#
#	Pops up a messagebox with an application-supplied message with
#	an icon and a list of buttons. This procedure will be called
#	by tk_messageBox if the platform does not have native
#	messagebox support, or if the particular type of messagebox is
#	not supported natively.
#
#	Color icons are used on Unix displays that have a color
#	depth of 4 or more and $tk_strictMotif is not on.
#
#	This procedure is a private procedure shouldn't be called
#	directly. Call tk_messageBox instead.
#
#	See the user documentation for details on what tk_messageBox does.
#
sub Populate {
    my($w, $args) = @_;

    $w->SUPER::Populate($args);

    {
	my $icon = delete $args->{-icon};
	if (!defined $icon) {
	    $icon = 'info';
	}
	if (!grep {$icon eq $_} qw(info warning error question)) {
	    die "bad -icon value \"$icon\": must be error, info, question, or warning";
	}
	if ($Tk::platform eq 'aqua') {
	    if ($icon eq 'error') {
		$icon = 'stop';
	    } elsif ($icon eq 'warning') {
		$icon = 'caution';
	    } elsif ($icon eq 'info') {
		$icon = 'note';
	    }
	}
	$w->{Configure}{icon} = $icon;
    }

    {
	my $type = delete $args->{-type};
	if (!defined $type) {
	    $type = 'ok';
	}
	if      ($type =~ m{^abortretryignore$}i) {
	    $w->{Names}  = [qw(abort retry ignore)];
	    $w->{Labels} = [qw(&Abort &Retry &Ignore)];
	    $w->{Cancel} = 'abort';
	} elsif ($type =~ m{^ok$}i) {
	    $w->{Names}  = [qw(ok)];
	    $w->{Labels} = [qw(&OK)];
	    $w->{Cancel} = 'ok';
	} elsif ($type =~ m{^okcancel$}i) {
	    $w->{Names}  = [qw(ok cancel)];
	    $w->{Labels} = [qw(&OK &Cancel)];
	    $w->{Cancel} = 'cancel';
	} elsif ($type =~ m{^retrycancel$}i) {
	    $w->{Names}  = [qw(retry cancel)];
	    $w->{Labels} = [qw(&Retry &Cancel)];
	    $w->{Cancel} = 'cancel';
	} elsif ($type =~ m{^yesno$}i) {
	    $w->{Names}  = [qw(yes no)];
	    $w->{Labels} = [qw(&Yes &No)];
	    $w->{Cancel} = 'no';
	} elsif ($type =~ m{^yesnocancel$}i) {
	    $w->{Names}  = [qw(yes no cancel)];
	    $w->{Labels} = [qw(&Yes &No &Cancel)];
	    $w->{Cancel} = 'cancel';
	} else {
	    die "bad -type value \"$type\": must be abortretryignore, ok, okcancel, retrycancel, yesno, or yesnocancel";
	}
	$w->{Configure}{type} = $type;
    }

    $w->iconname('Dialog');

    $w->withdraw;
    #
    # The default value of the title is space (" ") not the empty string
    # because for some window managers, a 
    #		wm title .foo ""
    # causes the window title to be "foo" instead of the empty string.
    #
    $w->ConfigSpecs
	('-default' => ['PASSIVE',undef,undef,''],
	 '-detail'  => ['PASSIVE',undef,undef,''],
	 '-message' => ['PASSIVE',undef,undef,''],
	 #not needed, we can do it different way in perl: '-parent' => ['PASSIVE' ...],
	 '-title'   => ['PASSIVE',undef,undef," "],
	);
    $w;
}

sub Show {
    my $w = shift;

    # If reusing the widget:
    $_->destroy for ($w->children);

    my @buttons;
    for my $i (0 .. $#{ $w->{Names} }) {
	my($name, $lab) = ($w->{Names}[$i], $w->{Labels}[$i]);
	push @buttons, [$name, -text => $lab]; # XXX missing: mc (message catalog)
    }

    # If no default button was specified, the default default is the 
    # first button (Bug: 2218).

    if ($w->cget(-default) eq "") {
	$w->configure(-default => $buttons[0][0]);
    }

    my $valid = 0;
    foreach my $btn (@buttons) {
	if ($btn->[0] eq $w->cget(-default)) {
	    $valid = 1;
	    last;
	}
    }
    if (!$valid) {
	die "invalid default button \"" . $w->cget(-default) . "\"";
    }

    ### skip the step 2, not needed in Perl/Tk

    # 3. Create the top-level window and divide it into top
    # and bottom parts.

    $w->title($w->cget(-title));
    $w->protocol('WM_DELETE_WINDOW' => sub {
		     $w->Subwidget($w->{Cancel})->invoke;
		 });
    # There is only one background colour for the whole dialog
    my $bg = $w->cget(-background);

    # Message boxes should be transient with respect to their parent so that
    # they always stay on top of the parent window.  But some window managers
    # will simply create the child window as withdrawn if the parent is not
    # viewable (because it is withdrawn or iconified).  This is not good for
    # "grab"bed windows.  So only make the message box transient if the parent
    # is viewable.
    #
    if ($w->parent->toplevel->viewable) {
	$w->transient($w->parent);
    }    

#XXX?
#     if {$windowingsystem eq "aqua"} {
# 	unsupported::MacWindowStyle style $w dBoxProc # XXX what's that?
#     }

    my $w_bot = $w->Frame(-background => $bg)->pack(qw(-side bottom -fill both));
#XXX??? NYI?    $w_bot->gridAnchor('center');
    my $w_top = $w->Frame(-background => $bg)->pack(qw(-side top -fill both -expand 1));
    if ($Tk::platform ne "aqua") {
	$w_bot->configure(qw(-relief raised -bd 1));
	$w_top->configure(qw(-relief raised -bd 1));
    }

    # 4. Fill the top part with bitmap, message and detail (use the
    # option database for -wraplength and -font so that they can be
    # overridden by the caller).

    $w->optionAdd('*MsgBox.msg.wrapLength', '3i', 'widgetDefault');
    $w->optionAdd('*MsgBox.dtl.wrapLength', '3i', 'widgetDefault');
    if ($Tk::platform eq "aqua") {
	$w->optionAdd('*MsgBox.msg.font', 'system', 'widgetDefault');
	$w->optionAdd('*MsgBox.dtl.font', 'system', 'widgetDefault');
    } else {
	$w->optionAdd('*MsgBox.msg.font', [qw(Times 14)], 'widgetDefault');
	$w->optionAdd('*MsgBox.dtl.font', [qw(Times 10)], 'widgetDefault');
    }

    my $w_msg = $w->Label(qw(Name msg -anchor nw -justify left),
			  -text => $w->cget(-message),
			  -background => $bg,
			 );
    my $w_dtl;
    if ($w->cget(-detail) ne "") {
	$w_dtl = $w->Label(qw(Name dtl -anchor nw -justify left),
			   -text => $w->cget(-detail),
			   -background => $bg,
			  );
    }
    my $w_bitmap;
    my $icon = $w->{Configure}{icon};
    if ($icon ne "") {
	if ($Tk::platform eq "aqua"
	    || ($w->depth < 4) || $Tk::strictMotif) {
	    $w_bitmap = $w->Label(Name => "bitmap",
				  -bitmap => $icon,
				  -background => $bg,
				 );
	} else {
	    $w_bitmap = $w->Canvas(Name => "bitmap",
				   qw(-width 32 -height 32 -highlightthickness 0),
				   -background => $bg,
				  );
	    my $mw = $w->MainWindow;
	    if ($icon eq 'error') {
		$w_bitmap->create(qw(oval 0 0 31 31 -fill red -outline black));
		$w_bitmap->create(qw(line 9 9 23 23 -fill white -width 4));
		$w_bitmap->create(qw(line 9 23 23 9 -fill white -width 4));
	    } elsif ($icon eq 'info') {
		$w_bitmap->create(qw(image 0 0 -anchor nw),
				  -image => $image{b1}{$mw});
		$w_bitmap->create(qw(image 0 0 -anchor nw),
				  -image => $image{b2}{$mw});
		$w_bitmap->create(qw(image 0 0 -anchor nw),
				  -image => $image{i}{$mw});
	    } elsif ($icon eq 'question') {
		$w_bitmap->create(qw(image 0 0 -anchor nw),
				  -image => $image{b1}{$mw});
		$w_bitmap->create(qw(image 0 0 -anchor nw),
				  -image => $image{b2}{$mw});
		$w_bitmap->create(qw(image 0 0 -anchor nw),
				  -image => $image{'q'}{$mw});
	    } else {
		$w_bitmap->create(qw(image 0 0 -anchor nw),
				  -image => $image{w1}{$mw});
		$w_bitmap->create(qw(image 0 0 -anchor nw),
				  -image => $image{w2}{$mw});
		$w_bitmap->create(qw(image 0 0 -anchor nw),
				  -image => $image{w3}{$mw});
	    }
	}
    } else {
	# Unlike in Tcl/Tk, a placeholder is needed here
	$w_bitmap = $w->Label;
    }
    Tk::grid($w_bitmap, $w_msg, -in => $w_top, qw(-sticky news -padx 2m -pady 2m));
    $w_top->gridColumnconfigure(qw(1 -weight 1));
    if (Tk::Exists($w_dtl)) {
	Tk::grid('^', $w_dtl, -in => $w_top, qw(-sticky news -padx 2m), -pady => [0, '2m']);
	$w_top->gridRowconfigure(qw(1 -weight 1));
    } else {
	$w_top->gridRowconfigure(qw(0 -weight 1));
    }

    # 5. Create a row of buttons at the bottom of the dialog.

    my $i = 0;
    for my $but (@buttons) {
	my($name, @opts) = @$but;
	if (!@opts) {
	    # Capitalize the first letter of $name
	    @opts = (-text => ucfirst $name);
	}

	my $cw = $w->AmpWidget('Button', -padx => '3m', @opts,
			       -command => sub { $w->{selectedButton} = $name });
	$w->Advertise($name => $cw);
	if ($name eq $w->cget(-default)) {
	    $cw->configure(qw(-default active));
	} else {
	    $cw->configure(qw(-default normal));
	}
	$cw->grid(-in => $w_bot, -row => 0, -column => $i,
		  qw(-padx 3m -pady 2m -sticky ew));
	$w_bot->gridColumnconfigure($i, -uniform => 'buttons');
        $i++;

	# create the binding for the key accelerator, based on the underline
	#
        # set underIdx [$w.$name cget -under]
        # if {$underIdx >= 0} {
        #     set key [string index [$w.$name cget -text] $underIdx]
        #     bind $w <Alt-[string tolower $key]>  [list $w.$name invoke]
        #     bind $w <Alt-[string toupper $key]>  [list $w.$name invoke]
        # }
    }
    $w->bind('<Alt-Key>', [sub { $w->AltKeyInDialog($_[1]) }, Ev('%A')]);

    if ($w->cget(-default) ne "") {
	$w->bind('<FocusIn>', sub {
		     my $e = shift->XEvent;
		     my $w = $e->W;
		     if ($w->isa('Tk::Button')) {
			 $w->configure(qw(-default active));
		     }
		 });
	$w->bind('<FocusOut>', sub {
		     my $e = shift->XEvent;
		     my $w = $e->W;
		     if ($w->isa('Tk::Button')) {
			 $w->configure(qw(-default normal));
		     }
		 });
    }

    # 6. Create bindings for <Return>, <Escape> and <Destroy> on the dialog

    $w->bind('<Return>' => sub {
		 my $e = shift->XEvent;
		 my $w = $e->W;
		 if ($w->isa('Tk::Button')) {
		     $w->invoke;
		 }
	     });

    # Invoke the designated cancelling operation
    $w->bind('<Escape>' => sub { $w->Subwidget($w->{Cancel})->invoke });

    # At <Destroy> the buttons have vanished, so must do this directly.
    $w_msg->bind('<Destroy>' => sub { $w->{selectedButton} = $w->{Cancel} });

    # 7. Withdraw the window, then update all the geometry information
    # so we know how big it wants to be, then center the window in the
    # display and de-iconify it.

    #    ::tk::PlaceWindow $w widget $data(-parent)
    $w->withdraw;
    $w->Popup;

    # 8. Set a grab and claim the focus too.

    my $focus;
    if ($w->cget(-default) ne "") {
	$focus = $w->Subwidget($w->cget(-default));
    } else {
	$focus = $w;
    }
    $w->SetFocusGrab($focus);

    # 9. Wait for the user to respond, then restore the focus and
    # return the index of the selected button.  Restore the focus
    # before deleting the window, since otherwise the window manager
    # may take the focus away so we can't redirect it.  Finally,
    # restore any grab that was in effect.

    $w->waitVariable(\$w->{selectedButton});
    # Copy the result now so any <Destroy> that happens won't cause
    # trouble
    my $result = $w->{selectedButton};

    $w->RestoreFocusGrab($focus, 'withdraw');

    return $result;
}

sub _tk_messageBox {
    my(%args) = @_;
    my $parent = delete $args{-parent};
    my $md = $parent->MsgBox(%args);
    my $an = $md->Show;
    $md->destroy;
    $an;
}

1;

__END__
