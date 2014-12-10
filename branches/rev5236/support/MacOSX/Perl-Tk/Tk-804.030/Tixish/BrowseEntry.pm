#
# BrowseEntry is a stripped down version of ComboBox.tcl from Tix4.0
#
# Some additions by Slaven Rezic <slaven@rezic.de> to make the widget
# look like the Windows' Combobox. There are also additional options.
#

package Tk::BrowseEntry;

use vars qw($VERSION);
$VERSION = '4.015'; # was: sprintf '4.%03d', q$Revision: #13 $ =~ /\D(\d+)\s*$/;

use Tk qw(Ev);
use Carp;
use strict;

use base qw(Tk::Frame);
Construct Tk::Widget 'BrowseEntry';

require Tk::LabEntry;

sub LabEntryWidget { "LabEntry" }
sub ButtonWidget   { "Button"   }
sub ListboxWidget  { "Listbox"  }

sub Populate {
    my ($w, $args) = @_;

    my %labelArgs;
    while(my($k,$v) = each %$args) {
	$labelArgs{$k} = $v;
	delete $args->{$k};
    }

    $w->Tk::Frame::Populate($args);

    while(my($k,$v) = each %labelArgs) {
	$args->{$k} = $v;
    }

    # entry widget and arrow button
    my $lpack = delete $args->{-labelPack};
    if (not defined $lpack) {
	$lpack = [-side => 'left', -anchor => 'e'];
    }
    $w->{_BE_Style} = delete $args->{-style} || $Tk::platform;
    my $LabEntry = $w->LabEntryWidget;
    my $Listbox  = $w->ListboxWidget;
    my $Button   = $w->ButtonWidget;
    # XXX should this be retained?
#      if (defined $args->{-state} and $args->{-state} eq 'readonly') { # XXX works only at construction time
#  	$LabEntry = "NoSelLabEntry";
#  	require Tk::NoSelLabEntry;
#      }
    my $e;
    my $var = "";
    my @LabEntry_args = (-textvariable => \$var);
    if (exists $args->{-label}) {
	$e = $w->$LabEntry(-labelPack => $lpack,
			   -label => delete $args->{-label},
			   @LabEntry_args,
			  );
    } else {
	$e = $w->$LabEntry(@LabEntry_args);
    }
    my $b = $w->$Button(-bitmap => '@' . Tk->findINC($w->{_BE_Style} eq 'MSWin32' ? 'arrowdownwin.xbm' : 'cbxarrow.xbm'));
    $w->Advertise('entry' => $e);
    $w->Advertise('arrow' => $b);

    # Pack the button to align vertically with the entry widget
    my @anch;
    my $edge = {@$lpack}->{-side};
    push(@anch,-anchor => 's') if ($edge && $edge eq 'top');
    push(@anch,-anchor => 'n') if ($edge && $edge eq 'bottom');
    $b->pack(-side => 'right', -padx => 1, @anch);

    $e->pack(-side => 'right', -fill => 'x', -expand => 1); #XXX, -padx => 1);

    # popup shell for listbox with values.
    my $c = $w->Toplevel(-bd => 2,
			 -relief => ($w->{_BE_Style} eq 'MSWin32'
				     ? "solid" : "raised"));
    $c->overrideredirect(1);
    $c->withdraw;
    my $sl = $c->Scrolled( $Listbox, qw/-selectmode browse -scrollbars oe/ );
    if ($w->{_BE_Style} eq 'MSWin32' and $Tk::platform eq 'MSWin32') {
	$sl->configure(-bg => 'SystemWindow', -relief => "flat");
    }
    $w->Advertise('choices' => $c);
    $w->Advertise('slistbox' => $sl);
    $sl->pack(-expand => 1, -fill => 'both');

    $sl->Subwidget("scrolled")->bind("<Motion>",sub {
	return unless ($w->{_BE_Style} eq 'MSWin32');
	my $e = $_[0]->XEvent;
	my $y = $e->y;
	my $inx = $sl->nearest($y);
	if (defined $inx) {
	    $sl->selectionClear(0, "end");
	    $sl->selectionSet($inx);
	}
   });

    # other initializations
    $w->SetBindings;
    $w->{'_BE_popped'} = 0;
    $w->Delegates(get => $sl, DEFAULT => $e);
    $w->ConfigSpecs(
        -font        => [qw/DESCENDANTS font Font/],
        -listwidth   => [qw/PASSIVE  listWidth   ListWidth/,   undef],
        -listheight  => [{-height => $sl}, qw/listHeight ListHeight/, undef],
        -listcmd     => [qw/CALLBACK listCmd     ListCmd/,     undef],
        -autolistwidth   => [qw/PASSIVE autoListWidth AutoListWidth/, undef],
        -autolimitheight => [qw/PASSIVE autoLimitHeight AutoLimitHeight 0/],
        -browsecmd   => [qw/CALLBACK browseCmd   BrowseCmd/,   undef],
	-browse2cmd  => [qw/CALLBACK browse2Cmd  Browse2Cmd/,  undef],
        -choices     => [qw/METHOD   choices     Choices/,     undef],
        -state       => [qw/METHOD   state       State         normal/],
        -arrowimage  => [ {-image => $b}, qw/arrowImage ArrowImage/, undef],
        -variable    => [ {'-textvariable' => $e} ],
	-colorstate  => [qw/PASSIVE  colorState  ColorState/,  undef],
        -command     => '-browsecmd',
        -options     => '-choices',
	-label       => [qw/PASSIVE  label       Label/,       undef],
	-labelPack   => [qw/PASSIVE  labelPack   LabelPack/,   undef],
		    #-background  => [$e, qw/background Background/,   undef],
		    #-foreground  => [$e, qw/foreground Foreground/,   undef],
	-buttontakefocus => [{-takefocus => $b}, 'buttonTakefocus',
			     'ButtonTakefocus', 1],
        DEFAULT      => [$e] );
}

sub SetBindings {
    my ($w) = @_;

    my $e = $w->Subwidget('entry');
    my $b = $w->Subwidget('arrow');

    # set bind tags
    $w->bindtags([$w, 'Tk::BrowseEntry', $w->toplevel, 'all']);
    # as we don't bind $e here leave its tags alone ...
    # $e->bindtags([$e, ref($e), $e->toplevel, 'all']);

    # bindings for the button and entry
    $b->bind('<1>',[$w,'BtnDown']);
    $b->toplevel->bind('<ButtonRelease-1>',[$w,'ButtonHack']);
    $b->bind('<space>',[$w,'space']);

    # bindings for listbox
    my $sl = $w->Subwidget('slistbox');
    my $l = $sl->Subwidget('listbox');
    $l->bind('<ButtonRelease-1>',[$w,'ListboxRelease',Ev('x'),Ev('y')]);
    $l->bind('<Escape>' => [$w,'LbClose']);
    $l->bind('<Return>' => [$w,'Return',$l]);

    # allow click outside the popped up listbox to pop it down.
    $w->bind('<1>','BtnDown');
}

sub space
{
 my $w = shift;
 $w->BtnDown;
 $w->{'_BE_savefocus'} = $w->focusCurrent;
 $w->Subwidget('slistbox')->focus;
}


sub ListboxRelease
{
 my ($w,$x,$y) = @_;
 $w->ButtonHack;
 $w->LbChoose($x, $y);
}

sub Return
{
 my ($w,$l) = @_;
 my($x, $y) = $l->bbox($l->curselection);
 $w->LbChoose($x, $y)
}


sub BtnDown {
    my ($w) = @_;
    return if $w->cget( '-state' ) eq 'disabled';

    if ($w->{'_BE_popped'}) {
	$w->Popdown;
	$w->{'_BE_buttonHack'} = 0;
    } else {
	$w->PopupChoices;
	$w->{'_BE_buttonHack'} = 1;
    }
}

sub PopupChoices {
    my ($w) = @_;

    if (!$w->{'_BE_popped'}) {
	$w->Callback(-listcmd => $w);
	my $e = $w->Subwidget('entry');
	my $c = $w->Subwidget('choices');
	my $s = $w->Subwidget('slistbox');
	my $a = $w->Subwidget('arrow');
	my $y1 = ($w->{_BE_Style} eq 'MSWin32'
		  ? $a->rooty + $a->height
		  : $e->rooty + $e->height + 3
		 );
	my $bd = $c->cget(-bd) + $c->cget(-highlightthickness);
	# using the real listbox reqheight rather than the
	# container frame one, which does not change after resizing the
	# listbox
	my $ht = $s->Subwidget("scrolled")->reqheight + 2 * $bd;
	my $x1 = ($w->{_BE_Style} eq 'MSWin32'
		  ? $e->Subwidget("entry")->rootx
		  : $e->rootx
		 );
	my ($width, $x2);
	if (defined $w->cget(-listwidth)) {
	    $width = $w->cget(-listwidth);
	    $x2 = $x1 + $width;
	} else {
	    $x2 = $a->rootx + $a->width;
	    $width = $x2 - $x1;
	}
    	my $rw = $c->reqwidth;
    	if ($rw < $width) {
    	    $rw = $width
    	} else {
    	    if ($rw > $width * 3) {
    		$rw = $width * 3;
    	    }
    	    if ($rw > $w->vrootwidth) {
    		$rw = $w->vrootwidth;
    	    }
    	}
    	$width = $rw;

	# if listbox is too far right, pull it back to the left
	#
	if ($x2 > $w->vrootwidth) {
	    $x1 = $w->vrootwidth - $width;
	}

	# if listbox is too far left, pull it back to the right
	#
	if ($x1 < 0) {
	    $x1 = 0;
	}

	# if listbox is below bottom of screen, pull it up.
	# check the Win32 taskbar, if possible
	my $rootheight;
	if ($Tk::platform eq 'MSWin32' and $^O eq 'MSWin32') {
	    eval {
		require Win32Util; # XXX should not use a non-CPAN widget
		$rootheight = (Win32Util::screen_region($w))[3];
	    };
	}
	if (!defined $rootheight) {
	    $rootheight = $w->vrootheight;
	}

	my $y2 = $y1 + $ht;
	if ($y2 > $rootheight) {
	    $y1 = $y1 - $ht - ($e->height - 5);
	}
	$c->geometry(sprintf('%dx%d+%d+%d', $rw, $ht, $x1, $y1));
	$c->deiconify;
	$c->raise;
	$e->focus;
	$w->{'_BE_popped'} = 1;

	# highlight current selection
	my $current_sel = $e->get;
	if (defined $current_sel) {
	    my $i = 0;
	    foreach my $str ($s->get(0, "end")) {
		local $^W = 0; # in case of undefined strings
		if ($str eq $current_sel) {
		    $s->selectionClear(0, "end");
		    $s->selectionSet($i);
		    last;
		}
		$i++;
	    }
	}

	$c->configure(-cursor => 'arrow');
	$w->{'_BE_grabinfo'} = $w->grabSave;
	$w->grabGlobal;
    }
}

# choose value from listbox if appropriate
sub LbChoose {
    my ($w, $x, $y) = @_;
    my $l = $w->Subwidget('slistbox')->Subwidget('listbox');
    if ((($x < 0) || ($x > $l->Width)) ||
        (($y < 0) || ($y > $l->Height))) {
        # mouse was clicked outside the listbox... close the listbox
        $w->LbClose;
    } else {
        # select appropriate entry and close the listbox
        $w->LbCopySelection;
	$w->Callback(-browsecmd, $w, $w->Subwidget('entry')->get());
	$w->Callback(-browse2cmd => $w, $w->LbIndex);
    }
}

# close the listbox after clearing selection
sub LbClose {
    my ($w) = @_;
    my $l = $w->Subwidget('slistbox')->Subwidget('listbox');
    $l->selection('clear', 0, 'end');
    $w->Popdown;
}

# copy the selection to the entry and close listbox
sub LbCopySelection {
    my ($w) = @_;
    my $index = $w->LbIndex;
    if (defined $index) {
	$w->{'_BE_curIndex'} = $index;
	my $l = $w->Subwidget('slistbox')->Subwidget('listbox');
        my $var_ref = $w->cget( '-textvariable' );
        $$var_ref = $l->get($index);
	if ($w->{'_BE_popped'}) {
	    $w->Popdown;
	}
    }
    $w->Popdown;
}

sub LbIndex {
    my ($w, $flag) = @_;
    my ($sel) = $w->Subwidget('slistbox')->Subwidget('listbox')->curselection;
    if (defined $sel) {
	return int($sel);
    } else {
	if (defined $flag && ($flag eq 'emptyOK')) {
	    return undef;
	} else {
	    return 0;
	}
    }
}

# pop down the listbox
sub Popdown {
    my ($w) = @_;
    if ($w->{'_BE_savefocus'} && Tk::Exists($w->{'_BE_savefocus'})) {
	$w->{'_BE_savefocus'}->focus;
	delete $w->{'_BE_savefocus'};
    }
    if ($w->{'_BE_popped'}) {
	my $c = $w->Subwidget('choices');
	$c->withdraw;
	$w->grabRelease;
	if (ref $w->{'_BE_grabinfo'} eq 'CODE') {
	    $w->{'_BE_grabinfo'}->();
	    delete $w->{'_BE_grabinfo'};
	}
	$w->{'_BE_popped'} = 0;
    }
}

# This hack is to prevent the ugliness of the arrow being depressed.
#
sub ButtonHack {
    my ($w) = @_;
    if ($w->{'_BE_buttonHack'}) {
	my $b = $w->Subwidget('arrow');
	if (Tk::Exists($b)) {
	    $b->butUp;
	}
    }
}

sub choices
{
 my ($w,$choices) = @_;
 if (@_ > 1)
  {
   $w->delete( qw/0 end/ );
   my %hash;
   my $var = $w->cget('-textvariable');
   my $old = $$var;
   foreach my $val (@$choices)
    {
     local $^W = 0; # in case of undefined values
     $w->insert( 'end', $val);
     $hash{$val} = 1;
    }
   $old = $choices->[0]
    if defined $old && !exists $hash{$old} && defined $choices->[0];
   $$var = $old;
  }
 else
  {
   return( $w->get( qw/0 end/ ) );
  }
}

sub _set_edit_state {
    my( $w, $state ) = @_;

    my $entry  = $w->Subwidget( 'entry' );
    my $button = $w->Subwidget( 'arrow' );

    if ($w->cget( '-colorstate' )) {
	my $color;
	if( $state eq 'normal' ) {                  # Editable
	    $color = 'gray95';
	} else {                                    # Not Editable
	    $color = $w->cget( -background ) || 'lightgray';
	}
	$entry->Subwidget( 'entry' )->configure( -background => $color );
    }

    if( $state eq 'readonly' ) {
        $entry->configure( -state => 'disabled' );
        $button->configure( -state => 'normal' );
	if ($w->{_BE_Style} eq 'MSWin32') {
	    $entry->bind('<1>',[$w,'BtnDown']);
	    $w->{_BE_OriginalCursor} = $entry->cget( -cursor );
	    $entry->configure( -cursor => 'left_ptr' );
	}
    } else {
        $entry->configure( -state => $state );
	if (exists $w->{_BE_OriginalCursor}) {
	    $entry->configure(-cursor => delete $w->{_BE_OriginalCursor});
	}
        $button->configure( -state => $state );
	if ($w->{_BE_Style} eq 'MSWin32') {
	    $entry->bind('<1>',['Button1',Tk::Ev('x')]);
	}
    }
}

sub state {
    my $w = shift;
    unless( @_ ) {
        return( $w->{Configure}{-state} );
    } else {
        my $state = shift;
        $w->{Configure}{-state} = $state;
        $w->_set_edit_state( $state );
    }
}

sub _max {
    my $max = shift;
    foreach my $val (@_) {
        $max = $val if $max < $val;
    }
    return( $max );
}

sub shrinkwrap {
    my( $w, $size ) = @_;

    unless( defined $size ) {
        $size = _max( map( length, $w->get( qw/0 end/ ) ) ) || 0;;
    }

    my $lb = $w->Subwidget( 'slistbox' )->Subwidget( 'listbox' );
    $w->configure(  -width => $size );
    $lb->configure( -width => $size );
}

sub limitheight {
    my $w = shift;
    my $choices_number = shift || $w->Subwidget('slistbox')->index("end");
    $choices_number = 10 if $choices_number > 10;
    $w->configure(-listheight => $choices_number) if ($choices_number > 0);
}

sub insert {
    my $w = shift;
    $w->Subwidget("slistbox")->insert(@_);
    if ($w->cget(-autolimitheight)) {
	$w->limitheight;
    }
    if ($w->cget(-autolistwidth)) {
	$w->updateListWidth(@_[1..$#_]);
    }
}

sub delete {
    my $w = shift;
    $w->Subwidget("slistbox")->delete(@_);
    if ($w->cget(-autolimitheight)) {
	$w->limitheight;
    }
    if ($w->cget(-autolistwidth)) {
	$w->updateListWidth();
    }
}

sub updateListWidth {
    my $w = shift;
    my @ins = @_;
    if (!@ins) {
	@ins = $w->get(0, "end");
    }

    my $max_width = 0;
    foreach my $ins (@ins) {
	my $new_width = $w->fontMeasure($w->cget(-font), $ins);
	if ($new_width > $max_width) {
	    $max_width = $new_width;
	}
    }
    if ($max_width > 20) { # be sane
	$w->configure(-listwidth => $max_width + 32); # XXX for scrollbar
    }
}

1;

__END__

