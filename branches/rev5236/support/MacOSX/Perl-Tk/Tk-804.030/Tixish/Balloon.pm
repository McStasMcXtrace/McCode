#
# The help widget that provides both "balloon" and "status bar"
# types of help messages.
#
# This is a patched version of Balloon 3.037 - it adds support
# for different orientations of the balloon widget, depending
# on wether there's enough space for it. The little arrow now
# should always point directly to the client.
# Added by Gerhard Petrowitsch (gerhard.petrowitsch@philips.com)
#
# Nov 1, 2003 - Jack Dunnigan
# Added support for more than one screen in single logical
# screen mode (i.e. xinerama, dual monitors)

package Tk::Balloon;

use vars qw($VERSION);
$VERSION = '4.012'; # was: sprintf '4.%03d', q$Revision: #10 $ =~ /\D(\d+)\s*$/;

use Tk qw(Ev Exists);
use Carp;
require Tk::Toplevel;

Tk::Widget->Construct('Balloon');
use base qw(Tk::Toplevel);

# use UNIVERSAL; avoid the UNIVERSAL.pm file subs are XS in perl core

use strict;

my @balloons;
my $button_up = 0;
my %arrows = ( TL => 'R0lGODlhBgAGAJEAANnZ2QAAAP///////yH5BAEAAAAALAAAAAAGAAYAAAINjA0HAEdwLCwMKIQfBQA7',
	       TR => 'R0lGODlhBgAGAJEAANnZ2QAAAP///////yH5BAEAAAAALAAAAAAGAAYAAAIRBGMDwAEQkgAIAAoCABEEuwAAOw==',
	       BR => 'R0lGODlhBgAGAJEAANnZ2QAAAP///////yH5BAEAAAAALAAAAAAGAAYAAAIPDOHHhYVRAIgIAEISQLELADs=',
	       BL => 'R0lGODlhBgAGAJEAANnZ2QAAAP///////yH5BAEAAAAALAAAAAAGAAYAAAIPhB1xAUFALCIMKAaAWQAVADs=',
	       NO => 'R0lGODlhAQABAJEAANnZ2f///////////yH5BAEAAAAALAAAAAABAAEAAAICRAEAOw=='
	     );


sub ClassInit {
    my ($class, $mw) = @_;
    $mw->bind('all', '<Motion>', ['Tk::Balloon::Motion', Ev('X'), Ev('Y'), Ev('s')]);
    $mw->bind('all', '<Leave>',  ['Tk::Balloon::Motion', Ev('X'), Ev('Y'), Ev('s')]);
    $mw->bind('all', '<Button>', 'Tk::Balloon::ButtonDown');
    $mw->bind('all', '<ButtonRelease>', 'Tk::Balloon::ButtonUp');
    return $class;
}

sub Populate {
    my ($w, $args) = @_;

    $w->SUPER::Populate($args);

    $w->overrideredirect(1);
    $w->withdraw;
    # Only the container frame's background should be black... makes it
    # look better.
    $w->configure(-background => 'black');

    # the balloon arrows
    $w->{img_tl} = $w->Photo(-data => $arrows{TL}, -format => 'gif');
    $w->{img_tr} = $w->Photo(-data => $arrows{TR}, -format => 'gif');
    $w->{img_bl} = $w->Photo(-data => $arrows{BL}, -format => 'gif');
    $w->{img_br} = $w->Photo(-data => $arrows{BR}, -format => 'gif');
    $w->{img_no} = $w->Photo(-data => $arrows{NO}, -format => 'gif');
    $w->OnDestroy([$w, '_destroyed']);

    $w->{'pointer'} = $w->Label(-bd=>0, -relief=>'flat',-image=>$w->{img_no});

    # the balloon message
    # We give the Label a big borderwidth
    # ..enough to slide a 6x6 gif image along the border including some space

    my $ml = $w->Label(-bd => 0,
                -padx => 10,
                -pady => 3,
                -justify => 'left',
                -relief=>'flat');
    $w->Advertise('message' => $ml);

    $ml->pack(
        -side => 'top',
        -anchor => 'nw',
        -expand => 1,
        -fill => 'both',
        -padx => 0,
        -pady => 0);

    # append to global list of balloons
    push(@balloons, $w);
    $w->{'popped'} = 0;
    $w->{'buttonDown'} = 0;
    $w->{'menu_index'} = 'none';
    $w->{'menu_index_over'} = 'none';
    $w->{'canvas_tag'} = '';
    $w->{'canvas_tag_over'} = '';
    $w->{'current_screen'} = 0;

    $w->ConfigSpecs(-installcolormap => ['PASSIVE', 'installColormap', 'InstallColormap', 0],
		    -initwait => ['PASSIVE', 'initWait', 'InitWait', 350],
		    -state => ['PASSIVE', 'state', 'State', 'both'],
		    -statusbar => ['PASSIVE', 'statusBar', 'StatusBar', undef],
		    -statusmsg => ['PASSIVE', 'statusMsg', 'StatusMsg', ''],
		    -balloonmsg => ['PASSIVE', 'balloonMsg', 'BalloonMsg', ''],
		    -balloonposition => ['PASSIVE', 'balloonPosition', 'BalloonPosition', 'widget'],
		    -postcommand => ['CALLBACK', 'postCommand', 'PostCommand', undef],
		    -cancelcommand => ['CALLBACK', 'cancelCommand', 'CancelCommand', undef],
		    -motioncommand => ['CALLBACK', 'motionCommand', 'MotionCommand', undef],
		    -background => ['DESCENDANTS', 'background', 'Background', '#C0C080'],
                    -foreground => ['DESCENDANTS', 'foreground', 'Foreground', undef],
		    -font => [$ml, 'font', 'Font', '-*-helvetica-medium-r-normal--*-120-*-*-*-*-*-*'],
		    -borderwidth => ['SELF', 'borderWidth', 'BorderWidth', 1],
                    -numscreens=>['PASSIVE', 'numScreens','NumScreens',1],
		   );
}

sub _get_client {
    my ($w, $client) = @_;
    if ($client->can("Subwidget") and my $scrolled = $client->Subwidget("scrolled")) {
        $scrolled;
    } else {
	$client;
    }
}

# attach a client to the balloon
sub attach {
    my ($w, $client, %args) = @_;
    $client = $w->_get_client($client);
    foreach my $key (grep(/command$/,keys %args))
     {
      $args{$key} = Tk::Callback->new($args{$key});
     }
    my $msg = delete $args{-msg};
    $args{-balloonmsg} = $msg unless exists $args{-balloonmsg};
    $args{-statusmsg}  = $msg unless exists $args{-statusmsg};
    $w->{'clients'}{$client} = \%args;
    $client->OnDestroy([$w, 'detach', $client]);
}

# detach a client from the balloon.
sub detach {
    my ($w, $client) = @_;
    $client = $w->_get_client($client);
    if (Exists($w))
     {
      $w->Deactivate if ($client->IS($w->{'client'}));
     }
    delete $w->{'clients'}{$client};
}

sub GetOption
{
 my ($w,$opt,$client) = @_;
 $client = $w->{'client'} unless defined $client;
 if (defined $client)
  {
   my $info = $w->{'clients'}{$client};
   return $info->{$opt} if exists $info->{$opt};
  }
 return $w->cget($opt);
}

sub Motion {
    my ($ewin, $x, $y, $s) = @_;

    return if not defined $ewin;

    # Find which window we are over
    my $over = $ewin->Containing($x, $y);

    return if &grabBad($ewin, $over);

    foreach my $w (@balloons) {
	# if cursor has moved over the balloon -- ignore
	next if defined $over and $over->toplevel eq $w;

	# find the client window that matches
	my $client = $over;
	while (defined $client) {
	    last if (exists $w->{'clients'}{$client});
	    if ($client->can("MasterMenu")) {
		my $master = $client->MasterMenu;
		if ($master && exists $w->{'clients'}{$master}) {
		    $w->{'clients'}{$client} = $w->{'clients'}{$master};
		    last;
		}
	    }
	    $client = $client->Parent;
	}
	if (defined $client) {
	    # popping up disabled -- ignore
	    my $state = $w->GetOption(-state => $client);
	    next if $state eq 'none';
	    # Check if a button was recently released:
	    my $deactivate = 0;
	    if ($button_up) {
	      $deactivate = 1;
	      $button_up = 0;
	    }
	    # Deactivate it if the motioncommand says to:
            my $command = $w->GetOption(-motioncommand => $client);
	    $deactivate = $command->Call($client, $x, $y) if defined $command;
            if ($deactivate)
             {
              $w->Deactivate;
             }
            else
             {
              # warn "deact: $client $w->{'client'}";
              $w->Deactivate unless $client->IS($w->{'client'});
              my $msg = $client->BalloonInfo($w,$x,$y,'-statusmsg','-balloonmsg');
              if (defined($msg))
               {
                my $delay = delete $w->{'delay'};
                $delay->cancel if defined $delay;
                my $initwait = $w->GetOption(-initwait => $client);
                $w->{'delay'} = $client->after($initwait, sub {$w->SwitchToClient($client);});
                $w->{'client'} = $client;
               }
             }
	} else {
	    # cursor is at a position covered by a non client
	    # pop down the balloon if it is up or scheduled.
	    $w->Deactivate;
	}
    }
}

sub ButtonDown {
    my ($ewin) = @_;

    foreach my $w (@balloons) {
	    $w->Deactivate;
    }
}

sub ButtonUp {
    $button_up = 1;
}

# switch the balloon to a new client
sub SwitchToClient {
    my ($w, $client) = @_;
    return unless Exists($w);
    return unless Exists($client);
    return unless $client->IS($w->{'client'});
    return if &grabBad($w, $client);
    my $command = $w->GetOption(-postcommand => $client);
    if (defined $command) {
    	# Execute the user's command and return if it returns false:
	    my $pos = $command->Call($client);
	    return if not $pos;
	    if ($pos =~ /^(\d+),(\d+)$/) {
	        # Save the returned position so the Popup method can use it:
	        $w->{'clients'}{$client}{'postposition'} = [$1, $2];
	    }
    }
    my $state = $w->GetOption(-state => $client);
    $w->Popup if ($state =~ /both|balloon/);
    $w->SetStatus if ($state =~ /both|status/);
    $w->{'popped'} = 1;
    $w->{'delay'}  = $w->repeat(200, ['Verify', $w, $client]);
}

sub grabBad {

    my ($w, $client) = @_;

    return 0 unless Exists($client);
    my $g = $w->grabCurrent;
    return 0 unless defined $g;
    return 0 if $g->isa('Tk::Menu');
    return 0 if $g eq $client;

    # Ignore grab check if $w is the balloon itself.
    # XXX Why is this necessary? Is it possible to remove the grabBad
    # condition in SwitchToClient altogether?
    return 0 if $w->isa(__PACKAGE__);

    # The grab is OK if $client is a decendant of $g. Use the internal Tcl/Tk
    # pathname (yes, it's cheating, but it's legal).

    return 0 if $g == $w->MainWindow;
    my $wp = $w->PathName;
    my $gp = $g->PathName;
    return 0 if $wp =~ /^$gp/;
    return 1;                   # bad grab

} # end grabBad


sub Subclient
{
 my ($w,$data) = @_;
 if (defined($w->{'subclient'}) && (!defined($data) || $w->{'subclient'} ne $data))
  {
   $w->Deactivate;
  }
 $w->{'subclient'} = $data;
}

sub Verify {
    my $w      = shift;
    my $client = shift;
    my ($X,$Y) = (@_) ? @_ : ($w->pointerxy);
    my $over = $w->Containing($X,$Y);
    return if not defined $over or ($over->toplevel eq $w);
    my $deactivate = # DELETE? or move it to the isa-Menu section?:
		     # ($over ne $client) or
		     not $client->IS($w->{'client'})
#                     or (!$client->isa('Tk::Menu') && $w->grabCurrent);
#                     or $w->grabbad($client);
		     or &grabBad($w, $client);
    if ($deactivate)
     {
      $w->Deactivate;
     }
    else
     {
      $client->BalloonInfo($w,$X,$Y,'-statusmsg','-balloonmsg');
     }
}

sub Deactivate {
    my ($w) = @_;
    my $delay = delete $w->{'delay'};
    $delay->cancel if defined $delay;
    if ($w->{'popped'}) {
	    my $client = $w->{'client'};
	    my $command = $w->GetOption(-cancelcommand => $client);
	    if (defined $command) {
	        # Execute the user's command and return if it returns false:
	        return if not $command->Call($client);
	    }
	    $w->withdraw;
	    $w->ClearStatus;
	    $w->{'popped'} = 0;
	    $w->{'menu_index'} = 'none';
	    $w->{'canvas_tag'} = '';
    }
    $w->{'client'} = undef;
    $w->{'subclient'} = undef;
    $w->{'location'} = undef;
}

sub Popup {
    my ($w) = @_;
    if ($w->cget(-installcolormap)) {
	    $w->colormapwindows($w->winfo('toplevel'))
    }
    my $client = $w->{'client'};
    return if not defined $client or not exists $w->{'clients'}{$client};
    my $msg = $client->BalloonInfo($w, $w->pointerxy,'-balloonmsg');
    # Dereference it if it looks like a scalar reference:
    $msg = $$msg if UNIVERSAL::isa($msg, 'SCALAR');

    $w->Subwidget('message')->configure(-text => $msg);
    $w->idletasks;

    return unless Exists($w);
    return unless Exists($client);
    return if $msg eq '';  # Don't popup empty balloons.

    my ($x, $y);
    my $pos = $w->GetOption(-balloonposition => $client);
    my $postpos = delete $w->{'clients'}{$client}{'postposition'};
    if (defined $postpos) {
	# The postcommand must have returned a position for the balloon - I will use that:
	($x,$y) = @{$postpos};
    } elsif ($pos eq 'mouse') {
        ($x,$y)=$client->pointerxy; # We adjust the position later
    } elsif ($pos eq 'widget') {
	$x = int($client->rootx + $client->width/2);
	$y = int($client->rooty + int ($client->height/1.3));
    } else {
	croak "'$pos' is not a valid position for the balloon - it must be one of: 'widget', 'mouse'.";
    }

    $w->idletasks;

    # Explanation of following code. [JD]
    # PREMISE: We want to ensure that the balloon is always "on screen".
    # To do this we use calculate the size of the
    # toplevel before it is mapped. Then we adjust it's position with respect to the
    # mouse cursor or widget. Balloons are usually shown below and to the right of the target.
    # From extensive KDE experience using Xinerama, and from using dual monitors on WinXP..
    # the balloon will extend across two monitors in single logical screen mode (SLS).
    # This is an undesirable characteristic indeed. Trying to read a disjointed balloon
    # across monitors is not fun.
    #
    # The intent of the following code is to fix this problem. We do this by avoiding
    # placement of any part of the balloon over,say, the "half screenwidth" mark (for two
    # monitors in SLS mode) or "thirds of screenwidth" mark (for 3 monitors) and so on...
    # i.e. In SLS mode these *WILL BE* separate screens and as such, should be considered hard
    # boundaries to be avoided.
    #
    # The only drawback of this code, is I know of no way to actually determine this on a
    # user by user basis. This means that the developer or administrator will have to know
    # the hardware (monitor) setup for which the application is designed.
    #
    # This code uses Gerhard's GIF images but changes *how* the image gets shown. Instead
    # of creating four separate labels, we configure only ONE label with the proper image.
    # Then using the place geometry manager, this image/label can be "slid" along the
    # appropriate side of the toplevel so that it always points directly at the target widget.
    #
    # Here we go..

    my ($width, $height) = ($w->reqwidth, $w->reqheight);
    my ($sw, $sh) = ($w->screenwidth, $w->screenheight);
    my $numscreen = $w->cget(-numscreens);
    my $deltax = $sw/$numscreen;
    my $leftedge;
    my $rightedge;
    my $count = 0;
    for (my $i=0; $i<$sw; $i+=$deltax){
	$leftedge = $i;
	$rightedge = $i + $deltax;
	if ($x >= $leftedge && $x < $rightedge ){
	    last;
	}
	    $count++;
    }

    # Force another look at balloon location because mouse has switched
    # virtual screens.
    $w->{'location'} = undef unless ( $count == $w->{'current_screen'} );
    $w->{'current_screen'} = $count;

    my $xx=undef;
    my $yy=undef; # to hold final toplevel placement
    my $slideOffsetX = 0;
    my $slideOffsetY = 0;
    my $cornerOffset = 5; #default - keep corner away from pointer
    my $testtop = $y - $height - $cornerOffset;
    my $testbottom = $y + $height + (2*$cornerOffset);
    my $testright = $x + $width + (2*$cornerOffset);
    my $testleft = $x - $width - $cornerOffset;
    my $vert='bottom'; #default
    my $horiz='right'; #default


    if ( defined $w->{'location'} ){
      # Once balloon is activated, **don't** change the location of the balloon.
      # It is annoying to have it jump from one location to another.
        ( $w->{'location'}=~/top/  ) ? ( $vert = 'top'   ) : ( $vert = 'bottom' );
        ( $w->{'location'}=~/left/ ) ? ( $horiz = 'left' ) : ( $horiz = 'right' );

        if ($vert eq 'top' && $testtop < 0) {
            $yy = 0;
            $slideOffsetY = $testtop;
        }
        elsif ($vert eq 'bottom' && $testbottom > $sh) {
            $slideOffsetY = $testbottom - $sh;
        }

        if ($horiz eq 'left' && $testleft < $leftedge) {
            $xx = $leftedge;
        }
        elsif ($horiz eq 'right' && $testright > $rightedge) {
            $slideOffsetX = $testright - $rightedge;
        }
    }
    else {
        #Test balloon positions in the vertical
        if ($testbottom > $sh) {
            #Then offscreen to bottom, check top
            if ($testtop >= 0) {
                $vert = 'top';
            }
            elsif ($y > $sh/2) {
 	    #still offscreen to top but there is more room above then below
                $vert = 'top';
                $yy=0;
                $slideOffsetY = $testtop;
 	        }
	    if ($vert eq 'bottom'){
                #Calculate Yoffset to fit entire balloon onto screen
                $slideOffsetY = $testbottom - $sh;
            }
        }
        #Test balloon positions in the horizontal

        if ($testright > $rightedge) {
            #The offscreen, check left
            if ($testleft >= $leftedge) {
                $horiz = 'left';
            }
            elsif ($x > ($leftedge+$deltax) ) {
                #still offscreen to left but there is more room to left than right
	        $horiz = 'left';
                $xx=0;
                $slideOffsetX = $testleft;
	    }
	    if ($horiz eq 'right'){
                #Calculate Xoffset to fit entire balloon onto screen
                $slideOffsetX = $testright - $rightedge;
            }
        }
    }

    $w->{'location'} = $vert.$horiz unless (defined $w->{'location'});

    if ($w->{'location'} eq 'bottomright') {
        if ( $slideOffsetX or $slideOffsetY ) {
            $w->{'pointer'}->configure(-image => $w->{img_no});
        }
        else {
            $w->{'pointer'}->configure(-image => $w->{img_tl});
        }

        $w->{'pointer'}->place(
            -in=>$w,
#            -relx=>0, -x=>$slideOffsetX + 2,
#            -rely=>0, -y=>$slideOffsetY + 2,
            -relx=>0, -x=>2,
            -rely=>0, -y=>2,
            -bordermode=>'outside',
            -anchor=>'nw');

        $xx=$x-$slideOffsetX+(2*$cornerOffset) unless (defined $xx);
        $yy=$y-$slideOffsetY+(2*$cornerOffset) unless (defined $yy);

    }
    elsif ($w->{'location'} eq 'bottomleft') {
        if ( $slideOffsetX or $slideOffsetY ) {
            $w->{'pointer'}->configure(-image => $w->{img_no});
        }
        else {
            $w->{'pointer'}->configure(-image => $w->{img_tr});
        }

        $w->{'pointer'}->place(-in=>$w,
#            -relx=>1, -x=>$slideOffsetX - 2,
#            -rely=>0, -y=>$slideOffsetY + 2,
            -relx=>1, -x=>-2,
            -rely=>0, -y=>2,
            -bordermode=>'outside',
            -anchor=>'ne');

        $xx=$x-$width-$slideOffsetX-$cornerOffset unless (defined $xx);
        $yy=$y-$slideOffsetY+(2*$cornerOffset) unless (defined $yy);

    }
    elsif ($w->{'location'} eq 'topright') {
        if ( $slideOffsetX or $slideOffsetY ) {
            $w->{'pointer'}->configure(-image => $w->{img_no});
        }
        else {
            $w->{'pointer'}->configure(-image => $w->{img_bl});
        }

        $w->{'pointer'}->place(-in=>$w,
#            -relx=>0, -x=>$slideOffsetX + 2,
#            -rely=>1, -y=>$slideOffsetY - 2,
            -relx=>0, -x=>2,
            -rely=>1, -y=>-2,
            -bordermode=>'outside',
            -anchor=>'sw');

        $xx=$x-$slideOffsetX+$cornerOffset unless (defined $xx);
        $yy=$y-$height-$slideOffsetY-$cornerOffset unless (defined $yy);
    }
    elsif ($w->{'location'} eq 'topleft') {
        if ( $slideOffsetX or $slideOffsetY ) {
            $w->{'pointer'}->configure(-image => $w->{img_no});
        }
        else {
            $w->{'pointer'}->configure(-image => $w->{img_br});
        }

        $w->{'pointer'}->place(-in=>$w,
#            -relx=>1, -x=>$slideOffsetX - 2,
#            -rely=>1, -y=>$slideOffsetY - 2,
            -relx=>1, -x=>-2,
            -rely=>1, -y=>-2,
            -bordermode=>'outside',
            -anchor=>'se');

        $xx=$x-$width-$slideOffsetX-$cornerOffset unless (defined $xx);
        $yy=$y-$height-$slideOffsetY-$cornerOffset unless (defined $yy);
    }

    $w->{'pointer'}->raise;
    $xx = int($xx);
    $yy = int($yy);
    $w->geometry("+$xx+$yy");
    $w->deiconify();
    $w->raise;
}

sub SetStatus {
    my ($w) = @_;
    my $client = $w->{'client'};
    my $s = $w->GetOption(-statusbar => $client);
    if (defined $s and $s->winfo('exists')) {
    	my $vref = $s->cget(-textvariable);
        return if not defined $client or not exists $w->{'clients'}{$client};
        my $msg = $client->BalloonInfo($w, $w->pointerxy,'-statusmsg');
        # Dereference it if it looks like a scalar reference:
        $msg = $$msg if UNIVERSAL::isa($msg, 'SCALAR');
        if (not defined $vref) {
	        eval { $s->configure(-text => $msg); };
        } else {
	        $$vref = $msg;
        }
    }
}

sub ClearStatus {
    my ($w) = @_;
    my $client = $w->{'client'};
    my $s = $w->GetOption(-statusbar => $client);
    if (defined $s and $s->winfo('exists')) {
	my $vref = $s->cget(-textvariable);
	if (defined $vref) {
	    $$vref = '';
	} else {
	    eval { $s->configure(-text => ''); }
	}
    }
}

sub _destroyed {
    my ($w) = @_;
    # This is called when widget is destroyed (no matter how!)
    # via the ->OnDestroy hook set in Populate.
    # remove ourselves from the list of baloons.
    @balloons = grep($w != $_, @balloons);

    # FIXME: If @balloons is now empty perhaps remove the 'all' bindings
    # to reduce overhead until another balloon is created?

    # Delete the images
    for (qw(no tl tr bl br)) {
        my $img = delete $w->{"img_$_"};
	    $img->delete if defined $img;
    }
}

1;


