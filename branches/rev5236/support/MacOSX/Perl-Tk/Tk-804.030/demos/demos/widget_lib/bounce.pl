# bounce.pl

use Ball;
use Tk qw/:eventtypes/;
use subs qw/ClearMsg DoSingleStep NotDone ShowMsg SimStart SimStop mkmb/;
use vars qw/$TOP/;

my(@menu_button_list, $quit_flag, $quit_code,
   $bounce_status, $bounce_speed, $bounce_running, $bounce_counter);

sub bounce {

    # This began as a borrowed idea from Python distribution examples, ended up
    # with a Ball module of its own. It illustrates how one can run something
    # without blocking XEvent processing in a simple-minded sorta way.
    #
    # Handles resizes to the main window
    #
    # Gurusamy Sarathy (gsar@engin.umich.edu)
    # Tidied up by SOL.
    #
    # 97/06/10 This demo is sufficiently bizarre enough that we don't use
    #          WidgetDemo! (-:  Plus, you get to see Exists() in action.

    my($demo) = @_;

    $TOP->destroy if Exists($TOP);
    $TOP = $MW->Toplevel;
    $TOP->title('Bouncing Ball Simulator');
    $TOP->iconname('bounce');

    @menu_button_list = ();
    $quit_flag = 0;
    $quit_code = sub {$quit_flag = 1};
    $TOP->protocol('WM_DELETE_WINDOW' => $quit_code);

    my $menubar = $TOP->Frame(qw/-relief raised -background DarkGreen -bd 2/);
    $menubar->pack(-side => 'top', -fill => 'x');

    mkmb($menubar, 'File', 0, 'File related stuff',
         [
          ['Open',      \&NotDone,     0],
          ['New',       \&NotDone,     0],
          ['Print',     \&NotDone,     0],
          ['Exit',      sub{$TOP->bell},        0],
          ]);

    mkmb($menubar, 'Simulate', 0, 'Simulator control',
	 [
	  ['Start',     \&SimStart,    2],
	  ['Stop',      \&SimStop,     2],
	  ]);

    mkmb($menubar, 'Display', 0, 'Display settings',
	 [
	  ['Redraw',    \&NotDone,     2],
	  ['Clear',     \&NotDone,     2],
	  ]);

    mkmb($menubar, 'Options', 0, 'Various preferences',
	 [
	  ['Steptime',  \&NotDone,     0],
	  ['Colors',    \&NotDone,     0],
	  ['Display',   \&NotDone,     0],
	  ]);

    mkmb($menubar, 'Help', 0, 'There when you need it',
	 [
	  ['About..',   \&NotDone,     0],
	  ['Intro',     \&NotDone,     0],
	  ['Contents',  \&NotDone,     0],
	  ]);
    $menu_button_list[$#menu_button_list]->pack(-side => 'right');

    my $feedback = $TOP->Frame();
    $feedback->pack(-side => 'bottom', -fill => 'x');
    $bounce_status = $feedback->Text(
        -relief      => 'sunken',
	-height      => 1,
	-background  => 'gray',
	-borderwidth => 2,
    );
    $bounce_status->pack(-side => 'left', -fill => 'x', -expand => 1);

    my $drawarea = $TOP->Frame();
    $drawarea->pack(-side => 'top', -fill => 'both', -expand => 1);

    my $canvas = $drawarea->Canvas(
        -relief      => 'ridge',
	-height      => 400,
	-width       => 600,
	-borderwidth => 2,
    );
    $canvas->pack(-side => 'left', -fill => 'both', -expand => 1);

    $bounce_speed = $drawarea->Scale(
        -orient      => 'vert',
	-showvalue   => 0,
	-width       => 10,
	-from        => 100,
	-to          => 0,
        -borderwidth => 1,
    );
    $bounce_speed->pack(-side => 'left', -fill => 'y');
    $bounce_speed->bind('<Enter>' => sub {
	ClearMsg; ShowMsg('Adjust slider for ball speed');
    });
    $bounce_speed->bind('<Leave>' => \&ClearMsg);
    $bounce_speed->set(50);

    my $w_buttons = $TOP->Frame;
    $w_buttons->pack(qw(-side bottom -expand y -fill x -pady 2m));
    my $w_dismiss = $w_buttons->Button(
        -text    => 'Dismiss',
        -command => $quit_code,
    );
    $w_dismiss->pack(qw(-side left -expand 1));
    my $w_see = $w_buttons->Button(
        -text    => 'See Code',
        -command => [\&see_code, $demo],
    );
    $w_see->pack(qw(-side left -expand 1));
    my $w_ball = $w_buttons->Button(
        -text    => 'View Ball Class Module',
        -command => [\&view_widget,
		     Tk->findINC('demos/widget_lib') . '/Ball.pm'],
    );
    $w_ball->pack(qw(-side left -expand 1));

    $bounce_running = 0;
    $menu_button_list[1]->cget(-menu)->entryconfigure(1, -state => 'disabled');

    $canvas->Ball;
    $canvas->Ball(-color => 'red', -size => 30, -position => [200, 75]);
    $canvas->Ball(
        -color    => 'green',
        -size     => 60,
        -position => [490, 275],
        -velocity => [8.0, 12.0],
    );
    $canvas->Ball(
        -color    => 'yellow',
        -size     => 100,
        -position => [360, 60],
        -velocity => [8.0, 12.0],
    );

    $bounce_counter = 0;
    $TOP->repeat(1000 => sub {
	return unless $bounce_running;
	ClearMsg;
	ShowMsg(sprintf("%6d interations/second", $bounce_counter));
	$bounce_counter = 0
    });


    # This runs the Tk mainloop. Note that the simulation itself has a main
    # loop which must be processed. DoSingleStep runs a bit of the simulation
    # during every iteration. Also note  that, with a flag of 0,
    # Tk::DoOneEvent will suspend the  process until an X-event arrives,
    # effectively blocking the  while loop.
    #
    # My original idea was to run the simulation mainloop as an  asynchronous
    # proc handler that runs when Tk is idle, but the necessary Async(3) calls
    # from Tcl haven't made it into nTk yet.

    while (1) {
	if ($quit_flag) {
	    $TOP->destroy;
	    return;
	}
	DoOneEvent($bounce_running ? DONT_WAIT : ALL_EVENTS);
	DoSingleStep($canvas) if $bounce_running;
    }

} # end bounce

sub mkmb {

    # (Ripped from nTk examples)
    # Make a Menubutton widget; note that the menu is automatically created.
    # We maintain a list of the Menubutton references since some callers
    # need to refer to the Menubutton, as well as to suppress stray name
    # warnings with Perl -w.

    my($mb0, $mb_label, $mb_label_underline, $mb_msg, $mb_list_ref) = @_;
    my $mb = $mb0->Menubutton(
        -text       => $mb_label,
	-underline  => $mb_label_underline,
	-background => 'DarkGreen',
        -foreground => 'Yellow',
    );
    my($menu) = $mb->Menu(-tearoff => 0);
    $mb->configure(-menu => $menu);

    my $mb_list;
    foreach $mb_list (@{$mb_list_ref}) {
        $mb->command(
            -label      => $mb_list->[0],
            -command    => $mb_list->[1] ,
            -underline  => $mb_list->[2],
            -background => 'DarkGreen',
            -foreground => 'White',
        );
    }
    $mb->pack(-side => 'left');
    $TOP->bind($mb, '<Enter>' => sub {ClearMsg; ShowMsg($mb_msg)});
    $TOP->bind($mb, '<Leave>' => \&ClearMsg);

    push @menu_button_list, $mb;
    return $mb;

} # end mkmb

sub SimStart {

    if (not $bounce_running) {
        $bounce_running = 1;
        $menu_button_list[1]->cget(-menu)->entryconfigure(0,
            -state => 'disabled',
        );
        $menu_button_list[1]->cget(-menu)->entryconfigure(1,
            -state => 'normal',
        );
    }

} # end SimStart

sub SimStop {

    if ($bounce_running) {
        $bounce_running = 0;
        $menu_button_list[1]->cget(-menu)->entryconfigure(0,
            -state => 'normal',
        );
        $menu_button_list[1]->cget(-menu)->entryconfigure(1,
            -state => 'disabled',
        );
    }

} # end SimStop

sub NotDone {

    print "Not yet implemented.\n";

} # end NotDone

sub ShowMsg {

    my($msg) = shift;
    $bounce_status->insert('1.0', $msg);

} # end ShowMsg

sub ClearMsg {

    $bounce_status->delete('1.0', 'end');

} # end ClearMsg

sub DoSingleStep {

    # The simulation handler.
    #
    # Note that this handler must be cooperative and return after a short
    # period, so that other X events may be  processed by the mainloop below.

    my($canvas) = @_;

    $bounce_counter++;
    Ball->move_all_balls($canvas, $bounce_speed->get() / 100.0);

} # end DoSingle Step
