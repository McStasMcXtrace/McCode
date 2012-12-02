# icon.pl

use vars qw/$TOP/;

sub icon {

    # Create a top-level window that displays a bunch of iconic buttons.

    my($demo) = @_;
    $TOP = $MW->WidgetDemo(
        -name     => $demo,
        -text     => ['This window shows three ways of using bitmaps or images in radiobuttons and checkbuttons.  On the left are two radiobuttons, each of which displays a bitmap and an indicator.  In the middle is a checkbutton that displays a different image depending on whether it is selected or not.  On the right is a checkbutton that displays a single bitmap but changes its background color to indicate whether or not it is selected.', qw/-wraplength 5i/],
        -title    => 'Iconic Button Demonstration',
        -iconname => 'icon',
    );

    $TOP->Bitmap('flagup',
        -file     => Tk->findINC('demos/images/flagup'),
	-maskfile => Tk->findINC('demos/images/flagup'),
    );
    $TOP->Bitmap('flagdown',
        -file     => Tk->findINC('demos/images/flagdown'),
	-maskfile => Tk->findINC('demos/images/flagdown'),
    );

    my $frame = $TOP->Frame(qw/-borderwidth 10/);
    $frame->pack(qw/-side top/);

    my(@pl) = qw/-side left -expand yes -padx 5m/;
    my $frame_left = $frame->Frame;
    $frame_left->pack(@pl);

    my $frame_b1 = $frame->Checkbutton(
        -image            => 'flagdown',
        -selectimage      => 'flagup',
        -indicatoron      => 0,
    );
    $frame_b1->pack(@pl);
    $frame_b1->configure(-selectcolor => $frame_b1->cget(-background));
    my $frame_b2 = $frame->Checkbutton(
        -bitmap      => '@' . Tk->findINC('demos/images/letters'),
        -indicatoron => 0,
	-selectcolor => 'SeaGreen1',
    );
    $frame_b2->pack(@pl);

    my $letters = '';
    @pl = qw/-side top -expand yes/;
    my $frame_left_b3 = $frame_left->Radiobutton(
        -bitmap   => '@' . Tk->findINC('demos/images/letters'),
        -variable => \$letters,
        -value    => 'full',
    );
    $frame_left_b3->pack(@pl);
    my $frame_left_b4 = $frame_left->Radiobutton(
        -bitmap   => '@' . Tk->findINC('demos/images/noletters'),
        -variable => \$letters,
        -value    => 'empty',
    );
    $frame_left_b4->pack(@pl);

} # end icon

1;
