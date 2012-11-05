# Learn how to write your own widget demonstration.

use vars qw/$TOP/;

sub TEMPLATE {
    my($demo) = @_;
    $TOP = $MW->WidgetDemo(
        -name             => $demo,
        -text             => 'Learn how to write a widget demonstration!',
	-geometry_manager => 'grid',
        -title            => 'WidgetDemo Example',
        -iconname         => 'WidgetDemo',
    );
    $TOP->Label(-text => 'Click "See Code".')->grid;
}
__END__

The template code above specifies how user contributed widget demonstrations
can be written.

widget looks in the directory specified on the command line to load user
contributed demonstrations.  If no directory name is specified when widget is
invoked and the environment variable WIDTRIB is defined then demonstrations
are loaded from the WIDTRIB directory. If WIDTRIB is undefined then widget
defaults to the released user contributed directory.

The first line of the file is the DDD (Demonstration Description Data), which
briefly describes the purpose of the demonstration.  The widget program reads
this line and uses it when building its interface.

Demonstrations must have a unique subroutine which is the same as the filename
with .pl stripped off.  When widget calls your subroutine it's passed one
argument, the demonstration name. So file TEMPLATE.pl contains subroutine
TEMPLATE().  But a demo can actually be an entire program - read on!

For consistency your demonstration should use the WidgetDemo widget.  This is
a toplevel widget with three frames. The top frame contains descriptive
demonstration text.  The bottom frame contains the "Dismiss" and "See Code"
buttons.  The middle frame is the demonstration container, which can be
managed by either the pack or grid geometry manager.

Since your subroutine can "see" all of widget's global variables, you
use $MW (the main window reference) to create the WidgetDemo toplevel; be sure
to pass at least the -name and -text parameters.  -geometry_manager defaults
to "pack".  The call to WidgetDemo() returns a reference to the containing
frame for your demonstration, so treat it as if it were the MainWindow, the
top-most window of your widget hierarchy.

Alternatively the .pl file may contain typical Perl/Tk code of the form:

    # Demonstration Description Data

    use Tk;
    my $top = MainWindow->new;
    $top->Label(-text => 'Whatever');
    MainLoop;
    __END__

widget has re-defined normal MainWindow to actually create a WidgetDemo
on your code's behalf. MainLoop is optional in a demo (it will immediately
return as MainLoop is already active).

Other consideration:

    . widget global variables are all uppercase, like $MW - be careful not
      to stomp on them!

    . Demo files should really be run in private packages to avoid those
      problems.

    . Don't subvert the inheritance mechanism by calling Tk::MainWindow
      in your demo code.

    . The description should really be extracted from POD documentation
      in the .pl file rather than a magic comment.

    . If your demonstration has a Quit button change it to ring the bell
      and use the builtin Dismiss instead. In particular destroying a
      MainWindow is acceptable, but exit will shut down widget itself!

    . Be sure $TOP is declared in a "use vars" statement and not as a
      lexical my() in the subroutine (see below).

    . If you're wrapping an existing main program in a subroutine be very
      alert for closure bugs.  Lexicals inside a subroutine become closed
      so you may run into initialization problems on the second and
      subsequent invokations of the demonstration.  The npuz and plop
      demonstrations show how to work around this.  Essentially, remove
      all "global" my() variables and place them within a "use vars".
      This practice is prone to subtle bugs and is not recommended!


