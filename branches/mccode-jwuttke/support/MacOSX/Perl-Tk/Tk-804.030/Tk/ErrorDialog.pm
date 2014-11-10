package Tk::ErrorDialog;

use vars qw($VERSION);
#$VERSION = sprintf '4.%03d', q$Revision: #7 $ =~ /\D(\d+)\s*$/;
$VERSION = '4.008';

use Tk ();
require Tk::Dialog;
use base qw(Tk::Toplevel);


# ErrorDialog - a translation of bgerror() from Tcl/Tk to Perl/Tk.
#
# Currently TkPerl background errors are sent to stdout/stderr; use this
# module if you want them in a window.  You can also "roll your own" by
# supplying the routine Tk::Error.

use strict;

Construct Tk::Widget 'ErrorDialog';

my %options = ( -buttons => ['OK', 'Skip Messages', 'Stack trace'],
                -bitmap  => 'error'
              );
my %ED_OBJECT; # ErrorDialog per MainWindow (singleton)

sub import
{
 my $class = shift;
 while (@_)
  {
   my $key = shift;
   my $val = shift;
   $options{$key} = $val;
  }
}

sub Populate {

    # ErrorDialog constructor.  Uses `new' method from base class
    # to create object container then creates the dialog toplevel and the
    # traceback toplevel.

    my($cw, $args) = @_;

    my $mw = $cw->MainWindow;
    my $dr = $mw->Dialog(
        -title          => 'Error in '.$mw->name,
        -text           => 'on-the-fly-text',
        -bitmap         => $options{'-bitmap'},
	-buttons        => $options{'-buttons'},
    );
    $cw->minsize(1, 1);
    $cw->title('Stack Trace for Error');
    $cw->iconname('Stack Trace');
    my $t_ok = $cw->Button(
        -text    => 'OK',
        -command => [
            sub {
		shift->withdraw;
	    }, $cw,
        ]
    );
    my $t_text = $cw->Text(
        -relief  => 'sunken',
        -bd      => 2,
        -setgrid => 'true',
        -width   => 60,
        -height  => 20,
    );
    my $t_scroll = $cw->Scrollbar(
        -relief => 'sunken',
        -command => ['yview', $t_text],
    );
    $t_text->configure(-yscrollcommand => ['set', $t_scroll]);
    $t_ok->pack(-side => 'bottom', -padx => '3m', -pady => '2m');
    $t_scroll->pack(-side => 'right', -fill => 'y');
    $t_text->pack(-side => 'left', -expand => 'yes', -fill => 'both');
    $cw->withdraw;

    $cw->Advertise(error_dialog => $dr); # advertise dialog widget
    $cw->Advertise(text => $t_text);     # advertise text widget
    $cw->ConfigSpecs(-cleanupcode => [PASSIVE => undef, undef, undef],
                     -appendtraceback => [ PASSIVE => undef, undef, 1 ]);
    $ED_OBJECT{$mw} = $cw;
    $cw->protocol('WM_DELETE_WINDOW' => sub {$cw->withdraw});
    return $cw;

} # end Populate

sub Tk::Error {

    # Post a dialog box with the error message and give the user a chance
    # to see a more detailed stack trace.

    my($w, $error, @msgs) = @_;

    my $grab = $w->grab('current');
    $grab->Unbusy if (defined $grab);

    my $mw = $w->MainWindow;
    my $ed = $ED_OBJECT{$mw} || $w->ErrorDialog;

    my($d, $t) = ($ed->Subwidget('error_dialog'), $ed->Subwidget('text'));

    $d->configure(-text => "Error:  $error");
    $d->bell;
    $mw->deiconify if $mw->state ne 'normal';
    my $ans = $d->Show;

    $t->delete('0.0', 'end') if not $ed->{'-appendtraceback'};
    $t->insert('end', "\n");
    $t->mark('set', 'ltb', 'end');
    $t->insert('end', "--- Begin Traceback ---\n$error\n");
    my $msg;
    for $msg (@msgs) {
	$t->insert('end', "$msg\n");
    }
    $t->yview('ltb');

    $ed->deiconify if ($ans =~ /trace/i);

    my $c = $ed->{Configure}{'-cleanupcode'};
    &$c if defined $c;		# execute any cleanup code if it was defined
    $w->break if ($ans =~ /skip/i);

} # end Tk::Error

1;
