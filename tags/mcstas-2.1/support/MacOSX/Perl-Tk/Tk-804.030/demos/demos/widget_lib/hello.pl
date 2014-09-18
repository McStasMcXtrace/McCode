# hello.pl

use Config;
use Tk::widgets qw/ ROText /;
use vars qw/ $TOP /;
use strict;

sub hello {

    my( $demo ) = @_;

    $TOP = $MW->WidgetDemo(
        -name             => $demo,
        -text             => [ "This demonstration describes the basics of Perl/Tk programming. Besides this small user guide, there are various FAQs and other resources and tutorials available on the web, such as:

http://phaseit.net/claird/comp.lang.perl.tk/ptkFAQ.html
http://www.perltk.org
http://user.cs.tu-berlin.de/~eserte
http://www.lehigh.edu/sol0/ptk
", -wraplength => '7i' ],
        -title            => 'Perl/Tk User Guide',
        -iconname         => 'hello',
    );

    # Pipe perldoc help output via fileevent() into a Scrolled ROText widget.

    my $t = $TOP->Scrolled(
        qw/ ROText -width 80 -height 25 -wrap none -scrollbars osoe/,
    );
    $t->focus;
    my $cmd = $Config{installbin} . '/perldoc -t Tk::UserGuide';
    $t->pack( qw/ -expand 1 -fill both / );

    open( H, "$cmd|" ) or die "Cannot get pTk user guide: $!";
    $TOP->fileevent( \*H, 'readable' => [ \&hello_fill, $t ] );

} # end hello

sub hello_fill {

    my( $t ) = @_;

    my $stat = sysread H, my $data, 4096;
    die "sysread error:  $!" unless defined $stat;
    if( $stat == 0 ) {		# EOF
	$TOP->fileevent( \*H, 'readable' => '' );
	return;
    }
    $t->insert( 'end', $data );

} # end hello_fill
