# BrowseEntry, another example.
#
# Chris Dean <ctdean@cogit.com>

use strict;
use Tk;
use Tk::BrowseEntry;

my $top = new MainWindow( -title => "BrowseEntry 2" );
main( $top );
MainLoop();

sub main {
    my( $top ) = @_;

    my @countries = qw( America Belize Canada Denmark Egypt Fruitopia );
    my @states = qw( normal readonly disabled );
    foreach my $i (0..$#states) {
        my $state = $states[$i];
        my $var = $countries[$i];
        my $f = $top->Frame->pack( qw/-side left/ );
        my $be = $f->BrowseEntry( -variable => \$var,
                                  -choices => \@countries,
                                  -state => $state )->pack;
        if( $state eq "disabled" ) {
            $be->configure( -arrowimage => $f->Getimage( "balArrow" ) )
        }
        foreach my $s (@states) {
            $f->Radiobutton( -text => $s,
                             -value => $s,
                             -variable => \$state,
                             -command => sub {
                                 $be->configure( -state => $state ); }
                           )->pack( qw/-anchor w/ );
        }
        $f->Button( -text => "Print value", -command => sub {
                        print "$var\n" } )->pack;
    }
}
