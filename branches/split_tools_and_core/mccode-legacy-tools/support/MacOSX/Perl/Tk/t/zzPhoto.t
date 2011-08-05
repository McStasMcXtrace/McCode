BEGIN { $|=1; $^W=1; }
use strict;
use Test;
use Tk;

BEGIN { plan tests => 27 };

my $mw = Tk::MainWindow->new;
my $xpm;
my $photo;

{
   eval { require Tk::Photo; };
   ok($@, '', 'Problem loading Tk::Photo');
   eval { $xpm = Tk::findINC('Tk::folder.xpm'); };
   ok (defined($xpm), 1, "Can't locate Tk::folder.xpm");
   eval { $photo = $mw->Photo(-file=>$xpm); };
   ok($@, '', 'Problem creating Photo widget');
}
##
## configure('-data') returned '-data {} {} {} {}' up and incl. Tk800.003
##
{
   my @opts;
   my $opts;
   foreach my $opt ( qw/-data -format -file -gamma -height -width/ )
     {
       eval { @opts = $photo->configure($opt); };
       ok($@, '', "can't do configure $opt");
       ok(scalar(@opts), 5, "configure $opt returned not 5 elements");
       eval { $opts = $photo->configure($opt); };
       ok($@, '', "can't do configure $opt");
       ok(scalar(@$opts), 5, "configure $opt returned not 5 elements");
     }
}

1;
__END__
