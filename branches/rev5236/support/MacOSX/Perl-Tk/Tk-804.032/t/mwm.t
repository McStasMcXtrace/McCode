BEGIN
{
 $^W = 1; $| = 1;
 require Tk if ($^O eq 'cygwin');
 if ($^O eq 'MSWin32' or ($^O eq 'cygwin' and $Tk::platform eq 'MSWin32'))
  {
   print "1..0 # skip Tk::Mwm not built on MSWin32\n";
   exit;
  }
}

use strict;
use Test;
use Tk;

BEGIN { plan tests => 2 };

my $mw = Tk::MainWindow->new;

##
## if there's only one (fixed) or no font family
## then something is wrong. Propably the envirionment
## and not perl/Tks fault.
##
{
    eval { require Tk::Mwm; };
    ok($@, "", "Can't load Tk::Mwm module");

    my $isrunning;
    eval { $isrunning = $mw->mwm('ismwmrunning'); };
    ok($@, "", "Can't determine if mwm is running")
}

1;
__END__
BEGIN { $^W = 1; $| = 1; }

use strict;
use Test;
use Tk;

BEGIN { plan tests => 2 };

my $mw = Tk::MainWindow->new;

##
## if there's only one (fixed) or no font family
## then something is wrong. Propably the envirionment
## and not perl/Tks fault.
##
{
    eval { require Tk::Mwm; };
    ok($@, "", "Can't load Tk::Mwm module");

    my $isrunning;
    eval { $isrunning = $mw->mwm('ismwmrunning'); };
    ok($@, "", "Can't determine if mwm is running")
}

1;
__END__
