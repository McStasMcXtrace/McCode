#!/usr/bin/perl -w

use strict;
use Test::More;
use Tk;

if ($^O eq 'MSWin32' || $^O eq 'cygwin') {
    print "1..0 # skip No real fork on Windows-like systems\n";
    exit;
}

plan tests => 3;

my $mw = tkinit;
$mw->geometry("+10+10");
ok Tk::IsParentProcess(), 'This is the parent';
pipe(my($rdr,$wtr));
if (fork == 0) {
    close $rdr;
    print $wtr (Tk::IsParentProcess() ? 'parent' : 'child'), "\n";
    print "# Child $$\n";
    CORE::exit();
}
else {
    close $wtr;
    print "# Parent $$\n";
}
my $child_result = <$rdr>;
like $child_result, qr{^child}, 'Child is not the parent process';
# Pause to allow child to exit, and to collect the 
select undef, undef, undef, 0.5;
$mw->update;
pass 'No segfaults';

__END__
