#!/usr/bin/perl -w

# This is an (automated) variation of https://bugzilla.redhat.com/show_bug.cgi?id=235666
# It segfaults on FreeBSD 7 with 5.10.0, but not with 5.8.8.
#
# The reason is an already known problem: destroying a MainWindow
# removes some parts of the interpreter which is strictly needed by
# subsequent MainWindows.

use strict;

use Test::More;
plan tests => 1;

use Tk;

sub yes_no{
  my $OKMOD;
  my $Alerte = MainWindow->new(
  		-title      =>	"Yes or No",
  		);
  
  my $BMOD = $Alerte->Button(
  		-text        =>	"Yes",
		-background  => "green",
  		-command     => sub{$OKMOD = 1; $Alerte->destroy() }
  		)->pack(-side=>	'left',	-expand=>1);
 
  my $BFIN = $Alerte->Button(
  		-text        =>	"No",
  		-background  => "red",
  		-command     => sub{$OKMOD = 0; $Alerte->destroy() }
  		)->pack(-side=>'left',	-expand=>1);
  $BMOD->afterIdle(sub { $BMOD->invoke });
  MainLoop;
  return $OKMOD;
}

my $n = 1000;
diag "Creating and destroying $n MainWindows. This may take some time...";
for(1..$n) {
    yes_no('test'); 
}
pass 'No segfault';

__END__
