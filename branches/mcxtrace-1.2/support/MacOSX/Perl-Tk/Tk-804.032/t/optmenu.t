# -*- perl -*-
BEGIN { $|=1; $^W=1; }
use strict;
use Test;

BEGIN
  {
   plan test => 25;
  };

eval { require Tk };
ok($@, "", "loading Tk module");

eval { require Tk::Optionmenu };
ok($@, "", "loading Tk::Optionmenu module");

my $mw;
eval {$mw = Tk::MainWindow->new();};
ok($@, "", "can't create MainWindow");
ok(Tk::Exists($mw), 1, "MainWindow creation failed");

my $foo = 12;
my @opt = (0..20);

# Granfather documented use of just -variable
my $opt = $mw->Optionmenu(-variable => \$foo,
	                  -options => \@opt)->pack;
ok($@, "", "can't create Optionmenu");
ok(Tk::Exists($opt), 1, "Optionmenu creation failed");

ok($ {$opt->cget(-variable)}, $foo, "setting of -variable failed");
ok($opt->cget(-variable),\$foo, "Wrong variable");

my $optmenu = $opt->cget(-menu);
ok($optmenu ne "", 1, "can't get menu from Optionmenu");
ok(ref $optmenu, 'Tk::Menu', "reference returned is not a Tk::Menu");
ok($optmenu->index("last"), 20, "wrong number of elements in menu");
ok($optmenu->entrycget("last", -label), "20", "wrong label");

# Test use of both variables on the list of lists case
my $foo3 = 5;
my $bar3 = "";
my $opt3 = $mw->Optionmenu(-variable => \$foo3,
                           -textvariable => \$bar3,
			   -options => [map { [ "Label $_"  => $_ ] } @opt],
			  )->pack;
ok($@, "", "can't create Optionmenu");
ok(Tk::Exists($opt3), 1, "Optionmenu creation failed");

ok($ {$opt3->cget(-variable)}, $foo3, "setting of -variable failed");
ok($bar3, "Label $foo3", "textvariable set to wrong value");
my $opt3menu = $opt3->cget(-menu);
ok($opt3menu ne "", 1, "can't get menu from Optionmenu");
ok($opt3menu->entrycget("last", -label), "Label 20", "wrong label");

# See if we have fixed use of just -variable in the list of lists case
my $foo2 = 5;
my $opt2 = $mw->Optionmenu(-variable => \$foo2,
			   -options => [map { [ "Label $_"  => $_ ] } @opt],
			  )->pack;
ok($@, "", "can't create Optionmenu");
ok(Tk::Exists($opt2), 1, "Optionmenu creation failed");

ok($ {$opt2->cget(-variable)}, $foo2, "setting of -variable failed");
my $opt2menu = $opt2->cget(-menu);
ok($opt2menu ne "", 1, "can't get menu from Optionmenu");
ok($opt2menu->entrycget("last", -label), "Label 20", "wrong label");

ok($ {$opt2->cget(-textvariable)}, "Label $foo2", "wrong label");

eval { $opt->eventGenerate('<<MenuSelect>>') };
ok($@, "", "problem sending a MenuSelect event");

#Tk::MainLoop();

1;
__END__
