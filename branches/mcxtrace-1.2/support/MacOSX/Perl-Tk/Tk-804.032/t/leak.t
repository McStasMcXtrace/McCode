#!/usr/bin/perl -w
# -*- perl -*-

#
# $Id: leak.t,v 1.3 2002/03/07 23:04:54 eserte Exp $
# Author: Slaven Rezic
#

# Some leak tests. You need Devel::Leak installed.

use strict;
use Config;
use Devel::Peek;

BEGIN {
    if (!eval q{
	use Test::More;
	use Devel::Leak;
	1;
    }) {
	print "# tests only work with installed Test and Devel::Leak modules\n";
	print "1..0 # skip need Devel::Leak\n";
	CORE::exit;
    }
}

use Tk;
use Tk::Button;
use Tk::Canvas;


plan tests => 15;

my $mw = new MainWindow;
$mw->optionAdd("*Button.Background",'#dcdcdc');
my $handle;
my($c1,$c2);

# Tests for leaking subroutine set

# first binding always creates some SVs
my $N = 100;

$mw->bind("<Motion>" => [sub { warn }]);

$c1 = Devel::Leak::NoteSV($handle);
for(1..100) {
    $mw->bind("<Motion>" => [sub { warn }]);
}
$c2 = Devel::Leak::NoteSV($handle);
cmp_ok($c2-$c1, "<=", 1, "Array reference with subroutine to bind");


$c1 = Devel::Leak::NoteSV($handle);
for(1..100) {
    $mw->bind("<Motion>" => sub { warn });
}
$c2 = Devel::Leak::NoteSV($handle);
is($c2, $c1, "Anonymous subroutine to bind");


$c1 = Devel::Leak::NoteSV($handle);
for(1..100) {
    $mw->bind("<Motion>" => \&test);
}
$c2 = Devel::Leak::NoteSV($handle);
is($c2, $c1, "Subroutine reference to bind");

my $btn = $mw->Button(-command => sub { warn });
$c1 = Devel::Leak::NoteSV($handle);
for(1..100) {
    $btn->configure(-command => sub { warn });
}
$c2 = Devel::Leak::NoteSV($handle);
cmp_ok($c2-$c1, "<", 10, "configure -command multiple times (got " . ($c2-$c1) . " leaked scalars)");

# Tests for leaking Tk_GetUid (e.g. canvas items)

my $c = $mw->Canvas->pack;
$c->delete($c->createLine(10,10,100,100, -tags => "a"));

$c1 = Devel::Leak::NoteSV($handle);
for(1..1000) {
    $c->createLine(10,10,100,100,-tags => "a");
    $c->delete("a");
}
$c2 = Devel::Leak::NoteSV($handle);
cmp_ok(($c2-$c1), "<", 10, "Leaking Tk_GetUid, e.g. canvas items");

$c1 = Devel::Leak::NoteSV($handle);
for(1..100) {
    my $id = $c->createLine(10,10,100,100);
    $c->delete($id);
}
$c2 = Devel::Leak::NoteSV($handle);
cmp_ok(($c2-$c1), "<", 10, "Canvas createLine");

# Tests for leaking widget destroys
my $btn2 = $mw->Button;
$btn2->destroy;
undef $btn2;

$c1 = Devel::Leak::NoteSV($handle);
for(1..100) {
    $btn2 = $mw->Button;
    $btn2->destroy;
}
$c2 = Devel::Leak::CheckSV($handle);
print "# was $c1 now $c2 ",($c2-$c1)/100," per iter\n";
cmp_ok(($c2-$c1), "<", 10, "Creating and destroying buttons");

# Tests for leaking fileevent callbacks
$mw->fileevent(\*STDIN, 'readable', sub { });
$mw->fileevent(\*STDIN, 'readable','');

$c1 = Devel::Leak::NoteSV($handle);
for (1..100)
 {
  $mw->fileevent(\*STDIN, 'readable', sub { });
  $mw->fileevent(\*STDIN, 'readable','');
 }
$c2 = Devel::Leak::CheckSV($handle);
cmp_ok(($c2-$c1), "<", 10, "Fileevent callbacks");

require Tk::After;
$mw->withdraw;
$mw->update;
my $count = 0;
Tk->after(cancel => Tk->after(10,sub { $count-- }));
$c1 = Devel::Leak::NoteSV($handle);
for (1..$N)
{
 Tk->after(cancel => Tk->after($_*10,sub { $count-- }));
}
$c2 = Devel::Leak::CheckSV($handle);
diag "was $c1 now $c2 ",($c2-$c1)/$N," per iter\n";
cmp_ok(($c2-$c1), "<", $N, "after");

$mw->repeat(30,sub {})->cancel;
my $id;
$c1 = Devel::Leak::NoteSV($handle);
$id = $mw->repeat(2, sub { $id->cancel if ($count++ == $N)});
Tk::DoOneEvent(0) while $id->[1];
undef $id;
$c2 = Devel::Leak::CheckSV($handle);
diag "was $c1 now $c2 ",($c2-$c1)/$N," per iter\n";
cmp_ok(($c2-$c1), "<", $N, "repeat");

sub test { warn }

{
    # Tk::Listbox::insert leak
    my $lb = $mw->Listbox;
    $lb->insert("end", 1);
    $c1 = Devel::Leak::NoteSV($handle);
    $lb->insert("end", 2);
    $c2 = Devel::Leak::CheckSV($handle);
    cmp_ok($c2-$c1, "<=", 1, "Insert one listbox element");

    for (1..100) { $lb->insert("end", $_+2) }
    $c1 = Devel::Leak::NoteSV($handle);
    $lb->insert("end", 9999);
    $c2 = Devel::Leak::CheckSV($handle);
    cmp_ok($c2-$c1, "<=", 1, "Insert one listbox element at end");

    $lb->delete(0, "end");
    $c1 = Devel::Leak::NoteSV($handle);
    $lb->insert("end", 1..10);
    $lb->delete(0, "end");
    $c2 = Devel::Leak::CheckSV($handle);
    is($c2-$c1, 0, "Inserting and deleting listbox elements");

    $lb->delete(0, 'end');
    $c1 = Devel::Leak::NoteSV($handle);
    for(1..10) {
	$lb->insert('end', $_);
    }
    $lb->delete(0, 'end');
    $c2 = Devel::Leak::CheckSV($handle);
    is($c2-$c1, 0, "Inserting and deleting individual listbox elements");
}

{
    $mw->afterIdle(sub {});
    $mw->idletasks;
    $c1 = Devel::Leak::NoteSV($handle);
    $mw->afterIdle(sub {});
    $mw->idletasks;
    $c2 = Devel::Leak::CheckSV($handle);

    is($c2-$c1, 0, "afterIdle callback");
}

__END__
