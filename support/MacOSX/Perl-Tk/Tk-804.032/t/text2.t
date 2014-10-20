#!/usr/bin/perl -w
# -*- perl -*-

#
# Author: Slaven Rezic
#

# Here goes tests for non-core Tk methods

use strict;
use Tk;

BEGIN {
    if (!eval q{
	use Test::More;
	1;
    }) {
	print "1..0 # skip no Test::More module\n";
	exit;
    }
}

plan tests => 11;

my $mw = MainWindow->new;
$mw->geometry("+10+10");

{
    my $t = $mw->Text(qw(-width 20 -height 10))->pack;
    $t->insert("end", "hello\nworld\nfoo\nbar\nworld\n");

    ok(!$t->FindNext('-f', '-e', '-c', 'doesnotexist'), 'Pattern does not exist');

    ok($t->FindNext('-f', '-e', '-c', 'world'), 'First search');
    my @first_index = split /\./, $t->index('insert');

    ok($t->FindNext('-forwards', '-e', '-c', 'world'), 'Second search');
    my @second_index = split /\./, $t->index('insert');
    cmp_ok($second_index[0], ">", $first_index[0], 'Really a forwards search');

    ok($t->FindNext('-b', '-e', '-c', 'world'), 'Backwards search');
    my @third_index = split /\./, $t->index('insert');
    cmp_ok($third_index[0], "<", $second_index[0], 'Really a backwards search');

    $t->destroy;
}

{
    my $t = $mw->Text(qw(-width 20 -height 10))->pack;
    tie *FH, ref $t, $t
	or die $!;

    print FH "Hello Text World!\n";
    printf FH "formatted: %s\n", "string";
    syswrite FH, "toto\n", 3, 2;

    is($t->Contents, "Hello Text World!\nformatted: string\nto\n", "tied handle and Contents()");
    # XXX untie attempted while 3 inner references still exist
    untie *FH;
    $t->destroy;
}

{
    my $t = $mw->Scrolled(qw(Text -width 20 -height 10))->pack;
    tie *FH, 'Tk::Text', $t
	or die $!;
    print FH "Scrolled\n";
    is($t->Contents, "Scrolled\n", "tied handle on scrolled Text widget");
    # XXX untie attempted while 3 inner references still exist
    untie *FH;
    $t->destroy;
}

{
    my $t = $mw->Scrolled(qw(Text -width 20 -height 10))->pack;
    is $t->Contents, '', 'fresh Tk::Text is empty';
    $t->Contents('newline-less');
    is $t->Contents, 'newline-less', 'content without newline';
    $t->Contents('');
    is $t->Contents, '', 'after emptying Tk::Text';
    $t->destroy;
}

__END__
