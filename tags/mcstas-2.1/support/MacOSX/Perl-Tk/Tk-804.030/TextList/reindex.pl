#!/bin/perl

use lib qw(/home1/gbartels/textlist);
use Tk;

use Tk::TextReindex qw(Tk::ROText ROTextReindex);

$mw=new MainWindow;

my $idx;

$w=$mw->ROTextReindex()->pack(-side => "top");
$t=$mw->Label(-textvariable => \$idx)->pack(-side => "bottom");

$w->bind('<Key>',sub{$idx=$w->index("insert")});

$w->insert('end',"abcd\n");
$w->insert('end',"efgh\n");
$w->insert('end',"mnop\n");
$w->insert('end',"qrst\n");
$w->insert('end',"uvwx\n");

$w->insert('2.0',"ijkl\n");

my $string = $w->get('4.0');


my $result = "reading index 4.0 : expect string to equal >q<, actual value is $string \n";
$w->insert('end',$result);

print $result;

MainLoop;
