# -*- coding:utf-8; -*-
use Test::More tests => 22;
use Tk;
use Tk::widgets qw(Text);
use utf8;

my $mw = MainWindow->new;
my $tw = $mw->Text;
my $all = <<'TEXT';

This is the text we are matching.
     $42
     £42

TEXT

$tw->insert(end => $all);
is($tw->get('1.0','end -1 char'),$all,"Contents as expected");

my $eposn;
my $ecoun;

$eposn = $tw->search(-count => \$ecount, -exact => 'matching','1.0');
is($eposn,'2.24',"Correct -exact postion");
is($ecount,8,"Correct -exact length");

my $rposn;
my $rcount;

$rposn = $tw->search(-count => \$rcount, -regexp => 'matching','1.0');
is($rposn,'2.24',"Correct -regexp postion");
is($rcount,8,"Correct -regexp length");

$rposn = $tw->search(-count => \$rcount, -regexp => 'tHiS','1.0');
is($rposn,undef,"Correct non-match");

$rposn = $tw->search(-count => \$rcount, -nocase => -regexp => 'tHiS','1.0');
is($rposn,'2.0',"Correct -regexp -nocase");

$eposn = $tw->search(-count => \$rcount, -nocase => -exact => 'tHiS','1.0');
is($eposn,'2.0',"Correct -exact -nocase");

$eposn = $tw->search(-count => \$ecount, -nocase => -exact => '£42','1.0');
is($eposn,'4.5',"Correct -exact high-bit posn");
is($ecount,3,"Correct -exact high-bit len");

TODO: {
  local $TODO = "perl regexp bug pre perl5.8.1" if $] < 5.008001;
$rcount = 0;
$rposn = $tw->search(-count => \$rcount, -nocase => -regexp => '£42','1.0');
is($rposn,'4.5',"Correct -regexp high-bit posn");
is($rcount,3,"Correct -regexp high-bit len");
};

my $qposn;
my $qcount;

$qposn = $tw->search(-count => \$qcount, -regexp => qr/matching/,'1.0');
is($qposn,'2.24',"Correct -regexp qr// postion");
is($qcount,8,"Correct -regexp qr// length");

SKIP:  {
  skip "Perl too old", 8 unless $] >= 5.008001;
my $tword = qr/\bt\w+|\D\d+/i;
my $start = '1.0';
my $i = 0;
my @word = $all =~ /$tword/g;
while ($qposn = $tw->search(-count => \$qcount, -regexp => $tword,$start,'end'))
 {
  $start = $tw->index("$qposn +$qcount chars");
  my $s = $tw->get($qposn,$start);
  print "# '$s'\n";
  is($s,$word[$i++],"Right word");
 }


$rposn = $tw->search(-count => \$rcount, -regexp => '£\d+','1.0');
$i     = $tw->get("$rposn+1 chars","$rposn + $rcount chars");
is($rposn,'4.5',"Correct -regexp postion");
is($rcount,3,"Correct -regexp length");
is($i,'42',"UTF-8 Skip correct");

}

