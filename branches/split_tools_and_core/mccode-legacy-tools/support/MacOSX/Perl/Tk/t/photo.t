BEGIN { $^W = 1; $| = 1;}
use strict;
use Test;
use Tk;
use Tk::Photo;

my $numFormats = 5;
eval { require Tk::JPEG };
$numFormats++ unless $@;

eval { require Tk::PNG  };
$numFormats++ unless $@;

my $mw  = MainWindow->new();
$mw->geometry('+100+100');

plan tests => (2*(7 * $numFormats) + 2 + 2);

my @files = ();

my $row = 0;
foreach my $leaf('Tk.xbm','Xcamel.gif')
 {
  my $file = Tk->findINC($leaf);
  my $src = $mw->Photo(-file => $file);
  ok(defined($src),1," Cannot load $file");
  my $kind = 'Initial';
  my $col = 0;
  $mw->Label(-text  => 'Initial')->grid(-row => $row, -column => $col);
  $mw->Label(-background => 'white',-image => $src)->grid(-row => $row+1, -column => $col++);
  $mw->update;

  foreach $kind ($src->formats)
   {
    print "# Testing $kind\n";
    my $f = lc("t/test.$kind");
    my $p = $f;
    push(@files,$f);
    print "$kind - $f\n";
    eval { $src->write($f, -format => "$kind") };
    ok($@,''," write $@");
    ok($p,$f,"File name corrupted");
    ok(-f $f,1,"No $f created");
    my $new;
    eval { $new = $mw->Photo(-file => $f, -format => "$kind") };
    ok($@,''," load $@");
    ok(defined($new),1,"Could not load $f");

    my $skip_unsupported_data_format = $kind =~ /^(PPM|gif)$/ ? "$kind is not supported" : "";

    my $data;
    my $new2;
    if ($skip_unsupported_data_format)
     {
      Tk::catch { $data = $src->data(-format => $kind) };
      ok($@,qr/image string format "$kind" is not supported/,"Error messaage");
      skip("No data for $kind",1,1);
     }
    else
     {
      $data = $src->data(-format => $kind);
      ok(defined($data) && $data ne "", 1, "$kind returns data");
      if (defined $data)
       {
        $new2 = $mw->Photo(-data => $data, -format => $kind) if defined $data;
        ok(defined $new2, 1,"Data back to image");
       }
      else
       {
        skip("No data was returned",1);
       }
     }

    $mw->Label(-text  => $kind)->grid(-row => $row, -column => $col);
    $mw->Label(-background => 'white', -image => $new)->grid(-row => $row+1, -column => $col);
    if (defined $new2) {
	$mw->Label(-background => 'white', -image => $new2)->grid(-row => $row+2, -column => $col);
    }
    $mw->update;
    $col++;
   }
 $row += 3;
}

# Extra tests
my $col = 0;
$mw->Label(-text => "Extra tests")->grid(-row => $row++, -column => $col);
my $file = Tk->findINC('Xcamel.gif');
my $data = do { open my $fh, $file or die $!; binmode $fh; local $/; <$fh> };

if ($Tk::VERSION <= 804.027)
 {
  skip("Binary GIF data not supported",1,1);
 }
else
 {
  my $image = $mw->Photo(-data => $data);
  ok(defined $image, 1, "Read binary GIF data");
  $mw->Label(-background => 'white', -image => $image)->grid(-row => $row, -column => $col);
  $mw->update;
 }
$col++;

if (!eval { require MIME::Base64; 1 })
 {
  skip("Need MIME::Base64 module",1,1);
 }
else
 {
  my $image = $mw->Photo(-data => MIME::Base64::encode_base64($data));
  ok(defined $image, 1, "Read base64 encoded GIF data");
  $mw->Label(-background => 'white', -image => $image)->grid(-row => $row, -column => $col);
  $mw->update;
 }
$col++;

$mw->after(2500,[destroy => $mw]);
MainLoop;

foreach (@files)
 {
  unlink($_) if -f $_;
 }

