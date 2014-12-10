BEGIN { $^W = 1; $| = 1;}
use strict;
use File::Temp qw(tempfile);
use Test::More;
use Tk;
use Tk::Photo;

my $numFormats = 5;
eval { require Tk::JPEG };
$numFormats++ unless $@;

eval { require Tk::PNG  };
$numFormats++ unless $@;

my $mw  = MainWindow->new();
$mw->geometry('+100+100');

plan tests => (2*(7 * $numFormats) + 2 + 2 + 1 + 2);

my @files = ();

my $row = 0;
foreach my $leaf('Tk.xbm','Xcamel.gif')
 {
  my $file = Tk->findINC($leaf);
  my $src = $mw->Photo(-file => $file);
  ok defined($src), "Load $file";
  my $kind = 'Initial';
  my $col = 0;
  $mw->Label(-text  => 'Initial')->grid(-row => $row, -column => $col);
  $mw->Label(-background => 'white',-image => $src)->grid(-row => $row+1, -column => $col++);
  $mw->update;

  foreach $kind ($src->formats)
   {
    my $f = lc("t/test.$kind");
    my $p = $f;
    push(@files,$f);
    eval { $src->write($f, -format => "$kind") };
    is $@, '', "No error writing $f as $kind";
    is $p, $f, "File name is not corrupted ($f)";
    ok -f $f, "File $f was created";
    my $new;
    eval { $new = $mw->Photo(-file => $f, -format => "$kind") };
    is $@, '', "No error loading $f as $kind";
    ok defined($new), "Loading $f ($kind)";

    my $skip_unsupported_data_format = $kind =~ /^(PPM|gif)$/ ? "$kind is not supported" : "";

    my $data;
    my $new2;
    if ($skip_unsupported_data_format)
     {
      Tk::catch { $data = $src->data(-format => $kind) };
      like $@, qr/image string format "$kind" is not supported/, "Error message for $kind";
      SKIP: { skip "No data for $kind", 1 }
     }
    else
     {
      $data = $src->data(-format => $kind);
      ok defined($data) && $data ne "", "$kind returns data";
      if (defined $data)
       {
        $new2 = $mw->Photo(-data => $data, -format => $kind) if defined $data;
        ok defined $new2, "Data back to image";
       }
      else
       {
	SKIP: { skip "No data was returned", 1 }
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

SKIP:
 {
  skip "Binary GIF data not supported", 1
   if $Tk::VERSION <= 804.027;

  my $image = $mw->Photo(-data => $data);
  ok defined $image, "Read binary GIF data";
  $mw->Label(-background => 'white', -image => $image)->grid(-row => $row, -column => $col);
  $mw->update;
 }
$col++;

SKIP:
 {
  skip "Need MIME::Base64 module", 1
   if !eval { require MIME::Base64; 1 };

  my $image = $mw->Photo(-data => MIME::Base64::encode_base64($data));
  ok defined $image, "Read base64 encoded GIF data";
  $mw->Label(-background => 'white', -image => $image)->grid(-row => $row, -column => $col);
  $mw->update;
 }
$col++;

{
    # RT #70429: correct file name in error message
    eval { $mw->Photo(-file => $0) };
    like $@, qr{\Q$0\E}, 'File name appears in error message';
}

{
    my($tmpfh,$tmpfile) = tempfile(SUFFIX => ".gif", UNLINK => 1)
	or die "Cannot create temporary file: $!";
    print $tmpfh "GIF89a\0\0\0\0";
    close $tmpfh or die $!;

    eval { $mw->Photo(-file => $tmpfile, -format => 'gif') };
    like $@, qr{\Q$tmpfile\E}, 'File name appears in error message';
    like $@, qr{\Qhas dimension(s) <= 0}, 'No dimensions error message';
}

$mw->after(2500,[destroy => $mw]);
MainLoop;

foreach (@files)
 {
  unlink($_) if -f $_;
 }

