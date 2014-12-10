sub OS2_massage {		# Need to put before BEGIN
  if (@ARGV) {
    die <<EOD;

Please start me as one of
	perl $0 x
	perl $0 open32
	perl $0 pm
EOD
  }
  if (not defined $win_arch) {
      $win_arch = 'pm';
      print STDERR <<EOP;

No Window architecture specified, building for PM.

Please start me as one of
	perl $0 x
	perl $0 open32
	perl $0 pm
if you want to specify architecture explicitely.

EOP
  }
  if ($win_arch ne 'x' and not -r 'pTk/mTk/open32/tkWinOS2.c' ) {
    print <<EOD;

Cannot find pTk/mTk/open32/tkWinOS2.c, trying to find Tk-OS2src nearby...

EOD
    my @zips = <../Tk-OS2src-*/perltk_os2_common.zip>;

    die <<EOD unless @zips;

Cannot find ../Tk-OS2src-*/perltk_os2_common.zip, did you read README.os2?

EOD
    print 'Extracting OS/2-related files...\nunzip', $zips[-1], "\n";
    system 'unzip', $zips[-1] and die "Unzip: $!";
    print "Extracted...\n";
  }
  if ($win_arch eq 'pm' and not -r 'pTk/mTk/os2/tkOS2Int.h') {
    print <<EOD unless @zips;

Cannot find pTk/mTk/os2/tkOS2Int.h, trying to find Tk-OS2src nearby...

EOD
    my @zips = <../Tk-OS2src-*/perltk_os2_pm.zip>;

    die <<EOD unless @zips;

Cannot find ../Tk-OS2src-*/perltk_os2_pm.zip, did you read README.os2?

EOD
    print 'Extracting OS/2-related files...\nunzip', $zips[-1], "\n";
    system 'unzip', $zips[-1] and die "Unzip: $!";
  }
  $test_perl = 'perl__.exe' if $win_arch ne 'x';
}

1;
