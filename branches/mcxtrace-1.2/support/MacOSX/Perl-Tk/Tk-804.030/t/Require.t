#!perl -w
use strict;
BEGIN  { $ENV{'PERL_DL_NONLAZY'} = 1 }

require Tk;
# $SIG{__WARN__} = sub { die shift };
my ($dir) = $INC{'Tk.pm'} =~ /^(.*)\.pm$/;
opendir(TK,$dir) || die "Cannot opendir $dir:$!";
my @files = grep(/\.pm$/,readdir(TK));
closedir(TK);
my $file;
$Test::ntest = @files;
print "1..",$Test::ntest,"\n";
my $count = 1;
foreach $file (@files)
 {
  if ($file =~ /\.pm$/)
   {
    # print "Tk/$file\n";
    eval { require "Tk/$file" };
    if ($@)
     {
      warn "Tk/$file: $@";
      print "not ";
     }
    print "ok ",$count++,"\n";
   }
 }

