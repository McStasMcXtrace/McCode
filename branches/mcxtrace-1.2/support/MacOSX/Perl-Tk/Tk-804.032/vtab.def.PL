#!perl -w
use strict;
my $file = __FILE__;
$file =~ s/\.PL$//;
chmod(0666,$file) unless -w $file;
rename($file,"$file.old") unless -w $file;
open(F,">$file") || die "Cannot open $file:$!";
select(F);
my @vtable = qw(Tkoption Lang Tkevent Tk Tkint Tkglue Tkintdecls Tkdecls Tcldecls);
my @wtable = qw(Tkintxlibdecls Tkplatdecls Tkintplatdecls);
my @utable = qw(Xlib);
my @tix    = qw(Tix Tixint);
my @tixxpm = qw(Tiximgxpm);
my @photo  = qw(Tkimgphoto Imgint);
my @event  = qw(Tkevent);


my $maxl = 0;
length > $maxl and $maxl = length foreach (@vtable,@wtable,@utable);

print <<'END';
#define IMPORT_VTABLE(ptr,type,name) do {                       \
  ptr = INT2PTR(type *,SvIV(get_sv(name,GV_ADDWARN|GV_ADD)));   \
  if ((*ptr->tabSize)() != sizeof(type)) {                      \
    Perl_warn(aTHX_ "%s wrong size for %s",name,#type);         \
  }                                                             \
 } while (0)

END

print "#ifdef WIN32\n";
do_tables(VTABLES => @vtable,@wtable);
print "#else\n";
do_tables(VTABLES => @vtable,@utable);
print "#endif\n";

do_tables(TIX => @tix);
do_tables(PHOTO => @photo);
do_tables(EVENT => @event);
do_tables(TIXXPM => @tixxpm);

sub do_tables
{
 my $name = shift;
 my @tables = sort(@_);
 print "#define DECLARE_$name\t\\\n";
 while (@tables)
  {
   my $tab = shift(@tables);
   my $pad = ' ' x ($maxl - length($tab));
   print $tab,"Vtab$pad *",$tab,"Vptr";
   print ";$pad\\" if @tables;
   print "\n";
  }
 print "\n";
 print "#define IMPORT_$name\tdo { \\\n";
 foreach my $tab (sort @_)
  {
   my $pad = ' ' x ($maxl - length($tab));
   print qq/IMPORT_VTABLE(${tab}Vptr$pad,$pad${tab}Vtab,"Tk::${tab}Vtab");$pad \\\n/;
  }
 print "} while (0)\n";
}
