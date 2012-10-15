
package PGPLOT;

#  Module to allow calling of PGPLOT from Perl
#
#  Karl Glazebrook [email: kgb@aaoepp.aao.gov.au]
#
#  WWW info: http://www.aao.gov.au/local/www/kgb/pgperl/

$VERSION="2.20";

use Exporter;
use DynaLoader;

@ISA = qw(Exporter DynaLoader);
@EXPORT = qw( pgarro pgask pgaxis pgband pgbbuf pgbeg pgbegin pgbin pgbox
pgcirc pgclos pgconb pgconf pgconl pgcons pgcont pgconx pgctab pgcurs pgcurse pgdraw
pgebuf pgend pgenv pgeras pgerrb pgerr1 pgerrx pgerry pgetxt pgfunt pgfunx
pgfuny pggray pghi2d pghist pgiden pgimag pglab pglabel pglcur pgldev
pglen pgline pgmove pgmtxt pgmtext pgncur pgncurse pgnumb pgolin pgopen pgpage
pgadvance pgpanl pgpap pgpaper pgpixl pgpnts pgpoly pgpt pgpt1 pgpoint pgptxt
pgptext pgqah pgqcf pgqch pgqci pgqclp pgqcir pgqcol pgqcr pgqcs pgqdt pgqfs pgqhs
pgqid pgqinf pgqitf pgqls pgqlw pgqndt pgqpos pgqtbg pgqtxt pgqvp pgqvsz pgqwin
pgrect pgrnd pgrnge pgsah pgsave pgunsa pgscf pgsch pgsci pgscir pgsclp pgscr
pgscrl pgscrn pgsfs pgshls pgshs pgsitf pgslct pgsls pgslw pgstbg pgsubp pgsvp
pgvport pgswin pgwindow pgtbox pgtick pgtext pgupdt pgvect pgvsiz pgvsize
pgvstd pgvstand pgwedg pgwnad );

if($^O =~ /mswin32/i) {
  local $DynaLoader::dl_dlext = 'xs.dll';
  bootstrap PGPLOT $VERSION;
  }

else {bootstrap PGPLOT $VERSION}

# Exit with OK status

1;

__DATA__

=head1 NAME

PGPLOT - allow subroutines in the PGPLOT graphics library to be called from Perl.

=head1 SYNOPSIS

 use PGPLOT;

 pgbegin(0,"/xserve",1,1);  
 pgenv(1,10,1,10,0,0);        
 pglabel('X','Y','My plot');  
 pgpoint(7,[2..8],[2..8],17);

 # etc...

 pgend;           

=head1 DESCRIPTION

Originally developed in the olden days of Perl4 (when it was known
as 'pgperl' due to the necessity of making a special perl executable)
PGPLOT is now a dynamically loadable perl module which interfaces
to the FORTRAN graphics library of the same name.

PGPLOT, originally developed as a FORTRAN library, is now available with
C bindings (which the Perl module uses), though a FORTRAN compiler is
still required to build it.

For every PGPLOT C/FORTRAN function the module provides an equivalent
Perl function with the same arguments. Thus the user of the module should
refer to the PGPLOT manual to learn all about how to use PGPLOT and for
the complete list of available functions.  This manual comes with the
PGPLOT distribution and is also available at the WWW address:

http://astro.caltech.edu/~tjp/pgplot/

Also refer to the extensive set of test scripts (C<test*.p>) included
in the module distribution for examples of usage of all kinds of
PGPLOT routines.

How the FORTRAN/C function calls map on to Perl calls is detailed below.


=head2 ARGUMENT MAPPING - SIMPLE NUMBERS AND ARRAYS

This is more or less as you might expect - use Perl scalars 
and Perl arrays in place of FORTRAN/C variables and arrays.

Any FORTRAN REAL/INTEGER/CHARACTER* scalar variable maps to a
Perl scalar (Perl doesn't care about the differences between
strings and numbers and ints and floats).

Thus you can say:

To draw a line to point (42,$x):

 pgdraw(42,$x); 

To plot 10 points with data in Perl arrays C<@x> and C<@y> with plot symbol
no. 17. Note the Perl arrays are passed by reference:

 pgpoint(10, \@x, \@y, 17);

You can also use the old Perl4 style:

 pgpoint(10, *x, *y, 17);

but this is deprecated in Perl5.

Label the axes:

 pglabel("X axis", "Data units", $label);

Draw ONE point, see how when C<N=1> C<pgpoint()> can take a scalar as well as
a array argument:

  pgpoint(1, $x, $y, 17);


=head2 ARGUMENT MAPPING - IMAGES AND 2D ARRAYS

Many of the PGPLOT commands (e.g. C<pggray>) take 2D arrays as
arguments. Several schemes are provided to allow efficient use
from Perl:

=over 4

=item 1.

Simply pass a reference to a 2D array, e.g: 

  # Create 2D array

  $x=[];
  for($i=0; $i<128; $i++) { 
     for($j=0; $j<128; $j++) {
       $$x[$i][$j] = sqrt($i*$j); 
     }
  }
  pggray( $x, 128, 128, ...);

=item 2.

Pass a reference to a 1D array:

  @x=();
  for($i=0; $i<128; $i++) { 
     for($j=0; $j<128; $j++) {
       $x[$i][$j] = sqrt($i*$j); 
     }
  }
  pggray( \@x, 128, 128, ...);

Here @x is a 1D array of 1D arrays. (Confused? - see perldata(1)).
Alternatively @x could be a flat 1D array with 128x128 elements, 2D
routines such as C<pggray()> etc. are programmed to do the right thing
as long as the number of elements match.

=item 3.

If your image data is packed in raw binary form into a character string
you can simply pass the raw string. e.g.:

   read(IMG, $img, 32768); 
   pggray($img, $xsize, $ysize, ...);

Here the C<read()> function reads the binary data from a file and the
C<pggray()> function displays it as a grey-scale image.

This saves unpacking the image data in to a potentially very large 2D
perl array. However the types must match. The string must be packed as a
C<"f*"> for example to use C<pggray>. This is intended as a short-cut for
sophisticated users. Even more sophisticated users will want to download
the C<PDL> module which provides a wealth of functions for manipulating
binary data.


PLEASE NOTE: As PGPLOT is a Fortran library it expects it's images to be
be stored in row order. Thus a 1D list is interpreted as a sequence of
rows end to end. Perl is similar to C in that 2D arrays are arrays of
pointers thus images end up stored in column order. 

Thus using perl multidimensional arrays the coordinate ($i,$j) should be
stored in $img[$j][$i] for things to work as expected, e.g:

   $img = [];
   for $j (0..$nx-1) for $i (0..$ny-1) { 
      $$img[$j][$i] = whatever();
   }}
   pggray($$img, $nx, $ny, ...);
   
Also PGPLOT displays coordinate (0,0) at the bottom left (this is
natural as the subroutine library was written by an astronomer!).


=back

=head2 ARGUMENT MAPPING - FUNCTION NAMES

Some PGPLOT functions (e.g. C<pgfunx>) take functions as callback
arguments. In Perl simply pass a subroutine reference or a name,
e.g.:

 # Anonymous code reference:

 pgfunx(sub{ sqrt($_[0]) },  500, 0, 10, 0);

 # Pass by ref:

 sub foo {
   my $x=shift;
   return sin(4*$x);
 }

 pgfuny(\&foo, 360, 0, 2*$pi, 0);

 # Pass by name:

 pgfuny("foo", 360, 0, 2*$pi, 0);


=head2 ARGUMENT MAPPING - GENERAL HANDLING OF BINARY DATA

In addition to the implicit rules mentioned above PGPLOT now provides
a scheme for explictly handling binary data in all routines.

If your scalar variable (e.g. C<$x>) holds binary data (i.e. 'packed')
then simply pass PGPLOT a reference to it (e.g. C<\$x>). Thus one can
say:

   read(MYDATA, $wavelens, $n*4);
   read(MYDATA, $spectrum, $n*4);
   pgline($n, \$wavelens, \$spectrum);

This is very efficient as we can be sure the data never gets copied
and will always be interpreted as binary.

Again see the C<PDL> module for sophisticated manipulation of
binary data. C<PDL> takes great advantage of these facilities.

Be VERY careful binary data is of the right size or your segments
might get violated.

