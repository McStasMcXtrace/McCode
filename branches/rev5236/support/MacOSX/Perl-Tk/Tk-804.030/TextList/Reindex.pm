package Tk::Reindex;


use vars qw($VERSION);
$VERSION = '4.006'; # $Id: //depot/Tkutf8/TextList/Reindex.pm#4 $

use Tk;
use base qw(Tk::Derived);


sub Populate
{
 my ($w, $args) = @_;

 $w->_callbase('Populate',$args);

 $w->ConfigSpecs(-linestart    => ["PASSIVE", "lineStart",    "LineStart", 0],
                 -toindexcmd   => ["CALLBACK", "toIndexCmd",  "ToIndexCmd" ,  [\&to_index,$w]],
                 -fromindexcmd => ["CALLBACK", "fromIndexCmd","FromIndexCmd", [\&from_index,$w]]);
}

sub import
{
  my($module,$base)=@_;
  my $pkg=(caller)[0];

  no strict 'refs';
  *{"${pkg}::_reindexbase"}=sub{$base};
}

sub _callbase
{
  my($w,$sub)=(shift,shift);
  my $supersub=$w->_reindexbase()."::$sub";
  $w->$supersub(@_);
}

BEGIN
{
  # list of subroutines and index argument number (-1 as first element means return value)
  my %subs=('bbox'      => [0],
            'compare'   => [0,2],
            'delete'    => [0,1],
            'dlineinfo' => [0],
            'dump'      => \&_find_dump_index,
            'get'       => [0,1],
            'index'     => [-1,0],
            'insert'    => [0],
            'mark'      => \&_find_mark_index,
            'search'    => \&_find_search_index,
            'see'       => [0],
            'tag'       => \&_find_tag_index,
            'window'    => [1],
            'image'     => [1],
           );

  foreach my $sub (keys %subs)
  {
    my $args=$subs{$sub};
    my $argsub=ref $args eq 'CODE'?$args:sub{$args};
    my $newsub=sub
    {
      my($w)=shift;
      my(@iargs)=grep($_<=$#_,@{$argsub->(@_)});
      my $iret; $iret=shift @iargs if @iargs && $iargs[0]==-1;
      my(@args)=@_;
      @args[@iargs]=$w->Callback(-toindexcmd,@args[@iargs]);
      my(@ret)=$w->_callbase($sub,@args);
      @ret=$w->Callback(-fromindexcmd,@ret) if $iret;
      wantarray?@ret:$ret[0];
    };
    no strict 'refs';
    *{$sub}=$newsub;
  }
}

sub to_index
{
  my $w=shift;
  my $offset=$w->cget(-linestart)+1;
  my(@args)=@_;
  foreach (@args)
   {
    s/^\d+(?=\.)/$&+$offset/e;
   }
  @args;
}

sub from_index
{
  my $w=shift;
  my $offset=$w->cget(-linestart)+1;
  my(@args)=@_;
  foreach (@args)
   {
    s/^\d+(?=\.)/$&-$offset/e
   }
  @args;
}

sub _find_dump_index
{
  my $idx=_count_options(@_);
  [$idx,$idx+1];
}

sub _find_search_index
{
  my $idx=_count_options(@_);
  [$idx+1,$idx+2];
}

sub _count_options
{
  my $idx=0;
  while($_[$idx]=~/^-/g)
  {
    $idx++;
    $idx++ if $' eq 'count' or $' eq 'command';
    last if $' eq '-';
  }
  $idx;
}

sub _find_tag_index
{
  return [1]   if $_[0] eq 'names';
  return [2,3] if $_[0]=~/^(add|remove|nextrange|prevrange)$/;
  return [-1]  if $_[0] eq 'ranges';
  return [];
}

sub _find_mark_index
{
  return [2] if $_[0] eq 'set';
  return [1] if $_[0] eq 'next' or $_[0] eq 'previous';
  return [];
}

1;

=head1 NAME

Tk::Reindex - change the base index of Text-like widgets

=for category Derived Widgets

=head1 SYNOPSIS

    use Tk::ReindexedText;
    $t1=$w->ReindexedText(-linestart => 2);

    use Tk::ReindexedROText;
    $t2=$w->ReindexedROText(-linestart => 0);

=head1 DESCRIPTION

Creates a new widget class based on B<Text>-like widgets that can
redefine the line number base (normally B<Text> widgets start line
numbers at 1), or possibly other manipulations on indexes.

=head1 STANDARD OPTIONS

The newly-defined widget takes all the same options as the base
widget, which defaults to B<Text>.

=head1 WIDGET-SPECIFIC OPTIONS

=over

=item Name:   B<lineStart>

=item Class:  B<LineStart>

=item Switch: B<-linestart>

Sets the line number of the first line in the B<Text> widget. The
default B<-toindexcmd> and B<-fromindexcmd> use this configuration
option.

-item Name:   B<toIndexCmd>  B<fromIndexCmd>

-item Class:  B<ToIndexCmd>  B<FromIndexCmd>

-item Switch: B<-toindexcmd> B<-fromindexcmd>

These two options specify callbacks that are called with a list of
indexes and are responsible for translating them to/from indexes that
the base B<Text> widget can understand. The callback is passed the
widget followed by a list of indexes, and should return a list of
translated indexes. B<-toindexcmd> should translate from 'user'
indexes to 'native' B<Text>-compatible indexes, and B<-fromindexcmd>
should translate from 'native' indexes to 'user' indexes.

The default callbacks simply add/subtract the offset given by the
B<-linestart> option for all indexes in 'line.character' format.

It would probably be prudent to make these functions inverses of each
other.

=back

=head1 CLASS METHODS

=over

=item import

To make new Reindex widgets, this function should be called via B<use>
with the name of the Text-like base class that you are extending with
"Reindex" capability.  'use base(Tk::Reindex Tk::nameofbasewidget)'
should also be specified for that widget.

=back

=head1 BUGS

I've used the word "indexes" instead of "indices" throughout the
documentation.

All the built-in perl code for widget bindings & methods will use the
new 'user' indexes.  Which means all this index manipulation might
might break code that is trying to parse/manipulate indexes. Or even
assume that '1.0' is the beginning index.  B<Tk::Text::Contents> comes
to mind.

=head1 AUTHOR

Andrew Allen <ada@fc.hp.com>

This code may be distributed under the same conditions as Perl.

=cut
