package Tk::DummyEncode;

use vars qw($VERSION);
$VERSION = '4.007'; # $Id: //depot/Tkutf8/Tk/DummyEncode.pm#7 $

sub getEncoding
{
 my ($class,$name) = @_;
 return undef unless ($name =~ /(iso8859-1|X11ControlChars)/);
 my $pkg = $name;
 $pkg =~ s/\W+/_/g;
 return bless {Name => $name},$class.'::'.$pkg;
}

package Tk::DummyEncode::iso8859_1;
sub encode
{
 my ($obj,$uni,$chk) = @_;
 $_[1] = '' if $chk;
 return $uni;
}

sub decode
{
 my ($obj,$byt,$chk) = @_;
 $_[1]
= '' if $chk;
 return $byt;
}

package Tk::DummyEncode::X11ControlChars;
sub encode
{
 my ($obj,$uni,$chk) = @_;
 my $str = '';
 foreach my $ch (split(//,$uni))
  {
   $str .= sprintf("\\x{%x}",ord($ch));
  }
 $_[1] = '' if $chk;
 return $str;
}

1;

__END__
