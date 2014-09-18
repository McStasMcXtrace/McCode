package Tk::JPEG;
require DynaLoader;

use vars qw($VERSION);
$VERSION = '4.003'; # was: sprintf '4.%03d', q$Revision: #2$ =~ /\D(\d+)\s*$/;
use Tk 800.015;
require Tk::Image;
require Tk::Photo;
require DynaLoader;

use vars qw($VERSION $XS_VERSION);

@ISA = qw(DynaLoader);

$XS_VERSION = $Tk::VERSION;
bootstrap Tk::JPEG;

1;

__END__

=head1 NAME

Tk::JPEG - JPEG loader for Tk::Photo

=head1 SYNOPSIS

  use Tk;
  use Tk::JPEG;

  my $image = $widget->Photo('-format' => 'jpeg', -file => 'something.jpg');


=head1 DESCRIPTION

This is an extension for Tk which supplies
JPEG format loader for Photo image type.

JPEG access is via release 5 of the The Independent JPEG Group's (IJG)
free JPEG software.

=head1 HISTORY

This extension works for Tk800.015 and later and is by default bundled
with Perl/Tk since Tk804.

=head1 AUTHOR

Nick Ing-Simmons E<lt>nick@ing-simmons.netE<gt>

=head1 SEE ALSO

L<Tk::Photo>.

=cut


