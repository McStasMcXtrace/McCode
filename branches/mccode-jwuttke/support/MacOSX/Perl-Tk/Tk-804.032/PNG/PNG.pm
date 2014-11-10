package Tk::PNG;
require DynaLoader;

use vars qw($VERSION);
$VERSION = '4.004'; # was: sprintf '4.%03d', q$Revision: #3 $ =~ /\D(\d+)\s*$/;

use Tk 800.005;
require Tk::Image;
require Tk::Photo;

use base qw(DynaLoader);

bootstrap Tk::PNG $Tk::VERSION;

1;

__END__

=head1 NAME

Tk::PNG - PNG loader for Tk::Photo

=head1 SYNOPSIS

  use Tk;
  use Tk::PNG;

  my $image = $widget->Photo('-format' => 'png', -file => 'something.png');


=head1 DESCRIPTION

This is an extension for Tk which supplies
PNG format loader for Photo image type.

=head1 HISTORY

This extension is by default bundled with Perl/Tk since Tk804.

=head1 AUTHOR

Nick Ing-Simmons E<lt>nick@ing-simmons.netE<gt>

=head1 SEE ALSO

L<Tk::Photo>.

=cut


