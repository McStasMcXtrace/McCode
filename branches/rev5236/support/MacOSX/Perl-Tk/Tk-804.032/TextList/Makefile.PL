use Tk::MMutil;

Tk::MMutil::TkExtMakefile(
    DISTNAME => "Tixish",
    VERSION => '1.1',
    'dist' => { COMPRESS => 'gzip', SUFFIX => 'gz' }
);

package MY;
sub init_PM {
    my $self = shift;
    $self->SUPER::init_PM(@_);
    delete $self->{PM}->{"reindex.pl"};
}
