package Tk::CodeText::None;

use vars qw($VERSION);
$VERSION = '0.3';

use strict;
use Data::Dumper;

sub new {
   my ($proto, $rules) = @_;
   my $class = ref($proto) || $proto;
	my $self = {};
   bless ($self, $class);
   return $self;
}

sub highlight {
	my $hlt = shift;
	return ();
}

sub rules {
	my $hlt = shift;
	return [];
}

sub stateCompare {
	return 1;
}

sub stateGet {
	my $hlt = shift;
	return ()
}
	
sub stateSet {
	my $hlt = shift;
}

sub syntax {
	my $hlt = shift;
	return 'None'
}

1;

__END__

=head1 NAME

Tk::CodeText::None - a Plugin for No syntax highlighting

=head1 SYNOPSIS

 require Tk::CodeText::None;
 my $hl = new Tk::CodeText::None;
 my @line = $hl->highlight($line);

=head1 DESCRIPTION

Tk::CodeText::None is some kind of a dummy plugin module. All methods
to provide highlighting in a Tk::CodeText widget are there, ready
to do nothing.

It only provides those methods, that Tk::CodeText is going to call upon.

=head1 METHODS

=over 4

=item B<highlight>(I<$string>);

returns an empty list.

=back

The description of the remaining methods is more a description of what they are
supposed to do if you write your own plugin. These methods actually do as little
as possible.

=over 4

=item B<rules>(I<$txtwidget>,I<\@rules>)

sets and returns a reference to a list of tagnames and options.
By default it is set to [ ].

=item B<stateCompare>(\@state);

Compares two lists, \@state and the stack. returns true if they
match.

=item B<stateGet>

Returns a list containing the entire stack.

=item B<stateSet>(I<@list>)

Accepts I<@list> as the current stack.

=item B<syntax>

returns B<None>

=back

=cut

=head1 AUTHOR

Hans Jeuken (haje@toneel.demon.nl)

=cut

=head1 BUGS

Unknown.

=cut








