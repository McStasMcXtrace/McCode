package Tk::CodeText::Perl;

use vars qw($VERSION);
$VERSION = '0.4';
use Syntax::Highlight::Perl;
use base 'Syntax::Highlight::Perl';

use strict;
use Data::Dumper;

sub new {
	my ($proto, $rules) = @_;
	my $class = ref($proto) || $proto;
	my $self = $class->SUPER::new;
	if (not defined($rules)) {
		$rules =  [
			['DEFAULT', -foreground => 'black'],
			['Comment_Normal', -foreground => 'lightblue'],
			['Comment_Pod', -foreground => 'lightblue'],
			['Directive', -foreground => 'brown'],
			['Label', -foreground => 'black'],
			['Quote', -foreground => 'red'],
			['String', -foreground => 'red'],
			['Variable_Scalar', -foreground => 'blue'],
			['Variable_Array', -foreground => 'blue'],
			['Variable_Hash', -foreground => 'blue'],
			['Subroutine', -foreground => 'orange'],
			['Character', -foreground => 'magenta'],
			['Keyword', -foreground => 'brown'],
			['Builtin_Operator', -foreground => 'darkgreen'],
			['Operator', -foreground => 'brown'],
			['Number', -foreground => 'darkblue'],
		];
	};
	$self->{'rules'} = [];
	bless ($self, $class);
	$self->rules($rules);
	$self->unstable(1);
	return $self;
}

sub highlight {
	my $hlt = shift;
	my $txt =  $hlt->format_string(shift);
	my @target = ();
	my @lst = split /\e\e\e/, $txt; #start to retrieve the color info tags.
	while (@lst) { #set up the insert command options.
		push(@target, length(shift @lst), shift @lst);
	};
	return @target;
}

sub rules {
	my $hlt = shift;
	if (@_) {
		my $r = shift;
		my %format = ();
		foreach my $k (@$r) {
			$format{$k->[0]} = ["", "\e\e\e" . $k->[0] . "\e\e\e"];
		}
		$hlt->set_format(%format);
		$hlt->reset;
		$hlt->{'rules'} = $r;
	}
	return $hlt->{'rules'};
}

sub stateCompare {
	my ($hlt, $state) = @_;
	my $h = [ $hlt->stateGet ];
	my $equal = 1;
	if (Dumper($h) ne Dumper($state)) { $equal = 0 };
	return $equal;
}

sub stateGet {
	my $hlt = shift;
	return (
		$hlt->in_heredoc,
		$hlt->in_string,
		$hlt->in_pod,
		$hlt->was_pod,
		$hlt->in_data,
		$hlt->{'quote_instigator'},
		$hlt->{'quote_terminator'},
		$hlt->{'quote_type'},
		$hlt->{'found_multi'},
		$hlt->{'awaiting_multi'},
		$hlt->{'awaiting_variable'},
		$hlt->{'awaiting_class'},
		$hlt->{'last_token'},
		$hlt->{'last_token_type'},
		$hlt->{'reentrant'},
	);
}

sub stateSet {
	my $hlt = shift;
	$hlt->{'in_heredoc'} = shift;
	$hlt->{'in_string'} = shift;
	$hlt->{'in_pod'} = shift;
	$hlt->{'was_pod'} = shift;
	$hlt->{'in_data'} = shift;
	$hlt->{'quote_instigator'} = shift;
	$hlt->{'quote_terminator'} = shift;
	$hlt->{'quote_type'} = shift;
	$hlt->{'found_multi'} = shift;
	$hlt->{'awaiting_multi'} = shift;
	$hlt->{'awaiting_variable'} = shift;
	$hlt->{'awaiting_class'} = shift;
	$hlt->{'last_token'} = shift;
	$hlt->{'last_token_type'} = shift;
	$hlt->{'reentrant'} = shift;
}

sub syntax {
	my $hlt = shift;
	return 'Perl',
}

1;

__END__


=head1 NAME

Tk::CodeText::Perl - a Plugin for Perl syntax highlighting

=head1 SYNOPSIS

Tk::CodeText::Perl inherits Syntax::Highlight::Perl;

For its limitations see also there.
This module provides extra methods to provide syntax highlighting
for the Perl programming language.

=head1 METHODS

=over 4

=item B<highlight>(I<$string>);

returns a list of string snippets and tags that can be inserted
in a Tk::Text like widget instantly.

=item B<rules>(I<$txtwidget>,I<\@list>)

sets and returns a reference to a list of tagnames and options.
By default it is set to:

 [
    ['Comment_Normal', -foreground => 'lightblue'],
    ['Comment_Pod', -foreground => 'lightblue'],
    ['Directive', -foreground => 'black'],
    ['Label', -foreground => 'black'],
    ['Quote', -foreground => 'red'],
    ['String', -foreground => 'red'],
    ['Variable_Scalar', -foreground => 'blue'],
    ['Variable_Array', -foreground => 'blue'],
    ['Variable_Hash', -foreground => 'blue'],
    ['Subroutine', -foreground => 'orange'],
    ['Character', -foreground => 'magenta'],
    ['Keyword', -foreground => 'darkgreen'],
    ['Builtin_Operator', -foreground => 'darkgreen'],
    ['Operator', -foreground => 'brown'],
    ['Number', -foreground => 'darkblue'],
 ]

=item B<rulesConfigure>(I<$txtwidget>,I<\@list>)

Used internally. Don't call it yourself.

=item B<rulesDelete>(I<$txtwidget>,I<\@list>)

=item B<stateCompare>(\@state);

Compares @state to the current state of the formatter.
returns true when equal.

=item B<stateGet>

Returns a list of the current state of the formatter. 
Called by the highlighting routines in Tk::CodeText.

=item B<stateSet>(I<@list>)

Sets the state of the formatter. Called by the highlighting routines
in Tk::CodeText.


=back

=cut

=head1 AUTHOR

Hans Jeuken (haje@toneel.demon.nl)

=cut

=head1 BUGS

Propably plenty

=cut






