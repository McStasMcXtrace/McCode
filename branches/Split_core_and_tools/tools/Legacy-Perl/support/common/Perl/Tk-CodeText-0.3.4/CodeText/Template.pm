package Tk::CodeText::Template;

use vars qw($VERSION);
$VERSION = '0.3';

use strict;
use Data::Dumper;

sub new {
	my ($proto, $rules) = @_;
	my $class = ref($proto) || $proto;
	if (not defined($rules)) {
		$rules =  [];
	};
	my $self = {};
	$self->{'lists'} = {};
	$self->{'out'} = [];
	$self->{'rules'} = $rules,
	$self->{'stack'} = [];
	$self->{'snippet'} = '';
	$self->{'callbacks'} = {};
	$self->{'oneliners'} = [],
	bless ($self, $class);
	return $self;
}

sub callbacks {
	my $hlt = shift;
	if (@_) { $hlt->{'callbacks'} = shift; };
	return $hlt->{'callbacks'};
}

sub highlight {
	my ($hlt, $text) = @_;
	$hlt->snippetParse;
	my $out = $hlt->out;
	@$out = ();
	while ($text) {
#		print "highlighting '$text'\n";
#		print "mode is", $hlt->stackTop, "\n";
		my $sub = $hlt->callbacks->{$hlt->stackTop};
		$text = &$sub($hlt, $text);
	}
	$hlt->snippetParse;
	return @$out;
}

sub lists {
	my $hlt = shift;
	if (@_) { $hlt->{'lists'} = shift; };
	return $hlt->{'lists'};
}

sub listAdd {
	my $hlt = shift;
	my $listname = shift;
#	print "listname $listname\n";
	my $lst = $hlt->lists;
	if (@_) {
		$lst->{$listname} = [@_];
	} else {
		$lst->{$listname} = [];
	}
	my $r = $hlt->lists->{$listname};
#	print "added tokens\n"; foreach my $f (@$r) { print "   $f\n"; };
}

sub rules {
	my $hlt = shift;
	if (@_) { $hlt->{'rules'} = shift;	}
	return $hlt->{'rules'};
}

sub out {
	my $hlt = shift;
	if (@_) { $hlt->{'out'} = shift; }
	return $hlt->{'out'};
}

sub parserError {
	my ($hlt, $text) = @_;
	my $s = $hlt->stack;
	if (@$s eq 1) { #we cannot dump this mode because it's the lowest.
		warn "Parser error\n\tmode: '" . $hlt->stackTop . "'\n" .
			"text: '$text'\nparsing  as plain text";
		$hlt->snippetParse($text);
		$text =''; #Let's call it a day;
	} else {
		warn "Parser error\n\tmode: '" . $hlt->stackTop . "'\n" .
			"text: '$text'\nexiting mode";
		$hlt->stackPull;
	};
	return $text;
}

sub snippet {
	my $hlt = shift;
	if (@_) { $hlt->{'snippet'} = shift; }
	return $hlt->{'snippet'};
}

sub snippetAppend {
	my ($hlt, $ch) = @_;
	$hlt->{'snippet'} = $hlt->{'snippet'} . $ch;
}

sub snippetParse {
	my $hlt = shift;
	my $snip = shift;
	my $attr = shift;
	unless (defined($snip)) { $snip = $hlt->snippet }
	unless (defined($attr)) { $attr = $hlt->stackTop }
	my $out = $hlt->{'out'};
#	print "parsing '$snip' with attribute '$attr'\n";
	if ($snip) {
		push(@$out, length($snip), $attr);
		$hlt->snippet('');
	}
}

sub stack {
	my $hlt = shift;
	return $hlt->{'stack'};
}

sub stackPush {
	my ($hlt, $val) = @_;
#	print "pushing $val\n";
	my $stack = $hlt->stack;
	unshift(@$stack, $val);
}

sub stackPull {
	my ($hlt, $val) = @_;
	my $stack = $hlt->stack;
	return shift(@$stack);
}

sub stackTop {
	my $hlt = shift;
	return $hlt->stack->[0];
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
	my $s = $hlt->stack;
	return @$s;
}

sub stateSet {
	my $hlt = shift;
	my $s = $hlt->stack;
	@$s = (@_);
}

sub syntax {
	my $hlt = shift;
	my $class = ref $hlt;
	$class =~ /Tk::CodeText::(.*)/;
	return $1;
}

sub tokenParse {
	my $hlt = shift;
	my $tkn = shift;
	$hlt->stackPush($tkn);
	$hlt->snippetParse(@_);
	$hlt->stackPull;
}

sub tokenTest {
	my ($hlt, $test, $list) = @_;
#	print "tokenTest $test\n";
	my $l = $hlt->lists->{$list};
	my @list = reverse sort @$l;
#	return grep { ($test =~ /^$_/)  } @$l;
	my @rl = grep { (substr($test, 0, length($_)) eq $_)  } @list;
#	foreach my $r (@rl) { print "$r\n" }
	if (@rl) {
		return $rl[0]
	} else {
		return undef;
	}
}
1;

__END__

=head1 NAME

Tk::CodeText::Template - a template for syntax highlighting plugins

=head1 SYNOPSIS


=head1 DESCRIPTION

Tk::CodeText::Template is a framework to assist authors of plugin modules.
All methods to provide highlighting in a Tk::CodeText widget are there, Just
no syntax definitions and callbacks. An instance of Tk::CodeText::Template 
should never be created, it's meant to be sub classed only. 

=head1 METHODS

=over 4

=item B<callbacks>({I<'Tagname'> => I<\&callback>, ...});

sets and returns the instance variable 'callbacks'

=item B<highlight>(I<$text>);

highlights I<$text>. It does so by selecting the proper callback
from the B<commands> hash and invoke it. It will do so untill
$text has been reduced to an empty string.

=item B<listAdd>(I<'listname'>, I<$item1>, I<$item2> ...);

Adds a list to the 'lists' hash.

=item B<lists>(I<?\%lists?>);

sets and returns the instance variable 'lists'.

=item B<out>(I<?\@highlightedlist?>);

sets and returns the instance variable 'out'.

=item B<parserError>(I<'text'>);

Error trapping method. Tries to escape the current mode. If that is not
possible, it will parse the text with the default tag. Furthermore it
complains about being called at all. Usefull for debugging when writing
a new plugin.

=item B<rules>(I<?\@rules?>)

sets and returns a reference to a list of tagnames and options.
By default it is set to [].

=item B<snippetAppend>(I<$string>)

appends I<$string> to the current snippet.

=item B<snippetParse>(I<?$text?>, I<?$tagname?>)

parses $text to the 'out' list, and assigns $tagname to it. If $tagname is
not specified it will look for the tagname by calling B<stackTop>. If I<$text>
is also not specified it will look for text by calling B<snippet>.

=item B<stack>

sets and returns the instance variable 'stack', a reference to an array.

=item B<stackPull>

retrieves the element that is on top of the stack, decrements stacksize by 1.

=item B<stackPush>(I<$tagname>)

puts I<$tagname> on top of the stack, increments stacksize by 1

=item B<stackTop>

retrieves the element that is on top of the stack.

=item B<stateCompare>(I<\@state>);

Compares two lists, \@state and the stack. returns true if they
match.

=item B<stateGet>

Returns a list containing the entire stack.

=item B<stateSet>(I<@list>)

Accepts I<@list> as the current stack.

=item B<tokenParse>(I<'Tagname'>);

Parses the currently build snippet and tags it with 'Tagname'

=item B<tokenTest>(I<$value>, I<'Listname'>);

returns true if $value is and element of 'Listname' in the 'lists' hash

=back

=cut

=head1 AUTHOR

Hans Jeuken (haje@toneel.demon.nl)

=cut

=head1 BUGS

Unknown.

=cut














