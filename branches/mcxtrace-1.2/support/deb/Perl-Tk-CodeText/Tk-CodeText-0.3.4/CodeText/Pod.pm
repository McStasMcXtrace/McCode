package Tk::CodeText::Pod;

use vars qw($VERSION);
$VERSION = '0.2';

use strict;
use base 'Tk::CodeText::Template';

sub new {
	my ($proto, $rules) = @_;
	my $class = ref($proto) || $proto;
	if (not defined($rules)) {
		$rules =  [
			['Text'],
			['Bold', -foreground => 'purple'],
			['Italic', -foreground => 'purple'],
			['Exact', -foreground => 'brown'],
			['Command', -foreground => 'orange'],
			['Space', -background => 'beige'],
			['Tab', -background => 'pale green'],
		];
	};
	my $self = $class->SUPER::new($rules);
	bless ($self, $class);
	$self->listAdd('specchars', 'B', 'I');
	$self->listAdd('specmodes', 'Bold', 'Italic');
	$self->stackPush('Text');
	return $self;
}

sub highlight {
	my ($hlt, $in) = @_;
	$hlt->snippetParse;
	my $out = $hlt->out;
	@$out = ();
	my $first = substr($in, 0, 1);
	if (substr($in, 0, 5) eq '=head') {
	#head mode
		$hlt->snippet($in);
		$hlt->tokenParse('Command');
	} elsif ($first eq '=') {
	#command mode
		$in =~ /(=[^\s]+)/g;
		$hlt->snippet($1);
		$hlt->tokenParse('Command');
		$hlt->parseText(substr($in, length($1), length($in) - length($1)));
	} elsif (($first eq "\t") or ($first eq ' ')) {
	#exact mode
		$in =~ /(^[^\S]+)/g;
		my @sp = split //, $1;
		while (@sp) {
			my $k = shift @sp;
			if ($k eq " ") { 
				$hlt->snippet($k);
				$hlt->tokenParse('Space');
			} elsif ($k eq "\t") {
				$hlt->snippet($k);
				$hlt->tokenParse('Tab');
			}
		}
		$hlt->tokenParse('Command');
		$hlt->snippet(substr($in, length($1), length($in) - length($1)));
		$hlt->tokenParse('Exact');
	} else {
	#text mode
		$hlt->parseText($in);
	}
	return @$out;
}

sub parseText {
	my $hlt = shift;
	my @c = split //, shift;
	while (@c) {
		my $t = shift @c;
		if ($hlt->tokenTest($t, 'specchars')) {
			if ((@c) and ($c[0] eq '<')) {
				if ($t eq 'B') {
					$hlt->snippetParse;
					$hlt->snippetAppend($t);
					$hlt->stackPush('Bold');
				} elsif ($t eq 'I') {
					$hlt->snippetParse;
					$hlt->snippetAppend($t);
					$hlt->stackPush('Italic');
				} else {
					$hlt->snippetAppend($t);
				}
			} else {
				$hlt->snippetAppend($t);
			}
		} elsif ($t eq '>') {
			if ($hlt->tokenTest($hlt->stackTop, 'specmodes')) {
				$hlt->snippetAppend($t);
				$hlt->snippetParse;
				$hlt->stackPull;
			}
		} else {
			$hlt->snippetAppend($t);
		}
		
	};
	$hlt->snippetParse;
}

1;

__END__


=head1 NAME

Tk::CodeText::Pod - a Plugin for syntax highlighting of pod files.

=head1 SYNOPSIS

 require Tk::CodeText::Pod;
 my $sh = new Tk::CodeText::Pod([
    ['Text'],
    ['Bold', -font => [-weight => 'bold']],
    ['Italic', -font => [-slant => 'italic']],
    ['Exact', -foreground => 'brown'],
    ['Command', -foreground => 'orange'],
    ['Space', -background => 'beige'],
    ['Tab', -background => 'pale green'],
 ]);

=head1 DESCRIPTION

Tk::CodeText::Pod is a  plugin module that provides syntax highlighting
for pod files to a Tk::CodeText text widget.

It inherits Tk::CodeText::Template. See also there.

=head1 METHODS

=over 4

=item B<highlight>(I<$string>);

returns a list of string snippets and tags that can be inserted
in a Tk::Text like widget instantly.

=item B<syntax>

returns 'Pod'.

=back

=cut

=head1 AUTHOR

Hans Jeuken (haje@toneel.demon.nl)

=cut

=head1 BUGS

Unknown

=cut


