package Tk::CodeText::HTML;

use vars qw($VERSION);
$VERSION = '0.2';

use strict;
use base('Tk::CodeText::Template');

sub new {
	my ($proto, $rules) = @_;
	my $class = ref($proto) || $proto;
	if (not defined($rules)) {
		$rules =  [
			['Text'],
			['Tag', -foreground => 'brown'],
			['Attr', -foreground => 'darkblue'],
			['Comment', -foreground => 'lightblue'],
			['Value', -foreground => 'orange'],
			['String', -foreground => 'red'],
			['SpChar', -foreground => 'magenta'],
		];
	};
	my $self = $class->SUPER::new($rules);
	$self->stackPush('Text');
	bless ($self, $class);
	return $self;
}

sub highlight {
	my $hlt = shift;
	my @in = split //, shift;
	$hlt->snippetParse;
	my $out = $hlt->out;
	@$out = ();
	foreach my $c (@in) {
		if ($c eq '<') {
			if ($hlt->stackTop eq 'Text') {
#				print "opening Tag\n";
				$hlt->snippetParse;
				$hlt->snippetAppend($c);
				$hlt->stackPush('Tag');
			} else {
				$hlt->snippetAppend($c)
			}
		} elsif ($c eq '>') {
			if ($hlt->stackTop eq 'Tag') {
#				print "closing Tag\n";
				$hlt->snippetAppend($c);
				$hlt->snippetParse;
				$hlt->stackPull;
			} elsif (($hlt->stackTop eq 'Value') or ($hlt->stackTop eq 'Attr') or ($hlt->stackTop eq 'Comment')) {
#				print "closing Tag\n";
				$hlt->snippetParse;
				$hlt->stackPull;
				$hlt->snippetAppend($c);
				$hlt->snippetParse;
				$hlt->stackPull;
			} else {
				$hlt->snippetAppend($c);
			}
		} elsif ($c eq '"') {
			if (($hlt->stackTop eq 'Value') or ($hlt->stackTop eq 'Comment')) {
#				print "opening String\n";
				$hlt->snippetParse;
				$hlt->snippetAppend($c);
				$hlt->stackPush('String');
			} elsif ($hlt->stackTop eq 'String') {
#				print "closing String\n";
				$hlt->snippetAppend($c);
				$hlt->snippetParse;
				$hlt->stackPull;
			} else {
				$hlt->snippetAppend($c);
			}
		} elsif ($c eq '!') {
			if ($hlt->stackTop eq 'Tag') {
#				print "opening Comment\n";
				$hlt->snippetParse;
				$hlt->snippetAppend($c);
				$hlt->stackPush('Comment');
			} else {
				$hlt->snippetAppend($c);
			}
		} elsif ($c eq '&') {
			if ($hlt->stackTop eq 'Text') {
#				print "opening SpChar\n";
				$hlt->snippetParse;
				$hlt->snippetAppend($c);
				$hlt->stackPush('SpChar');
			} else {
				$hlt->snippetAppend($c);
			}
		} elsif ($c eq ';') {
			if ($hlt->stackTop eq 'SpChar') {
#				print "closing SpChar\n";
				$hlt->snippetAppend($c);
				$hlt->snippetParse;
				$hlt->stackPull;
			} else {
				$hlt->snippetAppend($c);
			}
		} elsif ($c eq '=') {
			if ($hlt->stackTop eq 'Attr') {
#				print "opening Value\n";
				$hlt->snippetParse;
				$hlt->stackPull;
				$hlt->snippetAppend($c);
				$hlt->snippetParse;
				$hlt->stackPush('Value');
			} else {
				$hlt->snippetAppend($c);
			}
		} elsif ($c =~ /\s/) {
			if ($hlt->stackTop eq 'Tag') {
#				print "opening Attr\n";
				$hlt->snippetParse;
				$hlt->snippetAppend($c);
				$hlt->stackPush('Attr');
			} elsif ($hlt->stackTop eq 'Value') {
				$hlt->snippetParse;
				$hlt->snippetAppend($c);
				$hlt->stackPull;
				$hlt->stackPush('Attr');
			} elsif ($hlt->stackTop eq 'SpChar') {
				$hlt->snippetParse;
				$hlt->snippetAppend($c);
				$hlt->stackPull;
			} else {
				$hlt->snippetAppend($c);
			}
		} else {
			$hlt->snippetAppend($c);
		}
	}
	$hlt->snippetParse;
	return @$out;
}

1;

__END__


=head1 NAME

Tk::CodeText::HTML - a Plugin for HTML syntax highlighting

=head1 SYNOPSIS

 require Tk::CodeText::HTML;
 my $sh = new Tk::CodeText::HTML($textwidget, [
   ['Text'],
   ['Tag', -foreground => 'brown'],
   ['Attr', -foreground => 'darkblue'],
   ['Comment', -foreground => 'lightblue'],
   ['Value', -foreground => 'orange'],
   ['String', -foreground => 'red'],
   ['SpChar', -foreground => 'magenta'],
 ]);

=head1 DESCRIPTION

Tk::CodeText::HTML is a  plugin module that provides syntax highlighting
for HTML to a Tk::CodeText text widget.

It works quite fine, but can use refinement and optimization.

It inherits Tk::CodeText::None. See also there.

=head1 METHODS

=over 4

=item B<highlight>(I<$string>);

returns a list of string snippets and tags that can be inserted
in a Tk::Text like widget instantly.

=item B<syntax>

returns 'HTML'.

=back

=cut

=head1 AUTHOR

Hans Jeuken (haje@toneel.demon.nl)

=cut

=head1 BUGS

Unknown

=cut







