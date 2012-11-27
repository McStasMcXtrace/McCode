package Tk::CodeText::Bash;

use vars qw($VERSION);
$VERSION = '0.1'; # Initial release;

use strict;
use warnings;
use base('Tk::CodeText::Template');

my $separators = '\||&|;|(|)|<|>|\s|\'|"|`|#|$';

sub new {
  	my ($proto, $rules) = @_;
  	my $class = ref($proto) || $proto;
	if (not defined($rules)) {
		$rules =  [
			['Text'],
			['Comment', -foreground => 'gray'],
			['Reserved', -foreground => 'brown'],
			['Keyword', -foreground => 'orange'],
			['String', -foreground => 'red'],
			['Backticked', -foreground => 'purple'],
			['String intrapolated', -foreground => 'red'],
			['Escaped character', -foreground => 'magenta'],
			['Operator', -foreground => 'darkblue'],
			['Variable', -foreground => 'blue'],
		];
	};
	my $self = $class->SUPER::new($rules);
	$self->lists({
		'Reserved' => [
			'!', 'case', 'do', 'done', 'elif', 'else', 'esac', 'fi', 'for',
			'function', 'if', 'in', 'select', 'then', 'until', 'while', '{',
			'}', 'time', '[[', ']]',
		],
		'Keyword' => [
			'alias', 'bind', 'bg','builtin', 'break', 'cd', 'command', 'compgen',
			'complete', 'continue', 'cp', 'declare', 'disown', 'dirs', 'echo', 
			'enable', 'eval', 'exec', 'exit', 'export', 'false', 'fc', 'fg', 
			'function', 'getopts', 'hash', 'help', 'history', 'jobs', 'kill',
			'let', 'local', 'logout', 'mv', 'popd', 'printf', 'pushd','pwd',  'read', 
			'readonly', 'return', 'rm', 'select', 'set', 'shift', 'shopt', 'source', 
			'suspend', 'test', 'trap', 'true', 'type', 'typeset', 'ulimit',
			'umask', 'unalias', 'unset', 'variables', 'wait',
		],
	});
  	bless ($self, $class);
	$self->callbacks({
		'Backticked' => \&parseBackticked,
		'Comment' => \&parseComment,
		'Escaped character' => \&parseEscaped,
		'Keyword' => \&parseKeyword,
		'Operator' => \&parseOperator,
		'Reserved' => \&parseReserved,
		'String' => \&parseString,
		'String intrapolated' => \&parseIString,
		'Text' => \&parseText,
		'Variable' => \&parseVariable,
	});
	$self->stackPush('Text');
  	return $self;
}

sub parseBackticked {
	my ($self, $text) = @_;
	if ($text =~ s/^(`)//) { #backtick stop
		$self->snippetParse($1);
		$self->stackPull;
		return $text;
	}
	return $self->parseText($text);
}

sub parseComment {
	my ($self, $text) = @_;
	return $self->parserError($text);
}

sub parseEscaped {
	my ($self, $text) = @_;
	return $self->parserError($text);
}

sub parseIString {
	my ($self, $text) = @_;
	if ($text =~ s/^(\\.)//) { #escaped character
		$self->snippetParse($1, 'Escaped character');
		return $text;
	}
	if ($text =~ s/^(\$[^$separators]*)//) { #variable
		$self->snippetParse($1, 'Variable');
		return $text;
	}
	if ($text =~ s/^(`)//) { #backticked
		$self->stackPush('Backticked');
		$self->snippetParse($1);
		return $text;
	}
	if ($text =~ s/^(")//) { #string stop
		$self->snippetParse($1);
		$self->stackPull;
		return $text;
	}
	if ($text =~ s/^([^"|\$|`]+)//) { #string content
		$self->snippetParse($1);
		return $text;
	}
	return $self->parserError($text);
}

sub parseKeyword {
	my ($self, $text) = @_;
	return $self->parserError($text);
}

sub parseOperator {
	my ($self, $text) = @_;
	return $self->parserError($text);
}

sub parseReserved {
	my ($self, $text) = @_;
	return $self->parserError($text);
}

sub parseString {
	my ($self, $text) = @_;
	if ($text =~ s/^([^']+)//) { #string content
		$self->snippetParse($1);
		return $text;
	}
	if ($text =~ s/^(')//) { #string stop
		$self->snippetParse($1);
		$self->stackPull;
		return $text;
	}
	return $self->parserError($text);
}

sub parseText {
	my ($self, $text) = @_;
	if ($text =~ s/^(^#!\/.*)//) { #launch line
		$self->snippetParse($1, 'Reserved');
		return $text;
	}
	if ($text =~ s/^(#.*)//) { #comment
		$self->snippetParse($1, 'Comment');
		return $text;
	}
	if ($text =~ s/^(\s+)//) { #spaces
		$self->snippetParse($1);
		return $text;
	}
	if ($text =~ s/^(`)//) { #backticked
		$self->stackPush('Backticked');
		$self->snippetParse($1);
		return $text;
	}
	if ($text =~ s/^(")//) { #string intrapolated
		$self->stackPush('String intrapolated');
		$self->snippetParse($1);
		return $text;
	}
	if ($text =~ s/^('[^']*)//) { #string start
		$self->snippet($1);
		if ($text) { #if there is still text to be parsed, string ends at same line
			if ($text =~ s/(^')//) {
				$self->snippetParse($1)
			}
		} else {
			$self->stackPush('String');
		}
		return $text;
	}
	if ($text =~ s/^(\$[^$separators]*)//) { #variable
		$self->snippetParse($1, 'Variable');
		return $text;
	}
	if ($text =~ s/^([\|\||\||&&|&|;;|;|(|)])//) { #operator
		$self->snippetParse($1, 'Operator');
		return $text
	}
	if ($text =~ s/^([<|>])//) { #remaining separators
		$self->snippetParse($1);
		return $text
	}
	if ($text =~ s/^(\\.)//) { #escaped character
		$self->snippet($1, 'Escaped character');
		return $text;
	}
	if ($text =~ s/^([^$separators]+)//) {	#fetching a bare part
		if ($self->tokenTest($1, 'Reserved')) {
			$self->snippetParse($1, 'Reserved');
		} elsif ($self->tokenTest($1, 'Keyword')) {
			$self->snippetParse($1, 'Keyword');
		} else { #unrecognized text
			$self->snippetParse($1);
		}
		return $text
	}
	#It shouldn't have come this far, but it has.
	return $self->parserError($text);
}

sub parseVariable {
	my ($self, $text) = @_;
	return $self->parserError($text);
}



1;

__END__


=head1 NAME

Tk::CodeText::Bash - a Plugin for HTML syntax highlighting

=head1 SYNOPSIS

 require Tk::CodeText::Bash;
 my $sh = new Tk::CodeText::Bash( [
	['Text'],
   ['Tag', -foreground => 'brown'],
   ['Attr', -foreground => 'darkblue'],
   ['Comment', -foreground => 'lightblue'],
   ['Value', -foreground => 'orange'],
   ['String', -foreground => 'red'],
   ['SpChar', -foreground => 'magenta'],
 ]);

=head1 DESCRIPTION

Tk::CodeText::Bash is a  plugin module that provides syntax highlighting
for Bash to a Tk::CodeText text widget.

It inherits Tk::CodeText::Template. See also there.

=head1 AUTHOR

Hans Jeuken (haje@toneel.demon.nl)

=cut

=head1 BUGS

Unknown

=cut





