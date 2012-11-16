package Tk::CodeText::McStas;

use vars qw($VERSION);
$VERSION = '0.1'; # Initial release;

use strict;
use warnings;
use base('Tk::CodeText::Template');

my $separators = '\||&|;|(|)|<|>|\s|\'|"|`|#|$';

sub new { #purple #magenta #orange
    my ($proto, $rules) = @_;
    my $class = ref($proto) || $proto;
    if (not defined($rules)) {
	$rules =  [
		   ['Text'],
		   ['Comment', -foreground => 'darkblue'],
		   ['Comment2', -foreground => 'darkblue'],
		   ['Reserved', -foreground => 'magenta'],
		   ['Keyword', -foreground => 'purple'],
		   ['McSections', -foreground => 'blue'],
		   ['McOther', -foreground => 'darkred'],
		   ['String', -foreground => 'red'],
		   ['Defines', -foreground => 'darkorange'],
		   ['String intrapolated', -foreground => 'red'],
		   ['Operator', -foreground => 'darkgreen'],
		   ];
    };
    my $self = $class->SUPER::new($rules);
    $self->lists({
	    'Reserved' => [ # C statements and important functions
		       'for', 'if', 'else', 'case', 'switch', 'default', 'printf', 'sprintf', 'fprintf',
		       'static',
		       ],
	    'Keyword' => [ # McStas types, C variable types
			  'COMPONENT', 'INSTRUMENT', 'int', 'long', 'char', 'double', 'void',
      			  ],
	    'McSections' => [ # McStas sections
			     'DEFINE','DECLARE','INITIALIZE', 'TRACE', 'SHARE','SAVE','MCDISPLAY',
			     'FINALLY', 'END',
			     ],
	    'McOther' => [ # McStas placement keywords plus section separators
			   '%{','%}', 'AT', 'ABSOLUTE','RELATIVE','EXTEND','COPY',
			   'ROTATED','GROUP','PREVIOUS','JUMP','WHEN','ITERATE','SPLIT','MYSELF','NEXT'
			  ],
	});
    bless ($self, $class);
    $self->callbacks({
	'Comment' => \&parseSimple,
	'Comment2' => \&parseComment2,
	'Keyword' => \&parseSimple,
	'McSections' => \&parseSimple,
	'McOther' => \&parseSimple,
	'Operator' => \&parseSimple,
	'Reserved' => \&parseSimple,
	'Defines' => \&parseSimple,
	'String' => \&parseString,
	'String intrapolated' => \&parseIString,
	'Text' => \&parseText,
    });
    $self->stackPush('Text');
    return $self;
}

sub parseSimple {
    my ($self, $text) = @_;
    return $self->parserError($text);
}

sub parseComment2 {
    my ($self, $text) = @_;
    # Nothing stops comment2 but */
    if ($text =~ s/^(.*[*]\/)//) {
	$self->snippetParse($1);
	$self->stackPull;
	return $text;
    } elsif ($text =~ s/^(\/[*].*)//) {
	$self->snippetParse($1);
	return $text;
    } elsif ($text =~ s/^([^\/*]+)//) { #string content
	$self->snippetParse($1);
	return $text;
    } else {
	# Saw no stop sign...
	$self->snippetParse($text);
	return;
    }
}

sub parseIString {
    my ($self, $text) = @_;
    if ($text =~ s/^(\\.)//) { #escaped character
	$self->snippetParse($1, 'Escaped character');
	return $text;
    }
    if ($text =~ s/^(\")//) { #string stop
	$self->snippetParse($1);
	$self->stackPull;
	return $text;
    }
    if ($text =~ s/^([^\"|\$|\`]+)//) { #string content
	$self->snippetParse($1);
	return $text;
    }
    return $self->parserError($text);
}

sub parseString {
    my ($self, $text) = @_;
    if ($text =~ s/^([^\']+)//) { #string content
	$self->snippetParse($1);
	return $text;
    }
    if ($text =~ s/^(\')//) { #string stop
	$self->snippetParse($1);
	$self->stackPull;
	return $text;
    }
    return $self->parserError($text);
}

sub parseText {
    my ($self, $text) = @_;
    if ($text =~ s/^(^\#[a-zA-Z]*)//) { #launch line
	$self->snippetParse($1, 'Defines');
	return $text;
    }
    if ($text =~ s/^(\/\/.*)//) { # C++ style comment
	$self->snippetParse($1, 'Comment');
	return $text;
    }
    if ($text =~ /^\/[*].*/) { # C style comment
	$self->stackPush('Comment2');
	return $text;
    }
    if ($text =~ s/^(\s+[0]*)//) { #spaces
	$self->snippetParse($1);
	return $text;
    }
    if ($text =~ s/^(\")//) { #string intrapolated
	$self->stackPush('String intrapolated');
	$self->snippetParse($1);
	return $text;
    }
    if ($text =~ s/^(\'[^\']*)//) { #string start
	$self->snippet($1);
	if ($text) { #if there is still text to be parsed, string ends at same line
	    if ($text =~ s/(^\')//) {
		$self->snippetParse($1)
		}
	} else {
	    $self->stackPush('String');
	}
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
    if ($text =~ s/^([^$separators]+)//) {	#fetching a bare part
	if ($self->tokenTest($1, 'Reserved')) {
	    $self->snippetParse($1, 'Reserved');
	} elsif ($self->tokenTest($1, 'Keyword')) {
	    $self->snippetParse($1, 'Keyword');
	} elsif ($self->tokenTest($1, 'McSections')) {
	    $self->snippetParse($1, 'McSections');
	} elsif ($self->tokenTest($1, 'McOther')) {
	    $self->snippetParse($1, 'McOther');
	} else { #unrecognized text
	    $self->snippetParse($1);
	}
	return $text
	}
    #It shouldn't have come this far, but it has.
    return $self->parserError($text);


}

sub tokenTest {
    my ($hlt, $test, $list) = @_;
    my $l = $hlt->lists->{$list};
    my @list = reverse sort @$l;

    foreach my $t (@$l) {
	if ($t eq $test) {
	    return $t;
	}
    }
}
1;

__END__


    =head1 NAME

    Tk::CodeText::McStas - a Plugin for HTML syntax highlighting

    =head1 SYNOPSIS

    require Tk::CodeText::McStas;
my $sh = new Tk::CodeText::McStas([
				    ['Text'],
				    ['Tag', -foreground => 'brown'],
				    ['Attr', -foreground => 'darkblue'],
				    ['Comment', -foreground => 'lightblue'],
				    ['Value', -foreground => 'orange'],
				    ['String', -foreground => 'red'],
				    ['SpChar', -foreground => 'magenta'],
				    ]);

=head1 DESCRIPTION

    Tk::CodeText::McStas is a  plugin module that provides syntax highlighting
    for McStas to a Tk::CodeText text widget.

    This module is more or less a hack/rewrite of Tk::CodeText::Bash

    It inherits Tk::CodeText::Template. See also there.

    =head1 AUTHOR

    Peter Willendrup (peter.willendrup@risoe.dk)

    =cut

    =head1 BUGS

    Unknown

    =cut





