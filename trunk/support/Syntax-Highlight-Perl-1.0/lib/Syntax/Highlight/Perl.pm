package Syntax::Highlight::Perl;
#
# Syntax::Highlight::Perl
#
#   Created        01-23-2001 by Cory Johns
#   Last Modified  04-04-2001 by Cory Johns
#
#  (See end of file (or POD) for revision history.)
#
# Freely distributable under the same conditions as Perl itself.
#

($VERSION) = '1.00' =~ /([.,\d]+)/;


=head1 NAME

Syntax::Highlight::Perl  -  Highlighting of Perl Syntactical Structures

=head1 VERSION

This file documents Syntax::Highlight::Perl version B<1.0>.

=head1 SYNOPSIS

    # simple procedural
    use Syntax::Highlight::Perl ':BASIC';  # or ':FULL'

    print format_string($my_string);


    # OO
    use Syntax::Highlight::Perl;

    my $formatter = new Syntax::Highlight::Perl;
    print $formatter->format_string($my_string);

=head1 DESCRIPTION

This module provides syntax highlighting for Perl code.  The design bias is roughly line-oriented
and streamed (ie, processing a file line-by-line in a single pass).  Provisions I<may> be made
in the future for tasks related to "back-tracking" (ie, re-doing a single line in the middle of
a stream) such as speeding up state copying.

=over 4

=head2 Constructors

The only constructor provided is C<new()>.  When called on an existing object, C<new()> will
create a new I<B<copy>> of that object.  Otherwise, C<new()> creates a new copy of the (internal)
I<Default Object>.  Note that the use of the procedural syntax modifies the I<Default Object>
and that those changes I<will> be reflected in any subsequent C<new()> calls.

=head2 Formatting

Formatting is done using the C<format_string()> method.  Call C<format_string()> with one or more
strings to format, or it will default to using C<$_>.

=head2 Setting and Getting Formats

You can set the text used for formatting a syntax element using C<set_format()> (or set
the start and end format individually using C<set_start_format()> and C<set_end_format()>,
respectively).

You can also retrieve the text used for formatting for an element via C<get_start_format()>
or C<get_end_format>.  Bulk retrieval of the names or values of defined formats is possible
via C<get_format_names_list()> (names), C<get_start_format_values_list()> and C<get_end_format_values_list()>.

See L<"FORMAT TYPES"> later in this document for information on what format elements can be used.

=head2 Checking and Setting the State

You can check certain aspects of the state of the formatter via the methods: C<in_heredoc()>,
C<in_string()>, C<in_pod()>, C<was_pod()>, C<in_data()>, and C<line_count()>.

You can reset all of the above states (and a few other internal ones) using C<reset()>.

=head2 Stable and Unstable Formatting Modes

You can set or check the stability of formatting via C<unstable()>.

In unstable (TRUE) mode, formatting is not considered to be persistent with nested formats.
Or, put another way, when unstable, the formatter can only "remember" one format at a time
and must reinstate formatting for each token.  An example of unstable formatting is using
ANSI color escape sequences in a terminal.

In stable (FALSE) mode (the default), formatting is considered persistent within arbitrarily
nested formats.  Even in stable mode, however, formatting is never allowed to span multiple lines;
it is always fully closed at the end of the line and reinstated at the beginning of a new line,
if necessary.  This is to ensure properly balanced tags when only formatting a partial code snippet.
An example of stable formatting is HTML.

=head2 Substitutions

Using C<define_substitution()>, you can have the formatter substitute certain strings with others,
after the original string has been parsed (but before formatting is applied).  This is useful
for escaping characters special to the output mode (eg, E<gt> and E<lt> in HTML) without them
affecting the way the code is parsed.

You can retrieve the current substitutions (as a hash-ref) via C<substitutions()>.

=back

=head1 FORMAT TYPES

The Syntax::Highlight::Perl formatter recognizes and differentiates between many Perl syntactical
elements.  Each type of syntactical element has a Format Type associated with it.  There is also a
'DEFAULT' type that is applied to any element who's Format Type does not have a value.

Several of the Format Types have underscores in their name.  This underscore is special, and
indicates that the Format Type can be "generalized."  This means that you can assign a value to
just the first part of the Format Type name (the part before the underscore) and that value will
be applied to all Format Types with the same first part.  For example, the Format Types for all
types of variables begin with "Variable_".  Thus, if you assign a value to the Format Type "Variable",
it will be applied to any type of variable.  Generalized Format Types take precedence over
non-generalized Format Types.  So the value assigned to "Variable" would be applied to
"Variable_Scalar", even if "Variable_Scalar" had a value explicitly assigned to it.

You can also define a "short-cut" name for each Format Type that can be generalized.  The short-cut
name would be the part of the Format Type name after the underscore.  For example, the short-cut
for "Variable_Scalar" would be "Scalar".  Short-cut names have the least precedence and are only
assigned if neither the generalized Type name, nor the full Type name have values.

Following is a list of all the syntactical elements that Syntax::Highlight::Perl currently
recognizes, along with a short description of what each would be applied to.

=over 4

=item Comment_Normal

A normal Perl comment.  Starts with '#' and goes until the end of the line.

=item Comment_POD

Inline documentation.  Starts with a line beginning with an equal sign ('=') followed by
a word (eg: '=pod') and continuing until a line beginning with '=cut'.

=item Directive

Either the "she-bang" line at the beginning of the file, or a line directive altering what
the compiler thinks the current line and file is.

=item Label

A loop or statement label (to be the target of a goto, next, last or redo).

=item Quote

Any string or character that begins or ends a String.  Including, but not necessarily limited to:
quote-like regular expression operators (C<m//>, C<s///>, C<tr///>, etc), a Here-Document terminating
line, the lone period terminating a format, and, of course, normal quotes (C<'>, C<">, C<`>, C<q{}>,
C<qq{}>, C<qr{}>, C<qx{}>).

=item String

Any text within quotes, C<format>s, Here-Documents, Regular Expressions, and the like.

=item Subroutine

The identifier used to define, identify, or call a subroutine (or method).  Note that
Syntax::Highlight::Perl cannot recognize a subroutine if it is called without using parentheses
or an ampersand, or methods called using the indirect object syntax.  It formats those as barewords.

=item Variable_Scalar

A scalar variable.

Note that (theoretically) this format is not applied to non-scalar variables that are
being used as scalars (ie: array or hash lookups, nor references to anything other than scalars).
Syntax::Highlight::Perl figures out (or at least tries to) the actual I<type> of the variable
being used (by looking at how you're subscripting it) and formats it accordingly.  The first
character of the variable (ie, the C<$>, C<@>, C<%>, or C<*>) tells you the type of value being
used, and the color (hopefully) tells you the type of variable being used to get that value.

(See L<"KNOWN ISSUES"> for information about when this doesn't work quite right.)

=item Variable_Array

An array variable (but not usually a slice; see above).

=item Variable_Hash

A hash variable.

=item Variable_Typeglob

A typeglob.  Note that typeglobs not beginning with an asterisk (*) (eg: filehandles) are formatted
as barewords.  This is because, well, they are.

=item Whitespace

Whitespace.  Not usually formatted but it can be.

=item Character

A special, or backslash-escaped, character.  For example: C<\n> (newline), or C<\d> (digits).

Only occurs within strings or regular expressions.

=item Keyword

A Perl keyword.  Some examples include: my, local, sub, next.

Note that Perl does not make any distinction between keywords and built-in functions (at least
not in the documentation).  Thus I had to make a subjective call as to what would be considered
keywords and what would be built-in functions.

The list of keywords can be found (and overloaded) in the variable
C<$Syntax::Highlight::Perl::keyword_list_re> as a pre-compiled regular expression.

=item Builtin_Function

A Perl built-in function, called as a function (ie, using parentheses).

The list of built-in functions can be found (and overloaded) in the variable
C<$Syntax::Highlight::Perl::builtin_list_re> as a pre-compiled regular expression.

=item Builtin_Operator

A Perl built-in function, called as a list or unary operator (ie, without using parentheses).

The list of built-in functions can be found (and overloaded) in the variable
C<$Syntax::Highlight::Perl::builtin_list_re> as a pre-compiled regular expression.

=item Operator

A Perl operator.

The list of operators can be found (and overloaded) in the variable
C<$Syntax::Highlight::Perl::operator_list_re> as a pre-compiled regular expression.

=item Bareword

A bareword.  This can be user-defined subroutine called without parentheses, a typeglob used
without an asterisk (*), or just a plain old bareword.

=item Package

The name of a package or pragmatic module.

Note that this does not apply to the package portion of a fully qualified variable name.

=item Number

A numeric literal.

=item Symbol

A symbol (ie, non-operator punctuation).

=item CodeTerm

The special tokens that signal the end of executable code and the begining of the
DATA section.  Specifically, 'C<__END__>' and 'C<__DATA__>'.

=item DATA

Anything in the DATA section (see C<CodeTerm>).

=back

=head1 PROCEDURAL vs. OBJECT ORIENTED

Syntax::Highlight::Perl uses OO method-calls internally (and actually defines a Default Object
that is used when the functions are invoked procedurally) so you will not gain anything
(efficiency-wise) by using the procedural interface.  It is just a matter of style.

It is actually recommended that you use the OO interface, as this allows you to instantiate
multiple, concurrent-yet-separate formatters.  Though I cannot think of I<why> you would I<need>
multiple formatters instantiated. :-)

One point to note: the C<new()> method uses the Default Object to initialize new objects.  This
means that any changes to the state of the Default Object (including Format definitions) made by
using the procedural interface will be reflected in any subsequently created objects.  This can
be useful in some cases (eg, call C<set_format()> procedurally just before creating a batch of new
objects to define default Formats for them all) but will most likely lead to trouble.

=cut

use strict;

#===================================================================================================
# Yay constants!
#===================================================================================================

use constant FALSE => 0;
use constant TRUE  => not FALSE;



#===================================================================================================
# Setup package exportation.
#===================================================================================================

use base 'Exporter';

use vars qw(@EXPORT_OK %EXPORT_TAGS);

@EXPORT_OK = qw(
    reset
    unstable
    in_heredoc
    in_string
    in_pod
    was_pod
    in_data
    line_count
    substitutions
    define_substitution
    set_start_format
    set_end_format
    set_format
    get_start_format
    get_end_format
    get_format_names_list
    get_start_format_values_list
    get_end_format_values_list
    format_string
    format_token
);

%EXPORT_TAGS = (
    BASIC => [ qw(
        set_format
        format_string
    ) ],

    FULL  => [ @EXPORT_OK ],
);



#===================================================================================================
# Define package globals.
#===================================================================================================

#
# This is the (overloadable) name of the current object.
# Used to bless new objects.
#
use vars qw($THIS_CLASS);
$THIS_CLASS = __PACKAGE__;


#
# These regular expressions do much of the work for this package.  They recognize and separate
# the lines into small, meaningful (and usefull) tokens.
#
use vars qw($quotes_re $heredoc_re $identifier_re $builtin_vars_re $number_re);
use vars qw($keyword_list_re $builtin_list_re $operator_list_re);

#
# Quotes and quote-like constructs.
#
#   Note: This regex primarily matches quote _instigators_ (as opposed to terminators)
#         (when it can tell the difference).  For example, given:
#
#               qw (blah blah blah)
#
#         it will match 'qw ('.  It matches up to the open parenthesis so that we will
#         be able to tell later on what we should look for to close the quote (ie ')').
#
$quotes_re = qr/(?: q[qxwr]? | m | s | tr | y ) (?: \s+ [^\s\#] | [^\w\s] ) | [\'\"\`] /x;

#
# Here-document instigators.
#
$heredoc_re = qr/<<(?:\s*['"`]\w+['"`]|\w+)?/;

#
# Identifiers (variables, subroutines, and packages).
#
$identifier_re = qr/(?:(?:[A-Za-z_]|::)(?:\w|::)*)/;

#
# Variable class specifiers.
#
my $varchars = qr/(?:(?:[\@\%\*]|\$\#?)\$*)/;

#
# Perl builtin variables.
#
$builtin_vars_re =
qr/ \$\#?_
  | \$(?:\^[LAECDFHIMOPRSTWX]|[0-9&`'+*.\/|,\\";#%=\-~^:?!@\$<>()\[\]])
  | \$\#?ARGV(?:\s*\[)?
  | \$\#?INC\s*\[
  | \$(?:ENV|SIG|INC)\s*\{
  | \@(?:_|ARGV|INC)
  | \%(?:INC|ENV|SIG)
/x;

#
# Numbers.
#
$number_re = qr/0x[\da-fA-F]+|[_.\d]+([eE][-+]?\d+)?/;

#
# The list of keywords. Okay, so it's ugly.
#
$keyword_list_re =
qr/ continue
  | foreach
  | require
  | package
  | scalar
  | format
  | unless
  | local
  | until
  | while
  | elsif
  | next
  | last
  | goto
  | else
  | redo
  | sub
  | for
  | use
  | no
  | if
  | my
/x;

#
# The list of builtin functions.  Damn, that's ugly.
#
$builtin_list_re =
qr/ getprotobynumber
  | getprotobyname
  | getservbyname
  | gethostbyaddr
  | gethostbyname
  | getservbyport
  | getnetbyaddr
  | getnetbyname
  | getsockname
  | getpeername
  | setpriority
  | getprotoent
  | setprotoent
  | getpriority
  | endprotoent
  | getservent
  | setservent
  | endservent
  | sethostent
  | socketpair
  | getsockopt
  | gethostent
  | endhostent
  | setsockopt
  | setnetent
  | quotemeta
  | localtime
  | prototype
  | getnetent
  | endnetent
  | rewinddir
  | wantarray
  | getpwuid
  | closedir
  | getlogin
  | readlink
  | endgrent
  | getgrgid
  | getgrnam
  | shmwrite
  | shutdown
  | readline
  | endpwent
  | setgrent
  | readpipe
  | formline
  | truncate
  | dbmclose
  | syswrite
  | setpwent
  | getpwnam
  | getgrent
  | getpwent
  | ucfirst
  | sysread
  | setpgrp
  | shmread
  | sysseek
  | sysopen
  | telldir
  | defined
  | opendir
  | connect
  | lcfirst
  | getppid
  | binmode
  | syscall
  | sprintf
  | getpgrp
  | readdir
  | seekdir
  | waitpid
  | reverse
  | unshift
  | symlink
  | dbmopen
  | semget
  | msgrcv
  | rename
  | listen
  | chroot
  | msgsnd
  | shmctl
  | accept
  | unpack
  | exists
  | fileno
  | shmget
  | system
  | unlink
  | printf
  | gmtime
  | msgctl
  | semctl
  | values
  | rindex
  | substr
  | splice
  | length
  | msgget
  | select
  | socket
  | return
  | caller
  | delete
  | alarm
  | ioctl
  | index
  | undef
  | lstat
  | times
  | srand
  | chown
  | fcntl
  | close
  | write
  | umask
  | rmdir
  | study
  | sleep
  | chomp
  | untie
  | print
  | utime
  | mkdir
  | atan2
  | split
  | crypt
  | flock
  | chmod
  | BEGIN
  | bless
  | chdir
  | semop
  | shift
  | reset
  | link
  | stat
  | chop
  | grep
  | fork
  | dump
  | join
  | open
  | tell
  | pipe
  | exit
  | glob
  | warn
  | each
  | bind
  | sort
  | pack
  | eval
  | push
  | keys
  | getc
  | kill
  | seek
  | sqrt
  | send
  | wait
  | rand
  | tied
  | read
  | time
  | exec
  | recv
  | eof
  | chr
  | int
  | ord
  | exp
  | pos
  | pop
  | sin
  | log
  | abs
  | oct
  | hex
  | tie
  | cos
  | vec
  | END
  | ref
  | map
  | die
  | \-C
  | \-b
  | \-S
  | \-u
  | \-t
  | \-p
  | \-l
  | \-d
  | \-f
  | \-g
  | \-s
  | \-z
  | uc
  | \-k
  | \-e
  | \-O
  | \-T
  | \-B
  | \-M
  | do
  | \-A
  | \-X
  | \-W
  | \-c
  | \-R
  | \-o
  | \-x
  | lc
  | \-w
  | \-r
/x;

#
# The list of operators.  Also pretty ugly.
#
$operator_list_re =
qr/ xor
  | \.\.\.
  | and
  | not
  | \|\|\=
  | cmp
  | \>\>\=
  | \<\<\=
  | \<\=\>
  | \&\&\=
  | or
  | \=\>
  | \!\~
  | \^\=
  | \&\=
  | \|\=
  | \.\=
  | x\=
  | \%\=
  | \/\=
  | \*\=
  | \-\=
  | \+\=
  | \=\~
  | \*\*
  | \-\-
  | \.\.
  | \|\|
  | \&\&
  | \+\+
  | \-\>
  | ne
  | eq
  | \!\=
  | \=\=
  | ge
  | le
  | gt
  | lt
  | \>\=
  | \<\=
  | \>\>
  | \<\<
  | \,
  | \=
  | \:
  | \?
  | \^
  | \|
  | x
  | \%
  | \/
  | \*
  | \<
  | \&
  | \\
  | \~
  | \!
  | \>
  | \.
  | \-
  | \+
/x;




#===================================================================================================
# Package-private variables.
#
#   Basically, just defaults.
#===================================================================================================



#
# The default object variable.
#
# Note that all of these values (including Formats) will be *COPIED* (recursively)
# into any newly created object.
#
# Also note that any changes made to this object (via procedural-style calls to almost
# any of the exported functions) _will be reflected in any objects created after those
# changes are made_.  In other words, you probably don't want to mix OO-style and
# Procedural-style unless you really know what you're doing.
#
# Further note that this object is blessed when this package is 'use'd (or 'require'd)
# (ie, before you have a chance to overload the $THIS_CLASS package-global variable).
# If you're inheriting from this package, you'll probably want to re-bless this object.
# Although it is perhaps not totally necessary as long as you overload the $THIS_CLASS
# package-global since new() re-blesses any copies of this object using that class.
#
use vars qw($Default_Object);
$Default_Object = bless {

    #
    # State variables.
    #

    'in_string'         =>  FALSE,   # Boolean.
    'awaiting_multi'    =>  FALSE,   # Boolean.  Waiting for second part of Multipart construct.
    'found_multi'       =>  FALSE,   # Boolean.
    'awaiting_options'  =>  FALSE,   # Boolean.  Waiting for options for a Optioned construct.
    'quote_instigator'  =>     '',
    'quote_terminator'  =>     '',
    'quote_type'        =>     '',   # Any in (Interpolated, Multipart, Optioned)
    'nested_quote'      =>      0,   # Anything >= 0.

    'awaiting_variable' =>  FALSE,   # Boolean.  Indicates we're awaiting a variable identifier.
    'awaiting_class'    =>     '',   # Any in (Scalar, Array, Hash).  Class of the variable we're waiting on.

    'in_heredoc'        =>      0,   # Anything >= 0 (boolean count).
    'here_terminator'   =>     [],   # List of Here-Doc terminators that we're waiting for.

    'in_pod'            =>  FALSE,   # Boolean.  Indicates that we're inside a POD.
    'was_pod'           =>  FALSE,   # Boolean.  Indicates that we _were_ inside a POD. (For in_pod() method.)

    'in_data'           =>  FALSE,   # Boolean.  We don't format __DATA__.

    'in_comment'        =>  FALSE,   # Boolean.  Indicates we're inside a normal comment.

    'last_token'        =>     '',   # The last (non-whitespace, non-comment) token processed.
    'last_token_type'   =>     '',   # Type of the last token.

    'reentrant'         =>  FALSE,   # Boolean.  Indicates we're formatting a sub-token.

    'line_count'        =>      0,   # The number of lines that have been processed.

    'unstable'          =>  FALSE,   # Boolean.  Indicates formats are not persistent (eg. terminal colors).


    #
    # Option tables.
    #

    'Substitution Table'=>     {},   # Table of characters to replace before formatting.

    'Formats'           => {
        'DEFAULT' => ['', ''],
    },

}, $THIS_CLASS;




#===================================================================================================
# Public Methods
#
#   The stuff.
#===================================================================================================




=head1 METHODS

=over 4

=item new PACKAGE

=item new OBJECT

Creates a new object.  If called on an existing object, creates a new copy of that
object (which is thenceforth totally separate from the original).

=cut

sub new {

    my $class = shift;
    my $true_class = ref($class) || $class; # We don't want our class being the string version of a reference.
    my $source = ref($class) ? $class : $Default_Object; # If called as method of exisiting object, copy.

    #
    # Hrm.  Maybe I'll eventually take a look inside Data::Dumper and see how
    # it goes about descending the complex structures and write a non-eval
    # deep-copy routine. :-)
    #
    # Seems kinda silly to put that much work onto the perl parser when we could
    # just copy it in-memory.
    #
    use Data::Dumper;  # For deep-copy.
    use Carp;          # For croak().

    local $SIG{__WARN__} = sub { chomp($@ = $_[0]) }; # eval() sometimes doesn't put all it's errors in $@

    local $Data::Dumper::Deepcopy = 1; # Descend into nested (complex) structures.
    local $Data::Dumper::Terse    = 1; # Don't print variable names and assignments.
    local $Data::Dumper::Purity   = 1; # Ensure nested references are correctly recreated.

    my $code = Dumper($source); # Save in case of errors.

    my $self = eval($code) or
        croak "Deepcopy failed in $true_class\::new() with the error:\n  $@\non the code:\n$code";

    return bless($self, $true_class);

}




=item reset

Resets the object's internal state.  This breaks out of strings and here-docs, ends PODs,
resets the line-count, and otherwise gets the object back into a "normal" state to begin
processing a new stream.

Note that this does B<I<not>> reset any user options (including formats and format stability).

=cut

sub reset {

    my $self = ref($_[0]) ? shift : $Default_Object;

    $self->{'in_string'}         =  FALSE;
    $self->{'awaiting_multi'}    =  FALSE;
    $self->{'found_multi'}       =  FALSE;
    $self->{'awaiting_options'}  =  FALSE;
    $self->{'quote_instigator'}  =     '';
    $self->{'quote_terminator'}  =     '';
    $self->{'quote_type'}        =     '';

    $self->{'awaiting_variable'} =  FALSE;
    $self->{'awaiting_class'}    =     '';

    $self->{'in_heredoc'}        =      0;
  @{$self->{'here_terminator'}}  =     ();  # No need for a whole new anon. array.  Just empty this one.

    $self->{'in_pod'}            =  FALSE;
    $self->{'was_pod'}           =  FALSE;

    $self->{'in_data'}           =  FALSE;

    $self->{'in_comment'}        =  FALSE;

    $self->{'last_token'}        =     '';
    $self->{'last_token_type'}   =     '';

    $self->{'reentrant'}         =  FALSE;

    $self->{'line_count'}        =      0;

}




=item unstable EXPR

=item unstable

Returns true if the formatter is in unstable mode.

If called with a non-zero number, puts the formatter into unstable formatting mode.

In unstable mode, it is assumed that formatting is not persistent one token to the
next and that each token must be explicitly formatted.

=cut

sub unstable {
    my $self = ref($_[0]) ? shift : $Default_Object;
    return @_ ? $self->{'unstable'} = shift : $self->{'unstable'};
}




=item in_heredoc

Returns true if the next string to be formatted will be inside a Here-Document.

=cut

sub in_heredoc {
    my $self = ref($_[0]) ? shift : $Default_Object;
    return $self->{'in_heredoc'};
}




=item in_string

Returns true if the next string to be formatted will be inside a multi-line string.

=cut

sub in_string {
    my $self = ref($_[0]) ? shift : $Default_Object;
    return $self->{'in_string'};
}




=item in_pod

Returns true if the formatter would consider the next string passed to it as begin within
a POD structure.  This is false immediately before any POD instigators
(C<=pod>, C<=head1>, C<=item>, etc), true immediately after an instigator, throughout the POD and
immediately before the POD terminator (C<=cut>), and false immediately after the POD terminator.

=cut

sub in_pod {
    my $self = ref($_[0]) ? shift : $Default_Object;
    return $self->{'in_pod'};
}




=item was_pod

Returns true if the last line of the string just formatted was part of a POD structure.
This includes the C</^=\w+/> POD instigators and terminators.

=cut

sub was_pod {
    my $self = ref($_[0]) ? shift : $Default_Object;
    return $self->{'was_pod'};
}




=item in_data

Returns true if the next string to be formatted will be inside
the DATA section (ie, follows a C<__DATA__> or C<__END__> tag).

=cut

sub in_data {
    my $self = ref($_[0]) ? shift : $Default_Object;
    return $self->{'in_data'};
}




=item line_count

Returns the number of lines processed by the formatter.

=cut

sub line_count {
    my $self = ref($_[0]) ? shift : $Default_Object;
    return $self->{'line_count'};
}




=item substitutions

Returns a reference to the substitution table used.  The substitution
table is a hash whose keys are the strings to be replaced, and whose values
are what to replace them with.

=cut

sub substitutions {

    my $self = ref($_[0]) ? shift : $Default_Object;

    return $self->{'Substitution Table'};

}




=item define_substitution HASH_REF

=item define_substitution LIST

Allows user to define certain characters that will be substituted
before formatting is done (but after they have been processed for
meaning).

If the first parameter is a reference to a hash, the formatter
will replace it's own hash with the given one, and subsequent
changes to the hash outside the formatter will be reflected.

Otherwise, it will copy the arguments passed into it's own
hash, and any substitutions already defined (but not in the
parameter list) will be preserved. (ie, the new substitutions
will be added, without destroying what was there already.)

=cut

sub define_substitution {

    my $self = ref($_[0]) ? shift : $Default_Object;

    if(ref($_[0]) eq 'HASH') {
        $self->{'Substitution Table'} = $_[0];
    } else {
        my %tmphash = @_;
        @{$self->{'Substitution Table'}}{keys %tmphash} = values %tmphash;
    }

}




=item set_start_format HASH_REF

=item set_start_format LIST

Given either a list of keys/values, or a reference to a hash of keys/values,
copy them into the object's Formats list.

=cut

sub set_start_format {

    my $self = ref($_[0]) ? shift : $Default_Object;
    my %tmphash = ref($_[0]) ? %{$_[0]} : @_;

    $self->{'Formats'}{$_}[0] = $tmphash{$_} foreach(keys %tmphash);

}




=item set_end_format HASH_REF

=item set_end_format LIST

Given either a list of keys/values, or a reference to a hash of keys/values,
copy them into the object's Formats list.

=cut

sub set_end_format {

    my $self = ref($_[0]) ? shift : $Default_Object;
    my %tmphash = ref($_[0]) ? %{$_[0]} : @_;

    $self->{'Formats'}{$_}[1] = $tmphash{$_} foreach(keys %tmphash);

}




=item set_format LIST

Sets the formatting string for one or more formats.

You should pass a list of keys/values where the keys are the format names and the values
are references to arrays containing the starting and ending formatting strings (in that order)
for that format.

=cut

sub set_format {

    my $self = ref($_[0]) ? shift : $Default_Object;
    my %tmphash = ref($_[0]) ? %{$_[0]} : @_;

    foreach(keys %tmphash) {
        @{$self->{'Formats'}{$_}}[0 .. $#{$tmphash{$_}}] = @{$tmphash{$_}}[0 .. $#{$tmphash{$_}}];
    }

}




=item get_start_format LIST

Retrieve the string that is inserted to begin a given format type (starting format string).

The names are looked for in the following order:

B<First:> Prefer the names joined by underscore, from most general to least.  For example, given
("Variable", "Scalar"): "Variable" then "Variable_Scalar".

B<Second:> Then try each name singly, in reverse order.  For example,
"Scalar" then "Variable".

See L<"FORMAT TYPES"> for more information.

=cut

sub get_start_format {

    my $self  = (@_ && ref($_[0]) ? shift : $Default_Object);

    #
    # Prefer the names joined by an underscore from most general to least.
    # For example, the parameters:
    #   'Identifier', 'Variable', 'Scalar'
    # tries 'Identifier' first, then 'Identifier_Variable', and finally 'Identifier_Variable_Scalar'.
    #
    my $format_id = '';
    foreach my $format (@_) {
        $format_id .= '_' if($format_id ne '');
        $format_id .= $format;
        return $self->{'Formats'}{$format_id}[0] if(exists $self->{'Formats'}{$format_id});
    }

    #
    # Otherwise, look for each name singly, in reverse order.
    # In other words, if they say:
    #   'Variable', 'Scalar'
    # look for 'Scalar' first, then 'Variable'.
    #
    foreach my $i (-$#_ .. 0) {
        return $self->{'Formats'}{$_[$i]}[0] if(exists $self->{'Formats'}{$_[$i]});
    }

    #
    # Otherwise, return the DEFAULT.
    #
    return $self->{'Formats'}{'DEFAULT'}[0];

}




=item get_end_format LIST

Retrieve the string that is inserted to end a given format type (ending format string).

=cut

sub get_end_format {

    my $self  = (@_ && ref($_[0]) ? shift : $Default_Object);

    #
    # Prefer the names joined by an underscore from most general to least.
    # For example, the parameters:
    #   'Identifier', 'Variable', 'Scalar'
    # tries 'Identifier' first, then 'Identifier_Variable', and finally 'Identifier_Variable_Scalar'.
    #
    my $format_id = '';
    foreach my $format (@_) {
        $format_id .= '_' if($format_id ne '');
        $format_id .= $format;
        return $self->{'Formats'}{$format_id}[1] if(exists $self->{'Formats'}{$format_id});
    }
    
    #
    # Otherwise, look for each name in reverse order.
    # In other words, if they say:
    #   'Variable', 'Scalar'
    # look for 'Scalar' first, then 'Variable'.
    #
    for my $i (-$#_ .. 0) {
        return $self->{'Formats'}{$_[$i]}[1] if(exists $self->{'Formats'}{$_[$i]});
    }

    #
    # Otherwise, return the DEFAULT.
    #
    return $self->{'Formats'}{'DEFAULT'}[1];

}




=item get_format_names_list

Returns a list of the I<names> of all the Formats defined.

=cut

sub get_format_names_list {

    my $self = (@_ && ref $_[0] ? shift : $Default_Object);

    return keys %{$self->{'Formats'}};

}




=item get_start_format_values_list

Returns a list of the I<values> of all the start Formats defined (in the same order
as the names returned by C<get_format_names_list()>).

=cut

sub get_start_format_values_list {

    my $self = (@_ && ref $_[0] ? shift : $Default_Object);

    return map $$_[0], values %{$self->{'Formats'}};

}




=item get_end_format_values_list

Returns a list of the I<values> of all the end Formats defined (in the same order
as the names returned by C<get_format_names_list()>).

=cut

sub get_end_format_values_list {

    my $self = (@_ && ref $_[0] ? shift : $Default_Object);

    return map $$_[1], values %{$self->{'Formats'}};

}




=item format_string LIST

Formats one or more strings of Perl code.  If no strings are specified, defaults to C<$_>.
Returns the list of formatted strings (or the first string formatted if called in scalar context).

B<Note:>  The end of the string is considered to be the end of a line, regardless of whether
or not there is a trailing line-break (but trailing line-breaks will I<not> cause an extra, empty
line).

B<Another Note:>  The function actually uses C<$/> to determine line-breaks, unless C<$/> is
set to C<\n> (newline).  If C<$/> I<is> C<\n>, then it looks for the first match
of C<m/\r?\n|\n?\r/> in the string and uses that to determine line-breaks.  This is to make
it easy to handle non-unix text.  Whatever characters it ends up using as line-breaks are
preserved.

=cut

sub format_string {

    my $self = (@_ && ref($_[0]) ? shift : $Default_Object);

    @_ or push @_, $_;               # Default to $_ if no strings passed.
    splice(@_, 1) unless(wantarray); # Ignore all but the first string passed if called in scalar context.

    my @results = ();

    foreach (@_) {

        my $string = $_; # Have to do this to avoid "Modification of a read-only value..." errors.

        #
        # We support any funky kind of line terminator but it must be
        # consistant throughout the string (though not between strings).
        #
        # Unless $/ is custom.  Then we use it.
        #
        my $line_sep = ($/ eq "\n" and $string =~ /(\r\n?|\n\r?)/) ? $1 : $/;

        #
        # Don't treat trailing $line_sep's as an extra, blank line.
        # But do save it, because we'll re-add it later.
        #
        my $chomped = ($string =~ s/\Q$line_sep\E$//s)  ?  $line_sep  :  '';

        my @lines = split quotemeta($line_sep), $string, -1;  # Split on $line_sep...

        #
        # Compensate for a "problem" w/ split.  Specifically:
        #
        #   $str = <any non-empty string>;
        #   split('-', $str, -1);           # returns ($str)
        #
        #   $str = '';
        #   split('-',  $str, -1)           # does _not_ return ($str); returns () instead!
        #
        @lines = ('') unless(@lines);  # Replace empty list with list of 1 empty string.

        $_ = $self->_format_line($_) foreach(@lines);         # Format.  Note that this modifies @lines!

        push @results, join($line_sep, @lines) . $chomped;    # And recombine.

    }

    #
    # If we're called in scalar context and not given anything to work with,
    # avoid "uninitialized value" warnings by returning an empty string.
    #
    return (wantarray ? @results : (@results ? $results[0] : ''));

}




=item format_token TOKEN, LIST

Returns TOKEN wrapped in the start and end Formats corresponding to LIST
(as would be returned by C<get_start_format( LIST )> and C<get_end_format( LIST )>,
respectively).

No syntax checking is done on TOKEN but substitutions defined with C<define_substitution()>
are performed.

=cut

sub format_token {

    my $self  = (@_ && ref($_[0]) ? shift : $Default_Object);
    my $token = shift;

    return '' if(not(defined $token) or $token eq '');

    #
    # Do any substitutions that are defined.
    #
    my %tmp_table = %{$self->{'Substitution Table'}};
    my $keys = join('|', map qr/\Q$_\E/, keys %tmp_table);
    $token =~ s/($keys)/$tmp_table{$1}/eg if(defined $keys and $keys ne '');

    #
    # Pointer to the array containing the formats, so we don't have to look them up twice.
    #
    my $ra_formats = undef;

    #
    # Prefer the names joined by an underscore from most general to least.
    # For example, the parameters:
    #   'Identifier', 'Variable', 'Scalar'
    # tries 'Identifier' first, then 'Identifier_Variable', and finally 'Identifier_Variable_Scalar'.
    #
    my $format_id = '';
    foreach my $format (@_) {
        $format_id .= '_' if($format_id ne '');
        $format_id .= $format;
        if(exists $self->{'Formats'}{$format_id}) {
            $ra_formats = $self->{'Formats'}{$format_id};
            last;
        }
    }

    #
    # Otherwise, look for each name singly, in reverse order.
    # In other words, if they say:
    #   'Variable', 'Scalar'
    # look for 'Scalar' first, then 'Variable'.
    #
    unless(defined $ra_formats) {
        foreach my $i (-$#_ .. 0) {
            if(exists $self->{'Formats'}{$_[$i]}) {
                $ra_formats = $self->{'Formats'}{$_[$i]};
                last;
            }
        }
    }

    #
    # Otherwise, if nothing found, use the DEFAULT.
    #
    unless(defined $ra_formats) {
        $ra_formats = $self->{'Formats'}{'DEFAULT'};
    }

    return $ra_formats->[0] . $token . $ra_formats->[1];

}





#===================================================================================================
# Private Methods
#
#   Don't call these, please (unless you really want to).
#===================================================================================================


#
# Define typeglob aliases for internal use (to save typing).
#
{
    no strict 'refs';
    my $pkg = __PACKAGE__;

    *{ "$pkg\::_start" } = \&{ "$pkg\::get_start_format" };
    *{ "$pkg\::_end"   } = \&{ "$pkg\::get_end_format"   };
    *{ "$pkg\::_wrap"  } = \&{ "$pkg\::format_token"     };
}



#
# *The* method.  :-)
#
# Formats a line of Perl code.
#
# Note:
#
#     When I say "line", I mean that the whole string will be treated
#     as a *single line*.  Regardless of embedded newlines.  In fact,
#     embedded newlines will be treated as whitespace and will not affect
#     the parsing of anything, *EVEN COMMENTS*!  Yes, I do realize that
#     this is wrong from Perl's point of view.  Deal.  I might fix that
#     later.  Who knows.  You should be using format_string() anyway.
#
# Another Note:
#
#     This method only processes a *single* line (scalar variable) at a time.
#     Any aditional parameters will be ignored/lost.  Defaults to $_ if no
#     parameters are given.
#

sub _format_line {

    my $self      = (@_ && ref($_[0]) ? shift : $Default_Object);
    my $line      = (@_ ? shift : $_);  # Default to $_.

    my $new_line  = '';  # Buffer variable for holding the processed part of the line.
    


    #
    # Check for special case of recursive call indicating an incorrectly tokenized line-piece.
    # (ie, don't do start-of-line stuff)
    #
    unless($self->{'reentrant'}) {

        #
        # Increase the line-count.
        #
        ++$self->{'line_count'};

        #
        # After parsing __END__ or __DATA__, we do no more formatting.
        #
        # Note:  I just figured out that Here-Documents actually "override"
        #        __END__ and __DATA__.  By "override", I mean that, though you
        #        can specify __END__ or __DATA__ on the same line as another
        #        statement (and it affects anything on the following lines),
        #        if the statements before the __(END|DATA)__ cause perl to expect
        #        a Here-Document, it will process the Here-Document lines before
        #        instating the DATA section.  Consider the following code as an
        #        example (it will print the string "This will be printed\n"):
        #
        #            print << "HEREDOC"; __END__ This is not processed as code but cannot be read from DATA.
        #            This will be printed
        #            HEREDOC
        #            This is data and can be read from DATA.
        #
        #        The formatter will handle such insanity correctly.
        #
        return $self->_wrap($line, 'DATA') if($self->{'in_data'} and not $self->{'in_heredoc'});


        #
        # Do POD stuff.
        #
        if(not $self->{'in_string'} and not $self->{'in_heredoc'} and
          ($line =~ /^=\w+/ and $self->{'last_token'} =~ /^[;\}]?$/)) {

            $self->{'was_pod'} = $self->{'in_pod'} = TRUE;

        }

        if($self->{'in_pod'}) {

            #
            # Check for end of POD (if we're in one).
            #
            $self->{'in_pod'} = FALSE if($line =~ /^=cut/);

            #
            # Format as a comment.
            # Note that even the instigator and terminator get formatted as comment.
            #
            return $self->_wrap($line, 'Comment', 'POD');

        } else {

            $self->{'was_pod'} = FALSE;

        }


        #
        # Check for "here-doc" terminator.  It must be the ONLY thing on the line.
        #
        if($self->{'in_heredoc'} and $line =~ /^$self->{'here_terminator'}[0]$/) {

            #
            # We found one.  Decrement the here-doc level and remove the terminator from the list.
            #
            --$self->{'in_heredoc'};
            shift @{$self->{'here_terminator'}};

            $self->{'last_token'} = $line;
            $self->{'last_token_type'} = 'Quote';

            return $self->_wrap($line, 'Quote'); # Mark here-doc terminators as quotes.

        }


        #
        # Check for "she-bang" line (#!/usr/bin/perl).  Note that it *must* be the first line.
        #
        if($self->{'line_count'} == 1 and $line =~ /^#!/) {
            return $self->_wrap($line, 'Directive');
        }

        #
        # Check for line directives.
        #
        if($line =~ /^(#\s*line\s+\d+(?:\s+(?:"[^"]*"|\S+))?)(.*)$/) {
            return $self->_wrap($1, 'Directive') . $self->_wrap($2, 'Comment', 'Normal');
        }


        #
        # String formatting can continue for multiple lines.
        #
        if(not $self->{'unstable'} and ($self->{'in_string'} or $self->{'in_heredoc'})) {
            $new_line = $self->_start('String');
        }

    } # End of unless($reentrant) (line-based stuff) block.
    else {
        if($self->{'reentrant'} > 20) {
            my $sr = (caller(1))[3]; # Get the subroutine that called us (caller(0) returns us).
            my $ln = (caller(0))[2]; # Get the line that _we_ were called from.

            die "$0: deep recursion ( > 20 levels) caught in "
              . __PACKAGE__ . "::_format_line() called from $sr at line $ln\n";
        }
    }


    #
    # "Optimization" to prevent doing more work on empty strings than we have to.
    #
    # Note: this _must_ be done _after_ the line-based construct
    # checks (the unless($reentrant) stuff).
    #
    return '' if(not(defined $line) or $line eq '');

    #
    # Tokenize the line.
    #
    my @tokens = _tokenize($line);


    #
    # Main loop for processing the tokenized line.
    #
    foreach my $token (@tokens) {

        #
        # Skip empty tokens as a special case.
        #
        next if($token eq '');

        $new_line .= $self->_process_token($token);

    }



    #
    # End of line processing.
    #
    # EOL processing doesn't occur if we're reentrant because that means that
    # we're not actually processing a line (rather a piece of a line).
    #
    if(not $self->{'reentrant'}) {

        #
        # Comments always end at the end of the line.
        #
        if($self->{'in_comment'}) {

            $self->{'in_comment'} = FALSE;
            $new_line .= $self->_end('Comment', 'Normal') unless($self->{'unstable'});

        }

        #
        # Strings run to the end of line but the formats should never run *off* the end of line.
        # Instablility of formatting makes it pointless to start and end a line-wide format.
        #
        if(not $self->{'unstable'} and ($self->{'in_string'} or $self->{'in_heredoc'})) {
            $new_line .= $self->_end('String');
        }

        #
        # Setting $self->{'in_heredoc'} is postponed until end of line because the here-doc doesn't
        # actually start until the next line.  Due to the possibility of "nested" here-doc's,
        # we set $self->{'in_heredoc'} to the number of terminators we've gotten.
        #
        # For example, the code:
        #
        #   print << "END1", "****\n", << "END2";
        #
        # Creates two terminators ('END1' and 'END2').
        #
        $self->{'in_heredoc'} = @{$self->{'here_terminator'}};

    }

    #
    # Return the formatted line.
    #
    return $new_line;

} # End of _format_line()



#
# This _private_ function takes a line passed to it and breaks it into usefull pieces.
# I was using a single regex to do this but it didn't handle precedence properly.
#
sub _tokenize {

    my @tokens = ();

    local $_ = shift;   # Line to tokenize.

    #
    # Loop through the line, breaking off tokens as we go.  The tokens we look for
    # (in order of precedence) are:
    #
    #    Comments
    #    Quotes and quote-like constructs
    #    Here-Doc initiators
    #    Subroutine calls
    #    Variable identifiers
    #    Whitespace
    #    Special (backslashed, escaped) characters
    #    Keywords and builtin functions
    #    Operators
    #    Barewords
    #    Everything else (symbols)
    #
    LOOP: {
        if(m/\G($quotes_re)/gc                                                      or  # Quote Instigator
           m/\G($heredoc_re)/gc                                                     or  # Here-Doc Instigator
           m/\G(__END__|__DATA__)/gc                                                or  # Code Terminators
           m/\G($keyword_list_re)(?!\w|::)/gc                                       or  # Keywords
           m/\G($builtin_list_re(?!\w|::)(\s*\()?)/gc                               or  # Built-in Functions
           m/\G((?:\\\&|\&)$varchars?\s*(?:$identifier_re|\{))/gc                   or  # Subroutine: &ident
           m/\G((?!$operator_list_re\s*\()$identifier_re\s*\()/gc                   or  # Subroutine: ident(
           m/\G(\\?$varchars(?:$identifier_re(?:\s*[\[\{])?|\s*\{))/gc              or  # Variable
           m/\G(\\?(?:$varchars(?![@%]))?$builtin_vars_re(?:\s*[\[\{])?)/gc         or  # Built-in Variables
           m/\G(\s+)/gc                                                             or  # Whitespace
           m/\G(\\.)/gc                                                             or  # Character
           m/\G($identifier_re(\s*(=>|:(?!:)))?)/gc                                 or  # Bareword / Label
           m/\G($number_re)/gc                                                      or  # Numbers
           m/\G($operator_list_re)/gc                                               or  # Operator
           m/\G(.)/gc                                                                   # Symbol
        ) {
            push @tokens, $1;
            redo LOOP;
        };
    };

    return @tokens;

}



#
# This _private_ function tests a token against any or all of the available Formats,
# and formats it accordingly.  It also handles initiating and terminating quoted constructs.
#
sub _process_token {
    my $self  = (ref($_[0]) ? shift : $Default_Object);
    my $token = shift;


    #**********************************************************
    # Preemptive tests.
    #**********************************************************


    #
    # After parsing __END__ or __DATA__, we no longer format tokens
    # (er, rather, we format them as DATA).  Even ones on the same
    # line as the __END__ or __DATA__.
    #
    # This block only handles tokens on the same line as the __END/DATA__.
    # See _format_line() for the code that handles the lines following the __END/DATA__ line.
    #
    if($self->{'in_data'} and not $self->{'in_heredoc'}) {
        return $self->_wrap($token, 'DATA');
    }


    #
    # The only things we will allow between a class specifier and it's identifier are:
    #   A comment
    #   Whitespace
    #
    if($self->{'awaiting_variable'}) {
        unless($token =~ /^$identifier_re/ or $self->{'in_comment'} or $token eq '#' or $token =~ /^\s+$/) {
            $self->{'awaiting_variable'} = FALSE;
        }
    }



    #**********************************************************
    # Normal token tests.
    #**********************************************************


    #
    # Test for special character.
    #
    if($token =~ /^(\\.)(.+)?$/) {

        my $preamble  = $1;
        my $postamble = defined($2) ? $2 : '';

        #
        # We only do special characters when we're inside an
        # interpolated string or a here-doc, or the character
        # is super-special (meaning either a double back-slash,
        # or a back-slash escaped quote-terminator).
        #
        unless(
            (
                $self->{'in_string'} and (
                    $self->{'quote_type'}  =~  /Interpolated/
                 or $token                 eq  "\\\\"
                 or $token                 =~  /\\\Q$self->{'quote_terminator'}\E/
                ) 
            )
         or $self->{'in_heredoc'}
        ) {
            $postamble =  substr($preamble, 1, 1) . $postamble;  # The character (plus other stuff)
            $preamble  =  substr($preamble, 0, 1);               # The backslash

            ++$self->{'reentrant'};
            $preamble  = $self->_format_line($preamble);
            $postamble = $self->_format_line($postamble);
            --$self->{'reentrant'};

            return $preamble . $postamble;
        }

        $self->{'last_token'} = $preamble;
        $self->{'last_token_type'} = 'Character';

        if(defined $postamble) {
            ++$self->{'reentrant'};
            $postamble = $self->_format_line($postamble);
            --$self->{'reentrant'};
        }

        return $self->_wrap($preamble, 'Character') . $postamble;

    }



    #
    # Test for comment.
    #
    if(not($self->{'in_string'} or $self->{'in_heredoc'}) and ($token eq '#' or $self->{'in_comment'})) {

        #
        # Start a new comment.
        #
        if($token eq '#' and not $self->{'in_comment'}) {
            $self->{'in_comment'} = TRUE;
            if($self->{'unstable'}) {
                return $self->_wrap($token, 'Comment', 'Normal')
            } else {
                return $self->_start('Comment', 'Normal') . $self->_wrap($token, '')
            }
            return (
                $self->{'unstable'}
                    ?
                $self->_wrap($token, 'Comment', 'Normal')
                    :
                $self->_start('Comment', 'Normal') . $self->_wrap($token, '')
            );
        }

        return (
            $self->{'unstable'}
                ?
            $self->_wrap($token, 'Comment', 'Normal')
                :
            $self->_wrap($token, '')
        );
    }


    #
    # Test for (DATA Instigators) special tokens that signal the end of executable code.
    # We will format nothing after we encounter one of these.
    #
    if(not($self->{'in_string'} or $self->{'in_heredoc'}) and ($token eq '__END__' or $token eq '__DATA__')) {

        $self->{'in_data'} = TRUE;

        return $self->_wrap($token, 'CodeTerm');

    }



    #
    #  Quote-related tests.
    #

    #
    # Check for nested quotes.
    #
    # If our quotes are bracketing and we encounter another Instigator inside
    # the string, we require a matching terminator before we end the string.
    #
    # For example:
    #
    #   qq{ Because of this -> { the string does not end here -> }, it ends here -> }
    #
    if($self->{'in_string'} and
       $token !~ /\Q$self->{'quote_terminator'}\E/ and
       $token =~ /\Q$self->{'quote_instigator'}\E/
    ) {

        #
        # Token matches the quote instigator but not the terminator so it must be
        # a bracketing type of quote.  We need to increase the count of nested
        # quotes.  We do not do anything else w/ the token, just let it be processed
        # by subsequent code.
        #
        ++$self->{'nested_quote'};

    }
    
    #
    # Check for second part of Multipart quoted construct (if we're waiting for one).
    #
    # Comments (handled above) and whitespace are ignored between parts of a Multipart construct.
    #
    if($self->{'awaiting_multi'} and $token =~ /\S/) {
    
        #
        # Anything other than a comment or whitespace causes us to stop waiting
        # for the second part.  (It's technically an error.)
        #
        $self->{'awaiting_multi'} = FALSE;

        if($token =~ /\Q$self->{'quote_instigator'}\E/) {

            #
            # We have found the second part!
            #

            $self->{'last_token'} = $token;
            $self->{'last_token_type'} = 'Quote';

            $self->{'found_multi'} = TRUE;
            $self->{'in_string'}   = TRUE;

            return
                $self->_wrap($token, 'Quote')
              . ($self->{'unstable'} ? '' : $self->_start('String'));

        }

    }

    #
    # Check for options to an optioned quoted construct.
    #
    if($self->{'awaiting_options'}) {

        $self->{'awaiting_options'} = FALSE;

        if($token =~ /^([cgimosexd]+|s.*)$/) {

            my $postamble = '';

            $self->{'last_token'} = $token;
            $self->{'last_token_type'} = 'Quote';

            #
            # Check for lone `s' option incorrectly tokenized as a s/// instigator
            # and recursively re-process it if necessary.
            #
            if($token =~ /^(s)([^cgimoxed]*)$/) {
                $token = $1;
                $self->{'last_token'} = $token;

                ++$self->{'reentrant'};
                $postamble = $self->_format_line($2);
                --$self->{'reentrant'};
            }

            return $self->_wrap($token, 'Quote') . $postamble;

        }

    }



    #
    # Test for quote instigator (string-start).
    #
    # Note:  The forward-slash and question mark characters (/ and ?) as quote instigators are
    #        pretty difficult to disambiguate and if it becomes a problem, I may just leave them
    #        out.  Though I'd rather not.
    #
    #        The strategy we use here is to only make them operators (division or ternary conditional,
    #        respectively) if they immediately follow a closing bracket-type-thingy ([\)\]\}]),
    #        quote, variable, literal number, bareword, or a builtin or user function other than
    #        print, split, grep, or map.
    #
    #        Note that the function/subroutine situation is still ambiguous: some functions should
    #        cause them to start a quote and some should cause them to be operators, depending on
    #        the effective prototype of the function (eg: sin vs. time).
    #
    #        There are also some other ambiguities, as well.  Such as when they follow certain types
    #        of symbols or operators.
    #
    #        See http://perlmonks.org/index.pl?node=On+Parsing+Perl for more information.
    #
    if((not $self->{'in_string'}) and (not $self->{'in_heredoc'}) and ($token =~ /^$quotes_re/ or
      ($token =~ m{^[/?]} and
       $self->{'last_token'}      !~ /[\)\]\}]$/  and  # Closing bracket-type-thingy. Might add > later.
       $self->{'last_token_type'} ne 'Quote'      and
       $self->{'last_token_type'} ne 'Variable'   and
       $self->{'last_token_type'} ne 'Number'     and
       $self->{'last_token_type'} ne 'Bareword'   and
       $self->{'last_token_type'} ne 'Subroutine' and
      ($self->{'last_token_type'} ne 'Builtin'    or
       $self->{'last_token'}      eq 'print'      or
       $self->{'last_token'}      eq 'split'      or
       $self->{'last_token'}      eq 'grep'       or
       $self->{'last_token'}      eq 'map')))) {

        #
        # Create a copy of $token so we can modify it with impunity.
        # (Specifically for splitting up of '/='-like constructs (see below).)
        #
        my $quote = $token;

        #
        # In certain contexts (see above), the token '/=' or similar will be considered
        # the start of a pattern-match quote.  We have to split off the extra and process
        # it as part of the string so we store it in $postamble and re-process it later
        # (see the return at the bottom of this block).
        #
        my $postamble = '';
        if($quote =~ m{^([/?])(.+)$}) {

            #
            # Store the split-off quote instigator (/) and the postamble.
            #
            $quote     = $1;
            $postamble = $2;

        }

        #
        # The quote-instigator is the last character of the token.
        #
        $self->{'quote_instigator'} = substr $quote, -1;

        #
        # Define special terminators for bracketing characters.
        # Otherwise, use the same character as the instigator.
        #
        $self->{'quote_terminator'} =
            { '{' => '}',
              '(' => ')',
              '[' => ']',
              '<' => '>',
            }->{$self->{'quote_instigator'}} || $self->{'quote_instigator'};

        #
        # The type will be matched later on like so:
        #
        #   if($self->{'quote_type'} =~ /Interpolated/) {  # Is it interpolated?
        #
        # Note:  Some of the quoted constructs are interpolated or non-interpolated
        #        depending on the instigator/terminator character used.  Specifically,
        #        if a single quote is used as the delimter for qx, m, qr, and s, they
        #        are *not* interpolated.  Otherwise, they *are* interpolated.
        #
        $self->{'quote_type'} = (
            ($quote =~ /'|q[^qxwr]/    &&  'Literal')                                           or

            ($quote =~ /"|qq/          &&  'Literal, Interpolated')                             or

            ($quote =~ /`|qx/          &&  'Command' .
                                           ($self->{'quote_instigator'} eq q(') ? '' : ', Interpolated'))
                                                                                                or
            ($quote =~ /qw/            &&  'Word list')                                         or

            ($quote =~ /m|^[\/?]/      &&  'Pattern, Matching, Optioned' .
                                           ($self->{'quote_instigator'} eq q(') ? '' : ', Interpolated'))
                                                                                                or
            ($quote =~ /qr/            &&  'Pattern, Optioned' .
                                           ($self->{'quote_instigator'} eq q(') ? '' : ', Interpolated'))
                                                                                                or
            ($quote =~ /s/             &&  'Pattern, Substitution, Optioned, Multipart' .
                                           ($self->{'quote_instigator'} eq q(') ? '' : ', Interpolated'))
                                                                                                or
            ($quote =~ /tr|y/          &&  'Transliteration, Optioned, Multipart')              or

            'Error: Quote-Type Not Found'
        );

        $self->{'in_string'} = TRUE;

        $self->{'last_token'} = $quote;
        $self->{'last_token_type'} = 'Quote';

        #
        # Post-process the $postamble if necessary.
        #
        if($postamble ne '') {

            ++$self->{'reentrant'};
            $postamble = $self->_format_line($postamble);
            --$self->{'reentrant'};

        }

        return
            $self->_wrap($quote, 'Quote')
          . $self->_start('String')
          . $postamble
          . ($self->{'unstable'} ? $self->_end('String') : '');

    } # End test for Quote Instigator


    #
    # Check for Quote Terminator.  Here-doc terminators are line-based
    # constructs, and thus are checked in _format_line().
    #
    if($self->{'in_string'} and
       $token =~ /^(.*?)(?<!\\)(\Q$self->{'quote_terminator'}\E)(.*)$/
    ) {{ # Note double braces.  That's so we can use last (just below) to exit the if.

        #
        # If our quotes are bracketing and we've encountered another Instigator inside
        # the string, we require a matching terminator before we end the string.
        #
        # For example:
        #
        #   qq{ Because of this -> { the string does not end here -> }, it ends here -> }
        #
        if($self->{'nested_quote'} > 0) {
            --$self->{'nested_quote'};
            last;  # Exit enclosing if block (with double braces, not if nested_quote)
        }

        #
        # This block terminates (possibly just the first part of) a quoted structure.
        #

        $self->{'last_token'} = $token;
        $self->{'last_token_type'} = 'Quote';

        #
        # Note on $preamble and $postamble:
        #
        #     There might be extra "stuff" attached to the beginning or end of
        #     our quote terminator that should be formatted separately.  This
        #     is due to the fact that the tokenizer doesn't know when it's tokenizing
        #     that it should be breaking things up based on quote teriminators.
        #
        #     For example, the string '"$"' tokenizes as:
        #
        #         '"' (quote-start), '$"' (built-in variable)
        #
        #     It should actually be parsed:
        #
        #         '"' (quote-start), '$' (error in Perl), '"' (quote-end)
        #
        #     Another example; the string 's sjsjs' tokenizes as:
        #
        #         's s' (substitution-start), 'jsjs' (bareword)
        #
        #     Instead of:
        #
        #         's s' (sub.-start), 'j' (string), 's' (sub.-end 1), 'j' (string), 's' (sub.-end)
        #
        my $preamble   = $1;
        my $terminator = $2;
        my $new_string = '';  # May or may not be set; depends on if we find the second part of a multi-part.
        my $postamble  = $3;

        ++$self->{'reentrant'};
        $preamble = $self->_format_line($preamble);  # Must process preamble before doing multi-part logic.
        --$self->{'reentrant'};                      # Otherwise, we wouldn't know if it should be in_string.
        
        #
        # Handle multipart quotes.
        #
        if($self->{'found_multi'}) {

            #
            # If we've already found Multipart, we shouldn't look for it.
            #
            $self->{'found_multi'}      = FALSE;
            $self->{'in_string'}        = FALSE;
            $self->{'awaiting_options'} = TRUE if($self->{'quote_type'} =~ /Optioned/);

        } elsif($self->{'quote_type'} =~ /Multipart/) {

            #
            # If we haven't already found the Multipart, we should check for it immediately.
            #

            if($self->{'quote_instigator'} eq $self->{'quote_terminator'}) {

                #
                # We found it.
                #

                $self->{'found_multi'} = TRUE;
                $self->{'in_string'}   = TRUE;

                $new_string = $self->_start('String'); # Unstable logic done below (when this is used).

            } else {

                #
                # We didn't find it so we start waiting.
                #

                $self->{'awaiting_multi'} = TRUE;
                $self->{'in_string'}      = FALSE;

            }

        } else {

            #
            # This quoted construct isn't Multiparted so we just end it.
            #

            $self->{'in_string'}        = FALSE;
            $self->{'awaiting_options'} = TRUE if($self->{'quote_type'} =~ /Optioned/);
        }

        #
        # Do the formatting and re-processing.
        #
        my $formatted_token;
        $formatted_token .= $preamble;
        $formatted_token .= $self->_end('String')                unless($self->{'unstable'});
        $formatted_token .= $self->_wrap($terminator, 'Quote');
        $formatted_token .= $new_string                          unless($self->{'unstable'});
        #
        # Reprocess $postamble now that multi-part logic has been done.
        #
        ++$self->{'reentrant'};
        $formatted_token .= $self->_format_line($postamble);
        --$self->{'reentrant'};

        return $formatted_token;

    }}  # End check for Quote Terminator.  Note double braces.


    #
    # Test for Here-Doc (type of quote).
    #
    # Note:  This maybe should be combined with the test
    # for Quote Instigator or possibly Operator.  Maybe.
    #
    if(not($self->{'in_string'} or $self->{'in_heredoc'}) and $token =~ /^$heredoc_re$/) {

        #
        # Here-Document instigators are ambiguous in some situations.  The
        # rule used here is that it's considered a Here-Document instigator
        # unless it follows a Variable, Bareword, or Quote (string).  If it
        # does follow one of those, then it's probably a binary left shift
        # operator and the token should be reprocessed.
        #
        # This rule doesn't cover every situation (functions with certain
        # prototypes should also cause it to be considered a shift operator,
        # and there's probably other situtations) but it should cover the
        # most common ones.
        #
        if($self->{'last_token_type'} =~ /Quote|Bareword|Variable/) {

            my ($preamble, $postamble) = $token =~ /^(<<)(.*)$/;

            $preamble  = $self->_wrap($preamble, 'Operator');

            $self->{'last_token'} = $token;
            $self->{'last_token_type'} = 'Quote';

            ++$self->{'reentrant'};
            $postamble = $self->_format_line($postamble);
            --$self->{'reentrant'};

            return $preamble . $postamble;

        }

        $self->{'last_token'} = $token;
        $self->{'last_token_type'} = 'Quote';

        #
        # Push only the word part of the here-doc terminator (not any optional quotes) onto the list.
        # Note that this word part *can* be an empty string.
        #
        my ($term) = $token =~ /(\w+)/;
        $term = '' unless(defined $term);
        push @{$self->{'here_terminator'}}, $term;

        $self->{'quote_type'} = 'Here-Document, Interpolated';

        return $self->_wrap($token, 'Quote');

    }

    #
    #  End of quote-related tests.
    #


    #
    # Test for keyword.
    #
    if(not($self->{'in_string'} or $self->{'in_heredoc'}) and $token =~ /^(?:$keyword_list_re)$/) {

        $self->{'last_token'} = $token;
        $self->{'last_token_type'} = 'Keyword';

        #
        # Test for formats.  Treat them like here-docs (they terminate w/ line =~ /^.$/).
        #
        if($token eq 'format') {
            push @{$self->{'here_terminator'}}, '.';
        }

        return $self->_wrap($token, 'Keyword');

    }



    #
    # Test for builtin functions.
    #
    if(not($self->{'in_string'} or $self->{'in_heredoc'}) and $token =~ /^($builtin_list_re)(\s*)(\(?)$/ and
       $self->{'last_token'} ne 'sub' and $self->{'last_token'} ne '->') {

        $self->{'last_token'} = $token;

        if($3) {
            $self->{'last_token_type'} = 'Symbol';
        } else {
            $self->{'last_token_type'} = 'Builtin';
        }

        return
            $self->_wrap($1, 'Builtin', ($3 ? 'Function' : 'Operator')) .
            $self->_wrap($2, 'Whitespace') .
            $self->_wrap($3, 'Symbol');

    }



    #
    # Test for subroutine.
    #
    if(not($self->{'in_string'} or $self->{'in_heredoc'}) and
      ($token =~ /^(\&$varchars?)(\s*)($identifier_re)?()((?(4)|\{))$/ or
       $token =~ /^()()($identifier_re)(\s*)(\()$/ or
      ($token =~ /^()()($identifier_re)()()$/ and
      ($self->{'last_token'} eq 'sub' or $self->{'last_token'} eq '->')))) {

        my $ampersand   = $1 || '';
        my $whitespace1 = $2 || '';
        my $identifier  = $3 || '';
        my $whitespace2 = $4 || '';
        my $parenthesis = $5 || '';

        #
        # Check for package imports with parameters (they accidentally get tokenized
        # as subroutine calls.
        #
        if($self->{'last_token'} =~ /^(require|use|no)$/ and $ampersand eq '') {

            $self->{'last_token'} = $token;

            if($parenthesis) {
                $self->{'last_token_type'} = 'Symbol';
            } else {
                $self->{'last_token_type'} = 'Subroutine';
            }

            return
                $self->_wrap($identifier,  'Package')
              . $self->_wrap($whitespace2, 'Whitespace')
              . $self->_wrap($parenthesis, 'Symbol');

        }

        $self->{'last_token'} = $token;

        if($parenthesis) {
            $self->{'last_token_type'} = 'Symbol';
        } else {
            $self->{'last_token_type'} = 'Subroutine';
        }

        return (
            (
                $self->{'unstable'}
                    ?
                $self->_wrap($ampersand,   'Subroutine')
              . $self->_wrap($whitespace1, 'Whitespace')
              . $self->_wrap($identifier,  'Subroutine')
                    :
                $self->_wrap($ampersand
              . $self->_wrap($whitespace1, 'Whitespace')
              .              $identifier,  'Subroutine')  # _wrap($amp . _wrap($white1, 'WS') . $ident, 'SR')
            )
          . $self->_wrap($whitespace2, 'Whitespace')
          . $self->_wrap($parenthesis, 'Symbol')
        );

    } # End tests for subroutine.


    #
    # Test for variable.
    #
    # Note in the conditional for built-in variables (1st line):
    #   First we test for match, then, still in the conditional, we split it into the needed
    #   positional variables ($[1-6]) with the next match (3rd line).  Once the token matches
    #   the first pattern (1st line), it should *always* match the second (2nd line) as well.
    #
    if(($token =~ /^$varchars?$builtin_vars_re$/ and
        $token =~ /^($varchars)()(.+?)()([\[\{]?)$/) or # Split after match built-in variables.
        $token =~ /^($varchars)(\s*)($identifier_re)(\s*)([\[\{]?)$/ or
        $token =~ /^($varchars)(\s*)(\{)$/ or
        $token =~ /^([\$\@\%\*])$/ or   # Extra leading empty parens in regex to shift positional variables
       ($self->{'awaiting_variable'} and $token =~ /^()()($identifier_re)$/) ) {{ # Note double braces.

        #
        # The braces for this if() statement are doubled so that we can use the 'last'
        # loop control statement to break out of the entire conditional block in the
        # first conditional block below (the test for partial variables within Patterns).
        #
        
        my $class               = '';
        my $class_specifier     = $1 || '';
        my $whitespace1         = $2 || '';
        my $identifier          = (defined $3 ? $3 : ''); # Special case.  ident. might be 0 ($0)
        my $whitespace2         = $4 || '';
        my $subscript           = $5 || '';
        my $postamble           = '';

        #
        # Don't format variables inside of non-interpolated strings.
        # Remember to do this _after_ using the positional variables!
        #
        last if($self->{'in_string'} and $self->{'quote_type'} !~ /Interpolated/);

        #
        # We don't format partial variables (ie, ones without identifiers) inside
        # of Regex (Pattern) strings.
        #
        if($identifier eq '' and $self->{'in_string'} and $self->{'quote_type'} =~ /Pattern/) {

            #
            # This 'last' only works because of the doubled braces around the larger conditional
            # block (if($token =~ ...) (see above).  This trick is explained in the Perl syntax
            # documentation (perlsyn).  See `man perlsyn` under the Loop Control section for
            # more information.
            #

            last;

        }
        

        #
        # Hashes and typeglobs are not interpolated into strings
        # or here-documents, so we need to strip any leading hash
        # hash symbols (%) or typeglob symbols (*) and not consider
        # the token a variable.
        #
        if(($self->{'in_string'} or $self->{'in_heredoc'}) and $class_specifier =~ /^([\%\*]+)/) {

            my $preamble = $1;

            #
            # If the only thing there is the hash/typeglob character, just return.
            #
            if(length $token == length $preamble) {
                return (
                    $self->{'unstable'}
                        ?
                    $self->_wrap($token, 'String')  # Reinstate string formatting if unstable.
                        :
                    $self->_wrap($token, '')        # Format as '' to ensure substitutions.
                );
            }

            #
            # Reinstate string formatting if we're unstable.
            #
            $preamble = $self->_wrap($preamble, $self->{'unstable'} ? 'String' : '');

            #
            # Otherwise, strip off the leading symbol and reprocess the rest (via recursive call).
            #
            ++$self->{'reentrant'};
            my $result = $preamble . $self->_format_line(substr($token,1));
            --$self->{'reentrant'};

            return $result;

        }

        if($self->{'awaiting_variable'}) {

            $self->{'awaiting_variable'} = FALSE;

            # Adjust the class for a subscript or use the class determined before.
            $class = (
                $subscript
                    ?
                {'[' => 'Array', '{' => 'Hash'}->{$subscript}
                    :
                $self->{'awaiting_class'}
            );

            ++$self->{'reentrant'};
            my $return_val =
                $self->_wrap($whitespace1, 'Whitespace') 
              . $self->_wrap($class_specifier . $identifier, 'Variable', $class) 
              . $self->_wrap($whitespace2, 'Whitespace') 
              . $self->_format_line($subscript); # Reprocess it, in case we're in a string.
            --$self->{'reentrant'};

            $self->{'last_token'} = $token;
            $self->{'last_token_type'} = 'Variable';

            return $return_val;

        } elsif($class_specifier) {

            # Adjust the class for a subscript or use the class specifier.
            $class = (
                $subscript ?
                    {'[' => 'Array', '{' => 'Hash'}->{$subscript}
                        :
                    {'$' => 'Scalar',
                     '@' => 'Array',
                     '%' => 'Hash',
                     '*' => 'Typeglob',
                    }->{substr($class_specifier,0,1)}
            );

            $class = 'INVALID CLASS' unless(defined $class);

            #
            # The special case of $identifier eq '{' (as in $token eq '${' for '${foo}')
            # has to be handled.  In case we're inside a string (we want symbols formatted
            # consistantly, even if it's consistantly wrong), we need to recursively
            # re-process the identifier.
            #
            # The other special case of $identifier eq '' means that we should start waiting
            # for an identifier on a following line.
            #
            if($identifier eq '{') {

                ++$self->{'reentrant'};
                $postamble  = $self->_format_line($identifier);
                --$self->{'reentrant'};

                $identifier = '';

            } elsif($identifier eq '') {

                $self->{'awaiting_variable'} = TRUE;
                $self->{'awaiting_class'} = $class;

            }

            ++$self->{'reentrant'};
            my $return_val =
                $self->_wrap($whitespace1, 'Whitespace')
              . $self->_wrap($class_specifier . $identifier, 'Variable', $class)
              . $self->_wrap($whitespace2, 'Whitespace')
              . $self->_format_line($subscript)  # Reprocess in case we're in a string.
              . $postamble;
            --$self->{'reentrant'};

            $self->{'last_token'} = $token;
            $self->{'last_token_type'} = 'Variable';

            return $return_val;

        }

        #
        # If we're not awaiting a variable identifier and there's no class specifier
        # ($, @, or %), then it's probably actually a bareword or keyword or something.
        #
        # So we just let this if() statement fall through.
        #

    }} # End test for Variable.  Note double braces.



    #
    # Test for whitespace.
    #
    if(not($self->{'in_string'} or $self->{'in_heredoc'}) and $token =~ /^\s+$/) {
        return $self->_wrap($token, 'Whitespace');
    }



    #
    # Test for operator.
    #
    if(not($self->{'in_string'} or $self->{'in_heredoc'}) and $token =~ /^(?:$operator_list_re)$/) {

        $self->{'last_token'} = $token;
        $self->{'last_token_type'} = 'Operator';

        return $self->_wrap($token, 'Operator');

    }



    #
    # Test for number.
    #
    if(not($self->{'in_string'} or $self->{'in_heredoc'}) and $token =~ /^$number_re$/) {

        $self->{'last_token'} = $token;
        $self->{'last_token_type'} = 'Number';

        return $self->_wrap($token, 'Number');

    }


    #
    # Test for bareword (or package name, which is a type of bareword).
    #
    if(not($self->{'in_string'} or $self->{'in_heredoc'}) and
       $token =~ /^($identifier_re)((\s*)(=>|:(?!:)))?$/) {

        #
        # If $token =~ /BAREWORD\s*=>/, then BAREWORD should be formatted as a string.
        #
        # If $token =~ /BAREWORD\s*:/, then it's actually a label (elsif block).
        #
        if(defined $2 and substr($2, -1) ne ':') {  # It's a string.

            $self->{'last_token'} = $token;
            $self->{'last_token_type'} = 'String';

            ++$self->{'reentrant'};
            my $result = $self->_wrap($1, 'String') . $self->_format_line($2);
            --$self->{'reentrant'};

            return $result;

        } elsif(defined $2) {  # It's a label.

            $self->{'last_token'} = $token;
            $self->{'last_token_type'} = 'String';

            return $self->_wrap($1, 'Label') . $self->_wrap($3, 'Whitespace') . $self->_wrap($4, 'Label');

        }

        if($self->{'last_token'} =~ /^(require|package|use|no)$/) {

            #
            # This bareword is actually a package name.
            #
            $self->{'last_token'} = $token;
            $self->{'last_token_type'} = 'Package';
            return $self->_wrap($token, 'Package');

        } elsif(substr($self->{'last_token'}, -1) eq '{' and $self->{'last_token_type'} eq 'Variable') {

            #
            # This bareword is the subscript to a hash and should be formatted as a string.
            #
            $self->{'last_token'} = $token;
            $self->{'last_token_type'} = 'String';
            return $self->_wrap($token, 'String');

        } else {

            $self->{'last_token'} = $token;
            $self->{'last_token_type'} = 'Bareword';
            return $self->_wrap($token, 'Bareword');

        }

    }


    #
    # Test for symbol.
    #
    if(not($self->{'in_string'} or $self->{'in_heredoc'})) {

        #
        # The pound symbol (#) is special in that it starts a normal comment.
        #
        if($token eq '#') {

            $self->{'last_token'} = $token;
            $self->{'last_token_type'} = 'Comment';

            $self->{'in_comment'} = TRUE;

            return (
                $self->{'unstable'}
                    ?
                $self->_wrap($token, 'Comment', 'Normal')
                    :
                $self->_start('Comment', 'Normal') . $self->_wrap($token, '')
            );

        }

        $self->{'last_token'} = $token;
        $self->{'last_token_type'} = 'Symbol';

        return $self->_wrap($token, 'Symbol');

    }


    #
    # No matches were made so we just return the token (reinstating
    # string formatting if formatting is unstable).
    #
    return (
        (($self->{'in_string'} or $self->{'in_heredoc'}) and $self->{'unstable'})
            ?
        $self->_wrap($token, 'String')  # If we're unstable, we need to reinstate string formatting.
            :
        $self->_wrap($token, '')        # Format as '' to ensure substitutions; this causes no markup.
    );

} # End _process_token()




TRUE;  # Return true to indicate success.

=pod

=back

=head1 KNOWN ISSUES or LIMITATIONS

=over 4

=item *

Barewords used as keys to a hash are formatted as strings.  This is Good.  They should
not be, however, if they are not the only thing within the curly braces.  That can be
fixed.

=item *

This version does not handle formats (see L<perlform(1)|perlform/1>) very well.  It treats them as
Here-Documents and ignores the rules for comment lines, as well as the fact that picture
lines are not supposed to be interpolated.  Thus, your picture lines will look strange
with the '@'s being formatted as array variables (albeit, invalid ones).
Ideally, it would also treat value lines as normal Perl code and format accordingly.
I think I'll get to the comment lines and non-interpolating picture lines first.
If/When I do get this fixed, I will most likely add a format type of 'Format' or something,
so that they can be formatted differently, if so desired.

=item *

This version does not handle Regular Expression significant characters.  It simply treats
Regular Expressions as interpolated strings.

=item *

User-defined subroutines, called without parentheses, are formatted as barewords.
This is because there is no way to tell them apart from barewords without parsing
the code, and would require us to go as far as perl does when doing the C<-c> check
(ie, executing BEGIN and END blocks and the like).  That's not going to happen.

=item *

If you are indexing (subscripting) an array or hash, the formatter tries to figure
out the "real" variable class by looking at how you index the variable.  However, if
you do something funky (but legal in Perl) and put line-breaks or comments between
the variable class character ($) and your identifier, the formatter will get confused
and treat your variable as a scalar.  Until it finds the index character.  Then it
will format the scalar class character ($) as a scalar and your identifier as
the "correct" class.

=item *

If you put a line-break between your variable identifier and it's indexing character (see
above), which is also legal in Perl, the formatter will never find it and treat your
variable as a scalar.

=item *

If you put a line-break between a bareword hash-subscript and the hash variable, or between
a bareword and its associated C<=E<gt>> operator, the bareword will not be formatted correctly
(as a string).  I<(Noticing a pattern here?)>

=back

=head1 AUTHOR

Cory Johns B<darkness@yossman.net>

Copyright (c) 2001 Cory Johns.  This library is free software; you can
redistribute and/or modify it under the same conditions as Perl itself.

=head1 TO DO

=over 4

=item 1

Improve handling of regular expressions.  Add support for regexp-special characters.
Recognize the /e option to the substitution operator (maybe).

=item 2

Improve handling of formats.  Don't treat format definitions as interpolating.  Handle
format-comments.  Possibly format value lines as normal Perl code.

=item 3

Create in-memory deep-copy routine to replace C<eval(Data::Dumper)> deep-copy.

=item 4

Generalize state transitions (C<reset()> and, in the future, C<copy_state()>) to use
non-hard-coded keys and values for state variables.  Probably will extrapolate them into an
overloadable hash, and use the aforementioned deep-copy to assign them.

=item 5

Create a method to save or copy states between objects (C<copy_state()>).  Would be useful for
using this module in an editor.

=item 6

Add support for greater-than-one length special characters.  Specifically, octal,
hexidecimal, and control character codes.  For example, C<\644>, C<\x1a4> or C<\c[>.

=back

=head1 REVISIONS

=head2 04-04-2001  Cory Johns

=over 4

=item *

Fixed problem with special characters not formatting inside of Here-Documents.

=item *

Fixed bug causing hash variables to format inside of Here-Documents.

=back

=head2 03-30-2001  Cory Johns

=over 4

=item *

Fixed bug where quote-terminators were checked for inside of Here-Documents.

=back

=head2 03-29-2001  Cory Johns

=over 4

=item *

Moved token processing tests from _format_line() into _process_token()
(where they should've been all along), generally making _format_line()
more logical.  Contemplating extrapolating the tokenizing and token
loop into its own subroutine to avoid all the recursive calls.

=item *

Fixed bug that caused special characters to be recognized outside of
strings.

=item *

Added $VERSION variable.

=item *

Added support for different types of literal numbers: floating point,
exponential notation (eg: 1.3e10), hexidecimal, and underscore-separated.

=item *

Added the C<CodeTerm> and C<DATA> Formats.

=back

=head2 03-27-2001  Cory Johns

=over 4

=item *

Added was_pod() and updated the documentation for in_pod().

=back

=head2 03-20-2001  Cory Johns

=over 4

=item *

Added support for Perl formats (ie, `C<format = ...>').

=back
