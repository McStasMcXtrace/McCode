Tk804.028_501 should compile out-of-the box with Strawberry Perl
5.8.8.3 and 5.10.0.3.

Older stuff:

Previous Tk versions do not compile under Windows Vista, possibly
because of file permission problems.

Strawberry Perl's default CPAN.pm configuration in 5.8.8.2 and
5.10.0.2 has the setting

     makepl_arg         [LIBS=-LC:\strawberry\c\lib     INC=-IC:\strawberry\c\include]

This breaks the Tk build (and also other CPAN modules). The "fix" is
to change the setting to the usual default:

     o conf makepl_arg ""
