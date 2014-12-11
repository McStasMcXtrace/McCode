# unicodeout.pl

use vars qw/$TOP/;
use subs qw/unicodeadd/;

sub unicodeout {

    # This demonstration script shows how you can produce output (in label
    # widgets) using many different alphabets.

    my($demo) = @_;
    $TOP = $MW->WidgetDemo(
        -name     => $demo,
        -text     => 'This is a sample of Perl/Tk\'s support for languages that use non-Western character sets.  However, what you will actually seebelow depends largely on what character sets you have installed, and what you see for characters that are not present varies greatly between platforms as well.  The strings are written in Perl Unicode characters using the \\x{XXXX} escape sequence so as to do so in a portable fashion.',
        -title    => 'Unicode Label Demonstration',
        -iconname => 'unicodeout',
    );

    my $unicode_wait = $TOP->Label(
        -text => 'Please wait while loading fonts...',
        -font => 'Helvetica 12 italic',
    )->pack;
    $TOP->update;

    # Processing when some characters are missing might take a while, so make
    # sure we're displaying something in the meantime.

    $TOP->Busy;

    unicodeadd $TOP, 'Arabic',
    "\x{FE94}\x{FEF4}\x{FE91}\x{FEAE}\x{FECC}\x{FEDF}\x{FE8D}\x{FE94}\x{FEE4}\x{FEE0}\x{FEDC}\x{FEDF}\x{FE8D}";
    unicodeadd $TOP, "Trad. Chinese", "\x{4E2D}\x{570B}\x{7684}\x{6F22}\x{5B57}";
    unicodeadd $TOP, "Simpl. Chinese", "\x{6C49}\x{8BED}";
    unicodeadd $TOP, 'Greek', 
    "\x{0395}\x{03BB}\x{03BB}\x{03B7}\x{03BD}\x{03B9}\x{03BA}\x{03AE}\x{03B3}\x{03BB}\x{03CE}\x{03C3}\x{03C3}\x{03B1}";
    unicodeadd $TOP, 'Hebrew', 
    "\x{05DD}\x{05D9}\x{05DC}\x{05E9}\x{05D5}\x{05E8}\x{05D9}\x{05DC}\x{05D9}\x{05D0}\x{05E8}\x{05E9}\x{05D9}";
    unicodeadd $TOP, 'Japanese', 
    "\x{65E5}\x{672C}\x{8A9E}\x{306E}\x{3072}\x{3089}\x{304C}\x{306A}\x{6F22}\x{5B57}\x{3068}\x{30AB}\x{30BF}\x{30AB}\x{30CA}";
    unicodeadd $TOP, 'Korean',
    "\x{B300}\x{D55C}\x{BBFC}\x{AD6D}\x{C758}\x{D55C}\x{AE00}";
    unicodeadd $TOP, 'Russian', 
    "\x{0420}\x{0443}\x{0441}\x{0441}\x{043A}\x{0438}\x{0439}\x{044F}\x{0437}\x{044B}\x{043A}";

    # We're done processing, so change things back to normal running.

    $unicode_wait->destroy;
    $TOP->Unbusy;

} # end unicodeout

sub unicodeadd {

    my ($w, $language, @args) = @_;

    my $sample = join('', @args);
    my $l1 = $w->Label(-text => "$language: ", qw/-anchor nw -pady 0/);
    my $l2 = $w->Label(-text => $sample, qw/-anchor nw -width 30 -pady 0/);
    $l1->grid($l1, $l2, qw/-sticky ew -pady 0/);
    $l1->gridConfigure(qw/-padx 1m/);

} # end unicodeadd

1;
