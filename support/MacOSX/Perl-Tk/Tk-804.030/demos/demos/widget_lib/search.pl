# search.pl

use Tk::LabEntry;
use subs qw/search_flash_matches search_load_file search_text/;
use vars qw/$TOP/;

sub search {

    # Create a top-level window with a text widget that allows you to load a
    # file and highlight all instances of a given string.  A LabEntry widget
    # is used to collect the file name and search string.

    my($demo) = @_;
    $TOP = $MW->WidgetDemo(
        -name     => $demo,
        -text     =>'',
        -title    => 'Text Demonstration - Search and Highlight',
        -iconname => 'search',
    );

    my $file_name = '';
    my $file = $TOP->Frame;
    my $fn = $file->LabEntry(-label => 'File Name:      ', -width => 40,
        -labelPack => [qw/-side left -anchor w/],
        -textvariable => \$file_name)->pack(qw/-side left/);
    $fn->Subwidget('entry')->focus;
    my $fn_button = $file->Button(-text => 'Load File');
    $fn_button->pack(qw/-side left -pady 5 -padx 10/);

    my $search_string = '';
    my $kind = 'exact';
    my $string = $TOP->Frame;
    my $ss = $string->LabEntry(-label => 'Search string:', -width => 40,
        -labelPack => [qw/-side left -anchor w/],
        -textvariable => \$search_string)->pack(qw/-side left/);
    my $ss_button = $string->Button(-text => 'Highlight');
    $ss_button->pack(qw/-side left -pady 5 -padx 10/);

    my $text = $TOP->Scrolled(qw/Text -setgrid true -scrollbars e/);

    my $subframe = $TOP->Frame;
    my $exact  = $subframe->Radiobutton(-text => 'Exact match',
                                        -variable => \$kind,
                                        -value => 'exact');
    my $regexp = $subframe->Radiobutton(-text => 'Regular expression',
                                        -variable => \$kind,
                                        -value => 'regexp');
    $exact->pack(qw/-side left/, -fill => 'x');
    $regexp->pack(qw/-side right/, -fill => 'x');

    $file->pack(qw/-side top -fill x/);
    $string->pack(qw/-side top -fill x/);
    $subframe->pack(qw/-side top -fill x/);
    $text->pack(qw/-expand yes -fill both/);

    my $command =  sub {search_load_file $text, \$file_name, $ss};
    $fn_button->configure(-command => $command);
    $fn->bind('<Return>' => $command);

    $command = sub {search_text $text, \$search_string, 'search', $kind};
    $ss_button->configure(-command => $command);
    $ss->bind('<Return>' => $command);

    # Set up display styles for text highlighting.

    if ($TOP->depth > 1) {
	search_flash_matches $text,
            ['configure', 'search',
                -background => '#ce5555', -foreground => 'white'], 800,
            ['configure', 'search',
                -background => undef,     -foreground => undef],   200;
      } else {
	search_flash_matches $text,
            ['configure', 'search',
                -background => 'black',   -foreground => 'white'], 800,
            ['configure', 'search',
               -background => undef,      -foreground => undef],   200;
      }

    $text->insert('0.0', 'This window demonstrates how to use the tagging facilities in text
widgets to implement a searching mechanism.  First, type a file name
in the top entry, then type <Return> or click on "Load File".  Then
type a string in the lower entry and type <Return> or click on
"Highlight".  This will cause all of the instances of the string to
be tagged with the tag "search", and it will arrange for the tag\'s
display attributes to change to make all of the strings blink.');

    $text->mark(qw/set insert 0.0/);

} # end search

sub search_flash_matches {

    # The procedure below is invoked repeatedly to invoke two commands at
    # periodic intervals.  It normally reschedules itself after each execution
    # but if an error occurs (e.g. because the window was deleted) then it
    # doesn't reschedule itself.
    # Arguments:
    #
    # w -       Text widget reference.
    # cmd1 -	Reference to a list of tag options.
    # sleep1 -	Ms to sleep after executing cmd1 before executing cmd2.
    # cmd2 -	Reference to a list of tag options.
    # sleep2 -	Ms to sleep after executing cmd2 before executing cmd1 again.

    my($w, $cmd1, $sleep1, $cmd2, $sleep2) = @_;

    $w->tag(@{$cmd1});
    $w->after($sleep1,
	      [\&search_flash_matches, $w, $cmd2, $sleep2, $cmd1, $sleep1]);

} # end search_flash_matches

sub search_load_file {

    # The utility procedure below loads a file into a text widget, discarding
    # the previous contents of the widget. Tags for the old widget are not
    # affected, however.
    # Arguments:
    #
    # w -	The window into which to load the file.  Must be a text widget.
    # file -	Reference to the name of the file to load.  Must be readable.
    # e -       Entry widget to get next focus.

    my ($w, $file, $e) = @_;

    my ($buf, $bytes) = ('', 0);

    if (not open(F, "<$$file")) {
	$MW->Dialog(
            -title  => 'File Not Found',
            -text   => "$!: '$$file'",
            -bitmap => 'error',
        )->Show;
	return;
    }
    $w->delete(qw/1.0 end/);
    $bytes = read F, $buf, 10_000;	# after all, it IS just an example
    $w->insert('end', $buf);
    if ($bytes == 10000) {
	$w->insert('end', "\n\n**************** File truncated at 10,000 bytes! ****************\n");
    }
    close F;

    $e->Subwidget('entry')->focus;

} # end search_load_file

sub search_text {

    # The utility procedure below searches for all instances of a given
    # string in a text widget and applies a given tag to each instance found.
    # Arguments:
    #
    # w -	The window in which to search.  Must be a text widget.
    # string -	Reference to the string to search for.  The search is done
    #           using exact matching only;  no special characters.
    # tag -	Tag to apply to each instance of a matching string.

    my($w, $string, $tag, $kind) = @_;

    return unless ref($string) && length($$string);

    $w->tagRemove($tag, qw/0.0 end/);
    my($current, $length) = ('1.0', 0);

    while (1) {
	$current = $w->search(-count => \$length, "-$kind", $$string, $current, 'end');
	last if not $current;
	warn "Posn=$current count=$length\n",
	$w->tagAdd($tag, $current, "$current + $length char");
	$current = $w->index("$current + $length char");
    }

} # end search_text

1;
