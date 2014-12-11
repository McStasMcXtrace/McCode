# sayings.pl

use vars qw/$TOP/;

sub sayings {

    # Create a top-level window containing a listbox with a bunch of
    # well-known sayings.  The listbox can be scrolled or scanned in
    # two dimensions.

    my($demo) = @_;
    $TOP = $MW->WidgetDemo(
        -name     => $demo,
        -text     => 'The listbox below contains a collection of well-known sayings.  You can scan the list using either of the scrollbars or by dragging in the listbox window with button 2 pressed.',
        -title    => 'Listbox Demonstration (well-known sayings)',
        -iconname => 'sayings',
    );

    my $list = $TOP->Scrolled(qw/Listbox -width 20 -height 10 -setgrid 1
			      -scrollbars se/);
    $list->pack(qw/-expand yes -fill y/);
    $list->focus;

    $list->insert(0,
      'Waste not, want not',
      'Early to bed and early to rise makes a man healthy, wealthy, and wise',
      'Ask not what your country can do for you, ask what you can do for your country',
      'I shall return',
      'NOT',
      'A picture is worth a thousand words',
      'User interfaces are hard to build',
      'Thou shalt not steal',
      'A penny for your thoughts',
      'Fool me once, shame on you; fool me twice, shame on me',
      'Every cloud has a silver lining',
      'Where there\'s smoke there\'s fire',
      'It takes one to know one',
      'Take this job and shove it',
      'Up a creek without a paddle',
      'I\'m mad as hell and I\'m not going to take it any more',
      'An apple a day keeps the doctor away',
      'Don\'t look a gift horse in the mouth');

    $list->activate(0);

} # end sayings

1;
