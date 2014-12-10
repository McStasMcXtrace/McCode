# states.pl

use vars qw/$TOP/;

sub states {

    # Create a top-level window that displays a listbox with the names of the
    # 50 states.

    my($demo) = @_;
    $TOP = $MW->WidgetDemo(
        -name     => $demo,
        -text     => 'A listbox containing the 50 states is displayed below, along with a scrollbar.  You can scan the list either using the scrollbar or by scanning.  To scan, press button 2 in the widget and drag up or down.',
        -title    => 'Listbox Demonstration (50 states)',
        -iconname => 'states',
    );

    my $list = $TOP->Scrolled(qw/Listbox -setgrid 1 -height 12 -scrollbars e/);
    $list->pack(qw/-side left -expand yes -fill both/);
    $list->focus;

    $list->insert(0, qw/Alabama Alaska Arizona Arkansas California Colorado Connecticut Delaware Florida Georgia Hawaii Idaho Illinois Indiana Iowa Kansas Kentucky Louisiana Maine Maryland Massachusetts Michigan Minnesota Mississippi Missouri Montana Nebraska Nevada/, 'New Hampshire', 'New Jersey', 'New Mexico', 'New York', 'North Carolina', 'North Dakota', qw/Ohio Oklahoma Oregon Pennsylvania/, 'Rhode Island', 'South Carolina', 'South Dakota', qw/Tennessee Texas Utah Vermont Virginia Washington/, 'West Virginia', 'Wisconsin', 'Wyoming');

    $list->activate(0);

} # end states

1;
