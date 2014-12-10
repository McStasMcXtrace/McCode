# paned2.pl

use vars qw/$TOP/;

sub paned2 {

    # This demonstration script creates a toplevel window containing
    # a paned window that separates two windows vertically.

    my($demo) = @_;
    $TOP = $MW->WidgetDemo(
        -name     => $demo,
        -text     => 'The sash between the two scrolled windows below can be used to divide the area between them.  Use the left mouse button to resize without redrawing by just moving the sash, and use the middle mouse button to resize opaquely (always redrawing the windows in each position.)',
        -title    => 'Vertical Paned Window Demonstration',
        -iconname => 'paned2',
    );

    my $pw = $TOP->Panedwindow(qw/-orient vertical/);
    $pw->pack(qw/-side top -expand yes -fill both -pady 2 -padx 2m/);

    my $paneList = [
        'List of Tk Widgets', qw/
        button
        canvas
        checkbutton
        entry
        frame
        label
        labelframe
        listbox
        menu
        menubutton
        message
        panedwindow
        radiobutton
        scale
        scrollbar
        spinbox
        text
        toplevel
        /,
    ];

    my $f1 = $pw->Frame;
    my $lb = $f1->Listbox(-listvariable => $paneList);
    $lb->pack(qw/-fill both -expand 1/);
    my ($fg, $bg) = ($lb->cget(-foreground), $lb->cget(-background));
    $lb->itemconfigure(0, 
	-background => $fg,
        -foreground => $bg,
    );

    my $f2 = $pw->Frame;
    my $t = $f2->Text(qw/-width 30 -wrap none/);

    $t->grid(qw/-sticky nsew/);
    $f2->gridColumnconfigure(qw/0 -weight 1/);
    $f2->gridRowconfigure(qw/0 -weight 1/);
    $t->insert('1.0', 'This is just a normal text widget');
    
    $pw->add($f1, $f2);

} # end paned2

1;
