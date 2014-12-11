# bind.pl

use vars qw/$TOP/;

sub bind {

    # Create a top-level window that illustrates how you can bind Perl
    # commands to regions of text in a text widget.

    my($demo) = @_;
    $TOP = $MW->WidgetDemo(
        -name     => $demo,
        -text     =>'',
        -title    => 'Text Demonstration - Tag Bindings',
        -iconname => 'bind',
    );

    my $t = $TOP->Scrolled(qw/Text -setgrid true -width 60 -height 24
			   -scrollbars e -wrap word/, -font => $FONT);
    $t->pack(qw/-expand yes -fill both/);

    # Set up display styles

    my(@bold, @normal, $tag);
    if ($TOP->depth > 1) {
	@bold   = (-background => '#43ce80', qw/-relief raised -borderwidth 1/);
	@normal = (-background => undef, qw/-relief flat/);
    } else {
	@bold   = (qw/-foreground white -background black/);
	@normal = (-foreground => undef, -background => undef);
    }

    $t->insert('0.0', "The same tag mechanism that controls display styles in text widgets can also be used to associate Perl commands with regions of text, so that mouse or keyboard actions on the text cause particular Perl commands to be invoked.  For example, in the text below the descriptions of the canvas demonstrations have been tagged.  When you move the mouse over a demo description the description lights up, and when you press button 1 over a description then that particular demonstration is invoked.\n\n");
    $t->insert('end','1. Samples of all the different types of items that can be created in canvas widgets.', 'd1');
    $t->insert('end', "\n\n");
    $t->insert('end', '2. A simple two-dimensional plot that allows you to adjust the positions of the data points.', 'd2');
    $t->insert('end', "\n\n");
    $t->insert('end', '3. Anchoring and justification modes for text items.', 'd3');
    $t->insert('end', "\n\n");
    $t->insert('end', '4. An editor for arrow-head shapes for line items.', 'd4');
    $t->insert('end', "\n\n");
    $t->insert('end', '5. A ruler with facilities for editing tab stops.', 'd5');
    $t->insert('end', "\n\n");
    $t->insert('end', '6. A grid that demonstrates how canvases can be scrolled.', 'd6');

    foreach $tag (qw(d1 d2 d3 d4 d5 d6)) {
	$t->tagBind($tag, '<Any-Enter>' =>
            sub {shift->tagConfigure($tag, @bold)}
        );
	$t->tagBind($tag, '<Any-Leave>' =>
            sub {shift->tagConfigure($tag, @normal)}
        );
    }
    $t->tagBind(qw/d1 <1>/ => sub {&items('items')});
    $t->tagBind(qw/d2 <1>/ => sub {&plot('plot')});
    $t->tagBind(qw/d3 <1>/ => sub {&ctext('ctext')});
    $t->tagBind(qw/d4 <1>/ => sub {&arrows('arrows')});
    $t->tagBind(qw/d5 <1>/ => sub {&ruler('ruler')});
    $t->tagBind(qw/d6 <1>/ => sub {&cscroll('cscroll')});

    $t->mark(qw/set insert 0.0/);

} # end bind

1;
