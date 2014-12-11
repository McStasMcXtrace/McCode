BEGIN { $|=1; $^W=1; }
use strict;

BEGIN {
    if (!eval q{
	use Test::More;
	1;
    }) {
	print "1..0 # skip: no Test::More module\n";
	exit;
    }
}

use Tk;

BEGIN { plan tests => 19 };

my $mw = Tk::MainWindow->new;
$mw->geometry('+0+0');
my $w = $mw->Label(-text=>'a widget but not a Wm')->grid;

##
## appname (missing until Tk800 until .004)
##
{
    my $name;
    eval { $name = $w->appname; };
    is($@, "", "\$w->appname works");
    my ($leaf) = $name =~ /^(\w+)/;
    is( $leaf, 'widget', "Appname matches filename" );
    is( $mw->name, $name, "\$mw->name is equal to appname");
}
##
## scaling (missing until Tk800 until .004)
##
{
    my $scale;
    eval { $scale = $w->scaling; };
    is($@, "", "\$w->scaling works");
    like($scale, qr/^[0-9.]+$/, "Scaling factor is a number: '$scale'" );
}
##
## pathname did not work until Tk800.004
##
{
    my $path;
    my $c = $w->PathName;
    eval { $path = $mw->pathname($w->id); };
    is($@, "", "\$mw->pathname works");
    is( $path, $c, "Pathname and pathname agree" );
}

##
## Busy/Unbusy
##
{
    my $oldcursor = $mw->cget(-cursor);
    $mw->update; # make main window viewable, necessary for Busy
    $mw->Busy;
    is($mw->cget(-cursor), "watch", "The busy cursor");
    $mw->after(10);
    $mw->Unbusy;
    is($mw->cget(-cursor), $oldcursor, "Old cursor restored");
}

##
## Busy/Unbusy with recursion
##
{
    my $oldcursor = $mw->cget(-cursor);
    my $w2 = $mw->Label(-cursor => "cross")->grid;
    $mw->Busy(-recurse => 1, -cursor => "watch");
    is($mw->cget(-cursor), "watch", "The busy cursor");
    is($w2->cget(-cursor), "watch", "Subwidget has also the busy cursor");
    $mw->after(10);
    $mw->Unbusy;
    is($mw->cget(-cursor), $oldcursor, "Old cursor restored");
    is($w2->cget(-cursor), "cross", "Oldsubwidget cursor also restored");
    $w2->destroy;
}

## [rt.cpan.org #32858]
{
    my $top = $mw->Toplevel;
    $top->geometry('+0+0');
    $mw->update;
    $mw->Busy(-recurse => 1);
    for my $w ($mw, $top) {
	is(($w->bindtags)[0], 'Busy', "tag 'Busy' set for $w");
	is($w->cget('-cursor'), 'watch', "cursor 'watch'set for $w");
    }
    $mw->Unbusy;
    $top->destroy;
}

##
## PathName vs. Widget
##
{
    my $path = $w->PathName;
    is($mw->Widget($path), $w, "PathName() and Widget()");
}

## [rt.cpan.org #49515]
SKIP: {
    skip 'Probably does not work on monochrome displays', 1
	if $w->depth == 1;
    my $w2 = $mw->Label;
    $w2->RecolorTree({background => 'green'});
    is($w2->cget('-background'), 'green', 'RecolorTree was effective');
}

1;
__END__
