# clrpick.pl

use Tk qw/catch/;
use subs qw/setColor setColor_helper/;
use vars qw/$TOP/;

sub clrpick {
    my($demo) = @_;
    $TOP = $MW->WidgetDemo(
        -name             => $demo,
        -text             => 'Press the buttons below to choose the foreground and background colors for the widgets in this window.',
        -title            => 'chooseColor Demo',
        -iconname         => 'chooseColor',
    );

    my(@pl) = qw/-side top -anchor c -pady 2m/;
    my($back, $front);
    $back = $TOP->Button(-text => 'Set background color ...')->pack(@pl);
    $back->configure(-command => [\&setColor => $TOP, $back, '-background',
		      [-background, -highlightbackground]]);
    $front = $TOP->Button(-text => 'Set foreground color ...')->pack(@pl);
    $front->configure(-command => [\&setColor => $TOP, $front, '-foreground',
		      [-foreground]]);
}

sub setColor {
    my($top, $button, $name, $options) = @_;
    my $initialColor = $button->cget($name);
    my $color = $button->chooseColor(-title => "Choose a $name color",
				     -initialcolor => $initialColor);
    setColor_helper $top, $options, $color if defined $color;
}

sub setColor_helper {
    my($widget, $options, $color) = @_;
    foreach my $option (@$options) {
	catch {
	    $widget->configure($option => $color);
	}
    }
    foreach my $child ($widget->children) {
	setColor_helper $child, $options, $color;
    }
}

