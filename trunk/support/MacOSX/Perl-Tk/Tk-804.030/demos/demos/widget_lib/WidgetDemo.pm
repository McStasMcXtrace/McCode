package WidgetDemo;

use 5.005_03;

use vars qw($VERSION);
$VERSION = '4.012'; # sprintf '4.%03d', q$Revision: #11 $ =~ /\D(\d+)\s*$/;

use Tk 800.000;
use Carp;

use Tk;
use Tk::Toplevel;
use strict;
use base  'Tk::Toplevel';
Construct Tk::Widget 'WidgetDemo';

# %WIDGDEMO is a class global that tracks all WidgetDemo composite widgets,
# providing a means of destroying a previous instance of a demonstration.

my %WIDGDEMO;			# class hash of active widget demonstrations

sub Populate {
    my($self, $args) = @_;

    my (%arg_defaults) = (
        -name             => 'Unknown Demo Name',
	-font             => 'Helvetica 12',
	-text             => 'Unknown Demo Text',
	-geometry_manager => 'pack',
    );
    my $name = $arg_defaults{-name};
    $arg_defaults{-title} = "$name Demonstration",
    $arg_defaults{-iconname} = $name;

    my(@margs, %ahsh, @args);
    @margs = grep ! defined $args->{$_}, keys %arg_defaults;
    %ahsh = %$args;
    @ahsh{@margs} = @arg_defaults{@margs};
    my($demo, $font, $text, $title, $iconname, $gm) =
	@ahsh{-name, -font, -text, -title, -iconname, -geometry_manager};
    delete $args->{-name};
    delete $args->{-font};
    delete $args->{-iconname};
    delete $args->{-geometry_manager};

    $WIDGDEMO{$demo}->destroy if Exists($WIDGDEMO{$demo});
    $WIDGDEMO{$demo} = $self;

    $self->SUPER::Populate($args);
    $self->iconname($iconname);

    my(@label_attributes) = ();
    if (ref($text) eq 'ARRAY') {
	@label_attributes = @$text[1 .. $#{$text}];
	$text = $text->[0];
    }
    my $msg = $self->Label(
        -font       => $font,
        -wraplength => '4i',
        -justify    => 'left',
        -text       => $text,
        @label_attributes,
    );

    my $demo_frame = $self->Frame;
    $self->Advertise('WidgetDemo' => $demo_frame); # deprecated

    my $buttons = $self->Frame;
    my $dismiss = $buttons->Button(
        -text    => 'Dismiss',
        -command => [$self => 'destroy'],
    );
    my $see = $buttons->Button(-text => 'See Code',
			       -command => [\&main::see_code, $demo]);

    if ($gm eq 'pack') {
	$buttons->pack(qw/-side bottom -fill x -pady 2m/);
	$dismiss->pack(qw/-side left -expand 1/);
	$see->pack(qw/-side left -expand 1/);
	$msg->pack;
	$demo_frame->pack(qw/-fill both -expand 1 -anchor n/);
    } elsif ($gm eq 'grid') {
	$msg->grid;
	$demo_frame->grid(-sticky => "news");
	$demo_frame->gridColumnconfigure(0,-weight=>1);
	$demo_frame->gridRowconfigure(0,-weight=>1);
	$self->gridColumnconfigure(qw/0 -weight 1/);
	$self->gridRowconfigure(qw/1 -weight 1/);
	$buttons->grid(qw/-pady 2m -sticky ew/);
	$buttons->gridColumnconfigure(qw/0 -weight 1/);
	$buttons->gridColumnconfigure(qw/1 -weight 1/);
	$dismiss->grid(qw/-row 0 -column 0/);
	$see->grid(qw/-row 0 -column 1/);
    } else {
	croak "Only pack or grid geometry management supported.";
    }

    $self->Delegates('Construct' => $demo_frame);

    $self->ConfigSpecs(
        -text => [qw/METHOD text Text NoText/],
    );

    $self->{msg} = $msg;

    return $self;

} # end Populate

sub Top {return $_[0]->Subwidget('WidgetDemo')}	# deprecated
*top = *top = \&Top;  # peacify -w

sub text {

    my ($self, $text) = @_;

    my(@label_attributes) = ();
    if (ref($text) eq 'ARRAY') {
	@label_attributes = @$text[1 .. $#{$text}];
	$text = $text->[0];
    }

    $self->{msg}->configure(
        -text       => $text,
        @label_attributes,
    );

} # end text

1;
