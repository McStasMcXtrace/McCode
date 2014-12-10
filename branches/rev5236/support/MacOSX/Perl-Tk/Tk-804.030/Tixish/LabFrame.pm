#
# Labeled frame. Derives from Tk::Frame, but intercepts the labeling
# part.

package Tk::LabFrame;

use vars qw($VERSION);
$VERSION = '4.010'; # $Id: //depot/Tkutf8/Tixish/LabFrame.pm#11 $

use Tk;
use base qw(Tk::Frame);
Tk::Widget->Construct('LabFrame');

sub autoLabel { 0 }

sub Populate {
    my ($cw, $args) = @_;

    $cw->{m_geoMgr} = "";

    my $border = $cw->Component(
    Frame => 'border',
        -relief => 'groove',
        -bd => 2,
    );

    my $pad = $border->Frame;
    $cw->Advertise(pad => $pad);

    my $frame = $border->Frame;
    $cw->Advertise(frame => $frame);

    my $label = $cw->Component(Label => 'label');

    $cw->SUPER::Populate($args);

    $cw->Delegates(DEFAULT => $frame);
    $cw->ConfigSpecs(
        -background    => [[qw/SELF ADVERTISED/],
                            qw/background Background/],
        -borderwidth   => [$border, qw/borderWidth Border 2/],
        -font          => [$label, qw/font Font/],
        -foreground    => [$label, qw/foreground Foreground black/],
        -label         => [{-text => $label}, qw/label Label/],
        -labelside     => [qw/METHOD labelSide LabelSide acrosstop/],
        -labelvariable => [{-textvariable => $label}],
        -relief        => [$border, qw/relief Relief groove/],
        DEFAULT        => [$frame]
    );
    return $cw;
}

use Tk::Submethods(
    form  => [qw/check forget grid info slaves/],
    grid  => [qw/bbox columnconfigure configure forget info location
                 propagate rowconfigure remove size slaves/],
    pack  => [qw/forget info propagate slaves/],
    place => [qw/forget info slaves/]
);

sub labelside {
    my ($cw, $side) = @_;
    return $cw->{Configure}{-labelside} unless $side;

    my $border = $cw->Subwidget('border');
    my $pad = $cw->Subwidget('pad');
    my $frame = $cw->Subwidget('frame');
    my $label = $cw->Subwidget('label');

    ## packForget/formForget as appropriate
    foreach ($border, $label, $pad, $frame) {
        $_->formForget if $cw->{m_geoMgr} eq "form";
        $_->packForget if ($cw->{m_geoMgr} eq "pack" && $_->ismapped);
    }

    if ($side eq "acrosstop") {

        my $y = $label->reqheight / 2;
        my $ph = $y - ($border->cget(-bd));
        $ph = 0 if $ph < 0;

        $label->form(qw/-top 0 -left 4 -padx 6 -pady 2/);
        $border->form(-top => $y,
            qw/-bottom -1 -left 0 -right -1 -padx 2 -pady 2/);
        $pad->form(-bottom => $ph,
            qw/-top 0 -left 0 -right -1/);
        $frame->form(-top => $pad,
            qw/-bottom -1 -left 0 -right -1 -fill both/);
        $cw->{m_geoMgr} = "form";

    } else {

        $label->pack(-side => $side);
        $frame->pack(-expand => 1, -fill => 'both');
        $border->pack(-side => $side, -expand => 1, -fill => 'both');
        $cw->{m_geoMgr} = "pack";
    }
}

sub form {
    my $cw = shift;
    $cw = $cw->Subwidget('frame')
        if (@_ && $_[0] =~ /^(?:slaves)$/);
    $cw->SUPER::form(@_);
}

sub grid {
    my $cw = shift;
    $cw = $cw->Subwidget('frame') if (@_ && $_[0] =~
        /^(?:bbox
            |columnconfigure
            |location
            |propagate
            |rowconfigure
            |size
            |slaves)
        $/x);
    $cw->SUPER::grid(@_);
}


sub pack {
    my $cw = shift;
    $cw = $cw->Subwidget('frame')
        if (@_ && $_[0] =~ /^(?:propagate|slaves)$/);
    $cw->SUPER::pack(@_);
}

sub place {
    my $cw = shift;
    $cw = $cw->Subwidget('frame')
        if (@_ && $_[0] =~ /^(?:slaves)$/);
    $cw->SUPER::place(@_);
}

1;


