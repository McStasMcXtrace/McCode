# msgBox.pl

use vars qw/$TOP/;

sub msgBox {
    my($demo) = @_;
    $TOP = $MW->WidgetDemo(
        -name             => $demo,
        -text             => 'Choose the icon and type option of the message box. Then press the "Message Box" button to see the message box.',
        -title            => 'messageBox Demo',
        -iconname         => 'messageBox',
    );

    my $upper = $TOP->Frame->pack;
    my $left = $upper->Frame;
    my $right = $upper->Frame;
    $left->pack(qw/-side left -expand yes -fill y  -pady .5c -padx .5c/);
    $right->pack(qw/-side left -expand yes -fill y  -pady .5c -padx .5c/);

    my $icon = $left->Label(qw/-text Icon/);
    my $lsep = $left->Frame(qw/-relief ridge -bd 1 -height 2/);
    $icon->pack(qw/-side top/);
    $lsep->pack(qw/-side top -fill x -expand no/);

    my $iconvar = 'info';
    foreach my $i (qw/error info question warning/) {
	$left->Radiobutton(-text => $i, -variable => \$iconvar, -value => $i,
			   qw/-width 16 -anchor w -relief flat/)->pack(
			   qw/-side top -pady 2 -anchor w -fill x/);
    }


    my $rl = $right->Label(qw/-text Type/);
    my $rsep = $right->Frame(qw/-relief ridge -bd 1 -height 2/);
    $rl->pack(qw/-side top/);
    $rsep->pack(qw/-side top -fill x -expand no/);

    my $typevar = 'OK';
    foreach my $t (qw/AbortRetryIgnore OK OKCancel RetryCancel YesNo YesNoCancel/) {
	$right->Radiobutton(-text => $t, -variable => \$typevar, -value => $t,
			    qw/-relief flat -width 16 -anchor w/)->pack(
			    qw/-side top -pady 2 -anchor w -fill x/);
    }

    {
	my $cbf = $TOP->Frame->pack(-fill => 'x', -padx => '.5c', -pady => 3);
	my $fd;
	my $fdb = $cbf->Radiobutton
	    (-text => 'Tk::DialogBox',
	     -variable => \$fd,
	     -value => 'DialogBox',
	     -command => sub { local($^W) = 0;
			       *Tk::tk_messageBox = sub{ Tk::MessageBox('tk_messageBox', @_) };
			   })->pack(-side => 'left');
	$cbf->Radiobutton
	    (-text => 'Tk::MsgBox',
	     -variable => \$fd,
	     -value => 'MsgBox',
	     -command => sub { local($^W) = 0;
			       require Tk::MsgBox;
			       Tk::MsgBox->import('as_default');
			   })->pack(-side => 'left');
	$fdb->invoke;
    }

    my $show = $TOP->Button(-text => "Message Box",
			    -command => [\&show_messageBox, \$iconvar, \$typevar]);
    $show->pack;

} # end msgBox

sub show_messageBox {
    my($iconvar, $typevar) = @_;

    my $button = $TOP->messageBox('-icon' => $$iconvar, -type => $$typevar,
	-title => 'Message',
        -message => "This is a \"$$typevar\" type messagebox with the \"$$iconvar\" icon");

    $TOP->messageBox(qw/-icon info -type OK -message/ => "You have selected \"$button\"");

} # end show_messageBox

