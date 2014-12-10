package Tk::ColorEditor;

use vars qw($VERSION $SET_PALETTE);
$VERSION = '4.014';

use Tk qw(lsearch Ev);
use Tk::Toplevel;
use base  qw(Tk::Toplevel);
use Tk::widgets qw(Pixmap);
Construct Tk::Widget 'ColorEditor';

use Tk::Dialog;
use Tk::Pretty;

use Tk::ColorSelect ();
use Tk::ColorDialog ();

BEGIN { $SET_PALETTE = 'Set Palette' };

use subs qw(color_space hsvToRgb rgbToHsv);

# ColorEditor public methods.

sub add_menu_item
{
 my $objref = shift;
 my $value;
 foreach $value (@_)
  {
   if ($value eq 'SEP')
    {
     $objref->{'mcm2'}->separator;
    }
   else
    {
     $objref->{'mcm2'}->command( -label => $value,
           -command => [ 'configure', $objref, '-highlight' => $value ] );
     push @{$objref->{'highlight_list'}}, $value;
    }
  }
}

sub set_title
{
 my ($w) = @_;
 my $t = $w->{Configure}{'-title'} || '' ;
 my $h = $w->{Configure}{'-highlight'} || '';
 $w->SUPER::title("$t $h Color Editor");
}

sub highlight
{
 my ($w,$h) = @_;
 if (@_ > 1)
  {
   $w->{'update'}->configure( -text => "Apply $h Color" );
   my $state = ($h eq 'background') ? 'normal' : 'disabled';
   $w->{'palette'}->entryconfigure( $SET_PALETTE, -state => $state);
   $w->{'highlight'} = $h;
   $w->configure(-color => $w->Palette->{$h});
   $w->set_title;
  }
 return $w->{'highlight'};
}

sub title
{
 my ($w,$val) = @_;
 $w->set_title if (@_ > 1);
 return $w->{Configure}{'-title'};
}

sub delete_menu_item
{
 my $objref = shift;
 my $value;
 foreach $value (@_)
  {
   $objref->{'mcm2'}->delete($value);
   my $list_ord = $value =~ /\d+/ ? $value : lsearch($objref->{'highlight_list'}, $value);
   splice(@{$objref->{'highlight_list'}}, $list_ord, 1) if $list_ord != -1;
  }
}

sub delete_widgets {

    # Remove widgets from consideration by the color configurator.
    # $widgets_ref points to widgets previously added via `configure'.

    my($objref, $widgets_ref) = @_;

    my($i, $found, $r1, $r2, @wl) = (0, 0, 0, 0, @{$objref->cget(-widgets)});
    foreach $r1 (@{$widgets_ref}) {
        $i = -1;
        $found = 0;
        foreach $r2 (@wl) {
            $i++;
            next if $r1 != $r2;
            $found = 1;
            last;
        }
        splice(@wl, $i, 1) if $found;
    }
    $objref->configure(-widgets => [@wl]);

} # end delete_widgets

sub ApplyDefault
{
 my($objref) = @_;
 my $cb = $objref->cget('-command');
 my $h;
 foreach $h (@{$objref->{'highlight_list'}})
  {
   next if $h =~ /TEAR_SEP|SEP/;
   $cb->Call($h);
   die unless (defined $cb);
  }
}

sub Populate
{

    # ColorEditor constructor.

    my($cw, $args) = @_;

    $cw->SUPER::Populate($args);
    $cw->withdraw;

    my $color_space = 'hsb';    # rgb, cmy, hsb
    my(@highlight_list) = qw(
        TEAR_SEP
        foreground background SEP
        activeForeground activeBackground SEP
        highlightColor highlightBackground SEP
        selectForeground selectBackground SEP
        disabledForeground insertBackground selectColor troughColor
    );

    # Create the Usage Dialog;

    my $usage = $cw->Dialog( '-title' => 'ColorEditor Usage',
        -justify    => 'left',
        -wraplength => '6i',
        -text       => "The Colors menu allows you to:\n\nSelect a color attribute such as \"background\" that you wish to colorize.  Click on \"Apply\" to update that single color attribute.\n\nSelect one of three color spaces.  All color spaces display a color value as a hexadecimal number under the oval color swatch that can be directly supplied on widget commands.\n\nApply Tk's default color scheme to the application.  Useful if you've made a mess of things and want to start over!\n\nChange the application's color palette.  Make sure \"background\" is selected as the color attribute, find a pleasing background color to apply to all current and future application widgets, then select \"Set Palette\".",
    );

    # Create the menu bar at the top of the window for the File, Colors
    # and Help menubuttons.

    my $m0 = $cw->Frame(-relief => 'raised', -borderwidth => 2);
    $m0->pack(-side => 'top', -fill => 'x');
    my $mf = $m0->Menubutton(
        -text      => 'File',
        -underline => 0,
        -bd        => 1,
        -relief    => 'raised',
    );
    $mf->pack(-side => 'left');
    my $close_command = [sub {shift->withdraw}, $cw];
    $mf->command(
        -label       => 'Close',
        -underline   => 0,
        -command     => $close_command,
        -accelerator => 'Ctrl-w',
    );
    $cw->bind('<Control-Key-w>' => $close_command);
    $cw->protocol(WM_DELETE_WINDOW => $close_command);

    my $mc = $m0->Menubutton(
        -text      => 'Colors',
        -underline => 0,
        -bd        => 1,
        -relief    => 'raised',
    );
    $mc->pack(-side => 'left');
    my $color_attributes = 'Color Attributes';
    $mc->cascade(-label => $color_attributes, -underline => 6);
    $mc->separator;

    $mc->command(
        -label     => 'Apply Default Colors',
        -underline => 6,
        -command   => ['ApplyDefault',$cw]
    );
    $mc->separator;
    $mc->command(
        -label     => $SET_PALETTE,
        -underline => 0,
        -command   => sub { $cw->setPalette($cw->cget('-color'))}
    );

    my $m1 = $mc->cget(-menu);

    my $mcm2 = $m1->Menu;
    $m1->entryconfigure($color_attributes, -menu => $mcm2);
    my $mh = $m0->Menubutton(
        -text      => 'Help',
        -underline => 0,
        -bd        => 1,
        -relief    => 'raised',
    );
    $mh->pack(-side => 'right');
    $mh->command(
        -label       => 'Usage',
        -underline   => 0,
        -command     => [sub {shift->Show}, $usage],
    );

    # Create the Apply button.

    my $bot = $cw->Frame(-relief => 'raised', -bd => 2);
    $bot->pack(-side => 'bottom', -fill =>'x');
    my $update = $bot->Button(
        -command => [
            sub {
                my ($objref) = @_;
                $objref->Callback(-command => ($objref->{'highlight'}, $objref->cget('-color')));
		$cw->{'done'} = 1;
            }, $cw,
        ],
    );
    $update->pack(-pady => 1, -padx => '0.25c');

    # Create the listbox that holds all of the color names in rgb.txt, if an
    # rgb.txt file can be found.

    my $middle = $cw->ColorSelect(-relief => 'raised', -borderwidth => 2);
    $middle->pack(-side => 'top', -fill => 'both');
    # Create the status window.

    my $status = $cw->Toplevel;
    $status->withdraw;
    $status->geometry('+0+0');
    my $status_l = $status->Label(-width => 50,  -anchor => 'w');
    $status_l->pack(-side => 'top');

    $cw->{'highlight_list'} = [@highlight_list];
    $cw->{'mcm2'} = $mcm2;

    foreach (@highlight_list)
     {
      next if /^TEAR_SEP$/;
      $cw->add_menu_item($_);
     }

    $cw->{'updating'} = 0;
    $cw->{'pending'} = 0;
    $cw->{'Status'} = $status;
    $cw->{'Status_l'} = $status_l;
    $cw->{'update'} = $update;
    $cw->{'gwt_depth'} = 0;
    $cw->{'palette'} = $mc;

    my $pixmap = $cw->Pixmap('-file' => Tk->findINC('ColorEdit.xpm'));
    $cw->Icon(-image => $pixmap);

    $cw->ConfigSpecs(
        DEFAULT         => [$middle],
        -widgets        => ['PASSIVE', undef, undef,
                               [$cw->parent->Descendants]],
        -display_status => ['PASSIVE', undef, undef, 0],
        '-title'        => ['METHOD', undef, undef, ''],
        -command        => ['CALLBACK', undef, undef, ['set_colors',$cw]],
        '-highlight'    => ['METHOD', undef, undef, 'background'],
        -cursor         => ['DESCENDANTS', 'cursor', 'Cursor', 'left_ptr'],
    );

} # end Populate, ColorEditor constructor

sub Show {

    my($objref, @args) = @_;

    Tk::ColorDialog::Show(@_);

} # end show

# ColorEditor default configurator procedure - can be redefined by the
# application.

sub set_colors {

    # Configure all the widgets in $widgets for attribute $type and color
    # $color.  If $color is undef then reset all colors
    # to the Tk defaults.

    my($objref, $type, $color) = @_;
    my $display = $objref->cget('-display_status');

    $objref->{'Status'}->title("Configure $type");
    $objref->{'Status'}->deiconify if $display;
    my $widget;
    my $reset = !defined($color);

    foreach $widget (@{$objref->cget('-widgets')}) {
        if ($display) {
            $objref->{'Status_l'}->configure(
                -text => 'WIDGET:  ' . $widget->PathName
            );
            $objref->update;
        }
        eval {local $SIG{'__DIE__'}; $color = ($widget->configure("-\L${type}"))[3]} if $reset;
        eval {local $SIG{'__DIE__'}; $widget->configure("-\L${type}" => $color)};
    }

    $objref->{'Status'}->withdraw if $display;

} # end set_colors

# ColorEditor private methods.

1;

__END__

