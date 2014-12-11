package Tk::ColorSelect; # XXX why is this the Tk::ColorSelect package?
use strict;

use vars qw($VERSION);
$VERSION = '4.012'; # was: sprintf '4.%03d', q$Revision: #8 $ =~ /\D(\d+)\s*$/;

use Tk qw(Ev);

require Tk::Frame;

use base  qw(Tk::Frame);
Construct Tk::Widget 'ColorSelect';

sub Populate
{
    my ($middle,$args) = @_;
    my($i, @a);
    my %seen_names;
    foreach $i ($middle->_rgbTxtPath) {
        local *FOO;
        next if ! open FOO, $i;
        my $middle_left = $middle->Frame;
        $middle_left->pack(
            -side => 'left',
            -padx => '0.25c',
            -pady => '0.25c',
        );
        my $names = $middle->Listbox(
            -width           => 20,
            -height          => 12,
            -relief          => 'sunken',
            -borderwidth     => 2,
            -exportselection => 0,
        );
	$middle->Advertise(Names => $names);

        $names->bind('<Double-1>' => [$middle,'color',Ev(['getSelected'])]);

        my $scroll = $middle->Scrollbar(
            -orient      => 'vertical',
            -command     => ['yview', $names],
            -relief      => 'sunken',
            -borderwidth => 2,
        );
        $names->configure(-yscrollcommand => ['set',$scroll]);
        $names->pack(-in => $middle_left, -side => 'left');
        $scroll->pack(-in => $middle_left, -side => 'right', -fill => 'y');

        while(<FOO>) {
            chomp;
            next if /^!/;
            my @a = split;
            my $color = join(' ', @a[3 .. $#a]);
            my $hex;
	    eval { $hex = $middle->Hex($color); };
            if ($@) {
		#print STDERR "unknown color: '$color'\n";
	        if ($@ =~ /unknown color name "/) {
		    next;
		} else {
		    chomp $@;
		    die $@;
		}
            }
            if (!exists($seen_names{$hex}) ||
                length($seen_names{$hex}) > length($color)) {
                $seen_names{$hex} = $color;
                $names->insert('end', $color);
            }
        }
        close FOO;
        last;
    }

    # Create the three scales for editing the color, and the entry for typing
    # in a color value.

    my $middle_middle = $middle->Frame;
    $middle_middle->pack(-side => 'left', -expand => 1, -fill => 'y');
    my $mcm1 = $middle_middle->Optionmenu(-variable => \$middle->{'color_space'},
                                  -command => [ $middle, 'color_space'],
                                  -relief  => 'raised',
                                  -options => [ ['HSB color space' => 'hsb'],
                                                ['RGB color space' => 'rgb'],
                                                ['CMY color space' => 'cmy']]);
    $mcm1->pack(-side => 'top', -fill => 'x');

    my(@middle_middle, @label, @scale);
    $middle_middle[0] = $middle_middle->Frame;
    $middle_middle[1] = $middle_middle->Frame;
    $middle_middle[2] = $middle_middle->Frame;
    $middle_middle[3] = $middle_middle->Frame;
    $middle_middle[0]->pack(-side => 'top', -expand => 1);
    $middle_middle[1]->pack(-side => 'top', -expand => 1);
    $middle_middle[2]->pack(-side => 'top', -expand => 1);
    $middle_middle[3]->pack(-side => 'top', -expand => 1, -fill => 'x');
    $middle->{'Labels'} = ['zero','one','two'];
    foreach $i (0..2) {
        $label[$i] = $middle->Label(-textvariable => \$middle->{'Labels'}[$i]);
        $scale[$i] = $middle->Scale(
            -from     => 0,
            -to       => 1000,
            '-length' => '6c',
            -orient   => 'horizontal',
            -command  => [\&scale_changed, $middle],
        );
        $scale[$i]->pack(
            -in     => $middle_middle[$i],
            -side   => 'top',
            -anchor => 'w',
        );
        $label[$i]->pack(
            -in     => $middle_middle[$i],
            -side   => 'top',
            -anchor => 'w',
        );
    }
    my $nameLabel = $middle->Label(-text => 'Name:');
    $middle->{'Entry'} = '';
    my $name = $middle->Entry(
        -relief       => 'sunken',
        -borderwidth  => 2,
        -textvariable => \$middle->{'Entry'},
        -width        => 10,
# For some reason giving this font causes problems at end of t/create.t
#       -font         => '-*-Courier-Medium-R-Normal--*-120-*-*-*-*-*-*'
    );

    $nameLabel->pack(-in => $middle_middle[3], -side => 'left');
    $name->pack(
        -in     => $middle_middle[3],
        -side   => 'right',
        -expand => 1,
        -fill   => 'x',
    );
    $name->bind('<Return>' => [ $middle, 'color', Ev(['get'])]);

    # Create the color display swatch on the right side of the window.

    my $middle_right = $middle->Frame;
    $middle_right->pack(
        -side   => 'left',
        -pady   => '.25c',
        -padx   => '.25c',
        -anchor => 's',
    );
    my $swatch = $middle->Canvas(
        -width  => '2.5c',
        -height => '5c',
    );
    my $swatch_item = $swatch->create('oval', '.5c', '.3c', '2.26c', '4.76c');

    my $value = $middle->Label(
        -textvariable => \$middle->{'color'},
        -width        => 13,
        -font         => '-*-Courier-Medium-R-Normal--*-120-*-*-*-*-*-*'
    );

    $swatch->pack(
        -in     => $middle_right,
        -side   => 'top',
        -expand => 1,
        -fill   => 'both',
    );
    $value->pack(-in => $middle_right, -side => 'bottom', -pady => '.25c');

    $middle->ConfigSpecs(
        '-color_space'  => ['METHOD', undef, undef, 'hsb'],
        '-initialcolor' => '-color',
        '-color'        => ['METHOD', 'background', 'Background',
                               $middle->cget('-background')]
    );

    $middle->{'swatch'} = $swatch;
    $middle->{'swatch_item'} = $swatch_item;
    $middle->{'scale'} = [@scale];
    $middle->{'red'} = 0;
    $middle->{'blue'} = 0;
    $middle->{'green'} = 0;

}

sub _rgbTxtPath
{
 require Tk::Config;
 my(@xlibpath) = map { s/^-L//; "$_/X11/rgb.txt" }
                 split /\s+/, $Tk::Config::xlib;
 (
  @xlibpath,
  '/usr/local/lib/X11/rgb.txt',
  '/usr/lib/X11/rgb.txt',
  '/usr/X11R6/lib/X11/rgb.txt',
  '/usr/local/X11R5/lib/X11/rgb.txt',
  '/X11/R5/lib/X11/rgb.txt',
  '/X11/R4/lib/rgb/rgb.txt',
  '/usr/openwin/lib/X11/rgb.txt',
  '/usr/share/X11/rgb.txt', # This is the Debian location
  '/usr/X11/share/X11/rgb.txt', # seen on a Mac OS X 10.5.1 system
  '/usr/X11R6/share/X11/rgb.txt', # seen on a OpenBSD 4.2 system
  '/etc/X11R6/rgb.txt',
  '/etc/X11/rgb.txt', # seen on HP-UX 11.31
 );       
}

sub Hex
{
 my $w = shift;
 my @rgb = (@_ == 3) ? @_ : $w->rgb(@_);
 sprintf('#%04x%04x%04x',@rgb)
}

sub color_space {

    my($objref, $space) = @_;

    if (@_ > 1)
     {
      my %Labels = ( 'rgb' => [qw(Red Green Blue)],
                     'cmy' => [qw(Cyan Magenta Yellow)],
                     'hsb' => [qw(Hue Saturation Brightness)] );

      # The procedure below is invoked when a new color space is selected. It
      # changes the labels on the scales and re-loads the scales with the
      # appropriate values for the current color in the new color space

      $space = 'hsb' unless (exists $Labels{$space});
      my $i;
      for $i (0..2)
       {
        $objref->{'Labels'}[$i] = $Labels{$space}->[$i];
       }
      $objref->{'color_space'} = $space;
      $objref->afterIdle(['set_scales',$objref]) unless ($objref->{'pending'}++);
     }
 return $objref->{'color_space'};
} # color_space

sub hsvToRgb {

    # The procedure below converts an HSB value to RGB.  It takes hue,
    # saturation, and value components (floating-point, 0-1.0) as arguments,
    # and returns a list containing RGB components (integers, 0-65535) as
    # result.  The code here is a copy of the code on page 616 of
    # "Fundamentals of Interactive Computer Graphics" by Foley and Van Dam.

    my($hue, $sat, $value) = @_;
    my($v, $i, $f, $p, $q, $t);

    $v = int(65535 * $value);
    return ($v, $v, $v) if $sat == 0;
    $hue *= 6;
    $hue = 0 if $hue >= 6;
    $i = int($hue);
    $f = $hue - $i;
    $p = int(65535 * $value * (1 - $sat));
    $q = int(65535 * $value * (1 - ($sat * $f)));
    $t = int(65535 * $value * (1 - ($sat * (1 - $f))));
    return ($v, $t, $p) if $i == 0;
    return ($q, $v, $p) if $i == 1;
    return ($p, $v, $t) if $i == 2;
    return ($p, $q, $v) if $i == 3;
    return ($t, $p, $v) if $i == 4;
    return ($v, $p, $q) if $i == 5;

} # end hsvToRgb

sub color
{
 my ($objref,$name) = @_;
 if (@_ > 1 && defined($name) && length($name))
  {
      if ($name eq 'cancel') {
	  $objref->{color} = undef;
	  return;
      }
   my ($format, $shift);
   my ($red, $green, $blue);

   if ($name !~ /^#/)
    {
     ($red, $green, $blue) = $objref->{'swatch'}->rgb($name);
    }
   else
    {
       my $len = length $name;
       if($len == 4) { $format = '#(.)(.)(.)'; $shift = 12; }
         elsif($len == 7) { $format = '#(..)(..)(..)'; $shift = 8; }
           elsif($len == 10) { $format = '#(...)(...)(...)'; $shift = 4; }
             elsif($len == 13) { $format = '#(....)(....)(....)'; $shift = 0; }
       else {
	 $objref->BackTrace(
	   "ColorEditor error:  syntax error in color name \"$name\"");
	 return;
       }
       ($red,$green,$blue) = $name =~ /$format/;
       # Looks like a call for 'pack' or similar rather than eval
       eval "\$red = 0x$red; \$green = 0x$green; \$blue = 0x$blue;";
       $red   = $red   << $shift;
       $green = $green << $shift;
       $blue  = $blue  << $shift;
    }
   $objref->{'red'} = $red;
   $objref->{'blue'} = $blue;
   $objref->{'green'} = $green;
   my $hex = sprintf('#%04x%04x%04x', $red, $green, $blue);
   $objref->{'color'} = $hex;
   $objref->{'Entry'} = $name;
   $objref->afterIdle(['set_scales',$objref]) unless ($objref->{'pending'}++);
   $objref->{'swatch'}->itemconfigure($objref->{'swatch_item'},
            -fill => $objref->{'color'});
  }
 return $objref->{'color'};
}

sub rgbToHsv {

    # The procedure below converts an RGB value to HSB.  It takes red, green,
    # and blue components (0-65535) as arguments, and returns a list
    # containing HSB components (floating-point, 0-1) as result.  The code
    # here is a copy of the code on page 615 of "Fundamentals of Interactive
    # Computer Graphics" by Foley and Van Dam.

    my($red, $green, $blue) = @_;
    my($max, $min, $sat, $range, $hue, $rc, $gc, $bc);

    $max = ($red > $green) ? (($blue > $red) ? $blue : $red) :
      (($blue > $green) ? $blue : $green);
    $min = ($red < $green) ? (($blue < $red) ? $blue : $red) :
      (($blue < $green) ? $blue : $green);
    $range = $max - $min;
    if ($max == 0) {
        $sat = 0;
    } else {
        $sat = $range / $max;
    }
    if ($sat == 0) {
        $hue = 0;
    } else {
        $rc = ($max - $red) / $range;
        $gc = ($max - $green) / $range;
        $bc = ($max - $blue) / $range;
        $hue = ($max == $red)?(0.166667*($bc - $gc)):
          (($max == $green)?(0.166667*(2 + $rc - $bc)):
           (0.166667*(4 + $gc - $rc)));
    }
    $hue += 1 if $hue < 0;
    return ($hue, $sat, $max/65535);

} # end rgbToHsv

sub scale_changed {

    # The procedure below is invoked when one of the scales is adjusted.  It
    # propagates color information from the current scale readings to
    # everywhere else that it is used.

    my($objref) = @_;

    return if $objref->{'updating'};
    my ($red, $green, $blue);

    if($objref->{'color_space'} eq 'rgb') {
        $red = int($objref->{'scale'}->[0]->get * 65.535 + 0.5);
        $green = int($objref->{'scale'}->[1]->get * 65.535 + 0.5);
        $blue = int($objref->{'scale'}->[2]->get * 65.535 + 0.5);
    } elsif($objref->{'color_space'} eq 'cmy') {
        $red = int(65535 - $objref->{'scale'}->[0]->get * 65.535 + 0.5);
        $green = int(65535 - $objref->{'scale'}->[1]->get * 65.535 + 0.5);
        $blue = int(65535 - $objref->{'scale'}->[2]->get * 65.535 + 0.5);
    } else {
        ($red, $green, $blue) = hsvToRgb($objref->{'scale'}->[0]->get/1000.0,
                                         $objref->{'scale'}->[1]->get/1000.0,
                                         $objref->{'scale'}->[2]->get/1000.0);
    }
    $objref->{'red'} = $red;
    $objref->{'blue'} = $blue;
    $objref->{'green'} = $green;
    $objref->color(sprintf('#%04x%04x%04x', $red, $green, $blue));
    $objref->idletasks;

} # end scale_changed

sub set_scales {

    my($objref) = @_;
    $objref->{'pending'} = 0;
    $objref->{'updating'} = 1;

    # The procedure below is invoked to update the scales from the current red,
    # green, and blue intensities.  It's invoked after a change in the color
    # space and after a named color value has been loaded.

    my($red, $blue, $green) = ($objref->{'red'}, $objref->{'blue'},
                               $objref->{'green'});

    if($objref->{'color_space'} eq 'rgb') {
        $objref->{'scale'}->[0]->set(int($red / 65.535 + 0.5));
        $objref->{'scale'}->[1]->set(int($green / 65.535 + 0.5));
        $objref->{'scale'}->[2]->set(int($blue / 65.535 + 0.5));
    } elsif($objref->{'color_space'} eq 'cmy') {
        $objref->{'scale'}->[0]->set(int((65535 - $red) / 65.535 + 0.5));
        $objref->{'scale'}->[1]->set(int((65535 - $green) / 65.535 + 0.5));
        $objref->{'scale'}->[2]->set(int((65535 - $blue) / 65.535 + 0.5));
    } else {
        my ($s1, $s2, $s3) = rgbToHsv($red, $green, $blue);
        $objref->{'scale'}->[0]->set(int($s1 * 1000.0 + 0.5));
        $objref->{'scale'}->[1]->set(int($s2 * 1000.0 + 0.5));
        $objref->{'scale'}->[2]->set(int($s3 * 1000.0 + 0.5));
    }
    $objref->{'updating'} = 0;

} # end set_scales

package Tk::ColorDialog;
require Tk::Toplevel;
use base  qw(Tk::Toplevel);

Construct Tk::Widget 'ColorDialog';

sub Accept
{
 my $cw  = shift;
 $cw->withdraw;
 $cw->{'done'} = 1;
}

sub Cancel
{
 my $cw  = shift;
# $cw->configure(-color => undef);
 $cw->configure(-color => 'cancel');
 $cw->Accept;
}

sub Populate
{
 my ($cw,$args) = @_;
 $cw->SUPER::Populate($args);
 $cw->protocol('WM_DELETE_WINDOW' => [ 'Cancel' => $cw ]);
 $cw->transient($cw->Parent->toplevel);
 $cw->withdraw;
 my $sel = $cw->ColorSelect;
 my $accept = $cw->Button(-text => 'Accept', -command => ['Accept', $cw]);
 my $cancel = $cw->Button(-text => 'Cancel', -command => ['Cancel', $cw]);
 Tk::grid($sel);
 Tk::grid($accept,$cancel);
 $cw->ConfigSpecs(DEFAULT => [$sel]);
}

sub Show
{
 my $cw = shift;
 $cw->configure(@_) if @_;
 $cw->Popup();
 $cw->OnDestroy(sub { $cw->{'done'} = 0 }); # auto-cancel
 $cw->waitVariable(\$cw->{'done'});
 if (Tk::Exists($cw))
  {
   $cw->withdraw;
   $cw->cget('-color');
  }
 else
  {
   undef;
  }
}

package Tk::ColorEditor;

use vars qw($VERSION $SET_PALETTE);
$VERSION = '4.009'; # was: sprintf '4.%03d', q$Revision: #8 $ =~ /\D(\d+)\s*$/;

use Tk qw(lsearch Ev);
use Tk::Toplevel;
use base  qw(Tk::Toplevel);
use Tk::widgets qw(Pixmap);
Construct Tk::Widget 'ColorEditor';

use Tk::Dialog;
use Tk::Pretty;

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

