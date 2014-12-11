package Tk::ColorSelect; # XXX why is this the Tk::ColorSelect package?
use strict;

use vars qw($VERSION);
$VERSION = '4.014';

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

1;
