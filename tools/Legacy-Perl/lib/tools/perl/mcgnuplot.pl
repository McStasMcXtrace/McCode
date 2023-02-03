# Library of McStas plotting functions (gnuplot related)
#
#     This file is part of the McStas neutron ray-trace simulation package
#     Copyright (C) 1997-2004, All rights reserved
#     Risoe National Laborartory, Roskilde, Denmark
#     Institut Laue Langevin, Grenoble, France
#
#     This program is free software; you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation; version 3 of the License.
#
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program; if not, write to the Free Software
#     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
#

require "mcplotlib.pl";
use IPC::Open2;
use Config;

my $pid = 0;
my $instr_inf, $sim_inf, $datalist, $sim_error;
my $TkUp = 0;
my $termnum = 0;
my $default_term;
# This part should perhaps go to mccode_config.perl
if ($Config{'osname'} eq 'MSWin32') {
  $default_term="windows";
  $termnum ="";
} elsif ($Config{'osname'} eq 'darwin') {
  $default_term="aqua";
} else {
  $default_term="x11";
}
$default_term = $MCSTAS::mcstas_config{GNUDEV};
my $term = $default_term;
my @monitornames;
my $currentmon = 'Overview';
my $monitorlist;
my @gformats = ('psc','ps','gif','png');
my $gformat = 'psc';
my @suffixes = ('ps','ps','gif','png');
my $suffix = '.ps';
my @terms = ('postscript color','postscript','gif','png');

sub gnuplotit {
  # Set up pipe to gnuplot, do first overview plot
  # and build the Tk based gui.
  if ($pid == 0) {
    $pid = open2(FGNUPLOT, GNUPLOT, $MCSTAS::mcstas_config{GNUPLOT});
    # Allow gnuplot time to settle.
    sleep 2;
  }
  ($instr_inf, $sim_inf, $datalist, $sim_error) = read_sim_file($file);
  die "No data in simulation file '$file'"
      unless @$datalist;
  print GNUPLOT "\nset term $default_term\n";
  overview_gnuplot();
  if ($TkUp == 0) {
    Tkgui();
  } else {
    $currentmon = 'Overview';
    $monitorlist->configure(-options =>[@monitornames]);
  } 
}

sub overview_gnuplot {
  my ($nx, $ny) = calc_panel_size(int(@$datalist));
  my $type;
  @monitornames = ('Overview');
  # On Win32, multiple windows are not supported...
  if (int(@$datalist) < 100 || ($default_term eq "windows")) {
    $type = 0;
    print GNUPLOT "\nset multiplot layout $ny,$nx\n";
  } else {
    $type = 1;
  }
  my $info;
  if (($term eq $default_term) && !($default_term eq "windows")) {
    $termnum=0;
  }
  my $index=0;
  for $info (@$datalist) {
    if ($type) {
      print GNUPLOT "\nset term $term $termnum\n";
    }
    push @monitornames, $info->{'Component'};
    if ($info->{'Origin'} eq "scanfile") {
      $index++;
    }
    gnuplot_dat_info($info, $index,$nx*$ny>1);
    if ($type)  {
      $termnum++;
    }
  }
  if (!$type)  {
    print GNUPLOT "\nunset multiplot\n";
  }
}

sub gnuplot_single {
  if ($currentmon eq "Overview") {
    overview_gnuplot();
  } else {
    if (!($default_term eq "windows")) {
      $termnum++;
    }
    print GNUPLOT "\nset term $term $termnum\n";
    my $index=0;
    for $info (@$datalist) {
      if ($info->{'Origin'} eq "scanfile") {
	$index++;
      }
      if ($info->{'Component'} eq $currentmon) {
        gnuplot_dat_info($info, $index,0);
      }
    }
  }
}

sub gnuplot_save {
  my $i;
  for ($i=0; $i<@gformats; $i++) {
    if ($gformat eq $gformats[$i]) {
      $term = $terms[$i];
      $suffix = $suffixes[$i];
    }
  }
  print GNUPLOT "\nset term $term\n";
  if ($currentmon eq "Overview") {
    print GNUPLOT "\nset output \"$file.$suffix\"\n";
    overview_gnuplot();
    print "Saved overview plot \"$file.$suffix\"\n";
  } else {
    if (!($default_term eq "windows")) {
      $termnum++;
    } 
    print GNUPLOT "\nset term $term\n";
    my $index=0;
    for $info (@$datalist) {
      if ($info->{'Origin'} eq "scanfile") {
	$index++;
      }
      if ($info->{'Component'} eq $currentmon) {
     	print GNUPLOT "\nset output \"$currentmon.$suffix\"\n";
        gnuplot_dat_info($info, $index,0);
	print "Saved hardcopy \"$currentmon.$suffix\"\n";
      }
    }
  }  
  $term = $default_term;
  print GNUPLOT "\nset term $term\n";
}

sub gnuplot_dat_info {
    my ($info, $index, $overview) = @_;
    if ($index == 0) {
      $index = 1;
    }
    my $type = $info->{'Type'};
    if($type =~ /^\s*array_2d\s*\(\s*([0-9]+)\s*,\s*([0-9]+)\s*\)\s*$/i) {
      gnuplot_array_2d($info, $1, $2, $overview);
    }elsif($type =~ /^\s*array_1d\s*\(\s*([0-9]+)\s*\)\s*$/i) {
      gnuplot_array_1d($info, $1, $index, $overview);
    } else {
      print "Warning: Unimplemented plot type '$type' in file '$info->{Filename}' (plot_dat_info)";
    }
}

sub gnuplot_array_2d {
  my ($info,$m,$n,$overview) = @_;
  print GNUPLOT "\nset view map\n";
  if ($overview) {
    print GNUPLOT "\nunset colorbox\n";
    print GNUPLOT "\nunset xtics\n";
    print GNUPLOT "\nunset ytics\n";
  } else {
    print GNUPLOT "\nset colorbox\n";
    print GNUPLOT "\nset xlabel \"$info->{'Xlabel'}\"\nset ylabel \"$info->{'Ylabel'}\"\n";
    print GNUPLOT "\nset xtics\n";
    print GNUPLOT "\nset ytics\n";
  }
  print GNUPLOT "\nset title \"$info->{'Component'}\"\n";
  my ($x0,$x1,$y0,$y1) = @{$info->{'Limits'}};
  my ($dx,$dy) = (($x1 - $x0)/($m-1), ($y1 - $y0)/($n-1));
  print GNUPLOT "\nsplot \"$info->{Filename}\" using ($x0+\$1*$dx):($y0+\$2*$dy):3 matrix index 0 w image notitle\n";
  print "\nsplot \"$info->{Filename}\" using ($x0+\$1*$dx):($y0+\$2*$dy):3 matrix index 0 w image notitle\n";
}


sub gnuplot_array_1d {
  my ($info,$m,$n,$overview) = @_; 
  print GNUPLOT "\nset title \"$info->{'Component'}\"\n";  
  if ($overview) {
    print GNUPLOT "\nunset xtics\n";
    print GNUPLOT "\nunset ytics\n";
  } else {
    print GNUPLOT "\nset xlabel \"$info->{'Xlabel'}\"\nset ylabel \"$info->{'Ylabel'}\"\n";
    print GNUPLOT "\nset xtics\n";
    print GNUPLOT "\nset ytics\n";
  }
  my $int = 2*$n;
  my $err = 2*$n+1;
  print GNUPLOT "\nplot \"$info->{Filename}\" u 1:$int with lines notitle, ";
  print GNUPLOT " \"$info->{Filename}\" u 1:$int:$err with errorbars notitle\n";
}

sub Tkgui {
    use Tk;
    use Tk::Toplevel;
    use Tk::DirTree;
    use Tk::Balloon;
    $continue = 0;
    my $win = new MainWindow(-title => "McGnuplot");
    eval { # Try specified color palette...
      $win -> setPalette($MCSTAS::mcstas_config{'TKPALETTE'});
    }; 
    if ($@) { # or continue with system default if that failed.
      printf "Specified colorscheme '$MCSTAS::mcstas_config{'TKPALETTE'}' failed. Using system default.\n";
    }
    
    if (!($MCSTAS::mcstas_config{'TKFONT'} eq "")) { # Only try loading a font if non-empty string is defined
      eval { # Try loading specified font...
	$win->optionAdd("*font", $MCSTAS::mcstas_config{'TKFONT'});
	$win->optionAdd("*borderWidth", 1);
      };
      if ($@) { # or continue with system default if that failed
	printf "Specified font '$MCSTAS::mcstas_config{'TKFONT'}' failed. Using system default.\n";
      }
    }
    build_gui($win);
    $TkUp=1;
    MainLoop;
}

sub build_gui {
    # When mcdaemon is run without any input parms, we'll build a gui
    # to set the parameters.
    my ($win) = @_;
    my $topframe = $win->Frame(-relief => 'raised', -borderwidth => 2);
    my $b = $win->Balloon(-state => 'balloon');
    $topframe->pack(-side => "top", -fill => "both", -ipady => 3, -ipadx => 3);
    my $tmp1=$topframe->Label(-text => "Datafile:", -fg => 'blue', -anchor => 'w',
				     -justify => "center")->pack(-side => "left");
    $b->attach($tmp1, -balloonmsg => "Chosen Datafile");
    my $filelabel = $topframe->Label(-width => 60,
				    -textvariable => \$file)->pack(-side => "left");
    my $fileselect = $topframe->Button(-text => "Select", -command => [\&select_file, $win])->pack(-side => "right");
    $b->attach($fileselect, -balloonmsg => "Click here to select a new file to plot (e.g. mcstas.sim)");
    #my $dirselect = $topframe->Button(-text => "Dir", -command => [\&select_dir])->pack(-side => "left");
    #$b->attach($dirselect, -balloonmsg => "Click here to select a directory to monitor");
    my $midframe = $win->Frame(-relief => 'raised', -borderwidth => 2);
    $midframe->pack(-side => "top", -fill => "both", -ipady => 3, -ipadx => 3);
    my $tmp2= $midframe->Label(-text => "Plot monitor: ", -anchor => 'w',
		     -justify => "center")->pack(-side => "left");
    $b->attach($tmp2, -balloonmsg => "Sets the time between changes check");
    $monitorlist = $midframe->Optionmenu(-textvariable => \$currentmon, -options =>
					 [@monitornames])->pack(-side => 'left');
    my $tmp3=$midframe->Button(-text => "Display", -anchor => 'w',
		     -justify => "center", -command => [\&gnuplot_single])->pack(-side => "left");
    $b->attach($tmp3, -balloonmsg => "Plot selected monitor on screen");
    my $tmp4=$midframe->Button(-text => "Save, format ->", -anchor => 'w',
		     -justify => "center", -command => [\&gnuplot_save])->pack(-side => "left");
    $b->attach($tmp4, -balloonmsg => "Save selected monitor to file");
    my $graphics = $midframe->Optionmenu(-textvariable => \$gformat, -options =>
					 [@gformats])->pack(-side => 'left');
    my $bottomframe = $win->Frame(-relief => 'raised', -borderwidth => 2);
    $bottomframe->pack(-side => "top", -fill => "both", -ipady => 3, -ipadx => 3);

    $bottomframe->Button(-text => "Exit", -fg => 'green', -command => sub {$continue=1; $win->destroy; print GNUPLOT "\n\nexit\n";})->pack(-side => "right", -anchor => "w");
}

sub select_file {
    my ($w) = @_;
    my $filename = $w->getOpenFile(-title => "Select file to plot", -initialdir => getcwd());
    if (!$filename == '') {
	$file = $filename;
	gnuplotit();
    }
}

sub select_dir {
    my $top = new MainWindow;
    $top->withdraw;

    my $t = $top->Toplevel;
    $t->title("Choose dir to monitor:");
    my $ok = 0;
    my $f = $t->Frame->pack(-fill => "x", -side => "bottom");

    my $curr_dir = getcwd();

    my $d;
    $d = $t->Scrolled('DirTree',
		      -scrollbars => 'osoe',
		      -width => 35,
		      -height => 20,
		      -selectmode => 'browse',
		      -exportselection => 1,
		      -browsecmd => sub { $curr_dir = shift },
		      -command   => sub { $ok = 1 },
		      )->pack(-fill => "both", -expand => 1);
    $f->Button(-text => 'Ok',
	       -command => sub { $ok =  1 })->pack(-side => 'left');
    $f->Button(-text => 'Cancel',
	       -command => sub { $ok = -1 })->pack(-side => 'left');

    $f->waitVariable(\$ok);

    if ($ok == 1) {
	$filename = $curr_dir;
    }
    $top->destroy;
}

1;
