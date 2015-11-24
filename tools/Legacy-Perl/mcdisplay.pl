#! /usr/bin/perl
# Removed -w to get rid of "used only once" warnings

# In emacs, please make this -*- perl -*- mode. Thanks.

# Enhanced mcdisplay script for --trace output from McStas simulation.
#
# Implements graphic display using either
# PGPLOT
# Matlab
# VRML
# Mantid
#
# PW 20030320
#
#   This file is part of the McStas neutron ray-trace simulation package
#   Copyright (C) 1997-2004, All rights reserved
#   Risoe National Laborartory, Roskilde, Denmark
#   Institut Laue Langevin, Grenoble, France
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; version 2 of the License.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program; if not, write to the Free Software
#   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA



# Config module needed for Win32 vs unix setup.
# PW 20030320
use Config;

BEGIN {

# Default configuration (for all high level perl scripts)
# Included from perl_env_header.pl

    ENV_HEADER

    # custom configuration (this script)

    # If this is Win32, load OLE related modules -
    # we can not talk to Matlab using pipe on Win32 :(
    # PW 20030314
    if ($Config{'osname'} eq 'MSWin32') {
        require Win32::OLE;
        require Win32::OLE::Variant;
    }
}

use lib $MCSTAS::perl_dir;
use lib $MCSTAS::perl_modules;
require "mccode_config.perl";

# Overload with user's personal config
if ($ENV{"HOME"} && -e $ENV{"HOME"}."/.".$MCSTAS::mcstas_config{'MCCODE'}."/mccode_config.perl") {
  print "mcdisplay: reading local $MCSTAS::mcstas_config{'MCCODE'} configuration from " . $ENV{"HOME"}."/.".$MCSTAS::mcstas_config{'MCCODE'}."/mccode_config.perl\n";
  require $ENV{"HOME"}."/.".$MCSTAS::mcstas_config{'MCCODE'}."/mccode_config.perl";
}

require "mcrunlib.pl";
# IPC can probably be used safely, exists on sysv type systems,
# linux, Win32. Will investigate further regarding portability.
# PW 20030404
use IPC::Open2;
use Math::Trig;

$magnification = 1;
$zooming = 0;

my (%transformations, @components);

sub max {
    my ($max, @vars) = @_;
    for (@vars) {
        $max = $_ if $_ > $max;
    }
    return $max;
}

sub min {
    my ($min, @vars) = @_;
    for (@vars) {
        $min = $_ if $_ < $max;
    }
    return $min;
}

sub read_instrument {
    my ($in) = @_;
    my ($st, $comp);
    my $compheader;
    my $mantidfirst=1;
    my $mantidcount=-1;
    my $mantidcount2=-1;
    my $mantidlines=""; # For visualizing multiline and circle geometry
    my $mantidlinecount=0;
    my $mantidtypebuffer=""; # For building up assembly of multiline geometry etc.
    $st = 0;
    @components = ();
    while(<$in>) {
        if($st == 0 && /^INSTRUMENT:/) {
            # Start of instrument description.
            $st = 1;
            if ($MCSTAS::mcstas_config{'PLOTTER'} =~ /Matlab/i) {
              # Initialize matlab struct...
              write_process("if ~exist('mcdisplay'), addpath('$MCSTAS::perl_dir/../matlab'); end\n");
              write_process("mcdisplay('Init');\n");
              write_process("global INSTRUMENT;\n");
              write_process("INSTRUMENT.descr='$sim';\n");
              # Possibly, set firstcomp + lastcomp
              if ($first) { write_process("INSTRUMENT.firstcomp='$first';\n"); }
              if ($last)  { write_process("INSTRUMENT.lastcomp ='$last';\n");  }
            }
	    if ($MCSTAS::mcstas_config{'PLOTTER'} =~ /mantid/i) {
	      # Header for the IDF file
	      my $last_mod_time = (stat ($sim_cmd))[9];
	      write_process("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
	      write_process("<!-- IDF generated using McStas McDisplay and the Mantid backend -->\n");
	      write_process("<!-- For help on the notation used to specify an Instrument Definition File see http://www.mantidproject.org/IDF -->\n");
	      write_process("<instrument name=\"".$sim_cmd."\" valid-from   =\"1900-01-31 23:59:59\"\n");
	      write_process("valid-to     =\"2100-01-31 23:59:59\" last-modified=\"".localtime($last_mod_time)."\">\n");
	      write_process("<defaults>\n\t<length unit=\"meter\"/>\n\t<angle unit=\"degree\"/>\n\t<reference-frame>\n");
	      write_process("\t\t<!-- The z-axis is set parallel to and in the direction of the beam. The y-axis points up and the coordinate system is right handed. -->\n");
	      write_process("\t\t<along-beam axis=\"z\"/>\n");
	      write_process("\t\t<pointing-up axis=\"y\"/>\n");
	      write_process("\t\t<handedness val=\"right\"/>\n");
	      write_process("\t</reference-frame>\n");
	      write_process("\t\t<default-view axis-view=\"z-\"/>\n");
	      write_process("\t</defaults>\n\n");
	      write_process("<!-- LIST OF PHYSICAL COMPONENTS (which the instrument consists of) -->\n\n");
	      # Fallback, dummy component for Mantid use
	      write_process("<type name=\"Othercomp\"></type>\n\n");
	    }
	    if ($MCSTAS::mcstas_config{'PLOTTER'} =~ /VRML/i) {
	    my @argv=@ARGV;
	    for ($i=0; $i<@ARGV; $i++) {
	    	$argv[$i] =~ s!\"!\'!g;
	    }
		# Default viewpoint, 10 meters along z.
		write_process("#VRML V2.0 utf8\n
# Format: VRML 2.0\n
# Output from mcdisplay from the McStas package, see http://www.mcstas.org
# use freeWRL, openvrml, vrmlview, CosmoPlayer, Cortona, Octaga... to view file
#\n# Instrument used was $sim_cmd. Full cmdline was:\n#
# mcdisplay @ARGV
#\n# Please rerun instrument with -i option to get more info.\n#\n
WorldInfo {
  title \"McStas: $sim_cmd instrument\"
  info [ \"URL:    http://www.mcstas.org/\"
    \"Editor: mcdisplay @argv\"
    \"Creator:$sim_cmd simulation (McStas)\" ]
}
Viewpoint {
  description \"Default\"
   position 0 0.2 -1
  orientation 0 1 0 3.14
  jump FALSE
}
Background {
  skyAngle [ 1.57 1.57]
  skyColor [0 0 1, .1 .1 .1, 0.1 0 0]
}\n");
		# Definition of Origin + coordinate system arrows
		write_process("# Sphere at the origin
Shape {
  appearance Appearance {
  material Material {
  diffuseColor 1.0 1.0 0.0
  transparency 0.5 } }
  geometry Sphere { radius 0.01 }
}
# Axis-parallel arrows of length 1 metre
DEF ARROW Group {
children [
Transform {
  translation 0 0.5 0
  children [
  Shape {
  appearance DEF ARROW_APPEARANCE Appearance {
  material Material {
  diffuseColor .3 .3 1
  emissiveColor .1 .1 .33
  }
  }
  geometry Cylinder {
  bottom FALSE
  radius .005
  height 1
  top FALSE
  } } ] }
Transform {
  translation 0 1 0
  children [
  DEF ARROW_POINTER Shape {
  geometry Cone {
  bottomRadius .05
  height .1
  }
  appearance USE ARROW_APPEARANCE
  } ] }
] }
# the arrow along X axis
Transform {
  translation 0 0 0
  rotation 1 0 0 1.57
  children [
  Group {
  children [
  USE ARROW
  ] } ] }
# the arrow along Z axis
Transform {
  translation 0 0 0
  rotation 0 0 1 -1.57
  children [
  Group {
  children [
  USE ARROW
  ] } ] }
# the Y label (which is vertical)
DEF Y_Label Group {
children [
Transform {
  translation 0 1 0
  children [
  Billboard {
  children [
  Shape {
  appearance DEF LABEL_APPEARANCE Appearance {
  material Material {
  diffuseColor 1 1 .3
  emissiveColor .33 .33 .1
  } }
  geometry Text {
  string [\"y\" ]
  fontStyle FontStyle {  size .2 }
  } } ] } ] }
] }
# the X label
DEF X_Label Group {
children [
Transform {
  translation 1 0 0
  children [
  Billboard {
  children [
  Shape {
  appearance DEF LABEL_APPEARANCE Appearance {
  material Material {
  diffuseColor 1 1 .3
  emissiveColor .33 .33 .1
  } }
  geometry Text {
  string [\"x\"]
  fontStyle FontStyle {  size .2 }
  } } ] } ] }
] }
# the Z label
DEF Z_Label Group {
children [
Transform {
  translation 0 0.2 1
  children [
  Billboard {
  children [
  Shape {
  appearance DEF LABEL_APPEARANCE Appearance {
  material Material {
  diffuseColor 1 1 .3
  emissiveColor .33 .33 .1
  } }
  geometry Text {
  string [\"z\"]
  fontStyle FontStyle {  size .2 }
  } } ] } ] }
] }
# The text information (header data )
DEF Header Group {
children [
Transform {
  translation 0 1.2 0
  children [
  Billboard {
  children [
  Shape {
  appearance Appearance {
  material Material {
  diffuseColor .9 0 0
  emissiveColor .9 0 0 }
  }
  geometry Text {
  string [ \"McStas: $sim_cmd\" ]
  fontStyle FontStyle {
  style \"BOLD\"
  size .2
  } } } ] } ] }
] }
\n### Instrument begins here: ###\n");

	    }
        } elsif($st == 1 && /^COMPONENT:\s*"([a-zA-Z0-9_]+)"\s*/) {
            $comp = $1;
            @components = (@components, $comp);
            if ($MCSTAS::mcstas_config{'PLOTTER'} =~ /Matlab/i) {
              # Initialize components in matlab struct:
              write_process("INSTRUMENT.name{length(INSTRUMENT.name)+1}='$comp';\n");
            }
	    if ($MCSTAS::mcstas_config{'PLOTTER'} =~ /vrml/i) {
		$compheaders{$comp}="\n########################".
		    " $comp ".
		    "########################";
	    }
        } elsif($st == 1 && /^POS:(.*)$/) {
            my @T;
            @T = split ",", $1;
            $transformations{$comp} = \@T;
	    $compcnt=scalar(@components);
            if ($MCSTAS::mcstas_config{'PLOTTER'} =~ /Matlab/i) {
              # Component position for matlab struct:
              write_process("INSTRUMENT.$comp.T=ReshapeTransform([@T]);\n");
              write_process("INSTRUMENT.$comp.j=$compcnt;\n");
              write_process("INSTRUMENT.$comp.K=cell(0);\n");
            }
            if ($MCSTAS::mcstas_config{'PLOTTER'} =~ /mantid/i) {
              # Component position for mantid:
	      my $type = "Othercomp";
	      my $isa ="";
	      if ($comp =~ /sourceMantid/i) {
	      	$isa = "is=\"Source\"";
	      	$type = "source";
	      }
	      if ($comp =~ /sampleMantid/i) {
	      	$isa = "is=\"SamplePos\"";
	      	$type = "some-sample-holder";
	      }
	      if (!($comp =~ /nD_Mantid/i)) {
	      	 # Component position for mantid - but not Monitor_nD case - and only in case of the $complete==1 flag...:
		if ($complete==1 || (!($type eq "Othercomp"))) {
		    my $angle = (180/pi)*acos(($T[3]+$T[7]+$T[11]-1)/2);
		  my $d21=$T[8]-$T[10]; my $d02=$T[9]-$T[5]; my $d10=$T[4]-$T[6];
		  my $d=sqrt($d21*$d21+$d02*$d02+$d10*$d10);
		  my $rota="";
		  if($d!=0){
		    $rota=" rot=\"".$angle."\" axis-x=\"".$d21/$d."\" axis-y=\"".$d02/$d."\" axis-z=\"".$d10/$d."\"";
		  }
		  $type="$comp-type";
		  write_process("<component type=\"".$type."\" name=\"$comp\">\n");
		  write_process("<location x=\"".$T[0]."\" y=\"".$T[1]."\" z=\"".$T[2]."\" $rota />\n</component>\n\n");
		}
	      }
            }
	    if ($MCSTAS::mcstas_config{'PLOTTER'} =~ /vrml/i) {
		if($T[0]!=0 or $T[1]!=0 or $T[2]!=0){$transforms{$comp}.= "translation $T[0] $T[1] $T[2]\n";}
		my $angle = acos(($T[3]+$T[7]+$T[11]-1)/2);
		my $d21=$T[8]-$T[10]; my $d02=$T[9]-$T[5]; my $d10=$T[4]-$T[6];
		my $d=sqrt($d21*$d21+$d02*$d02+$d10*$d10);

		if($d!=0){$transforms{$comp}.= "rotation ".$d21/$d.' '.$d02/$d.' '.$d10/$d." $angle\n";}
		$nbcomp1++;
		if($transforms{$comp})
		{
		    $compheaders{$comp}="$compheaders{$comp}".
			"\nTransform {\n".
			"$transforms{$comp}".
			"children [";
		}
		$compheaders{$comp}="$compheaders{$comp}".
		    "\nShape {\nappearance Appearance {\n\t".
		    "material Material { emissiveColor ";
		my $color = vrml_setcolor($nbcomp2,$nbcomp1);
		$compheaders{$comp}="$compheaders{$comp}".
		    "$color".
		    "}}\n".
		    "geometry IndexedLineSet {\ncoord Coordinate {\npoint [\n";
		$nbcomp2++;
		$comp = "";
	      }
	} elsif (/^MANTID_RECTANGULAR_DET:(.*)$/) {
	  # Rectangular detector, widthlims, heightlims, nx, ny, first pixel id
	  $mantidcount2++;
	  my ($xmin, $xmax, $ymin, $ymax, $nx, $ny, $pixelmin)  = split ",", $1;
	  my $dx,$dy;
	  $dx = ($xmax-$xmin)/($nx-1);
	  $dy = ($ymax-$ymin)/($ny-1);
	  if ($MCSTAS::mcstas_config{'PLOTTER'} =~ /mantid/i) {
	      # First define a panel, cf. http://www.mantidproject.org/IDF#Creating_Rectangular_Area_Detectors
	      my $type = "MonNDtype-$mantidcount2";
	      my $angle = (180/pi)*acos(($transformations{$comp}[3]+$transformations{$comp}[7]+$transformations{$comp}[11]-1)/2);
	      my $d21=$transformations{$comp}[8]-$transformations{$comp}[10]; my $d02=$transformations{$comp}[9]-$transformations{$comp}[5]; my $d10=$transformations{$comp}[4]-$transformations{$comp}[6];
	      my $d=sqrt($d21*$d21+$d02*$d02+$d10*$d10);
	      my $rota="";
	      if($d!=0){
		$rota=" rot=\"".$angle."\" axis-x=\"".$d21/$d."\" axis-y=\"".$d02/$d."\" axis-z=\"".$d10/$d."\"";
	      }
	      write_process("\n<component type=\"".$type."\" name=\"$comp\" idstart=\"");
	      write_process(${pixelmin}+0);
	      write_process("\" idfillbyfirst=\"x\" idstepbyrow=\"".$nx."\">\n");
	      write_process("\t<location x=\"".$transformations{$comp}[0]."\" y=\"".$transformations{$comp}[1]."\" z=\"".$transformations{$comp}[2]."\" $rota />\n</component>\n\n");

	      # Panel pixellation
	      write_process("\<type name=\"MonNDtype-$mantidcount2\" is=\"RectangularDetector\" type=\"pixel-$mantidcount2\"\n");
	      write_process("\txpixels=\"$nx\" xstart=\"".$xmin."\" xstep=\"".$dx."\"\n");
	      write_process("\typixels=\"$ny\" ystart=\"".$ymin."\" ystep=\"".$dy."\">\n</type>\n");

	      # Individual pixel
	      write_process("<type is=\"detector\" name=\"pixel-$mantidcount2\">\n");
	      write_process("\t<cuboid id=\"pixel-shape-$mantidcount2\">\n");
	      write_process("\t\t<left-front-bottom-point x=\"".$dx/(2.0)."\" y=\"".-($dy/2.0)."\" z=\"0.0\" />\n");
	      write_process("\t\t<left-front-top-point x=\"".$dx/(2.0)."\" y=\"".-($dy/2.0)."\" z=\"0.00005\" />\n");
	      write_process("\t\t<left-back-bottom-point x=\"".$dx/(-2.0)."\" y=\"".-($dy/2.0)."\" z=\"0.0\" />\n");
	      write_process("\t\t<right-front-bottom-point x=\"".$dx/(2.0)."\" y=\"".($dy/2.0)."\" z=\"0.0\" />\n");
	      write_process("\t</cuboid>\n");
	      write_process("\t<algebra val=\"pixel-shape-$mantidcount2\" />\n");
	      write_process("</type>\n\n");
	    }
	} elsif (/^MANTID_BANANA_DET:(.*)$/) {
	  # Banana detector: radius, theta-lims, y-lims, ntheta, ny, first pixel id
	  $mantidcount2++;
	  my ($radius, $tmin, $tmax, $ymin, $ymax, $nt, $ny, $pixelmin) = split ",", $1;
	  
	  my $dx,$dt,$dy, $j, $yval;
	  $dt = ($tmax-$tmin)/($nt-1);
	  $dy = ($ymax-$ymin)/($ny-1);
	  # A quick estimate of the bin-width in carthesian coords...
	  $dx = (2*3.1415*$radius*($tmax-$tmin)/360)/($nt-1);
	  if ($MCSTAS::mcstas_config{'PLOTTER'} =~ /mantid/i) {
	      # First define a panel, cf. http://www.mantidproject.org/IDF#Creating_Rectangular_Area_Detectors
	      my $type = "MonNDtype-$mantidcount2";
	      my $angle = (180/pi)*acos(($transformations{$comp}[3]+$transformations{$comp}[7]+$transformations{$comp}[11]-1)/2);
	      my $d21=$transformations{$comp}[8]-$transformations{$comp}[10]; my $d02=$transformations{$comp}[9]-$transformations{$comp}[5]; my $d10=$transformations{$comp}[4]-$transformations{$comp}[6];
	      my $d=sqrt($d21*$d21+$d02*$d02+$d10*$d10);
	      my $rota="";
	      if($d!=0){
		$rota=" rot=\"".$angle."\" axis-x=\"".$d21/$d."\" axis-y=\"".$d02/$d."\" axis-z=\"".$d10/$d."\"";
	      }
	      write_process("\n<component type=\"".$type."\" name=\"$comp\" idlist=\"".$type."-list\">\n"); 
	      write_process("\t<locations x=\"".$transformations{$comp}[0]."\" y=\"".($transformations{$comp}[1]+$ymin)."\" y-end=\"".($transformations{$comp}[1]+$ymax)."\" n-elements=\"".$ny."\" z=\"".$transformations{$comp}[2]."\" $rota /> \n");
	      write_process("</component>\n\n");
	      write_process("<type name=\"".$type."\">\n");
	      write_process("\t<component type=\"pixel-".$mantidcount2."\">\n");
	      write_process("\t\t<locations r=\"".$radius."\" t=\"".$tmin."\" t-end=\"".$tmax."\" n-elements=\"".$nt."\" rot=\"".$tmin."\" rot-end=\"".$tmax."\" axis-x=\"0.0\" axis-y=\"1.0\" axis-z=\"0.0\"/>\n");
	      write_process("\t</component>\n\n");
	      write_process("</type>\n\n");

	      # Individual pixel
	      write_process("<type is=\"detector\" name=\"pixel-$mantidcount2\">\n");
	      write_process("\t<cuboid id=\"pixel-shape-$mantidcount2\">\n");
	      write_process("\t\t<left-front-bottom-point x=\"".$dx/(2.0)."\" y=\"".-($dy/2.0)."\" z=\"0.0\" />\n");
	      write_process("\t\t<left-front-top-point x=\"".$dx/(2.0)."\" y=\"".-($dy/2.0)."\" z=\"0.00005\" />\n");
	      write_process("\t\t<left-back-bottom-point x=\"".$dx/(-2.0)."\" y=\"".-($dy/2.0)."\" z=\"0.0\" />\n");
	      write_process("\t\t<right-front-bottom-point x=\"".$dx/(2.0)."\" y=\"".($dy/2.0)."\" z=\"0.0\" />\n");
	      write_process("\t</cuboid>\n");
	      write_process("\t<algebra val=\"pixel-shape-$mantidcount2\" />\n");
	      write_process("</type>\n\n");
	      write_process("<idlist idname=\"".$type."-list\">\n");
	      write_process("\t<id start=\"".(${pixelmin}+0)."\" end=\"".(${pixelmin}+$nt*$ny-1)."\"/>");
	      write_process("</idlist>\n");
	    }
	} elsif (/^MANTID_PIXEL:(.*)$/) {
	    # OFF-geometry, individual pixels defined for Mantid use
	    my ($pixID, $firstpix, $lastpix, $polyrank, $posx, $posy, $posz, $x0, $y0, $z0, $x1, $y1, $z1, $x2, $y2, $z2, $x3, $y3, $z3) = split ",", $1;
	    $pixID =~ s/\s//g; 
	    
	    if ($polyrank != 4) {
		print STDERR "Sorry - only rank 4 polygons are supported, this rank was given:" + $polyrank + "\n";
		exit(1);
	    } else {
		if ($MCSTAS::mcstas_config{'PLOTTER'} =~ /mantid/i) {
		    if ($pixID == $firstpix) {
			# First pixel
			$mantidcount2++;
						
			# Overall component placement
			my $type = "MonNDtype-$mantidcount2";
			my $angle = (180/pi)*acos(($transformations{$comp}[3]+$transformations{$comp}[7]+$transformations{$comp}[11]-1)/2);
			my $d21=$transformations{$comp}[8]-$transformations{$comp}[10]; my $d02=$transformations{$comp}[9]-$transformations{$comp}[5]; my $d10=$transformations{$comp}[4]-$transformations{$comp}[6];
			my $d=sqrt($d21*$d21+$d02*$d02+$d10*$d10);

			my $rota="";
			if($d!=0){
			    $rota=" rot=\"".$angle."\" axis-x=\"".$d21/$d."\" axis-y=\"".$d02/$d."\" axis-z=\"".$d10/$d."\"";
			}
			write_process("\n<component type=\"".$type."\" name=\"$comp\" idlist=\"".$type."-list\">\n"); 
			write_process("\t<location x=\"".$transformations{$comp}[0]."\" y=\"".$transformations{$comp}[1]."\" z=\"".$transformations{$comp}[2]."\" $rota />\n</component>\n\n");
			
			# Start writing the ID list
			write_process("<idlist idname=\"".$type."-list\">\n");
			write_process("\t<id start=\"".(${firstpix}+0)."\" end=\"".(${lastpix}+0)."\"/>\n");
			write_process("</idlist>\n");

			# Start writing the related type...
			$mantidtypebuffer = "\n<type name=\"".$type."\">\n\t<properties />\n";
		    }
		    # Define a hexahedron
		    my $type = "MonNDtype-$mantidcount2-pix-$pixID";
		    write_process("\n<type name=\"$type\" is=\"detector\">\n");
		    write_process("\t<hexahedron id=\"hexapix-".$pixID."\">\n");
		    write_process("\t\t<left-back-bottom-point  x=\"".$x0."\" y=\"".$y0."\" z=\"".$z0."\"  />\n");
		    write_process("\t\t<left-front-bottom-point x=\"".$x1."\" y=\"".$y1."\" z=\"".$z1."\"  />\n");
		    write_process("\t\t<right-front-bottom-point x=\"".$x2."\" y=\"".$y2."\" z=\"".$z2."\"  />\n");
		    write_process("\t\t<right-back-bottom-point  x=\"".$x3."\" y=\"".$y3."\" z=\"".$z3."\"  />\n");
		    write_process("\t\t<left-back-top-point  x=\"".$x0."\" y=\"".$y0."\" z=\"".($z0+0.001)."\"  />\n");
		    write_process("\t\t<left-front-top-point  x=\"".$x1."\" y=\"".$y1."\" z=\"".($z1+0.001)."\"  />\n");
		    write_process("\t\t<right-front-top-point  x=\"".$x2."\" y=\"".$y2."\" z=\"".($z2+0.001)."\"  />\n");
		    write_process("\t\t<right-back-top-point   x=\"".$x3."\" y=\"".$y3."\" z=\"".($z3+0.001)."\"  />\n");
		    write_process("\t</hexahedron>\n");
		    write_process("\t<bounding-box>\n");
		    write_process("\t\t<x-min val=\"".min($x0,$x1,$x2,$x3)."\"/>\n");
		    write_process("\t\t<x-max val=\"".max($x0,$x1,$x2,$x3)."\"/>\n");
		    write_process("\t\t<y-min val=\"".min($y0,$y1,$y2,$y3)."\"/>\n");
		    write_process("\t\t<y-max val=\"".max($y0,$y1,$y2,$y3)."\"/>\n");
		    write_process("\t\t<z-min val=\"".min($z0,$z1,$z2,$z3)."\"/>\n");
		    write_process("\t\t<z-max val=\"".(max($y0,$y1,$y2,$y3)+0.01)."\"/>\n");
		    write_process("\t</bounding-box>\n");
		    write_process("\t<algebra val=\"hexapix-".$pixID."\" />\n");
		    write_process("</type>\n\n");
		    
		    $mantidtypebuffer = $mantidtypebuffer."<component type=\"".$type."\">\n\t<location x=\"$posx\" y=\"$posy\" z=\"$posz\" />\n</component>\n";

		    if ($pixID == $lastpix) {
			write_process($mantidtypebuffer);
			write_process("</type>\n\n");
		    }
		} else {
		    next;
		}
	      }
	}  elsif($st == 1 && /^MCDISPLAY: start$/) {
            $st = 2;                # Start of component graphics representation
	} elsif($st == 2 && /^MCDISPLAY: component ([a-zA-Z0-9_]+)/) {
            $comp = $1;
	    $compdraw{$comp} = {};
            $compdraw{$comp}{'elems'} = [];
	    #$compdraw{$comp}{'header'} = $compheader;
	    if (! ($mantidlines eq "")) {
	      # Output last lines corresponding to previous component. - needs to be done at MCDISPLAY: end also
	      write_process($mantidlines);
	      # write_process($mantidlinesend);
	      write_process("</type>\n");
            }
	    
	    # Component position for mantid:
	    my $type = "Othercomp";
	    my $isa ="";
	    if ($MCSTAS::mcstas_config{'PLOTTER'} =~ /mantid/i) {
	      if ($comp =~ /sourceMantid/i) {
		$isa = "is=\"Source\"";
		$type = "source";
	      } elsif ($comp =~ /sampleMantid/i) {
		$isa = "is=\"SamplePos\"";
		$type = "some-sample-holder";
	      } else {
		$type="$comp-type";
	      }
	      if (!($comp =~ /nD_Mantid/i)) {
		if ($complete==1 || !($type eq "Othercomp")) {
		  $mantidlines="\n<type name=\"".$comp."-type\" $isa >\n";
		  $mantidlinecount=0;
		} else {
		  $mantidlines="";
		}
	      } else {
		$mantidlines="";
		# For now, do niente...
	      }
	    }	      
	} elsif($st == 2 && /^MCDISPLAY: magnify\('([xyz]*)'\)$/) {
            my $mag = $1;
            $compdraw{$comp}{'magX'} = 1 if $mag =~ /x/i;
            $compdraw{$comp}{'magY'} = 1 if $mag =~ /y/i;
            $compdraw{$comp}{'magZ'} = 1 if $mag =~ /z/i;
        } elsif($st == 2 && /^MCDISPLAY: multiline\(([0-9]+),([^()\n]+)\)$/) {
            my $count = $1;
            my @coords = split ',', $2;
            push @{$compdraw{$comp}{'elems'}},
                {type => 'multiline',
                 count => $count,
                 coords => \@coords};
            if ($MCSTAS::mcstas_config{'PLOTTER'} =~ /Matlab/i) {
              # Line elements for Matlab struct
              write_process("coords=[@coords];\n");
              write_process("x=coords(1:3:length(coords));\n");
              write_process("y=coords(2:3:length(coords));\n");
              write_process("z=coords(3:3:length(coords));\n");
              write_process("coords=[x;y;z;1+0*z];\n");
              write_process("INSTRUMENT.$comp.K{size(INSTRUMENT.$comp.K,2)+1}=coords;\n");
            }           
	    if ($MCSTAS::mcstas_config{'PLOTTER'} =~ /mantid/i) {
	      # Line elements for Mantid
	      if (!($comp =~ /nD_Mantid/i)) {
		if (($comp =~ /sourceMantid/i) || ($comp =~ /sampleMantid/i) || $complete==1 ) {
		  my $looper;
		  for ($looper =  0; $looper < $count-1; $looper++) {
		    # Coordinates to look at
		    my $x0, $y0, $z0, $x1, $y1, $z1, $dx, $dy, $dz, $length;
		    $x0 = $coords[3*$looper  ]; $y0 = $coords[3*$looper+1]; $z0 = $coords[3*$looper+2];
		    $x1 = $coords[3*$looper+3]; $y1 = $coords[3*$looper+4]; $z1 = $coords[3*$looper+5];
		    $dx = $x1-$x0; $dy = $y1-$y0; $dz = $z1-$z0;
		    $length = sqrt($dx*$dx + $dy*$dy + $dz*$dz);
		    write_process("\n");
		    write_process("<type name=\"line-$comp-$mantidlinecount\" >\n");
		    write_process("\t<cylinder id=\"dummy\" >\n");
		    write_process("\t\t<centre-of-bottom-base x=\"".$x0."\" y=\"".$y0."\" z=\"".$z0."\" />\n");
		    write_process("\t\t<axis x=\"".$dx."\" y=\"".$dy."\" z=\"".$dz."\" />\n");
		    write_process("\t\t<radius val=\"0.005\" />\n"); # Hard-coded dimension of 0.5cm
		    write_process("\t\t<height val=\"".$length."\" />\n");
		    write_process("\t</cylinder >\n");
		    write_process("</type>\n");
		    $mantidlines=$mantidlines."\t<component type=\"line-$comp-".${mantidlinecount}."\" >\n";
		    $mantidlines=$mantidlines."\t\t<location x=\"0\" y=\"0\" z=\"0\" />\n";
		    $mantidlines=$mantidlines."\t</component >\n";
		    $mantidlinecount++;
		  }
		} else {
		  $mantidlines="";
		}
	      }
	    }
	  } elsif($st == 2 &&
                /^MCDISPLAY: circle\('([xyzXYZ]{2})',([-+0-9.eE]+),([-+0-9.eE]+),([-+0-9.eE]+),([-+0-9.eE]+)\)$/) {
            my ($plane,$x,$y,$z,$r) = ($1,$2,$3,$4,$5);
            # Make a circle using a 25-order multiline.
            my @coords = ();
            my $i;
            for($i = 0; $i <= 24; $i++) {
                my $a = $r*cos(2*3.1415927/24*$i);
                my $b = $r*sin(2*3.1415927/24*$i);
                my ($x1,$y1,$z1) = ($x,$y,$z);
                if($plane =~ /xy|yx/i) {
                    $x1 += $a;
                    $y1 += $b;
                } elsif($plane =~ /xz|zx/i) {
                    $x1 += $a;
                    $z1 += $b;
                } elsif($plane =~ /yz|zy/i) {
                    $y1 += $a;
                    $z1 += $b;
                } else {
                    die "mcdisplay: Bad plane specifier in circle: '$plane'";
                }
                push @coords, $x1, $y1, $z1;
            }
            push @{$compdraw{$comp}{'elems'}},
                {type => 'multiline',
                 count => 25,
                 coords => \@coords};
            if ($MCSTAS::mcstas_config{'PLOTTER'} =~ /Matlab/i) {
              # Line elements for Matlab struct, circle representation
              # due to a possible argument max length of 1024, we compute the 
              # circle directly in Matlab
              write_process("cosr = $r*cos(2*pi/24*(0:24));\n");
              write_process("sinr = $r*sin(2*pi/24*(0:24));\n");
              write_process("cte  = ones(1, 25);\n");
              if($plane =~ /xy|yx/i) {
                write_process("x=$x+cosr ; y=$y+sinr ; z=$z*cte;\n");
              } elsif($plane =~ /xz|zx/i) {
                write_process("x=$x+cosr ; y=$y*cte ; z=$z+sinr;\n");
              } elsif($plane =~ /yz|zy/i) {
                write_process("x=$x*cte ; y=$y+cosr ; z=$z+sinr;\n");
              }
              write_process("coords=[x;y;z;1+0*z];\n");
              write_process("INSTRUMENT.$comp.K{size(INSTRUMENT.$comp.K,2)+1}=coords;\n");
            }
            if ($MCSTAS::mcstas_config{'PLOTTER'} =~ /mantid/i) {
	      # Circle elements for Mantid
	      if (!($comp =~ /nD_Mantid/i) &&  !($comp =~ /sample/i) &&  !($comp =~ /source/i)) {
		if ($complete==1) {
		  my $looper;
		  for ($looper =  0; $looper <24 ; $looper++) {
		    # Coordinates to look at
		    my $x0, $y0, $z0, $x1, $y1, $z1, $dx, $dy, $dz, $length;
		    $x0 = $coords[3*$looper  ]; $y0 = $coords[3*$looper+1]; $z0 = $coords[3*$looper+2];
		    $x1 = $coords[3*$looper+3]; $y1 = $coords[3*$looper+4]; $z1 = $coords[3*$looper+5];
		    $dx = $x1-$x0; $dy = $y1-$y0; $dz = $z1-$z0;
		    $length = sqrt($dx*$dx + $dy*$dy + $dz*$dz);
		    write_process("\n");
		    write_process("<type name=\"line-$comp-$mantidlinecount\" >\n");
		    write_process("\t<cylinder id=\"dummy\" >\n");
		    write_process("\t\t<centre-of-bottom-base x=\"".$x0."\" y=\"".$y0."\" z=\"".$z0."\" />\n");
		    write_process("\t\t<axis x=\"".$dx."\" y=\"".$dy."\" z=\"".$dz."\" />\n");
		    write_process("\t\t<radius val=\"0.005\" />\n"); # Hard-coded dimension of 0.5cm
		    write_process("\t\t<height val=\"".$length."\" />\n");
		    write_process("\t</cylinder >\n");
		    write_process("</type>\n");
		    $mantidlines=$mantidlines."\t<component type=\"line-$comp-".${mantidlinecount}."\" >\n";
		    $mantidlines=$mantidlines."\t\t<location x=\"0\" y=\"0\" z=\"0\" />\n";
		    $mantidlines=$mantidlines."\t</component >\n";
		    $mantidlinecount++;
		  }
		}
	      }
	    }
	 } elsif($st == 2 && /^MCDISPLAY: end$/) {
            $st = 1;  # End of component graphics representation
            if ($MCSTAS::mcstas_config{'PLOTTER'} =~ /Matlab/i) {
              # Matlab 'End of instrument'
              write_process("mcdisplay('Load');\n");
              write_process("PlotInstrument('init');\n");
              # Check if we were called with --save option, output matlab figure if so...
              if ($save) {
                # Clone the graph to another window...
                write_process("ax=gca;\n");
                write_process("h=figure('numbertitle','off','name','$sim McStas Instrument')\n;");
                write_process("copyobj(ax,h);\n");
                write_process("saveas(h,'$sim.fig','fig');\n");
                write_process("delete(h);\n");
                write_process("delete(INSTRUMENT.fig);\n");
                write_process("exit;\n");
              } else {
                write_process("set(INSTRUMENT.fig, 'closerequestfcn','exit;');\n");
                write_process("wait(INSTRUMENT.fig);\n");
              }
            }
            if ($MCSTAS::mcstas_config{'PLOTTER'} =~ /mantid/i) {
	      if (! ($mantidlines eq "")) {
		# Output last lines corresponding to previous component. - needs to be done at MCDISPLAY: end also
		write_process($mantidlines);
		# write_process($mantidlinesend);
		write_process("</type>\n");
	      }
              # Mantid 'End of instrument'
	      write_process("</instrument>\n");
            }
	    if ($MCSTAS::mcstas_config{'PLOTTER'} =~ /vrml/i) {
		foreach $comp (@components) {
		    write_process("$compheaders{$comp}");
		    my @multilineSize =();
		    foreach $elem (@{$compdraw{$comp}{'elems'}}) {
			push @multilineSize, $elem->{'count'};
			my @coords = @{$elem->{'coords'}};
			my $i=0;
			my $coordstring="";
			foreach $coord (@coords) {
			    if ($i<2) {
				$coordstring="$coordstring$coord ";
				$i++;
			    } else {
				$i=0;
				$coordstring="$coordstring$coord,";
			    }
			}
			write_process("$coordstring\n");
		    }
		    write_process("]}\n");
		    write_process("coordIndex [\n");

		    my $c=0,$i,$j;
		    my $m = join('/',@multilineSize);
		    foreach $i (@multilineSize)
		    {
			for($j=0; $j<$i; $j++)
			{
			    write_process("$c,");
			    $c++;
			}
			write_process("-1,\n");
		    }
		    write_process("]}}\n");
		    if($transforms{$comp})
		    {
			write_process("]}\n");
		    }
		    my @T=@{$transformations{$comp}};

		    write_process("Viewpoint {\n".
				  "description \"$comp\"".
				  " position".$T[0].' '.$T[1].' '.$T[2]."\n".
				  "orientation 0 1 0  3.14\n".
				  "jump FALSE ".
				  "}\n");
		}
	    }
	    
	} elsif($st == 1 && /^INSTRUMENT END:/) {
            $st = 100;
            last;
        } else {
            print;
        }
    }
    exit if($st != 100);        # Stop when EOF seen before instrument end.
    return $#components + 1;

}

sub max {
  my ($a, $b) = @_;
  if ($a >= $b) {
    return $a;
  } else {
    return $b;
  }
}

sub make_instrument {
    my (@x, @y, @z, @ori, @dis, @comp);
    my ($i, $c, %instr);
    my ($xmin, $xmax, $ymin, $ymax, $zmin, $zmax);

    $i = 0;
    foreach $c (@components) {
        my (@T, @U);
        @T = @{$transformations{$c}};
        $x[$i] = $T[0];
        $xmin = $x[$i] if !$xmin || $x[$i] < $xmin;
        $xmax = $x[$i] if !$xmax || $x[$i] > $xmax;
        $y[$i] = $T[1];
        $ymin = $y[$i] if !$ymin || $y[$i] < $ymin;
        $ymax = $y[$i] if !$ymax || $y[$i] > $ymax;
        $z[$i] = $T[2];
        $zmin = $z[$i] if !$zmin || $z[$i] < $zmin;
        $zmax = $z[$i] if !$zmax || $z[$i] > $zmax;

        @U = ($T[3], $T[4], $T[5], $T[6], $T[7], $T[8], $T[9], $T[10], $T[11]);
        $ori[$i] = \@U;
        $comp[$i] = $c;
        # Now transform coordinates for component graphics representations.
        if($compdraw{$c}) {
            my $magX = $compdraw{$c}{'magX'};
            my $magY = $compdraw{$c}{'magY'};
            my $magZ = $compdraw{$c}{'magZ'};
            foreach $elem (@{$compdraw{$c}{'elems'}}) {
                if($elem->{'type'} eq 'multiline') {
                    my @coords = @{$elem->{'coords'}};
                    my @xs = ();
                    my @ys = ();
                    my @zs = ();
                    my ($xv,$yv,$zv);
                    while(@coords) {
                        $xv = shift(@coords);
                        $yv = shift(@coords);
                        $zv = shift(@coords);
                        $xv *= $magnification if $magX;
                        $yv *= $magnification if $magY;
                        $zv *= $magnification if $magZ;
                        push @xs, ($xv*$T[3] + $yv*$T[6] + $zv*$T[9]  + $T[0]);
                        push @ys, ($xv*$T[4] + $yv*$T[7] + $zv*$T[10] + $T[1]);
                        push @zs, ($xv*$T[5] + $yv*$T[8] + $zv*$T[11] + $T[2]);
                    }
                    $elem->{'X'} = \@xs;
                    $elem->{'Y'} = \@ys;
                    $elem->{'Z'} = \@zs;
                }
            }
        }
        $i++;
    }
    if($xmin == $xmax) {
        $xmin--;
        $xmax++;
    }
    if($ymin == $ymax) {
        $ymin--;
        $ymax++;
    }
    if($zmin == $zmax) {
        $zmin--;
        $zmax++;
    }
    $xmin -= ($xmax - $xmin) / 6;
    $xmax += ($xmax - $xmin) / 6;
    $ymin -= ($xmax - $xmin) / 6;
    $ymax += ($xmax - $xmin) / 6;
    $zmin -= ($xmax - $xmin) / 6;
    $zmax += ($xmax - $xmin) / 6;
    %instr = ('x' => \@x, 'y' => \@y, z => \@z,
              ori => \@ori, dis => \@dis, comp => \@comp,
              xmin => $xmin, xmax => $xmax,
              ymin => $ymin, ymax => $ymax,
              zmin => $zmin, zmax => $zmax,
              zoom_xmin => $xmin, zoom_xmax => $xmax,
              zoom_ymin => $ymin, zoom_ymax => $ymax,
              zoom_zmin => $zmin, zoom_zmax => $zmax,
	      zoom_tmin => 0, zoom_tmax => $tmax);
    return %instr;
}


sub transform {
    my ($comp, $x, $y, $z, $vx, $vy, $vz, $t, $ph1, $ph2) = @_;
    if(!$comp) {
        return ($x, $y, $z, $vx, $vy, $vz, $t, $ph1, $ph2);
    } else {
        my ($nx, $ny, $nz, $nvx, $nvy, $nvz, $nt, $nph1, $nph2);
        my @T = @{$transformations{$comp}};
        $x *= $magnification if $compdraw{$comp} && $compdraw{$comp}{'magX'};
        $y *= $magnification if $compdraw{$comp} && $compdraw{$comp}{'magY'};
        $z *= $magnification if $compdraw{$comp} && $compdraw{$comp}{'magZ'};
        $nx = $x*$T[3] + $y*$T[6] + $z*$T[9] + $T[0];
        $ny = $x*$T[4] + $y*$T[7] + $z*$T[10] + $T[1];
        $nz = $x*$T[5] + $y*$T[8] + $z*$T[11] + $T[2];
        $nvx = $vx*$T[3] + $vy*$T[6] + $vz*$T[9];
        $nvy = $vx*$T[4] + $vy*$T[7] + $vz*$T[10];
        $nvz = $vx*$T[5] + $vy*$T[8] + $vz*$T[11];
        return ($nx, $ny, $nz, $nvx, $nvy, $nvz, $t, $ph1, $ph2);
    }
}


sub get_inspect_pos {
    my ($inspect, @comps) = @_;
    return 0 unless $inspect;
    my $i;
    for($i = 0; $i < @comps; $i++) {
        return $i if $comps[$i] eq $inspect;
    }
    die "Error: Inspected component $inspect not part of instrument $sim ?";
}


sub read_neutron {
    my ($in) = @_;
    my (@x, @y, @z, @vx, @vy, @vz, @t, @ph1, @ph2, @ncomp);
    my ($st, $i);
    my ($comp, $numcomp, $EndFlag);

    $EndFlag = 0;
    $st = 0;
    $i = 0;
    $numcomp = 0;
    $EndFlag = 0;
    my $dropit = 1;                # Flag to drop uninteresting neutron states.
    while(<$in>) {
        if($st == 0 && /^ENTER:/) {
            # Neutron enters instrument.
            $st = 1;
        } elsif($st == 0 && /^STATE:/) {
            # State after leaving - should probably be removed in McStas.
            next;
        } elsif($st == 1 && /^COMP:\s*"([a-zA-Z0-9_]+)"\s*$/) {
            # Neutron enters component local coordinate system.
            $comp = $1;
            $numcomp++;
            $dropit = 1;        # Drop the first state (entry point).
        } elsif($st == 1 && (/^STATE:(.*)$/ || /^SCATTER:(.*)$/)) {
	    # Neutron state.
	    $dropit = 0, next if $dropit; # Skip entry point
	    ($x[$i], $y[$i], $z[$i],
	     $vx[$i], $vy[$i], $vz[$i],
	     $t[$i], $ph1[$i], $ph2[$i]) = split ",", $1;
	    ($x[$i], $y[$i], $z[$i],
	     $vx[$i], $vy[$i], $vz[$i],
	     $t[$i], $ph1[$i], $ph2[$i]) =
		 transform($comp, $x[$i], $y[$i], $z[$i],
			   $vx[$i], $vy[$i], $vz[$i],
			   $t[$i], $ph1[$i], $ph2[$i]);
	    $ncomp[$i] = $comp;
	    if($TOF){
		$t[$i] = 1000*$t[$i]; # Units of milli-seconds
	    }
	    $i++;
        } elsif($st == 1 && /^ABSORB:/) {
            # Neutron was absorbed.
            next;                # No special action needed.
        } elsif($st == 1 && /^LEAVE:/) {
            # Neutron leaves instrument.
            $st = 2;
            last;
        } elsif (/^Detector:/){
	  $st = 2;
          if (! $MCSTAS::mcstas_config{'PLOTTER'} =~ /scriptfile/i) {
            # Should only be done if finished, and not called with --save flag...
            # Also, can only be done if tcl/tl available
            if ($MCSTAS::mcstas_config{'TCLTK'} ne "no" && $EndFlag == 0) {
              my $main = new MainWindow;
              $main->Label(-text => "Simulation $sim ended."
                          )->pack;
              $main->Label(-text => 'Press Ok to terminate backend'
                          )->pack;
              $main->Button(-text => 'Ok',
                            -command => sub{destroy $main}
                           )->pack;
              MainLoop;
              $EndFlag = 1;
            }
          }
          print;
        } else {
            # Pass on any output not meant for us.
            print;
        }
    }
    exit unless $st == 2;        # Stop when EOF seen before neutron data end.

    my %neutron = ('x' => \@x, 'y' => \@y, z => \@z,
                   vx => \@vx, vy => \@vy, vz => \@vz,
                   t => \@t, ph1 => \@ph1, ph2 => \@ph2,
                   comp => \@ncomp, numcomp => $numcomp, EndFlag => $EndFlag);
    return %neutron
}


sub plot_components { # PGPLOT stuff only
    my ($rx, $ry, $rori, $rdis, $axis1, $axis2) = @_;
    my (@x, @y, @ori);
    my ($i, $col);

    @x = @$rx;
    @y = @$ry;
    @ori = @$rori;

    PGPLOT::pgsci(2);
    if ($TOF) {
	my $zz;
	my @ZZ;
	my @TT;
	$col = 4;
	foreach $zz (@x) {
	    @ZZ = ($zz, $zz);
	    @TT = ($tmin, $tmax);
	    PGPLOT::pgsci($col++);
	    $col = 4 if $col > 15;
	    PGPLOT::pgline(2, \@TT, \@ZZ);
	}
    } else {
	PGPLOT::pgline($#x + 1, \@x, \@y);
        PGPLOT::pgpt($#x + 1, \@x, \@y, 20);
	$col = 4;
	for($i = 0; $i <= $#components; $i++) {
	    my $comp = $components[$i];
	    PGPLOT::pgsci($col++);
	    $col = 4 if $col > 15;
	    if($compdraw{$comp}) {
		foreach $elem (@{$compdraw{$comp}{'elems'}}) {
		    if($elem->{'type'} eq 'multiline') {
		      PGPLOT::pgline($elem->{'count'}, $elem->{$axis1}, $elem->{$axis2});
		    }
		}
	    } else {
	        PGPLOT::pgsch(1.4);
		  PGPLOT::pgpt(1, $x[$i], $y[$i], 26);
	      }
	}
    }
}


sub plot_neutron {
    my ($x, $y, $z, $vx, $vy, $vz, $comp) = @_;
    my ($i, $col, $oldcomp, $retval);
    if ($MCSTAS::mcstas_config{'PLOTTER'} =~ /McStas|PGPLOT/i) {
      # PGPLOT only
      PGPLOT::pgsci(3);
      PGPLOT::pgline(scalar(@$x), $x, $y);
      # Show component entry/exit points in same colour as respective component.
      # This should also be done w/ Matlab...
      $i = 0;
      $col = 4;
      while($i < scalar(@$x)) {
        if(!defined($oldcomp) || $oldcomp cmp $comp->[$i]) {
          $oldcomp = $comp->[$i];
            PGPLOT::pgsci($col++);
          $col = 4 if $col > 15;
        }
        # Exit point.
        PGPLOT::pgpt(1, $x->[$i], $y->[$i], 17);
        $i++;
      }
    } elsif ($MCSTAS::mcstas_config{'PLOTTER'} =~ /Matlab/i) {
      # Matlab (split across multiple lines - otherwise sometimes
      # crashes with component-rich instrs.)
      # - Unless on Win32 where this will not work with the OLE connection
      $retval=write_process("mcdisplay('Timeout');\n");
      if ($Config{'osname'} eq 'MSWin32' && (!$file_output)) {
	$retval=write_process("mcdisplay('PlotNeutron',[@$x],[@$y],[@$z]);\n");
      } else {
	$retval=write_process("mcdisplay('PlotNeutron', ");
	$retval=write_process("[@$x], ");
	$retval=write_process("[@$y], ");
	$retval=write_process("[@$z]);\n");
      }
      return $retval;
    } elsif ($MCSTAS::mcstas_config{'PLOTTER'} =~ /vrml/i) {
      write_process("\nShape {\nappearance Appearance {\n\t".
		    "material Material { emissiveColor 1 1 1\n transparency 0.7}}\n".
		    "geometry IndexedLineSet {\ncoord Coordinate {\npoint [\n");
      my $linelength="";
      while($i < scalar(@$x)) {
	  my $xx,$yy,$zz;
	  # Needs swapping because of coordinate system in VRML instrument...
	  $xx = $y->[$i];
	  $yy = $z->[$i];
	  $zz = $x->[$i];
	  write_process("$xx $yy $zz, ");
	  $linelength="$linelength$i ";
	  $i++;
      }
      write_process("\n]}\ncoordIndex [$linelength -1,\n]}}");
      return 1;
    }
}


sub show_comp_names { # PGPLOT stuff only
    my ($rinstr) = @_;
    my %instr = %$rinstr;
    my @comps = @{$instr{'comp'}};
    my $count = @comps;
    $count = 8 if $count < 8;
    my $col = 4;
    PGPLOT::pgsch(25/$count);
    my $i;
    for($i = 0; $i < @comps; $i++) {
	PGPLOT::pgsci($col++);
	  $col = 4 if $col > 15;
	if ($TOF) {
	    PGPLOT::pgmtxt('RV', 0.2, ($i+0.5)/$count, 0.0, $comps[$i]);
	} else {
	    PGPLOT::pgmtxt('RV', 0.2, 1 - ($i+0.5)/$count, 0.0, $comps[$i]);
	}
    }
}


sub reset_zoom {
    my ($rinstr, $vps) = @_;
    $rinstr->{'zoom_xmin'} = $rinstr->{'xmin'};
    $rinstr->{'zoom_xmax'} = $rinstr->{'xmax'};
    $rinstr->{'zoom_ymin'} = $rinstr->{'ymin'};
    $rinstr->{'zoom_ymax'} = $rinstr->{'ymax'};
    $rinstr->{'zoom_zmin'} = $rinstr->{'zmin'};
    $rinstr->{'zoom_zmax'} = $rinstr->{'zmax'};
    $zooming = 0;
}


sub do_zoom {
    my ($rinstr, $vps, $cx, $cy, $cx1, $cy1) = @_;
    my ($tmp, $a1, $a2);
    $tmp = $cx, $cx = $cx1, $cx1 = $tmp if $cx > $cx1;
    $tmp = $cy, $cy = $cy1, $cy1 = $tmp if $cy > $cy1;

    if($cx == $cx1 || $cy == $cy1) {
        print STDERR "Warning: bad zoom area.\n";
        return;
    }
    if($multi_view) {
        # Convert from screen coordinates to world coordinates.
        # First find which of the three views was choosen.
        if($cx < 0 && $cy < 1) {
            $cx = $cx + 1;
            $cx1 = $cx1 + 1;
            ($a1,$a2) = ("z", "y");
        } elsif($cx < 0 && $cy >= 1) {
            $cx = $cx + 1;
            $cx1 = $cx1 + 1;
            $cy = $cy - 1;
            $cy1 = $cy1 - 1;
            ($a1,$a2) = ("z", "x");
        } elsif($cx >= 0 && $cy >= 1) {
            $cy = $cy - 1;
            $cy1 = $cy1 - 1;
            ($a1,$a2) = ("x", "y");
        } else {
            print STDERR "Warning: bad zoom area.\n";
            return;
        }
        my $idx = "$a1-$a2";
        my $vpx0 = $vps->{$idx}{'VP'}[0];
        my $vpdx = $vps->{$idx}{'VP'}[1] - $vpx0;
        my $wx0 = $vps->{$idx}{'W'}[0];
        my $wdx = $vps->{$idx}{'W'}[1] - $wx0;
        my $vpy0 = $vps->{$idx}{'VP'}[2];
        my $vpdy = $vps->{$idx}{'VP'}[3] - $vpy0;
        my $wy0 = $vps->{$idx}{'W'}[2];
        my $wdy = $vps->{$idx}{'W'}[3] - $wy0;
        $cx = ($cx-$vpx0)/$vpdx*$wdx+$wx0;
        $cx1 = ($cx1-$vpx0)/$vpdx*$wdx+$wx0;
        $cy = ($cy-$vpy0)/$vpdy*$wdy+$wy0;
        $cy1 = ($cy1-$vpy0)/$vpdy*$wdy+$wy0;
    } else {
        ($a1, $a2) = ("z","x");
    }
    $rinstr->{"zoom_${a1}min"} = $cx;
    $rinstr->{"zoom_${a1}max"} = $cx1;
    $rinstr->{"zoom_${a2}min"} = $cy;
    $rinstr->{"zoom_${a2}max"} = $cy1;
    $zooming = 1;
    if ($TOF) {
	$TOFINIT = 0;
	$rinstr->{"zoom_tmin"} = $cx;
	$rinstr->{"zoom_tmax"} = $cx1;
	$rinstr->{"zoom_zmin"} = $cy;
	$rinstr->{"zoom_zmax"} = $cy1;
	$tmax=cx1;
    }
}

sub write_process {
  my ($command) = @_;
  # $pid == 0 covers file output, Matlab + Matlab on Win32 (OLE)
  if (!$pid eq 0) {
    (kill 0, $pid) || print STDERR "$plotter process terminated - ending...\n";
    return 2;
  }
  if ($Config{'osname'} eq 'MSWin32' && $MCSTAS::mcstas_config{'PLOTTER'} =~ /Matlab/i && (!$file_output)) {
    $ML->Execute($command);
    my $err = Win32::OLE::LastError();
    if ($err) {
      print STDERR "Matlab terminated! - Exiting.\n";
      return 2;
    }
  } else {
    # Simply write data to pipe/file
    print WRITER $command;
    return 1;
  }
}

sub vrml_setcolor	{
    ($num,$max)=@_;# 0<=$num<$max
	if($num % 2)
    {$num=$num/2;}
    else
    {$num=$num/2+$max/2;}
    my $num=sprintf("%d",$num);#floor
	my $H=(6*$num)/$max;# 0<=Hue<6
	my $iH=sprintf("%d",$H);#integer form 0 to 5
	#Saturation and Value are fixed to 1
	my $dH=$H-$iH;
    my $R,$G,$B;
    if($iH==0){$R=1    ;$G=$dH  ;$B=0    }
    elsif($iH==1){$R=1-$dH;$G=1    ;$B=0    }
    elsif($iH==2){$R=0    ;$G=1    ;$B=$dH  }
    elsif($iH==3){$R=0    ;$G=1-$dH;$B=1    }
    elsif($iH==4){$R=$dH  ;$G=0    ;$B=1    }
    else{$R=1    ;$G=0    ;$B=1-$dH}
    return "$R $G $B";
}

sub plot_instrument {
    my ($noninteractive, $rinstr, $rneutron) = @_;
    my %instr = %$rinstr;
    my %neutron = %$rneutron;
    my $retval;
    # The following only relevant in PGPLOT mode
    if ($MCSTAS::mcstas_config{'PLOTTER'} =~ /McStas|PGPLOT/i) {
      my ($xmin, $xmax, $ymin, $ymax, $zmin, $zmax) =
         ($instr{'zoom_xmin'}, $instr{'zoom_xmax'}, $instr{'zoom_ymin'},
         $instr{'zoom_ymax'}, $instr{'zoom_zmin'}, $instr{'zoom_zmax'});
      ($tmin, $tmax) = ($instr{'zoom_tmin'}, $instr{'zoom_tmax'});
      my %vps;                        # Viewport/window setup.
      my ($vpx1,$vpx2,$vpy1,$vpy2,$wx1,$wx2,$wy1,$wy2);

      # PGPLOT::pgpage;        # start new page/panel
      PGPLOT::pgbbuf;        # begin buffer batch output

      # First show instrument from "above" (view in direction of y axis).

      PGPLOT::pgsci(1);
      PGPLOT::pgsch(1.4);
      if ($TOF) {
	if ($TOFINIT==0) {
	    PGPLOT::pgenv($tmin, $tmax, $zmin, $zmax, ($zooming ? 0 : 1), 0);
	    PGPLOT::pglab("TOF [ms]", "Z Axis [m]", "TOF diagram: $sim_cmd");
	    $TOFINIT=1;
	    show_comp_names($rinstr);
	}
	plot_components($instr{'z'}, $instr{'x'},$instr{'ori'}, $instr{'dis'},'T', 'Z');
	plot_neutron($neutron{'t'}, $neutron{'z'}, $neutron{'y'},
                     $neutron{'vz'}, $neutron{'vx'}, $neutron{'vy'},$neutron{'comp'});
      } else {
	  if (!($keep) || ($keep && $PGINIT==0)) {
	      PGPLOT::pgenv($zmin, $zmax, $xmin, $xmax, ($zooming ? 0 : 1), 0);
	      PGPLOT::pglab("Z Axis [m]", "X Axis [m]", ($multi_view ? "Z-X view" : "Z-X view: $sim_cmd"));
	      show_comp_names($rinstr);
	      $PGINIT=1;
	  }
        PGPLOT::pgsch(1.4);
        plot_components($instr{'z'}, $instr{'x'}, $instr{'ori'}, $instr{'dis'},
                      'Z', 'X');
        plot_neutron($neutron{'z'}, $neutron{'x'}, $neutron{'y'},
                     $neutron{'vz'}, $neutron{'vx'}, $neutron{'vy'},$neutron{'comp'});
      }
      if($multi_view) {
        # Remember viewport setup for Z-X view.
        PGPLOT::pgqvp(0, $vpx1, $vpx2, $vpy1, $vpy2);
        PGPLOT::pgqwin($wx1, $wx2, $wy1, $wy2);
        $vps{'z-x'} = {VP => [$vpx1,$vpx2,$vpy1,$vpy2],
                       W => [$wx1,$wx2,$wy1,$wy2]};

        # Now show instrument viewed in direction of z axis.
        PGPLOT::pgsci(1);
        PGPLOT::pgsch(1.4);
        PGPLOT::pgenv($xmin, $xmax, $ymin, $ymax, ($zooming ? 0 : 1), 0);
        PGPLOT::pglab("X Axis [m]", "Y Axis [m]", "X-Y view");
        plot_components($instr{'x'}, $instr{'y'}, $instr{'ori'}, $instr{'dis'},
                        'X', 'Y');
        plot_neutron($neutron{'x'}, $neutron{'y'}, $neutron{'z'},
                     $neutron{'vx'}, $neutron{'vy'}, $neutron{'vz'} ,$neutron{'comp'});
        # Remember viewport setup for Z-X view.
        PGPLOT::pgqvp(0, $vpx1, $vpx2, $vpy1, $vpy2);
        PGPLOT::pgqwin($wx1, $wx2, $wy1, $wy2);
        $vps{'x-y'} = {VP => [$vpx1,$vpx2,$vpy1,$vpy2],
                       W => [$wx1,$wx2,$wy1,$wy2]};

        # Now show instrument viewed in direction of x axis.
        PGPLOT::pgsci(1);
        PGPLOT::pgsch(1.4);
        PGPLOT::pgenv($zmin, $zmax, $ymin, $ymax, ($zooming ? 0 : 1), 0);
        PGPLOT::pglab("Z Axis [m]", "Y Axis [m]", "Z-Y view");
        plot_components($instr{'z'}, $instr{'y'}, $instr{'ori'}, $instr{'dis'},
                        'Z', 'Y');
        plot_neutron($neutron{'z'}, $neutron{'y'}, $neutron{'x'},
                     $neutron{'vz'}, $neutron{'vy'}, $neutron{'vx'}, $neutron{'comp'});
        # Remember viewport setup for Z-Y view.
        PGPLOT::pgqvp(0, $vpx1, $vpx2, $vpy1, $vpy2);
        PGPLOT::pgqwin($wx1, $wx2, $wy1, $wy2);
        $vps{'z-y'} = {VP => [$vpx1,$vpx2,$vpy1,$vpy2],
                       W => [$wx1,$wx2,$wy1,$wy2]};

        # Set up viewport & window for mouse zoom.
        if ($multi_view) {
                PGPLOT::pgpanl(2,2);
                PGPLOT::pgsci(1);
                my $time=gmtime;
                PGPLOT::pgmtxt("t",0-1*1.2,0.0,0.0,"Date: $time");
                PGPLOT::pgmtxt("t",-2-1*1.2,0,0.0,"Simulation: ");
                PGPLOT::pgmtxt("t",-3-1*1.2,0.05,0.0,"$sim_cmd");
        }        # go to last panel
        else { PGPLOT::pgpanl(1,1); }                    # to activate full page
        PGPLOT::pgsvp(0,1,0,1);        # zoom for full page
        PGPLOT::pgswin(0,1,0,1);
      }
      PGPLOT::pgebuf;        # end buffer batch output

      return 0 if ($noninteractive || $TOF || $keep);

      # Now wait for a keypress in the graphics window.
      my ($cx, $cy, $cc);
      $cx = $cy = 0;
      PGPLOT::pgband(0, 0, 0, 0, $cx, $cy, $cc);
      if($cc =~ /[qQ]/) {
        return 2;                # Finished.
      } elsif($cc =~ /[pP]/) {        # B&W hardcopy.
        return 3;
      } elsif($cc =~ /[cC]/) {        # color hardcopy.
        return 4;
      } elsif($cc =~ /[gG]/) {        # GIF hardcopy.
        return 5;
      } elsif($cc =~ /[nN]/) {        # PNG hardcopy.
        return 6;
     } elsif($cc =~ /[hH]/) {        # Help
        print STDERR "McDisplay: q=quit, h=help\n";
        print STDERR "    output p=ps,   c=color ps, g=gif n=png\n";
        print STDERR "      zoom x=reset,z or d=zoom\n";
      } elsif($cc =~ /[zZdD]/) {        # Zoom.
        my ($cx1, $cy1, $cc1) = (0, 0, 0);
        PGPLOT::pgband(2,0,$cx,$cy,$cx1,$cy1,$cc1);
        do_zoom($rinstr, \%vps, $cx, $cy, $cx1, $cy1);
        return 1;
      } elsif($cc =~ /[xX]/) {        # Reset zoom.
              PGPLOT::pgpanl(1,1);
        PGPLOT::pgperas;
        if ($multi_view) {
                PGPLOT::pgpanl(1,2);
                PGPLOT::pgperas;
                PGPLOT::pgpanl(2,1);
                PGPLOT::pgperas;
                PGPLOT::pgpanl(2,2);
                PGPLOT::pgperas;
        }
        reset_zoom($rinstr, \%vps);
        return 1;
      }
    } else {
      # Leave further checks for plot_neutron
      $retval=plot_neutron($neutron{'z'}, $neutron{'x'}, $neutron{'y'},
                   $neutron{'vz'}, $neutron{'vx'}, $neutron{'vy'}, $neutron{'comp'});
      if ($retval==2) {
        return $retval;
      }
    }
    return 0;                        # Default: do not repeat this neutron.
}

sub get_device { # PGPLOT stuff only
    my ($what) = @_;
    my $dev;

    if (defined(&dev)) { $dev = dev($what); }
    else { $dev = PGPLOT::pgopen($what); }
    return $dev if $dev < 0;
    if($multi_view) {
    # We use a 2x2 display format to view the instrument from three angles.
        PGPLOT::pgsubp(2, 2);
    } else {
        # We use a 1x1 display for detail.
        PGPLOT::pgsubp(1, 1);
    }
    return $dev;
}


# Test the code.

# Check command line arguments.

undef $inspect;
undef $first;
undef $last;
undef $save;
undef $direct_output;
undef $sim_cmd;
undef $sim;
undef $TOF;
undef $tmax;
undef $keep;
my $STDIN=0;
$complete=0;
undef $PGINIT;
undef $paramfile;
my $plotter;
undef $file_output;
my $int_mode=0; # interactive mode(0), non interactive (1)
my $i;
my $show_help=0;

$plotter = $MCSTAS::mcstas_config{'PLOTTER'};

for($i = 0; $i < @ARGV; $i++) {
    if(($ARGV[$i] eq "-m") || ($ARGV[$i] eq "--multi")) {
        $multi_view = 1;
    } elsif($ARGV[$i] =~ /--help|-h$/) {
        $show_help=1;
    } elsif(($ARGV[$i] =~ /^-z([-0-9+.eE]+)$/) ||
            ($ARGV[$i] =~ /^--zoom=([-0-9+.eE]+)$/)) {
        $magnification = ($1 == 0 ? 1 : $1);
    } elsif(($ARGV[$i] eq "-gif") || ($ARGV[$i] eq "-ps") ||
            ($ARGV[$i] eq "-fig") || ($ARGV[$i] eq "-scg") ||
            ($ARGV[$i] eq "-psc") || ($ARGV[$i] eq "-png") || ($ARGV[$i] eq "-ppm")) {
        $direct_output = $ARGV[$i];
        $save = 1;
        $int_mode = 1;
    } elsif(($ARGV[$i] =~ /^-i([a-zA-Z0-9_]+)$/) ||
            ($ARGV[$i] =~ /^--inspect=([a-zA-Z0-9_]+)$/)) {
        $inspect = $1;
    } elsif($ARGV[$i] =~ /^--first=([a-zA-Z0-9_]+)$/) {
        $first = $1;
    } elsif($ARGV[$i] =~ /^--last=([a-zA-Z0-9_]+)$/) {
        $last = $1;
    } elsif($ARGV[$i] eq "--save") {
        $save = 1;
    } elsif(($ARGV[$i] =~ /^-p([a-zA-Z0-9_]+)$/) ||
              ($ARGV[$i] =~ /^--plotter=([a-zA-Z0-9_\"]+)$/) ||
              ($ARGV[$i] =~ /^--format=([a-zA-Z0-9_\"]+)$/)) {
        $plotter = $1;
    } elsif($ARGV[$i] eq "--complete") {
        $complete = 1;
   } elsif(($ARGV[$i] =~ /^-f([a-zA-Z0-9_\-\/\ \.\:\"]+)$/) ||
              ($ARGV[$i] =~ /^--file=([a-zA-Z0-9_\-\/\ \.\:]+)$/)) {
        $file_output = $1;
   } elsif($ARGV[$i] =~ /^--param=([a-zA-Z0-9_\ \"\.\-\:]+)$/) {
	$paramfile = $1;
   } elsif($ARGV[$i] eq "--stdin") {
       $STDIN = 1;
       $sim_cmd = "stdin";
   } elsif($ARGV[$i] eq "--TOF" || $ARGV[$i] eq "-T") {
       $TOF = 1;
   } elsif($ARGV[$i] eq "--keep" || $ARGV[$i] eq "-k") {
       $keep = 1;
       $int_mode = 0;
       $PGINIT = 0;
   } elsif($ARGV[$i] =~ /^--tmax=([0-9\.]+)$/) {
       $tmax=$1;
   } else {
        if (defined($sim_cmd)) { push @cmdline, $ARGV[$i]; }
        else {
          $sim_cmd = $ARGV[$i];
          $sim=$sim_cmd;
          # Remove trailing .out or .exe extension
          $sim=~ s|.out\Z||;
          $sim=~ s|.exe\Z||;
          $sim=~ s|.instr\Z||;
        }
   }
}
if ($show_help) { undef $sim_cmd; }
die "Usage: mcdisplay [-mzipfh][-gif|-ps|-psc] Instr.out [instr_options] params
 -h        --help            Show this help
 -m        --multi           Show the three instrument side views
 -T        --TOF             Special Time Of Flight acceptance diagram mode
           --tmax=TMAX       Maxiumum TOF [ms] (defaults to 50 ms)
 -zZF      --zoom=ZF         Show zoomed view by factor ZF
 -iCOMP    --inspect=COMP    Show only trajectories reaching component COMP
           --param=FILE      Read input parameters from parameter file
 -pPLOTTER --plotter=PLOTTER Output graphics using {PGPLOT,VRML,Matlab,Mantid/NeXus}
 --format=PLOTTER            --\"-- 
 --complete                  Flag to include ALL instrument geometry in Mantid IDF generation
 -fFNAME   --file=FNAME      Output graphics commands to file FNAME
                             (Only used when PLOTTER = {Matlab})
           --first=COMP      First component to visualize {Matlab}
           --last=COMP       Last component to visualize {Matlab}
 -k        --keep            Plot all neutrons events together (Primarily for
                             use with PGPLOT and -psc / -gif etc.)
           --save            Output a Matlab figure file and exit
                             (Filename is Instr.fig). Figure
                             files are used by mcgui.pl for visualising the
                             instrument. With PGPLOT, --save is nonfunctional.
                             With VRML, --save disables spaw of VRML viewer.
 -gif|-ps|-psc               Export figure as gif/b&w ps/color ps and exit
           --stdin           Do not start a simulation, instead take neutron / instrument 
                             data directly from standard input
                             
 When using -ps -psc -gif, the program writes the hardcopy file and exits.
 SEE ALSO: mcstas, mcdoc, mcplot, mcrun, mcgui, mcresplot, mcstas2vitess
 DOC:      Please visit http://www.mcstas.org/\n"
 unless $sim_cmd;

if($paramfile) {
    open(IN, "<$paramfile") || die "mcdisplay: Failed to open parameter file '$paramfile'";
    print "\nmcdisplay: Parameters specified using file \"$paramfile\"\n";
    while(<IN>) {
        my $p;
        for $p (split) {
	    if($p =~ /^([A-Za-z0-9_]+)\=(.*)$/) {
		$parm = $1;
		$val = $2;
	    }
	    @vals = split(',',$val);
	    if (@vals>1) {
		$val = ($vals[0]+$vals[1])/2;
		print "Parameter $parm: Substituting interval $vals[0],$vals[1] to $val\n";
		push @cmdline, "$parm=$val";
	    } else {
		push @cmdline, $p;
	    }
        }
    }
    print "\n";
}

if ($sim_cmd =~ m'\.instr$') # recompile .instr if needed
{ my @ccopts=();
  ($sim_cmd, undef) = get_out_file($sim_cmd, 0, @ccopts); }


# Check value of $plotter and $file_output variables, set
# $MCSTAS::mcstas_config{'PLOTTER'} with scriptfile keyword, always set for VRML
if ($file_output || $plotter =~ /VRML/i) { $plotter .= "_scriptfile"; }

if ($TOF) {
    $TOFINIT=0;
    if(! ($tmax)) {
	$tmax=50;
    }
    if (!($plotter =~ /McStas|PGPLOT/i)) {
	print STDERR "\n***************************************\n";
	print STDERR "TOF only possible using plotter PGPLOT\nSelecting PGPLOT";
	print STDERR "\n***************************************\n";
	$plotter="PGPLOT";
    }
}

if ($plotter =~ /scriptfile/i && not $file_output) {
  $file_output="mcdisplay_commands";
  if ($plotter =~ /Matlab/i) { $file_output .=".m"; }
  elsif ($plotter =~ /VRML/i) { $file_output .=".wrl"; }
  print STDERR "Outputting to file $file_output\n";
}

if ($plotter =~ /nexus/i) {
  # Mainly intended for when called from mcgui - gives a way to generate IDF's from gui
  $plotter="mantid";
}

if ($plotter =~ /mantid/i && not $file_output) {
  $file_output="$sim.instr.xml";
  print STDERR "Outputting to file $file_output\n";
}


# Final PLOTTER check, is PGPLOT wanted but not possible?
# - Ask user to rerun / set other default
if ($plotter =~ /McStas|PGPLOT/i && $MCSTAS::mcstas_config{'PGPLOT'} eq "no") {
  print STDERR "\n******************************************************\n";
  print STDERR "Default / selected PLOTTER is PGPLOT - Problems:\n\n";
  print STDERR "PGPLOT.pm not found on Perl \@INC path\n\nSolution:\n\n";
  print STDERR "1) Install pgplot + pgperl packages (Unix/Linux/Cygwin) \n";
  print STDERR "2) Rerun mcdisplay with -p/--plotter set to Matlab or VRML\n";
  print STDERR "3) Modify $MCSTAS::perl_dir/mccode_config.perl\n";
  print STDERR "   to set a different default plotter\n\n";
  print STDERR "******************************************************\n\n";
  die "mcdisplay: PGPLOT problems...\n";
}

$MCSTAS::mcstas_config{'PLOTTER'} = $plotter;

my $pg_devname = $MCSTAS::mcstas_config{'PGDEV'};
# Only set up PGPLOT stuff if needed
if ($plotter =~ /McStas|PGPLOT/i) { # PGPLOT is plotter!
  if ($int_mode == 1)
    {
      my $ext  = "ps";
      my $type = "ps";
      if($direct_output eq "-gif") { $ext="gif"; $type="gif"; }
      elsif($direct_output eq "-png") { $ext="png"; $type="png"; }
      elsif($direct_output eq "-psc") { $type="cps"; }
      $pg_devname = "$sim_cmd.$ext/$type";
    }
  $global_device = get_device($pg_devname);
  if($global_device < 0) {
    print STDERR "mcdisplay: Failed to open PGPLOT device $pg_devname\n";
    exit 1;
  }
  my $seq = "";                        # Sequence number for multiple hardcopy
  PGPLOT::pgask(0);
} elsif ($plotter =~ /vrml/i) {
  # VRML always with file handle. VRML browser spawned after write of file.
  print "Opening file ...\n";
  open(WRITER, "> $file_output");
  $pid=0;
  # Other VRML init stuff:
  my $nbcomp1=0,$nbcomp2=0;
  my %transforms;
  my %compheaders;
} elsif ($plotter =~ /Matlab/i && $plotter =~ /scriptfile/i) {
  # Matlab w/FILE is plotter - open a file handle
  open(WRITER, "> $file_output");
  $pid=0;
} elsif ($plotter =~ /mantid/i) {
  # Mantid FILE is plotter - open a file handle
  open(WRITER, "> $file_output");
  $pid=0;
} elsif ($plotter =~ /Matlab/i) {
  # Matlab is plotter - open a pipe / OLE connection
  if ($Config{'osname'} eq 'MSWin32') {
    $pid=0;
    $ML = Win32::OLE->new('Matlab.Application') || die "mcdisplay: Could not start Matlab\n";
  } else {
    my $cmd = "$MCSTAS::mcstas_config{'MATLAB'} $MCSTAS::mcstas_config{'MATLAB_COMMAND'} > /dev/null";
    $pid=open2(READER,WRITER, $cmd) || die "mcdisplay: Could not start Matlab\n$cmd\n";
  }
  print STDERR "Opened up pipe to matlab - pid is $pid\n";
  print STDERR "Building Matlab INSTRUMENT struct, please have patience...\n";
}

my ($numcomp, %neutron, %instr);

if ($STDIN==1) {
  printf STDERR "Taking simulation data from STDIN ...\n";
  open(IN,"-");
} else {
  $args = join(" ", @cmdline);
  $cmdline = "$sim_cmd --trace --no-output-files $args";
  printf STDERR "Starting simulation '$cmdline' ...\n";
  open(IN, "$cmdline |") || die "mcdisplay: Could not run simulation\n";
}

$numcomp = read_instrument(IN);
$inspect_pos = get_inspect_pos($inspect, @components);
%instr = make_instrument;

if ($int_mode == 0 && $plotter =~ /McStas|PGPLOT/i)  { printf STDERR "McDisplay/PGPLOT: Press H key for help.\n"; }
if ($plotter =~ /Matlab/i && not $plotter =~ /scriptfile/i) { print STDERR "Matlab INSTRUMENT done, starting gui....\n"; }

while(!eof(IN)) {
    %neutron = read_neutron(IN);
    next if $neutron{'numcomp'} <= $inspect_pos;

    my $ret;
    do {
        $ret = plot_instrument($int_mode, \%instr, \%neutron);
        if ($plotter =~ /McStas|PGPLOT/i) {
	  if (! ($TOF) && ! ($keep)) {
		if ($int_mode == 1) { $ret =2; print STDERR "Wrote \"$pg_devname\"\n"; }
	  }
          if($ret == 3 || $ret == 4 || $ret == 5 || $ret == 6) {
            my $ext="ps";
            my $type = $ret == 3 ? "ps" : "cps";
            if($ret == 5) { $type = "gif"; $ext="gif"; }
            if($ret == 6) { $type = "png"; $ext="png"; }
            my $tmp_pg_devname = "$sim_cmd$seq.$ext/$type";
            my $tmpdev=0;
            $tmpdev = get_device($tmp_pg_devname);
            if($tmpdev < 0) {
              print STDERR "mcdisplay: Warning: could not open PGPLOT output \"$tmp_pg_devname\" for hardcopy output\n";
            } else {
              plot_instrument(1, \%instr, \%neutron);
              print STDERR "Wrote \"$tmp_pg_devname\"\n";
              ++$seq;
	    }
            if (defined(&close_window)) {
		close_window($tmpdev);
	    } else {
		PGPLOT::pgclos();
	    }
	    PGPLOT::pgslct($global_device);
#	    PGPLOT::pgask(0);
          }
        }
    } while($ret != 0 && $ret != 2);
    last if $ret == 2;
  }
close(IN);
if ($plotter =~ /VRML/i && !($MCSTAS::mcstas_config{'VRMLVIEW'} eq "no")) {
    if (-e $file_output) {
	if (! $save) {
	    print STDERR "Spawning $MCSTAS::mcstas_config{'VRMLVIEW'} to view $file_output\n";
	    open(VRML, "$MCSTAS::mcstas_config{'VRMLVIEW'} $file_output|");
	}
    }
}

if ($plotter =~ /Mantid/i ) {
  print STDOUT "\n\nDONE generating IDF file ".$sim.".instr.xml for use with Mantid.\n";
}

# Properly close any open files etc.
if ($plotter =~ /McStas|PGPLOT/i) {
  if (defined(&close_window)) { close_window(); }
  else {  PGPLOT::pgclos(); }
} else {
  close(WRITER);
}
