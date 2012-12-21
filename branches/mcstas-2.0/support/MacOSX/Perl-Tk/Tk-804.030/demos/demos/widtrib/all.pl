# All widgets at a glance.
# -*- perl -*-

#
# $Id: $
# Author: Slaven Rezic
#
# Copyright (C) 2007 Slaven Rezic. All rights reserved.
# This program is free software; you can redistribute it and/or
# modify it under the same terms as Perl itself.
#
# Mail: slaven@rezic.de
# WWW:  http://www.rezic.de/eserte/
#

use strict;
use Tk::Pane;

use vars qw/$TOP $MW $DEMO_FILE/;

sub all {
    my($demo) = @_;
    $TOP = $MW->WidgetDemo(
	-name		  => $demo,
	-text		  => <<"EOF",
All Tk widgets at a glance in one Toplevel.

The left column contains the class name, the middle column a sample representation of this widget, and the right column a button to the widget's Pod (requires Tk::Pod from CPAN).

There are three sections: core Tk modules and Tix modules which come with stock Tk $Tk::VERSION and a sample of non-standard Tk modules from CPAN. The non-standard modules are only displayed if installed, otherwise they are skipped.
EOF
	-geometry_manager => 'pack',
	-title		  => 'All widgets',
	-iconname	  => 'All widgets',
    );

    my($px_w, $px_h) = (400, 200);
    my($txt_w, $txt_h) = (40, 6);
    my @px_geom  = (-width => $px_w, -height => $px_h);
    my @txt_geom = (-height => $txt_h, -width => $txt_w);
    my $insert_txt = sub {
	my $w = shift;
	if ($w->can("Subwidget") && $w->Subwidget("scrolled")) {
	    $w = $w->Subwidget("scrolled");
	}
	$w->insert("end", "This is some sample text for the widget class " . $w->Class);
    };
    my $insert_lb = sub {
	shift->insert("end", sort grep { !m{^/} } keys %INC);
    };

    my $f = $TOP->Scrolled('Pane',
			   qw(-width 900 -height 500), # XXX check for screensize!
			   -gridded => 'xy',
			   -scrollbars => 'osoe',
			   -sticky => 'news',
			  )->pack(qw(-expand 1 -fill both));
    $f = $f->Subwidget("scrolled");
    my @w_def = (
		 {separator => 'Core Tk modules'},

		 'Adjuster',
		 # XXX 'Balloon',
		 {class => 'BrowseEntry', action => $insert_lb},
		 {class => 'Button', w_args => [-text => 'This is a button']},
		 {class => 'Canvas', w_args => [@px_geom],
		  action => sub {
		      my($w) = @_;
		      my @colors = qw(red green blue orange black white);
		      for (1..50) {
			  $w->createLine(rand($px_w),rand($px_h),
					 rand($px_w),rand($px_h),
					 -fill => $colors[rand @colors],
					 -width => rand(4)+1,
					);
		      }
		      $w->configure(-scrollregion => [$w->bbox("all")]);
		  },
		 },
		 {class => 'Checkbutton', w_args => [-text => 'This is a checkbutton']},
		 {class => 'ColorEditor', dialog => 1},
		 {class => 'Dialog', dialog => 1},
		 {class => 'DialogBox', dialog => 1},
		 {class => 'DirTree', scrolled => 'oe'},
		 {class => 'Dirlist', scrolled => 'oe'},
		 {class => 'Entry', w_args => [-width => 20], action => $insert_txt},
		 {class => 'FBox', dialog => 1},
		 {class => 'FileDialog', dialog => 1},
		 {class => 'FileSelect', dialog => 1},
		 {class => 'Frame', w_args => [@px_geom, -bg => 'red']},
		 {class => 'Label', w_args => [-text => 'This is a label']},
		 {class => 'Labelframe', w_args => [@px_geom, -bg => 'red', -text => 'Title of frame']},
		 {class => 'Listbox', action => $insert_lb, scrolled => 'oe'},
		 # XXX Menubar?
		 {class => 'Menubutton', w_args => [-text => 'Menu button']},#XXX menuitems
		 {class => 'Message', w_args => [-text => 'This is a message widget']},
		 {class => 'MsgBox', dialog => 1},
		 {class => 'NoteBook', action => sub {
		      my $w = shift;
		      for (1..5) {
			  my $p = $w->add("page$_", -label => "Page $_");
			  $p->Label(-text => "A label in the page $_")->pack;
		      }
		  },
		 },
		 # XXX Pane
		 {class => 'Radiobutton', w_args => [-text => 'This is a radiobutton']},
		 {class => 'ROText', w_args => [@txt_geom], action => $insert_txt, scrolled => 'oe'},
		 {class => 'Scale', w_args => [-orient => 'horiz', -from => 0, -showvalue => 1, -to => 100]},
		 {class => 'Scrollbar', w_args => [-orient => 'horiz']},
		 {class => 'Spinbox', w_args => [-from => 0, -to => 100]},
		 {class => 'Text', w_args => [@txt_geom], action => $insert_txt, scrolled => 'oe'},
		 # disabled because of warning loop, line 189 ... {class => 'TextEdit', w_args => [@txt_geom], action => $insert_txt, scrolled => 'oe'},
		 {class => 'TextList', w_args => [@txt_geom], action => $insert_lb, scrolled => 'oe'},
		 {class => 'TextUndo', w_args => [@txt_geom], action => $insert_txt, scrolled => 'oe'},
		 # XXX Toplevel

		 {separator => 'Tix modules'},

		 'FloatEntry',
		 {class => 'HList', action => sub {
		      my $w = shift;
		      my $b = $w->Balloon;
		      my %binfo;
		      for my $path ('1', '1.1', '1.2', '2', '2.1') {
			  $w->add($path, -text => $path);
			  $binfo{$path} = "BalloonInfo: $path";
		      }
		      $b->attach($w, -balloonposition => "mouse", -msg => \%binfo);
		  },
		 },
		 qw(IconList InputO
		    LabEntry LabFrame LabRadio NBFrame Optionmenu
		    Panedwindow ProgressBar TList Table
		    Tiler TixGrid Tree
		  ),

		 {separator => 'Installed non-core Tk modules'},

 		 qw(Date DateEntry NumEntry NumEntryPlain
 		    PathEntry
		  ),
		 {class => 'FireButton', action => sub {
		      my $w = shift;
		      my $text = 'This is a firebutton 0';
		      $w->configure(-textvariable => \$text,
				    -command => sub {
					$text =~ s{(\d+)}{$1+1}e;
				    },
				   );
		  },
		 },
		 {class => 'TFrame', w_args => [@px_geom, -bg => 'red',
						-label => [ -text => 'Title' ],
						-borderwidth => 2,
						-relief => 'groove',
					       ],
		 },
		 {class => 'ToolBar', w_args => [qw/-movable 1 -side top
						    -indicatorcolor blue/],
		  action => sub {
		      my $tb = shift;
		      $tb->ToolButton  (-text  => 'Button',
					-tip   => 'tool tip',
					-command => sub { print "hi\n" });
		      $tb->ToolLabel   (-text  => 'A Label');
		      $tb->Label       (-text  => 'Another Label');
		      $tb->ToolLabEntry(-label => 'A LabEntry',
					-labelPack => [-side => "left",
						       -anchor => "w"]);
		  },
		 },
		 {class => 'HistEntry', action => sub {
		      my $w = shift;
		      $w->bind("<Return>" => sub {
				   # do something with value, and then:
				   $w->historyAdd;
				   $w->delete('0', 'end');
			       });
		  },
		 },
		 {class => 'MListbox', w_args => [-columns=>[[-text=>'Heading1',
							      -sortable=>0],
							     [-text=>'Heading2']]],
		  action => sub {
		      my $w = shift;
		      $w->insert("end", [qw(Cell11 Cell12)], [qw(Cell21 Cell22)]);
		  },
		 },
		 {class => 'Cloth', w_args => [@px_geom],
		  action => sub {
		      my($w) = @_;
		      my @colors = qw(red green blue orange black white);
		      for (1..50) {
			  $w->Line(-coords => [rand($px_w),rand($px_h),
					       rand($px_w),rand($px_h)],
				   -fill => $colors[rand @colors],
				   -width => rand(4)+1,
				  );
		      }
		  },
		 },
		 {class => 'DirSelect', dialog => 1},
		 {class => 'ExecuteCommand', w_args => [@txt_geom]},
		 {class => 'FontDialog', dialog => 1},
		 {class => 'JBrowseEntry', action => $insert_lb},
		 {class => 'JFileDialog', dialog => 1},
		 {class => 'More', w_args => [@txt_geom], action => sub {
		      shift->Load($DEMO_FILE),
		  }, scrolled => 'oe'},
		 {class => 'ObjEditor', w_args => [@txt_geom, -caller => { dummy => 'object'}]},
		 {class => 'ObjScanner', w_args => [@txt_geom, -caller => $TOP]},
		 {class => 'PodText', require => 'Tk::Pod::Text',
		  w_args => [@txt_geom, -file => 'Tk']},
		 {class => 'XMLViewer', w_args => [@txt_geom], action => sub {
		      shift->insertXML(-text => "<?xml version='1.0' ?><a><bla /><foo>bar</foo></a>");
		  }, scrolled => 'oe'},
		 {class => 'Zinc', w_args => [@px_geom],
		  action => sub {
		      my($w) = @_;
		      my @colors = qw(red green blue orange black white);
		      for (1..20) {
			  $w->add('curve', 1, [map { (rand($px_w),rand($px_h)) } (1..5)],
				  -relief => 'roundgroove',
				  -filled => 1,
				  -fillcolor => $colors[rand @colors],
				 );
		      }
		  },
		 },
		);
    $f->grid('columnconfigure', $_, -pad => 3, -weight => 1) for (0 .. 1);
    $f->grid('rowconfigure', $_, -pad => 3, -weight => 1) for (0 .. $#w_def);
    my $row = -1;
    for my $w_def (@w_def) {
	my($separator, $text, $class, @w_args, $action, $scrolled, $dialog, $dialog_action);
	if (UNIVERSAL::isa($w_def, "HASH")) {
	    $separator = $w_def->{separator};
	    if (!$separator) {
		$class    = $w_def->{class};
		$text     = $w_def->{text} || $class;
		@w_args   = @{ $w_def->{w_args} || [] };
		$action   = $w_def->{action};
		$scrolled = $w_def->{scrolled};
		$dialog   = $w_def->{dialog};
		$dialog_action = $w_def->{dialog_action};
		if ($dialog && !$dialog_action) {
		    $dialog_action = sub { shift->Show };
		}
		if ($w_def->{require}) {
		    eval 'require ' . $w_def->{require};
		    if ($@) {
			warn $@;
			next;
		    }
		}
	    }
	} else {
	    ($text, $class) = ($w_def, $w_def);
	}

	$row++;

	if ($separator) {
	    $f->Label(-text => $separator,
		      -font => 'Helvetica 18',
		      -pady => 5,
		     )->grid(-row => $row,
			     -column => 0,
			     -columnspan => 3,
			    );
	    next;
	}

	my $bgcolor = $row%2==0 ? '#c0c0c0' : '#a0a0a0';
	my $ff = $f->Frame(-background => $bgcolor,
			   )->grid(-row => $row,
				   -column => 1,
				   -sticky => 'news',
				  );
	my $cw = eval {
	    if ($dialog) {
		$ff->$class(@w_args)->destroy; # just load it...
		$ff->Button(-text => 'Open ' . $class,
			    -command => sub {
				# There are some buggy dialogs which display
				# already without calling a Show method (e.g. Tk::FBox),
				# so create here
				my $d = $ff->$class(@w_args);
				$dialog_action->($d) if $dialog_action;
			    })->pack;
	    } else {
		if ($scrolled) {
		    $ff->Scrolled($class, @w_args, -scrollbars => $scrolled)->pack;
		} else {
		    $ff->$class(@w_args)->pack;
		}
	    }
	};
	if ($@ || !$cw) {
	    warn $@;
	    $row--;
	    $ff->destroy;
	    next;
	}

	$action->($cw) if $action && !$dialog;

	$f->Label(-text => $text,
		  -background => $bgcolor,
		  -anchor => 'w',
		 )->grid(-row => $row,
			 -column => 0,
			 -sticky => "news",
			);

	$f->Button(-text => 'Pod',
		   -background => $bgcolor,
		   -command => sub {
		       require Tk::Pod;
		       $TOP->Pod(-file => 'Tk::' . $class);
		   },
		  )->grid(-row => $row,
			  -column => 2,
			  -sticky => 'news',
			 );
    }
    # $TOP->WidgetDump;
}

__END__
