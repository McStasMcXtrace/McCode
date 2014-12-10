# -*- perl -*-
BEGIN { $|=1; $^W=1; }
use strict;
use Test;
##
## Almost all widget classes:  load module, create, pack, and
## destory an instance.
##
## Menu stuff not tested up to now
##

use vars '@class';

BEGIN
  {
    @class = (
	# Tk core widgets
	qw(
		Frame
		Toplevel

		Label
		Button
		Checkbutton
		Radiobutton

		Entry
		Spinbox

		Listbox

		Scale
		Scrollbar

		Labelframe
		Panedwindow

		Canvas
		Text
	),
	# Tix core widgets
	qw(
		HList
		InputO
		NoteBook
		TList
		TixGrid
		Optionmenu
	),
	# Tixish composites
	qw(
		BrowseEntry
		Tree
		DirTree
	),
	# perl/Tk composites
	($^O eq 'MSWin32') ? () : qw(ColorEditor),
	qw(
		LabEntry
		LabFrame
		Optionmenu
		ROText
		Table
		Tiler
		TextUndo
		TextEdit
		Dialog
		DialogBox
		FileSelect
	),
	# Tclish composites
	qw(
		FBox
		IconList
	),
   );

   require Tk if ($^O eq 'cygwin');
   @class = grep(!/InputO/,@class) if ($^O eq 'MSWin32' or
			    ($^O eq 'cygwin' and defined($Tk::platform)
					     and $Tk::platform eq 'MSWin32'));

   plan test => (15*@class+3);

  };

eval { require Tk; };
ok($@, "", "loading Tk module");

my $mw;
eval {$mw = Tk::MainWindow->new();};
ok($@, "", "can't create MainWindow");
ok(Tk::Exists($mw), 1, "MainWindow creation failed");
eval { $mw->geometry('+10+10'); };  # This works for mwm and interactivePlacement

my $w;
foreach my $class (@class)
  {
    print "Testing $class\n";
    undef($w);

    eval "require Tk::$class;";
    ok($@, "", "Error loading Tk::$class");
    ok("Tk::$class"->isa('Tk::Widget'),1,"Tk::$class is not a widget");

    eval { $w = $mw->$class(); };
    ok($@, "", "can't create $class widget");
    skip($@, Tk::Exists($w), 1, "$class instance does not exist");


    if (Tk::Exists($w))
      {
        ok($w->class,$class,"Window class does not match");

        if ($w->isa('Tk::Wm'))
          {
	    # KDE-beta4 wm with policies:
	    #     'interactive placement'
	    #		 okay with geometry and positionfrom
	    #     'manual placement'
	    #		geometry and positionfrom do not help
	    eval { $w->positionfrom('user'); };
            #eval { $w->geometry('+10+10'); };
	    ok ($@, "", 'Problem set postitionform to user');

            eval { $w->Popup; };
	    ok ($@, "", "Can't Popup a $class widget")
          }
        else
          {
	    ok(1); # dummy for above positionfrom test
            eval { $w->pack; };
	    ok ($@, "", "Can't pack a $class widget")
          }
        print "# $class update\n";
        eval { $mw->update; };
        ok ($@, "", "Error during 'update' for $class widget");

        my @dummy;
        print "# $class configure list\n";
        eval { @dummy = $w->configure; };
        ok ($@, "", "Error: configure list for $class");
        my $dummy;
        print "# $class configure scalar\n";
        eval { $dummy = $w->configure; };
        ok ($@, "", "Error: configure scalar for $class");
        ok (scalar(@dummy),scalar(@$dummy), "Error: scalar config != list config");

        $@ = "";
        my %skip = (-class => 1);
        foreach my $opt ($w->CreateOptions)
         {
          $skip{$opt} = 1;
         }
        foreach my $opt (@dummy)
         {
          my @val = @$opt;
          if (@val != 2 && !exists($skip{$val[0]}) )
           {
            eval { $w->configure($val[0],$val[-1]) };
            if ($@)
             {
              print "#$class @val:$@";
              last;
             }
           }
         }
        ok($@,"","Cannot re-configure $class");

        print "# $class update post-configure\n";
        eval { $mw->update; };
        ok ($@, "", "Error: 'update' after configure for $class widget");

        print "# $class destroy\n";
        eval { $w->destroy; };
        ok($@, "", "can't destroy $class widget");
        ok(!Tk::Exists($w), 1, "$class: widget not really destroyed");

        # XXX: destroy-destroy test disabled because nobody vote for this feature
	# Nick Ing-Simmmons wrote:
	# The only way to make test pass, is when Tk800 would fail, to specifcally look
	# and see if method is 'destroy', and ignore it. Can be done but is it worth it?
	# Note I cannot call tk's internal destroy as I have no way of relating
	# (now destroy has happened) the object back to interp/MainWindow that it used
	# to be associated with, and hence cannot create the args I need to pass
	# to the core.

        # since Tk8.0 a destroy on an already destroyed widget should
        # not complain
        #eval { $w->destroy; };
        #ok($@, "", "Ooops, destroying a destroyed widget should not complain");

      }
    else
      {
        # Widget $class couldn't be created:
	#	Popup/pack, update, destroy skipped
	for (1..6) { skip (1,1,1, "skipped because widget could not be created"); }
      }
  }

1;
__END__
