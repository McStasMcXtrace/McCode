# Balloon, pop up help window when mouse lingers over widget.

use Tk;
use English;
use Carp;

use Tk::Frame;
use Tk::Balloon;

my $lmsg = "";

my $top = MainWindow->new;
my $f = $top->Frame;

# status bar widget
my $status = $top->Label(-width => 60, -relief => "sunken", -bd => 1, -anchor => 'w');
$status->pack(-side => "bottom", -fill => "y", -padx => 2, -pady => 1);

# create the widgets to be explained
my $mb = $top->Menubutton(-relief => 'raised',
			  -text => 'Menu button');
my $xxx = 0;
$mb->checkbutton(-label => 'checkbutton',
		 -variable => \$xxx);
$mb->cascade(-label => 'cascade entry');
my $menu = $mb->cget(-menu);
my $cm = $menu->Menu(-tearoff => 0);
$mb->entryconfigure('cascade entry', -menu => $cm);
$cm->command(-label => 'first');
$cm->command(-label => 'second');
$mb->separator;
$mb->command(-label => 'Close',
	     -command => sub {$top->destroy;});

my $tm = $top->Menu(-title => "Balloon menu");
$tm->cascade(-label => "Toplevel menu", -menu => $menu);
$top->configure(-menu => $tm);

my $b1 = $top->Button(-text => "Something Unexpected",
		      -command => sub {$top->destroy;});
my $b2 = $top->Button(-text => "Something Else Unexpected");
$b2->configure(-command => sub {$b2->destroy;});

# Pack the created widgets:
$mb->pack(-side => "top", -expand => 1);
$b1->pack(-side => "top", -expand => 1);
$b2->pack(-side => "top", -expand => 1);

my $t = $top->Text(-height => 10, -cursor => 'top_left_arrow')->pack;
$t->insert('end',<<END);

Move the mouse cursor over the buttons above and let it linger.
A message will be displayed in status box below and a descriptive
balloon will appear.  The top button is a menu button which has
different messages set for each menu entry.  This text widget has
a balloon attached to it which will change depending on which word
the mouse is over.

END

my $clbf = $top->Frame->pack;
my $cf   = $clbf->Frame->pack(-side => "left");

my $c1 = $cf->Canvas(-height => 100, -width => 300, -bg => 'white')->pack(-padx => 8, -pady => 8);
my $c2 = $cf->Canvas(-height => 100, -width => 300, -bg => 'white')->pack(-padx => 8, -pady => 8);
my $id = $c1->create('text', 10, 10,
		     -anchor => 'nw',
		     -text => "This is a canvas.  You can also attach\nballoons to specific items in a canvas");
$c1->create('rectangle', 40, 60, 80, 80,
	    -fill => 'red',
	    -tags => 'rectangle',);
$c1->create('oval', 100, 50, 140, 90,
	    -fill => 'blue',
	    -tags => 'circle',);
$c2->create('text', 10, 10,
	    -anchor => 'nw',
	    -text => "Or you can attach the balloon\nto the canvas as a whole.");

my $lb = $clbf->Listbox->pack(-side => "left");
$lb->insert(qw/end one two three four/);

# create the balloon widget
my $b = $top->Balloon(-statusbar => $status);

$b->attach($mb,
	   -msg => 'Press and hold this button to see the menu.');
$b->attach($menu,
	   #-state => 'status',
	   -balloonposition => 'mouse',
	   -msg => ['Use this to tear off the menu.',
		    'This is a checkbox entry.',
		    'cascade', # Cascade entry (ignored by Balloon)
		    'separator', # Separator: never active so no message will be displayed for this entry.
		    'This is a command entry - it will close this window.',
		   ],
	  );
$b->attach($cm,
	   -msg => 'This balloon is attached to the cascade menu, not it\'s entries',
	   #-statusmsg => 'msg cm',
	   #-balloonmsg => 'cm msg.',
	  );
$b->attach($b1,
	   -balloonmsg => "Close Window",
	   -statusmsg => "Press this button to close this window");
$b->attach($b2,
	   -balloonmsg => "Self-destruct\nButton",
	   -statusmsg => "Press this button and it will get rid of itself");

my $msg = '';
my @word = ('', '');  # Indicies surrounding the current word.
my @last = ('', '');  # Same for last word.
$b->attach($t, -msg => \$msg,
	   -balloonposition => 'mouse',  # Not really used since the postcommand returns the real position.
	   -postcommand => sub { if ($word[0] eq $word[1]) {
				   # No word under mouse - don't post the balloon.
				   0;
				 } else {
				   # Have a word under mouse - change the message:
				   my $word = $t->get($word[0], $word[1]);
				   # Skip it if it contains non-word chars:
				   return 0 if $word =~ /\W/;
				   $msg = "The word under the mouse is: $word";
				   $t->tag('add', 'sel', $word[0] => $word[1]);
				   # Find a good place to put the balloon (right below the last char in the word):
				   my $i = $t->index("$word[1] - 1 chars");
				   my @p = $t->bbox($i);
				   my $x = $t->rootx + $p[0] + $p[2] - 4;
				   my $y = $t->rooty + $p[1] + $p[3] + 2;
				   "$x,$y";
				 }
			       },
	   -motioncommand => sub { my $x = $t->pointerx - $t->rootx;
				   my $y = $t->pointery - $t->rooty;
				   @word = ($t->index("\@$x,$y wordstart"), $t->index("\@$x,$y wordend"));
				   if ($word[0] eq $last[0] and $word[1] eq $last[1]) {
				     # Same word - don't cancel the balloon.
				     0;
				   } else {
				     # New word under mouse - cancel it so a new balloon will be posted.
				     $t->SelectionClear;
				     @last = @word;
				     1;
				   }
				 },
	  );
$b->attach($c1,
	   -balloonposition => 'mouse',
	   -msg => {'rectangle' => 'You are over the red rectangle right now.',
		    $id => 'You are over the text right now.',
		    'circle' => 'You are over the blue circle right now.',
		   });
$b->attach($c2,
	   -msg => 'This balloon is attached to the canvas itself.',
	  );

$b->attach($lb,
	   -balloonposition => 'mouse',
	   -msg => [qw/1 2 3 4/],
	  );

# As $b is a child of $top it is destroyed when $top is destroyed.
# Balloon.pm now registers a handler for that, and so
# this hackery is no longer required (and did not actually work
# before).
# $top->OnDestroy(sub { $b->destroy; });

MainLoop;

