# mega.pl

use vars qw / $TOP /;
use strict;

sub mega {

    my( $demo ) = @_;

    $TOP = $MW->WidgetDemo(
        -name             => $demo,
        -text             => [ "Perl/Tk provides a powerful framework for creating custom widgets. There are two types of these mega-widgets: composite and derived. Subsequent demonstrations detail a complete mega-widget of each type.  Regardless of the type of mega-widget, they share a common programming structure. The key that differentiates the various types of mega-widget is the definition of the new widget's base class(es).", -wraplength => '7i' ],
        -title            => 'Introduction to writing pure Perl mega-widgets ',
        -iconname         => 'mega',
    );

    my $t = $TOP->Scrolled( qw/ ROText -wrap word -scrollbars oe/ );
    $t->focus;
    $t->pack( qw/ -fill both -expand 1 / );
    $t->insert( 'end', <<'end-of-instructions' );

Here, briefly, is the Perl/Tk mega-widget implementation for pure Perl mega-widgets. As much of the work as possible has been abstracted and incorporated into the pTk core. This mimimizes the code the mega-widget author has to write, increasing consistency.

There are two varieties of mega-widgets in Perl/Tk: composite and derived. A composite is Toplevel or Frame-based, having other, more elemental, widgets packed (or gridded) inside. A derived widget has a ISA-like relationship, generally adding (but sometimes subtracting) options/methods to/from a single, existing, widget. Of course, that single widget may itself be a composite widget.

To create a Perl/Tk mega-widget one:

. chooses a namespace (class name)
. defines a subroutine to initialize the class (optional)
. defines an instance constructor (subroutine) that (optional):
    . builds the new widget
    . defines options (configuration specifications, analagous to C widgets)
    . defines delegates for widget methods
. defines private and instance methods (optional)

As with core widgets, methods like configure() and cget() are automatically provided by the mega-widget framework and are "just there", and option database operations "just work".

Here is a minimal Perl/Tk Toplevel-based composite mega-widget:

    package Tk::Nil;
    use base qw/Tk::Toplevel/;
    Construct Tk::Widget 'Nil';

Unless overridden, options and methods are inherited from the mega-widget's base class(es).

You create a Nil just like any other Perl/Tk widget:

    my $nil = $mw->Nil;

And an empty Nil window appears that functions just like a Toplevel! But other than that, the Nil widget doesn't do anything more since no additonal subwidgets or behavior have been defined. The purpose of that example was to demonstrate how much the Perl/Tk mega-widget mechanism did for the mega-widget author - a fully functional composite mega-widget in three lines of code.

There's a second container-like mega-widget in Perl/Tk, based on a Frame. But other than the logical container, the two mega-widget types are more-or-less equivalent. The third and final mega-widget type we call a derived mega-widget, because it adds or subtracts behavior to/from an existing widget.

Functional mega-widgets look more like this:

    package Tk::MyNewWidget;

    # Declare base class.
    use base qw/ Tk::Frame /;    # Frame-based composite
or
    use base qw/ Tk::Toplevel /; # Toplevel-based composite
or
    use Tk:SomeWidget;
    use base qw/ Tk::Derived Tk::SomeWidget /; # derived from SomeWidget

    Construct Tk::Widget 'MyNewWidget'; # install MyNewWidget in pTk namespace

    sub ClassInit{               # called once to initialize new class
        my($class, $mw) = @_;
        $class->SUPER::ClassInit($mw);
    }

    sub Populate {               # called to build each widget instance
        my($self, $args) = @_;
        $self->SUPER::Populate($args);
        $self->Advertise();      # advertise subwidgets
        $self->Callback();       # invoke -command callbacks
        $self->Component();      # define a subwidget component
        $self->ConfigSpecs();    # define cget() / configure() options
        $self->Delegates();      # how methods are delegated to subwidgets
        $self->Subwidget();      # map a subwidget name to subwidget reference
    }

    # Private methods.

    # Public methods.

    1; # end class MyNewWidget

    # Don't forget POD documentation here!

Here's an excerpt from a Text dervived mega-widget called TraceText; you can examine the complete code in another demonstration. This widget defines its content using a new -textvariable option.

    package Tk::TraceText;
    use base qw/Tk::Derived Tk::Text/;
    Construct Tk::Widget 'TraceText';

    sub Populate {

        my( $self, $args ) = @_;

        $self->ConfigSpecs(
            -textvariable => 'METHOD', 'textVariable', 'TextVariable', undef,
        );

    } # end Populate

    # Private methods.

    sub textvariable {

        my ( $self, $vref ) = @_;

        $self->traceVariable( $vref, 'w', [ \&tracew => $self, $vref ] );
        $self->{_vref} = $vref;	# store watchpoint in an instance variable
    
    } # end textvariable

If you compare the preamble (the first three lines) with that of the Nil mega-widget, you'll note that they are virtually identical - the important difference is the addition of the Tk::Derived class that provides additional methods specifically for derived mega-widgets.

At that point, with three lines of code, we have a completely functional mega-widget called TraceText that is identical to the standard Text widget in every way, and the key to all this is the Construct() call, which, among other duties, installs the new widget name in the symbol table.

Construct() also arranges for the TraceText "instantiator" to call-out to the well-known method Populate() - this is how the mega-widget author adds behavior to the new widget. Similarly, the mega-widget author can provide a ClassInit() method that is called once per MainWindow to initialize class bindings, variables, images, etcetera.

Tk::TraceText::Populate defines the -textvariable option and provides a private method to establish the watchpoint. In Perl/Tk, all mega-widget options are specified via a ConfigSpecs() call, named after the C structure.

Briefly, ConfigSpecs() names options and tells Perl/Tk what to do when one is specified on a configure() or cget() call. It also specifies the option's database name, class name and default value for option DB lookups. For our -textvariable option, the Perl/Tk framework invokes a METHOD (subroutine) by the same name as the option, minus the dash, of course. Other choices include CHILDREN, DECENDENTS, a name (or list of names) of a subwidget, etcetera.

More details on mega-widget construction can be found in these man pages:

Tk::ConfigSpecs, Tk::Derived, Tk::composite, Tk::mega

end-of-instructions

} # end mega
