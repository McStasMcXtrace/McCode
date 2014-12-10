# image2.pl

use File::Basename;
use subs qw/image2_load_dir image2_load_image/;
use vars qw/$TOP/;

sub image2 {

    # This demonstration script creates a simple collection of widgets
    # that allow you to select and view images in a Tk label.

    my($demo) = @_;
    $TOP = $MW->WidgetDemo(
        -name     => $demo,
        -text     => 'This demonstration allows you to view images using a Tk "photo" image.  First type a directory name in the listbox, then type Return to load the directory into the listbox.  Then double-click on a file name in the listbox to see that image.',
        -title    => 'Image Demonstration #2',
        -iconname => 'image2',
    );

    my $dir_label = $TOP->Label(-text => 'Directory:');
    my $demo_img = Tk->findINC('demos/images');
    my $dir_name = $TOP->Entry(-width => 30, -textvariable => \$demo_img);
    my $frog0 = $TOP->Frame;
    my $frog = $frog0->Frame;
    my $file_label = $frog->Label(-text => 'File:');
    my $f = $frog->Frame;
    my(@pl) = qw/-side top -anchor w/;
    $dir_label->pack(@pl);
    $dir_name->pack(@pl);

    # All these "frog" and "toad" frames are just to repackage the listbox
    # and image side by side so they fit within an SVGA screen.

    $frog0->pack;
    $frog->pack(qw/-side left/);
    my $toad = $frog0->Frame;
    $toad->pack(qw/-side right/);
    $file_label->pack(@pl);
    $f->pack(@pl);

    my $f_list = $f->Listbox(-width => 20, -height => 10);
    $dir_name->bind('<Return>' => [\&image2_load_dir, $f_list, \$demo_img]);
    my $f_scroll = $f->Scrollbar(-command => [$f_list => 'yview']);
    $f_list->configure(-yscrollcommand => [$f_scroll => 'set']);
    @pl = qw/-side left -fill y -expand 1/;
    $f_list->pack(@pl);
    $f_scroll->pack(@pl);
    $f_list->insert(0, qw(earth.gif earthris.gif mickey.gif teapot.ppm));

    my $image2a = $TOP->Photo;
    $f_list->bind('<Double-1>' => [\&image2_load_image, $image2a, \$demo_img]);
    my $image_label = $toad->Label(-text => 'Image:');
    my $image = $toad->Label(-image => $image2a);
    @pl = qw/-side top -anchor w/;
    $image_label->pack(@pl);
    $image->pack(@pl);

} # end image2

sub image2_load_dir {

    # This procedure reloads the directory listbox from the directory
    # named in the demo's entry.
    #
    # Arguments:
    # e       -                 Reference to entry widget.
    # l       -                 Reference to listbox widget.
    # dir_name -                 Directory name reference.

    my($e, $l, $dir_name) = @_;

    $l->delete(0, 'end');
    my $i;
    local *DIR;
    opendir DIR, $$dir_name;
    foreach $i (sort readdir DIR) {
       $l->insert('end', $i);
    }
    closedir DIR;

} # end image2_load_dir

sub image2_load_image {

    # Given the name of the toplevel window of the demo and the mouse
    # position, extracts the directory entry under the mouse and loads
    # that file into a photo image for display.
    #
    # Arguments:
    # l       -         Reference to listbox widget.
    # i       -         Reference to image object.
    # dir_name -         Directory name reference.

    my($l, $i, $dir_name) = @_;

    my $e = $l->XEvent;
    my($x, $y) = ($e->x, $e->y);
    $i->configure(-file => "$$dir_name/" . $l->get("\@$x,$y"));

    # NOTE:  $l->get('active') works just as well.

} # end image2_load_image

1;
