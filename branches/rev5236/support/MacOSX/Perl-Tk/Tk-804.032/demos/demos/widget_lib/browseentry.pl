# BrowseEntry, entry with listbox to select list values.

use Tk::BrowseEntry;

my $month = "January";

outer:
{
    my $top = MainWindow->new;
    my $f = $top->Frame;
    my $c = $f->BrowseEntry(-label => "Month:", -variable => \$month);
    $c->pack;
    $c->insert("end", "January");
    $c->insert("end", "February");
    $c->insert("end", "March");
    $c->insert("end", "April");
    $c->insert("end", "May");
    $c->insert("end", "June");
    $c->insert("end", "July");
    $c->insert("end", "August");
    $c->insert("end", "September");
    $c->insert("end", "October");
    $c->insert("end", "November");
    $c->insert("end", "December");
    my $bf = $f->Frame;
    $bf->Button(-text => "Print value",
		-command => sub {
		    print "The month is $month\n";
		}, -relief => "raised")->pack;

    $bf->pack;
    $f->pack;
    MainLoop;
}
