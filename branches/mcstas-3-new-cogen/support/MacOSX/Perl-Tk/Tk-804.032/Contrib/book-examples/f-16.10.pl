#!/usr/local/bin/perl -w

#
# Figure 16.8, p166
#


use Tk;

my $mw = MainWindow->new;
my $colors = $mw->Listbox->pack;
open (F, "/usr/lib/X11/rgb.txt") || die "Can't open rgb.txt";
while (<F>) {
  next if /^!/;
  $colors->insert('end',(split)[3]);
}
$colors->bind('<Double-Button-1>', sub {
     $colors->configure(-background => $colors->get($colors->curselection))});

MainLoop;
