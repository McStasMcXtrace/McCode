use warnings;
use strict;
use Test::More;
use Tk;

plan (tests => 4);

use_ok 'Tk::TextUndo';

my $mw = MainWindow->new();
$mw->geometry("+10+10");

my $frame = $mw->Frame()->pack();

my $text = $frame->Scrolled('TextUndo',
                            -wrap      => 'none',
			    -width     => 30,
			   )->pack();
my $real_text = $text->Subwidget("scrolled");
isa_ok($real_text, "Tk::TextUndo");
isa_ok($real_text, "Tk::Text");

$text->focus;
$mw->update;
for (1..100){$text->eventGenerate('<KeyPress-x>')}
$mw->update;
# The failure can be reproduced with Tk804.028+Metacity, but not with fvwm2
ok(abs(${$text->xview}[1] - 1) < 0.01, 'Insertion cursor is visible at end of line');
