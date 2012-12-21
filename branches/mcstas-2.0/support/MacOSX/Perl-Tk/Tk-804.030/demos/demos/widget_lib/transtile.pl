# transtile.pl

use vars qw/$TOP/;

sub transtile {

    # Create a top-level window that demonstrates tiles
    # and transparent stuff.

    my($demo) = @_;
    $TOP = $MW->WidgetDemo(
        -name     => $demo,
        -text     => ['This window demonstrates tiles and transparent images. The Canvas has a yellow background, which displays for one second before it\'s overlayed with a tile of tiny camels. On top of the tile layer are three non-transparent images that obscure the tile. Canvas items such as ovals, rectangles and polygons cannot react to bound events unless they are filled with a color - move the cursor over the blue circle and note that it reacts to <Motion> events. The transparent circle outlined in red to the left of the blue circle does not react to <Motion> events in its interior because it\'s transparent, or unfilled. If you need a transparent item that also reacts to events then add a transparent stipple. The bottom red circle is both transparent and aware of events. Finally, a blue lattice (a transparent GIF that\'s been Base64 encoded so it can be embedded in the source) is overlayed on top of everything, anchored to the southeast corner of the Canvas.', -wraplength => '8i'],
        -title    => 'Tile and Transparent Demonstration',
        -iconname => 'transtile',
    );

    my $tile = $TOP->Photo(-file =>Tk->findINC('Camel.xpm'));

    # A tiled Canvas - the tile overlays the background color.

    my $c = $TOP->Canvas(
        -background  => 'yellow',
        -width       => 300,
        -height      => 250,
        -relief      => 'raised',
        -borderwidth => 3,
    )->grid;
    $c->update;
    $c->after(1000);
    $c->configure(-tile => $tile);

    # These images are not transparent, thus they obscure the tile
    # or background.

    my($x, $y) = (30, 30);
    foreach (qw/Xcamel.gif anim.gif icon.gif/) {
	$c->createImage($x, $y, -image => $TOP->Photo(-file => Tk->findINC($_)));
	$x += 50;
	$y += 50;
    }

    # Transparent Canvas items do not generate events.

    my $cb = sub {
	print "Over circle, args = @_!\n";
    };

    # Not filled (transparent), no events.

    my $o1 = $c->createOval(25, 25, 100, 100,
        -outline => 'red',
    );
    $c->bind($o1, '<Motion>' => $cb);

    # Filled (non-transparent), but with events.

    my $o2 = $c->createOval(155, 25, 225, 100,
        -outline => 'red',
        -fill    => 'blue',
    );
    $c->bind($o2, '<Motion>' => $cb);

    # Transparently filled, with events.

    my $o3 = $c->createOval(25, 120, 100, 195,
        -outline => 'red',
        -fill    => 'blue',
        -stipple => 'transparent',
    );
    $c->bind($o3, '<Motion>' => $cb);

    # A transparent GIF overlaying everything.

    $c->createImage(300, 300,
        -image => $TOP->Photo(-data => &encoded_gif, -format => 'gif'),
	-anchor => 'se',
    );

} # end transtile

sub encoded_gif {

    # A Base64 encoded transparent GIF.

    my $gif =  <<'END_OF_GIF';
R0lGODlhyADIAIAAAB0A/////yH+Dk1hZGUgd2l0aCBHSU1QACH5BAEKAAEALAAAAADIAMgAAAL+
jI+py+0Po5y02ouz3rz7D4biSJbmiabqCgJuC7DZK9Ly5+Zdrt8Sb+MAYzDUkKgZ+n5AXNOjLB03
R6TQmpxeo9seSTsDZ3ldctk7fo7EGC5VnQ6+zWd5CB6mx9Ft/N6p92WXBejXt4Z1trTI4MboM/i4
6ChZabkUeam5ydnp+QkaKjpKWmp6ipqqusoKldjqWvOqSQmbx3ebSTiH+yZrZHhR+9c3bGFcgcwb
KMy2PBtR5cpc3DsdTCF9Tc3Efez8rUysWOgNId6NPaGNaJ6t3uweDV2t+y5/9SuLf4M+z2/rXSh7
zQIaPIgwocKFDBs6fAgxosSJjehRHGhxB7z+ZBm/6StSrmMukec2piN4kuQDfyUBhjPZ0tpLlP9k
cnTZgN0dcDdhOtBZz2ZKmj95asRZkWVOo+uYDlW5FOk9oU2lLlBa1KoCrFGp5gu5DWpXoit9zmNF
lslHJxfbun0LN67cuXTr2r2LN6/evbvspj2oVcbfnoP7iR17uGK7xCl37iy81WxWxkm9xpM8GexR
ygi4ToZc1vNVzJVBJzDdmXRmKapHBz5tGVNsgSZEV3aMsRXq0Xx7+/4NPLjw4cSLGz+OPLmK3b5f
p3COinlN6QGo21bsCfp05tqrt3bNGfZm6ojDe+/e/fzs1eNqXz/wXjwM6PHTh17fdZT2+N7+BeMv
Wop1/ykHCnkEHohgggouyGCDDj4oCITRDQiYeYyYx598oti3wnfqkcfhPhRqeIJTMT02Im+a8fJc
fRamll6MKRqQ4WcGFjLfjKmFBYyHJb5Io4+s3QgekAHq6J5gcBEpYZNOPglllFJO2R+V2SFpZY5M
NvlijVXqZ+SWMHKHpXq45Yjjj0J+OV6XZfLnZZFijsRjnc+oGeI1K26WpntrRojmmbG0WOaRehmZ
ZaKKLspoo44+CmmkksyZZIJ54kkmoAX+2aGQHF5qJylAzQRknCIKuthrXsYJYpiFRkbfq/aBSo4g
+yEaJIqUBgXMW6YGCiGtGj25q6TGHov+bLLKLstss2ziOWhc0gn7YXif4morhpyWxtmqrz57Z7hD
flcjq1jC+S2sM8rI7rnuLucporNyWuypmy7mbK/57stvv/7+C3DAAvd1r4S/9gnSXAe3ieKG2MJn
Im21MvwJqKPeoqeWpbpp8Z8Z3thuqCJOuy0iGmf8o70UlfswvL6m22+9A89Mc80231yXzKLKiyvJ
OieMMp/AtjcsphZ+7LGP3oqJ9K3oxSvgpRdfNrTQRe+p67uoRlsxzEFSm9ycC+/7M85mn4122mqv
zfZXbSOWM89rUez2hE4j2bTS3zK9Zt5kvmcuk1NP5dzghFlLb8lf6+2i39q2KnXfks8fCzTCIx89
ude56Vv51W9/Dnrooo9Oeummn4566g4UAAA7
END_OF_GIF

    return $gif;

} # end encoded_gif

1;
