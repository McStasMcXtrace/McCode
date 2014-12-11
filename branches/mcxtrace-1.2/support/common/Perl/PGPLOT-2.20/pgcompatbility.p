
;# Single point routines provided for backwards compatability
;# with old perl4 version of pgperl - note array routines 
;# can now be used directly, e.g.: pgpt(1,$x,$y,$symbol) etc.

sub pgerrb1 {
    die 'Usage: &pgerrb1($dir,$x,$y,$e,$t)' if (scalar(@_)!=5);
    local($dir,$x,$y,$e,$t) = @_;
    pgerrb($dir,1,$x,$y,$e,$t);
}

sub pgerrx1 {
    die 'Usage: &pgerrx1($x1,$x2,$y,$t)' if (scalar(@_)!=4);
    local($x1,$x2,$y,$t) = @_;
    pgerrx(1,$x1,$x2,$y,$t);
}

sub pgerry1 {
    die 'Usage: &pgerry1($x,$y1,$y2,$t)' if (scalar(@_)!=4);
    local($x,$y1,$y2,$t) = @_;
    pgerry(1,$x,$y1,$y2,$t);
}

sub pgpoint1 {pgpt1(@_)}

sub pgpt1 {
    die 'Usage: &pgpt1($xpts,$ypts,$symbol)' if (scalar(@_)!=3);
    local($xpts,$ypts,$symbol) = @_;
    pgpt(1,$xpts,$ypts,$symbol);
}


;# Exit with OK status

1;

