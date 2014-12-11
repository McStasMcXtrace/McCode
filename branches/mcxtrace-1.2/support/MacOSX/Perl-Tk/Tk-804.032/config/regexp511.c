#include <EXTERN.h>
#include <perl.h>
#include <XSUB.h>

int main() {
    SV* sv = newSViv(0);
    REGEXP* rx = pregcomp(sv, 0);
    regexp *const re = (struct regexp *)SvANY(rx);
}
