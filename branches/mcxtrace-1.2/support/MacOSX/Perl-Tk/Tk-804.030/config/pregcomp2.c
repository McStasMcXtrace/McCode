#include <EXTERN.h>
#include <perl.h>
#include <XSUB.h>

int main() {
    SV* sv = newSViv(0);
    regexp* rx = pregcomp(sv, 0);
}
