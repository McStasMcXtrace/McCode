#include <EXTERN.h>
#include <perl.h>
#include <XSUB.h>

int main() {
    CopSTASH_set(PL_curcop, NULL);
}
