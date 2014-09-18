#include <EXTERN.h>
#include <perl.h>
#include <XSUB.h>

int main() {
    struct pmop op;
    op.op_pmdynflags = 0;
    op.op_pmpermflags = 0;
}
