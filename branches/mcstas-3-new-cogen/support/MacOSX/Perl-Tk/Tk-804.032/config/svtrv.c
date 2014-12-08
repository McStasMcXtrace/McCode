#include <EXTERN.h>
#include <perl.h>
#include <XSUB.h>

int main() {
    int i = 0;
    switch (i) {
    case SVt_RV:
    case SVt_IV:
	/* something */
	break;
    }
}
