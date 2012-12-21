#include <stdlib.h>

int main()
{char *e;
 unsigned long v = strtoul("DEADFACEgibberish",&e,16);
 if (v != 0xDEADFACEL || strcmp(e,"gibberish"))
  exit(1);
 return 0;
}
