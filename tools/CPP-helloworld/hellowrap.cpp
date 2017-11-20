#include <cstdlib>

#include "hellowrap.h"
#include "hello.h"

#ifdef __cplusplus
extern "C" {
#endif

int sayHi(char *name) {
  return hello(name);
}

#ifdef __cplusplus
}
#endif
