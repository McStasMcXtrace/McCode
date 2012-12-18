
import os


VERSION = "mcrun @MCCODE_VERSION@"

MCCODE_BIN = "@MCCODE_BIN@"
OUT_SUFFIX = "@OUT_SUFFIX@"


MCCODE_LIB = os.environ.get('MCSTAS',        '@MCCODE_LIB@')
CC         = os.environ.get('MCSTAS_CC',     '@CC@')
CFLAGS     = os.environ.get('MCSTAS_CFLAGS', '@CFLAGS@')
