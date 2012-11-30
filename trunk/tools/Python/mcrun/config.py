
import os

VERSION = "mcrun @MCCODE_VERSION@"


MCCODE_BIN = "@MCCODE_BIN@"


MCCODE_LIB = os.environ.get('MCSTAS', None)
MCCODE_LIB = MCCODE_LIB or "@MCCODE_LIB@"

CC = os.environ.get('MCSTAS_CC', None)
CC = CC or "@CC@"

CFLAGS = os.environ.get('MCSTAS_CFLAGS', None)
CFLAGS = CFLAGS or "@CFLAGS@"
