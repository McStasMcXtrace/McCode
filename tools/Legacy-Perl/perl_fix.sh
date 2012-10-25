#!/bin/sh

LIB="$1";
IN="$2";
OUT="$3";

# Find cutoff surrounding ENV_HEADER macro
N=$(grep -n ENV_HEADER ${IN} | (IFS=":" read -r N REST && echo $N))

let RETVAL=$?;
if [ $RETVAL -eq 0 ]; then
    USE_HEADER=true;
    let BEFORE=$N-1;
    let AFTER=$N+1;
else
    USE_HEADER=false;
fi

# Locate Perl
PERL="$(command -v perl)"
if [ $? -ne 0 ]; then
    PERL=/usr/bin/perl;
fi

(
    if ${USE_HEADER}; then
        head -n ${BEFORE} ${IN} &&  # Lines before ENV_HEADER
        cat perl_env_header.pl  &&  # ENV_HEADER
        tail -n +${AFTER} ${IN}     # Lines after ENV_HEADER
    else
        cat ${IN};
    fi
) |
sed -e 's+#! /usr/bin/perl+#! '${PERL}'+' \
	  -e 's+\$MCSTAS::sys_dir = "/usr/local/lib/mcstas"+\$MCSTAS::sys_dir = "'${LIB}'"+' \
	> ${OUT}

chmod +x ${OUT}
