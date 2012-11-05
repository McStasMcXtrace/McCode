#!/bin/sh


function get_absolute() {
    (
        cd $(dirname "$1");
        echo `pwd`/$(basename "$1");
    )
}


WORK="$1"
MCCODE="$2"

if [ "x${MCCODE}" = "x" ]; then
    # Search for a McCode checkout
    MCCODE=$(find .. -iname mcstas-comps -type d|head -n1);
    MCCODE="`dirname ${MCCODE}`"
fi

if [ -d "${MCCODE}" ]; then
    MCCODE=`get_absolute "${MCCODE}"`;
else
    echo "Error: cannot find McCode trunk (using: ${MCCODE})";
    exit 1;
fi


mkdir -p ${WORK}/McCode;
cd ${WORK};

(
    cd McCode;
    ln -s "${MCCODE}/mcstas-comps" "${MCCODE}/doc" . ||
    (echo "Error: ${MCCODE} is not a valid McCode trunk"; exit 1);
)
