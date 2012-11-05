#!/bin/sh


function get_absolute() {
    (
        cd "$1" &&
        echo `pwd`;
    )
}


WORK="$1"
MCCODE="$2"

if [ "x${MCCODE}" = "x" ]; then
    # Use trunk from current checkout
    MCCODE="../../.."
fi

if [ -d "${MCCODE}" ]; then
    MCCODE=`get_absolute "${MCCODE}"`;
else
    echo "Error: cannot find McCode trunk (using: ${MCCODE})";
    exit 1;
fi

echo "${MCCODE}"


mkdir -p ${WORK}/McCode;
cd ${WORK};

(
    cd McCode;
    ln -s "${MCCODE}/mcstas-comps" "${MCCODE}/doc" . ||
    (echo "Error: ${MCCODE} is not a valid McCode trunk"; exit 1);
)
