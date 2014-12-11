#!/bin/bash


function get_absolute() {
    (
        cd "$1" &&
        echo `pwd`;
    )
}


WORK="$1"
MCCODE="$2"

if [ "x${MCCODE}" = "x" ]; then
    # Use trunk from current checkout (search parents)
    MCCODE="`pwd`"
    while ! [ -d "${MCCODE}/mcxtrace-comps" ]; do
        MCCODE="`dirname ${MCCODE}`";
        if [ "${MCCODE}" = "/" ]; then
            echo "Error: cannot find McCode trunk. Please give as second argument.";
            exit 1;
        fi;
    done;
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
    ln -s "${MCCODE}/mcxtrace-comps" . ||
    (echo "Error: ${MCCODE} is not a valid McCode trunk"; exit 1);
)
