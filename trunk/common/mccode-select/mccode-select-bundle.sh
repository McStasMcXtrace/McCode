#!/bin/sh


PREFIX=/usr/local


INSTALL="";
if [ "x$1" = "x--install" ]; then
    INSTALL="--install";
    shift;
fi


NAME="$1"
VERSION="$2"

TOOLS="config convert daemon display doc formatgui gui plot resplot run stas2vitess"


function list() {
    (
        cd "${PREFIX}/bin";
        for ver in "${NAME}-"*; do
            echo "${ver}";
        done
    )
}


function flavor() {
    case "$1" in
        "mcstas" )
            echo "mc";
            ;;
        "mcxtrace" )
            echo "mx";
            ;;
    esac
}

function whenReal() {
    if ${DOIT}; then
        $*
    else
        echo $*
    fi
}

function switch_version() {
    (
        DOIT="$1"
        ret=0;

        cd "${PREFIX}";
        MC="`flavor ${NAME}`";

        # Setup core
        echo "Core:"
        for name in "${NAME}" "${MC}format"; do
            mccode-select $INSTALL "${name}" "${VERSION}" || ret=1;
        done

        # Setup tools
        echo ""
        echo "Tools:"
        for tool in ${TOOLS}; do
            mccode-select $INSTALL "${MC}${tool}" "${VERSION}" || \
                echo ".. skipping";
        done

        exit ${ret};
    )
}


if [ "x${VERSION}" = "x" ]; then
    # list available versions
    list;
else
    # test run
    switch_version false > /dev/null;

    if [ $? -eq 0 ]; then
        # things look good, do real run
        switch_version true
    else
        # rerun test run with messages
        switch_version false
    fi
fi
