#!/bin/sh


PREFIX=/usr/local


INSTALL=false;
if [ "x$1" = "x--install" ]; then
    INSTALL=true;
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


function doLink() {
    (
        FROM="$1"
        TO="$2"

        if [ -L "${TO}" ]; then
            rm "${TO}";
        fi

        if [ -e "${TO}" ]; then
            echo "Error: cannot replace existing file: ${TO}";
            exit 1;
        fi

        ln -vs "${FROM}" "${TO}" ;
    )
}

function linkBinary() {
    (
        name="$1";


        link="bin/${name}";
        file="${link}-${VERSION}";

        if ! [ -x "${file}" ]; then
            echo "Error: could not locate binary: ${PREFIX}/${file}"
            exit 1;
        else
            whenReal doLink "${PREFIX}/${file}" "${PREFIX}/${link}"
        fi


        link="man/man1/${name}.1";
        file="man/man1/${name}-${VERSION}.1";

        if [ -f "${file}" ]; then
            whenReal doLink "${PREFIX}/${file}" "${PREFIX}/${link}";
        fi

    )
}


function switch_version() {
    (
        DOIT="$1"
        ret=0;

        cd "${PREFIX}";
        MC="`flavor ${NAME}`";

        # Setup core
        echo ""
        echo "Core:"
        for name in "${NAME}" "${MC}format"; do
            linkBinary "${name}" || ret=1;
        done

        # Setup tools
        echo ""
        echo "Tools:"
        for tool in ${TOOLS}; do
            linkBinary "${MC}${tool}" || echo ".. skipping";
        done

        exit ${ret};
    )
}


if [ "x${VERSION}" = "x" ]; then
    # list available versions
    list;
else
    echo "Switching ${NAME} to version ${VERSION}..";

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
