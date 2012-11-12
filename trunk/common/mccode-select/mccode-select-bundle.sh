#!/bin/sh


PREFIX=/usr/local


function usage() {
    echo "usage: $0 [OPTION]... package [VERSION]";
    cat <<EOF
Select a specific version of McStas/McXtrace as default.

  --list     list available versions of package.
  --install  install a new version of package for later linking.
  --help     display this help and exit.

Report bugs to jsbn@fysik.dtu.dk
EOF
}



# Parse arguments
LIST=false;
INSTALL="";
DRYRUN="";

while true; do
    case "$1" in
        "--list" )
            LIST=true
            ;;
        "--install" )
            INSTALL="$1";
            ;;
        "--dryrun" )
            DRYRUN="$1";
            ;;
        * )
            # No match, drop out
            break;
    esac
    shift;
done

# Sanity check
if ${LIST} && [ "x${INSTALL}" = "x--install" ] ; then
    echo "Error: list and install cannot be combined. Pick one.";
    exit 1;
fi

# Check for help or missing package name after parsing arguments
if [ "x$1" = "x" ] || [ "x$1" = "x--help" ] ; then
    usage;
    exit 0;
fi


NAME="$1"
VERSION="$2"



TOOLS="config convert daemon display doc formatgui gui plot resplot run stas2vitess"


function list() {
    mccode-select --list ${NAME};
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

function switch_version() {
    (
        DRYRUN="$1"
        ret=0;

        cd "${PREFIX}";
        MC="`flavor ${NAME}`";

        # Setup core
        echo "Core:"
        for name in "${NAME}" "${MC}format"; do
            mccode-select $INSTALL $DRYRUN "${name}" "${VERSION}" || ret=1;
        done

        # Setup tools
        echo ""
        echo "Tools:"
        for tool in ${TOOLS}; do
            name="${MC}${tool}";
            vers="${VERSION}";
            # Install Perl tool
            mccode-select $INSTALL $DRYRUN "${name}" "${vers}" || \
                echo "> skipping: ${NAME}-tools-${VERSION} isn't installed?";
            if [ -x "bin/${name}-${vers}-py" ]; then
                # Install Python tool as well
                mccode-select $INSTALL $DRYRUN "${name}" "${vers}-py" || \
                    echo ".. skipping";
            fi
        done

        exit ${ret};
    )
}


if ${LIST}; then
    # list available versions
    list;
else
    if [ "x${VERSION}" = "x" ]; then
        usage;
        exit 1;
    fi

    # test run
    switch_version --dryrun > /dev/null;

    if [ $? -eq 0 ]; then
        # things look good, do real run
        switch_version ${DRYRUN};
    else
        # rerun test run with messages
        switch_version --dryrun;
    fi
fi
