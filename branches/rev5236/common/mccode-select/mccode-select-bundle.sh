#!/bin/sh

fullpath_dir() {
    (
        cd $(dirname $0);
        echo $(pwd)
    )
}

# Locate bin-folder and prefix from current script-location
BIN="$(fullpath_dir)"
PREFIX=$(dirname "${BIN}")


usage() {
    echo "usage: $0 [OPTION]... package [VERSION]";
    cat <<EOF
Select a specific version of McStas/McXtrace as default.

  --list     list available versions of package.
  --install  install a new version of package for later linking.
  --help     display this help and exit.

Report bugs to jsbn@fysik.dtu.dk
EOF
}


if [ "x$1" = "x--no-update-alternatives" ]; then
    HAS_ALTERNATIVES=false;
    shift;
else
    HAS_ALTERNATIVES=true;
fi


# Parse arguments
LIST=false;
INSTALL=false;
DRYRUN="";

while true; do
    case "$1" in
        "--list" )
            LIST=true
            ;;
        "--install" )
            INSTALL=true;
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

flavor() {
    case "$1" in
        "mcstas" )
            echo "mc";
            ;;
        "mcxtrace" )
            echo "mx";
            ;;
    esac
}
echo `flavor mcxtrace`

case "`flavor $1`" in
  "mc" )
    TOOLS="config convert daemon display doc formatgui gui plot resplot run stas2vitess"
        ;;
  "mx" )
    TOOLS="config convert daemon display doc formatgui gui plot run"
        ;;
esac

mcselect() {
    if ${HAS_ALTERNATIVES}; then
        mccode-select --install $*
    else
        mccode-select --no-update-alternatives --install $*
    fi
}

list() {
    mccode-select --list ${NAME};
}


switch_version() {
    (
        DRYRUN="$1"
        ret=0;

        cd "${PREFIX}";
        MC="`flavor ${NAME}`";

        # Setup core
        echo "Core:"
        for name in "${NAME}" "${MC}format"; do
            mcselect $DRYRUN "${name}" "${VERSION}" || ret=1;
        done

        # Setup tools
        echo ""
        echo "Tools:"
        for tool in ${TOOLS}; do
            name="${MC}${tool}";
            vers="${VERSION}";
            # Install Perl tool
            mcselect $DRYRUN "${name}" "${vers}" || \
                echo "> skipping: ${NAME}-tools-${VERSION} isn't installed?";
            if [ -x "bin/${name}-${vers}-py" ]; then
                # Install Python tool as well
                mcselect $DRYRUN "${name}" "${vers}-py" || \
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
    if ! ${INSTALL} || [ "x${VERSION}" = "x" ]; then
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
