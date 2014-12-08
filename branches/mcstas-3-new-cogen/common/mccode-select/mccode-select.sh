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

  --bootstrap  install called version of mccode-select and mccode-select-bundle.
  --list       list available versions of package.
  --install    install a new version of package for later linking.
  --dryrun     run checks and simulate actions only.
  --help       display this help and exit.

Report bugs to jsbn@fysik.dtu.dk
EOF
}

if [ "x$1" = "x" ] || [ "x$1" = "x--help" ] ; then
    usage;
    exit 0;
fi



# Check to see whether we have update-alternatives (Debian)
ALTERNATIVES=$(command -v update-alternatives)
if [ $? -eq 0 ] && [ -x ${ALTERNATIVES} ]; then
    HAS_ALTERNATIVES=true;
else
    HAS_ALTERNATIVES=false;
fi


if [ "x$1" = "x--no-update-alternatives" ]; then
    HAS_ALTERNATIVES=false;
    shift;
fi



if [ "x$1" = "x--bootstrap" ]; then
    VERSION=$(echo `basename $0` | cut -f'3-' -d'-')

    if [ "x" = "x${VERSION}" ]; then
        echo "Error: Cannot bootstrap without explicit version.";
        exit 1;
    fi

    if ${HAS_ALTERNATIVES}; then
        # Install mccode-select version
        $0 --install mccode-select        ${VERSION};
        $0 --install mccode-select-bundle ${VERSION};
        # Choose mccode-select version
        $0 mccode-select        ${VERSION};
        $0 mccode-select-bundle ${VERSION};
    else
        # Install mccode-select version
        $0 --no-update-alternatives --install mccode-select        ${VERSION};
        $0 --no-update-alternatives --install mccode-select-bundle ${VERSION};
        # Choose mccode-select version
        $0 --no-update-alternatives mccode-select        ${VERSION};
        $0 --no-update-alternatives mccode-select-bundle ${VERSION};
    fi
    exit 0;
fi



# Parse arguments
LIST=false;
INSTALL=false;
DOIT=true;

while true; do
    case "$1" in
        "--install" )
            INSTALL=true;
            ;;
        "--list" )
            LIST=true
            ;;
        "--dryrun" )
            DOIT=false;
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

# Set name and version
NAME="$1"
VERSION="$2"

# Define dirname where existing non-overwritable binaries are moved
BACKUPDIR=$PREFIX/bin/MCCODE_BACKUP_`date +%Y%m%d_%H.%M`

linksTo() {
    (
        link=$1
        file=$2
        if ( ls -l "${link}" | grep "${file}"'$' > /dev/null ); then
            exit 0;
        fi
        ALT=/etc/alternatives/"${link}";

        if [ -e "${ALT}" ] &&
           ( ls -l "${link}" | grep "${ALT}"'$'  > /dev/null ) &&
           ( ls -l "${ALT}"  | grep "${file}"'$' > /dev/null ); then
            exit 0;
        fi

        # Link does not point to file
        exit 1;
    )
}

list() {
    (
        cd "${PREFIX}/bin";
        for ver in "${NAME}-"[0-9]*; do
            SEL=""
            if [ -L "${NAME}" ] && ( linksTo "${NAME}" "${ver}" ); then
                SEL='*'
            fi
            echo "${ver} ${SEL}" | sed s/"${NAME}\-/${NAME}"': '/;
        done
    )
}


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

whenReal() {
    if ${DOIT}; then
        $*
    fi
}


doLink() {
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

check() {
    (
        name="$1"
        file="$2"
        link="$3"

        if ! [ -x "${file}" ]; then
            echo "${name}: cannot locate binary: ${file}"
            exit 1;
        fi
        if [ -e "${link}" ] && ( ! [ -L "${link}" ] ); then
	          echo
            echo "NOTE: ${name} was NOT a link: ${link}, moving to ${BACKUPDIR}/"
            echo

            ${DOIT} &&
            mkdir -p ${BACKUPDIR} &&
            mv ${link} ${BACKUPDIR}/`basename $link`;

            exit 0;
        fi
    )
}

installBinary() {
    (
        name="$1"
        vers="$2"

        link="${PREFIX}/bin/${name}"
        file="${link}-${vers}"

        # Priority:
        # only numbers and dot, then try and compute a priority from this
        prio=$( echo ${vers} | sed \
            -e 's/[^0-9\\.]//g' \
            -e 's/\./ * 100000 + /' | bc);
        # sanitise to only allow numbers (version can be something strange)
        prio=`echo ${prio} | sed 's/[^0-9]//g'`
        if [ "x${prio}" = "x" ]; then
            prio=1;
        fi

        # Python beats Perl
        if ( echo "${vers}" | grep '\-py' >/dev/null ); then
            prio=`echo ${prio} '+' 1 | bc`;
        fi

        # Sanity check
        if ! (check "${name}" "${file}" "${link}"); then
            exit 1;
        fi

        MANCMD=""
        manlink="${PREFIX}/man/man1/${name}.1"
        manfile="${PREFIX}/man/man1/${name}-${vers}.1"
        if [ -f "${manfile}" ]; then
            MANCMD="--slave ${manlink} ${name}.1 ${manfile}"
        fi

        # Install using update alternatives
        if ${HAS_ALTERNATIVES}; then
            echo "INSTALL: ${name}: ${file}"
            whenReal ${ALTERNATIVES} --install \
                "${link}" "${name}" "${file}" ${prio} \
                ${MANCMD} ;
        fi

        # When update-alternatives is not present --install only checks
    )
}

linkBinary() {
    (
        cd "${PREFIX}";

        name="$1";
        vers="$2"

        link="${PREFIX}/bin/${name}";
        file="${link}-${vers}";

        # Sanity check
        if ! (check "${name}" "${file}" "${link}"); then
            exit 1;
        fi


        if ${HAS_ALTERNATIVES}; then
            echo "${name} -> ${file}"
            whenReal installBinary "${name}" "${vers}"
            whenReal ${ALTERNATIVES} --set "${name}" "${file}"
            exit 0;
        fi
        whenReal doLink "${file}" "${link}"

        # Man pages are auto linked when using update-alternatives
        # The below is for manual linking only

        link="${PREFIX}/man/man1/${name}.1";
        file="${PREFIX}/man/man1/${name}-${vers}.1";

        if [ -f "${file}" ]; then
            whenReal doLink "${file}" "${link}";
        fi
    )
}


if ${LIST}; then
    list "${NAME}"
else
    if ${INSTALL}; then
        linkBinary "${NAME}" "${VERSION}"
    fi
fi
