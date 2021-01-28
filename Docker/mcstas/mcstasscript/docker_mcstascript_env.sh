#!/usr/bin/env bash
containername="mccode/mcstas-2.6.1-mcstasscript:1.1"
XSOCK=/tmp/.X11-unix

DOCK_UID=$(id -u)
DOCK_GID=$(id -g)

export XAUTH=/tmp/.docker.xauth

# Different handling of xauth and --dev on linux than macOS
if [[ "$OSTYPE" == "linux-gnu"* ]]; then
  export DEVICESTRING="--device /dev/dri"
  export DISPLAYVAR=$DISPLAY
  export DISPLAY_TO_USE=$DISPLAY
elif [[ "$OSTYPE" == "darwin"* ]]; then
  export DISPLAY_TO_USE=host.docker.internal:0
  # Hack to start XQuartz without raising a host-based xterm
  xhost > /dev/null
fi

xauth nlist $DISPLAYVAR | sed -e 's/^..../ffff/' | xauth -f ${XAUTH} nmerge -
mkdir -p $HOME/McStas-dockers
docker run -u docker -ti -e QT_X11_NO_MITSHM=1 -e JUPYTER_RUNTIME_DIR=/tmp -v $XSOCK:$XSOCK -v XAUTH:$XAUTH -e DISPLAY:${DISPLAY_TO_USE} -v $HOME:/home/docker -e XAUTHORITY=$XAUTH $DEVICESTRING $containername bash --login
