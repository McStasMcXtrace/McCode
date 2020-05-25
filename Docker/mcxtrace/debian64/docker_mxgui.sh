#!/usr/bin/env bash
containername="mccode/mcxtrace-1.5:1.0"
XSOCK=/tmp/.X11-unix


DOCK_UID=$(id -u)
DOCK_GID=$(id -g)

#docker image build --tag $containername .

#a simple version with out xauth gynmastics
#docker run -u docker -ti -e DISPLAY=$DISPLAY -v $XSOCK:$XSOCK $containername /usr/bin/mxgui

#more sophisticated version which seem to work
export XAUTH=/tmp/.docker.xauth

# Different handling of xauth and --dev on linux than macOS
if [[ "$OSTYPE" == "linux-gnu"* ]]; then
  export DEVICESTRING="--device /dev/dri"
  export DISPLAYVAR=$DISPLAY  
fi

xauth nlist $DISPLAYVAR | sed -e 's/^..../ffff/' | xauth -f ${XAUTH} nmerge -

docker run -u docker -ti -e QT_X11_NO_MITSHM=1 -e DISPLAY=host.docker.internal:0 -v $XSOCK:$XSOCK -v $XAUTH:$XAUTH -v $HOME:/home/docker -e XAUTHORITY=$XAUTH $DEVICESTRING $containername mxgui
