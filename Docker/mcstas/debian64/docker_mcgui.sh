#!bin/bash
containername="mccode/mcstas-2.6.1:1.0"
XSOCK=/tmp/.X11-unix


DOCK_UID=$(id -u)
DOCK_GID=$(id -g)

#docker image build --tag $containername .

#a simple version with out xauth gynmastics
#docker run -u docker -ti -e DISPLAY=$DISPLAY -v $XSOCK:$XSOCK $containername /usr/bin/mcgui

#more sophisticated version which seem to work
XAUTH=/tmp/.docker.xauth
xauth nlist :0 | sed -e 's/^..../ffff/' | xauth -f $XAUTH nmerge -
docker run -u docker -ti -e DISPLAY=$DISPLAY -v $XSOCK:$XSOCK -v $XAUTH:$XAUTH -v $HOME:/home/docker -e XAUTHORITY=$XAUTH --device /dev/dri $containername mcgui
