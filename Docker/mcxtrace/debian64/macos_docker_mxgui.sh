#/!bin/bash
containername="mcxtrace-1.5:1.0"
XSOCK=/tmp/.X11-unix


DOCK_UID=$(id -u)
DOCK_GID=$(id -g)

docker image build --tag $containername .
xhost +
#a simple version with out xauth gynmastics
docker run -u docker -ti -e DISPLAY=host.docker.internal:0 $containername /usr/bin/mxgui


