#/!bin/bash
containername="mcstas-2.6.1:1.0"
XSOCK=/tmp/.X11-unix


DOCK_UID=$(id -u)
DOCK_GID=$(id -g)

docker image build --tag $containername .
xhost +
#a simple version with out xauth gynmastics
docker run -v $HOME/dockerhome:/home/docker -u docker -ti -e DISPLAY=host.docker.internal:0 $containername /usr/bin/mcstas-2.6.1-environment


