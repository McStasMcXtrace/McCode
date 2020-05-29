docker run -u docker -it -e QT_X11_NO_MITSHM=1 --volume C:/Users/%USERNAME%/McXtrace-dockers:/home/docker -e DISPLAY="host.docker.internal:0" mccode/mcxtrace-1.5:1.0 mxgui
