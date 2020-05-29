docker run -u docker -it -e QT_X11_NO_MITSHM=1 --volume C:/Users/%USERNAME%/McStas-dockers:/home/docker -e DISPLAY="host.docker.internal:0" mccode/mcstas-2.6.1-slim:1.1 mcgui
