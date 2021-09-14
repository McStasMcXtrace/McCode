docker run -u docker -it -e QT_X11_NO_MITSHM=1 --volume C:/Users/%USERNAME%/McStas-dockers:/home/docker -e DISPLAY="host.docker.internal:0" mccode/mcstas-3.0:1.2 mcstas-3.0-environment
