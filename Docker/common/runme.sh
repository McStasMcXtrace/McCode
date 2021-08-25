docker run -e VNCRES='1920x1080' -v $HOME/McCode-dockers:/home/docker -d -p 127.0.0.1:5901:5901 -e VNCUSER='docker' -e VNCUID='1000' -e VNCGROUP='users' -e VNCPASS='docker' mccode/common-2021-08:1.0
