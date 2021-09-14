#!/bin/sh
docker run -e VNCRES='1920x1080' -v $HOME/McCode-dockers:/home/docker -d -p 127.0.0.1:5901:5901 -e VNCUSER='docker' -e VNCUID='1000' -e VNCGROUP='users' -e VNCPASS='docker' mccode/common-2021-08:1.0

echo Your McCode docker should now be running in the background!
echo
echo Connect to it via a VNC client on localhost:5901 and use the VNC password 'docker'
echo
echo The username in the docker is 'docker', whose home directory is mapped to
echo your local $HOME/McCode-dockers folder
echo
echo For other options, please refer to the documentation on this site:
echo https://github.com/McStasMcXtrace/ubuntu-desktop
echo
