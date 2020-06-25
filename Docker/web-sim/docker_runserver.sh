#!/usr/bin/env bash
containername="mccode/websim:1.1"

mkdir -p $HOME/McStas-dockers/sim-local
docker run -u docker -ti -v $HOME/McStas-dockers/sim:/home/docker/McWeb/mcsimrunner/sim/local -p 8000:8000 --workdir="/home/docker/McWeb/mcsimrunner" $containername bash go.sh

#./manage.py runserver 0.0.0.0:8000

