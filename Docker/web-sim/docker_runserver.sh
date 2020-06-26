#!/usr/bin/env bash
containername="mccode/websim:1.4"

mkdir -p $HOME/McStas-dockers/sim-local
mkdir -p $HOME/McStas-dockers/simdata-local
docker run -u docker -ti -v $HOME/McStas-dockers/sim-local:/home/docker/McWeb/mcsimrunner/sim/local -v $HOME/McStas-dockers/simdata-local:/home/docker/McWeb/mcsimrunner/static/data -p 8000:8000 --workdir="/home/docker/McWeb/mcsimrunner" $containername bash go.sh

#./manage.py runserver 0.0.0.0:8000

