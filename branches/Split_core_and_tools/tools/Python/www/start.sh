#!/bin/sh

# download and build uwsgi if necessary
UWSGI=uwsgi-1.2
if [ ! -d uwsgi ]; then
    rm -rf uwsgi ${UWSGI}*;
    wget http://projects.unbit.it/downloads/${UWSGI}.tar.gz
    tar xf ${UWSGI}.tar.gz && rm ${UWSGI}.tar.gz
    ln -s ${UWSGI} uwsgi
fi
cd uwsgi && make && cd ..

# run nginx (possibly from /usr/sbin)
PATH=/usr/sbin:$PATH

PID=/tmp/mcstas-www-nginx.pid
if [ -f $PID ]; then
    echo ""
    echo "## Killing old nginx process: `cat $PID`";
    echo ""
    sudo kill `cat $PID`
fi
echo 'Starting nginx..'
sudo nginx -c `pwd`/nginx/nginx.conf

# run uwsgi
./uwsgi/uwsgi --socket 127.0.0.1:9001 --module run --callable app

# clean up after uwsgi close
PID=/tmp/mcstas-www-nginx.pid
if [ -f $PID ]; then
    echo ""
    echo "## Stopping nginx process: `cat $PID`";
    kill `cat $PID`
fi
