#!/bin/sh

# grab dependencies
./bin/download.sh


# run nginx (possibly from /usr/sbin)
PATH=/usr/sbin:$PATH

NCONF=nginx/nginx.conf
MCONF=nginx/mcstas.conf
PID=/tmp/mcstas-www-nginx.pid
if [ -f $PID ]; then
    echo ""
    echo "## Killing old nginx process: `cat $PID`";
    echo ""
    sudo kill `cat $PID`
fi
# replace user in config file with current user
cat ${NCONF}.template|./preconf.sh > ${NCONF}
cat ${MCONF}.template|./preconf.sh > ${MCONF}

# start server
echo 'Starting nginx..'
sudo nginx -c `pwd`/nginx/nginx.conf || exit -1

# run uwsgi
./uwsgi/uwsgi --workers 10 --socket 127.0.0.1:9001 --module run --callable app

# clean up after uwsgi close
PID=/tmp/mcstas-www-nginx.pid
if [ -f $PID ]; then
    echo ""
    echo "## Stopping nginx process: `cat $PID`";
    sudo kill `cat $PID`
fi
