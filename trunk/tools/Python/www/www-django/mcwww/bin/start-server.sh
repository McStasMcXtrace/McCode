#!/bin/sh

# grab dependencies
./bin/get-dependencies.sh

# run nginx (possibly from /usr/sbin)
PATH=/usr/sbin:$PATH

PID=/tmp/mcstas-www-nginx.pid
if [ -f $PID ]; then
    echo ""
    echo "## Killing old nginx process: `cat $PID`";
    echo ""
    sudo kill `cat $PID`
fi

# replace user in config file with current user
./bin/write-config.sh

# start server
echo 'Starting nginx..'
sudo nginx -c `pwd`/nginx/nginx.conf || exit -1

# run uwsgi
./uwsgi/uwsgi -M --workers 10 --socket 127.0.0.1:9001 --env DJANGO_SETTINGS_MODULE=mcwww.settings \
    -w 'django.core.handlers.wsgi:WSGIHandler()'

# clean up after uwsgi close
PID=/tmp/mcstas-www-nginx.pid
if [ -f $PID ]; then
    echo ""
    echo "## Stopping nginx process: `cat $PID`";
    sudo kill `cat $PID`
fi
