#!/bin/sh
 
### BEGIN INIT INFO
# Provides:          mcwww
# Required-Start:    $remote_fs $syslog
# Required-Stop:     $remote_fs $syslog
# Default-Start:     2 3 4 5
# Default-Stop:      0 1 6
# Short-Description: Put a short description of the service here
# Description:       Put a long description of the service here
### END INIT INFO
 
# Change the next 3 lines to suit where you install your script and what you want to call it
DIR=/home/mcroot/McStas/www-django/mcwww
DAEMON=$DIR/manage.py
DAEMON_NAME=mcwww-worker
 
# This next line determines what user the script runs as.
DAEMON_USER=root
 
# The process ID of the script when it runs is stored here:
PIDFILEWRK=/var/run/$DAEMON_NAME-work
PIDFILESRV=/var/run/$DAEMON_NAME-srv
IPPORT=127.0.0.1:8000
NUMWORK=4

. /lib/lsb/init-functions
 
do_start () {
    log_daemon_msg "Starting system $DAEMON_NAME daemon"
    i=1
    while [ $i -le $NUMWORK ];
    do
	PIDFILE=$PIDFILEWRK-$i
	start-stop-daemon --start --background --pidfile $PIDFILE --make-pidfile --user $DAEMON_USER -d $DIR --chuid $DAEMON_USER --exec $DAEMON -- runworker
	((i = i + 1))
    done
    PIDFILE=$PIDFILESRV
    start-stop-daemon --start --background --pidfile $PIDFILE --make-pidfile --user $DAEMON_USER -d $DIR --chuid $DAEMON_USER --exec $DAEMON -- runserver $IPPORT
    log_end_msg $?
}
do_stop () {
    log_daemon_msg "Stopping system $DAEMON_NAME daemon"
    i=1
    while [ $i -le $NUMWORK ];
    do
	PIDFILE=$PIDFILEWRK-$i
	start-stop-daemon --stop --pidfile $PIDFILE --retry 10
	((i = i + 1))
    done
    PIDFILE=$PIDFILESRV
    start-stop-daemon --stop --pidfile $PIDFILE --retry 10
    ps -ef | grep manage.py | grep -v grep | cut -b 10-15 | xargs kill -TERM
    log_end_msg $?
}
 
case "$1" in
 
    start|stop)
	cd $DIR
        do_${1}
        ;;
 
    restart|reload|force-reload)
        do_stop
        do_start
        ;;
 
    status)
        status_of_proc "$DAEMON_NAME" "$DAEMON" && exit 0 || exit $?
        ;;
    *)
        echo "Usage: /etc/init.d/$DAEMON_NAME {start|stop|restart|status}"
        exit 1
        ;;
 
esac
exit 0
