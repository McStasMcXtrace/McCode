#!/bin/sh
if [ -f "static/data/db.sqlite3" ]; then
    cp static/data/db.sqlite3 .
fi
    
start-stop-daemon --start --background --pidfile PIDFILE_WRK --make-pidfile -d $PWD --exec ./manage.py -- runworker
./manage.py collect_instr
./manage.py runserver 0.0.0.0:8000
start-stop-daemon --stop --pidfile PIDFILE_WRK -d $PWD
cat worker.log
cp db.sqlite3 static/data/
cp worker.log static/data/
