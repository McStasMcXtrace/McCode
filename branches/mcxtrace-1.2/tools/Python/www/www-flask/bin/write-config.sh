#!/bin/sh

# replace user in config file with current user

NCONF=nginx/nginx.conf
MCONF=nginx/mcstas.conf

cat ${NCONF}.template | ./scripts/config-preprocessor.sh > ${NCONF}
cat ${MCONF}.template | ./scripts/config-preprocessor.sh > ${MCONF}
