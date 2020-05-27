#!/usr/bin/env bash
containername="mccode/mcstas-2.6.1-mcstasscript:1.1"
docker image build --tag $containername .

echo Build of $containername done.
