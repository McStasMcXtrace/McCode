#!/usr/bin/env bash
containername="mccode/websim:1.4"
docker image build --tag $containername .

echo Build of $containername done.
