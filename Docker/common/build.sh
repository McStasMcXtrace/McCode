#!/usr/bin/env bash
containername="mccode/common-2021-08:1.0"
docker image build --tag $containername .

echo Build of $containername done.
