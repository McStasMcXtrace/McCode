#!/usr/bin/env bash
containername="cloudbusting/mcstasscript:1.1"
podman build -t $containername -f Dockerfile.podman .

echo Build of $containername done.
