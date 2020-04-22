#!/bin/bash

#setup mccode repo
cd /etc/apt/sources.list.d/
sudo wget http://packages.mccode.org/debian/mccode.list

sudo apt-get update
sudo apt-get install mcxtrace-suite-python 
