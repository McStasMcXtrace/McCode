#!/bin/bash
./a.out | grep rand01 | cut -f2- -d:   > rand01.txt
./a.out | grep random | cut -f2- -d: > random.txt 
./a.out | grep randnorm | cut -f2- -d: > randnorm.txt 
