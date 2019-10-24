#!/bin/bash
./a.out -n1e4 Par1=1 | grep rand01 | cut -f2- -d: | cut -f3 -d\  > rand01.txt
./a.out -n1e4 Par1=1 | grep random | cut -f2- -d: | cut -f3 -d\  > random.txt 
./a.out -n1e4 Par1=1 | grep randnorm | cut -f2- -d: | cut -f3 -d\  > randnorm.txt 
