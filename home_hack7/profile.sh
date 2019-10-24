#!/bin/sh
nsys profile ./BNL_H8_simple_GPU_KISS.out -n1e6 lambda=2.36 -d 1e6profile
nsys profile ./BNL_H8_simple_GPU_KISS.out -n1e7 lambda=2.36 -d 1e7profile
nsys profile ./BNL_H8_simple_GPU_KISS.out -n1e8 lambda=2.36 -d 1e8profile

