#!/bin/bash
(time ./mini_multicore.out -n 1e4 -dmini_multicore_1e4 -s1000) &> mini_multicore_log
echo 1e5
(time ./mini_multicore.out -n 1e5 -dmini_multicore_1e5 -s1000) &>> mini_multicore_log
echo 1e6
(time ./mini_multicore.out -n 1e6 -dmini_multicore_1e6 -s1000) &>> mini_multicore_log
echo 1e7
(time ./mini_multicore.out -n 1e7 -dmini_multicore_1e7 -s1000) &>> mini_multicore_log
echo 1e8
(time ./mini_multicore.out -n 1e8 -dmini_multicore_1e8 -s1000) &>> mini_multicore_log
echo 1e9
(time ./mini_multicore.out -n 1e9 -dmini_multicore_1e9 -s1000) &>> mini_multicore_log
echo 1e10
(time ./mini_multicore.out -n 1e10 -dmini_multicore_1e10 -s1000) &>> mini_multicore_log
echo done
