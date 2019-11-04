#!/bin/bash
(time ./templateLaue_gcc.out reflections=C60.lau -n 1e4 -dtemplateLaue_gcc_1e4 -s1000) &> templateLaue_gcc_log
echo 1e5
(time ./templateLaue_gcc.out reflections=C60.lau -n 1e5 -dtemplateLaue_gcc_1e5 -s1000) &>> templateLaue_gcc_log
echo 1e6
(time ./templateLaue_gcc.out reflections=C60.lau -n 1e6 -dtemplateLaue_gcc_1e6 -s1000) &>> templateLaue_gcc_log
echo 1e7
(time ./templateLaue_gcc.out reflections=C60.lau -n 1e7 -dtemplateLaue_gcc_1e7 -s1000) &>> templateLaue_gcc_log
echo 1e8
(time ./templateLaue_gcc.out reflections=C60.lau -n 1e8 -dtemplateLaue_gcc_1e8 -s1000) &>> templateLaue_gcc_log
echo done
