Found what looks to be a functional MergeSort code using OpenACC pragmas

pgcc -ta=tesla:cc70,managed,deepcopy -Minfo=accel MergeSort.c -o MergeSort
min:
     51, Generating acc routine seq
         Generating Tesla code
mergeSort:
     60, Generating copy(arr[:n])
     64, Generating Tesla code
         66, #pragma acc loop gang /* blockIdx.x */
merge:
     84, Generating Tesla code
         95, #pragma acc loop vector /* threadIdx.x */
         98, #pragma acc loop vector /* threadIdx.x */
        123, #pragma acc loop seq
        131, #pragma acc loop seq
     95, Loop is parallelizable
     98, Loop is parallelizable
    106, Loop carried scalar dependence for i at line 119,126,125
         Loop carried scalar dependence for j at line 106,108
         Loop carried scalar dependence for i at line 111,110
         Loop carried scalar dependence for j at line 116
         Scalar last value needed after loop for i at line 123
         Loop carried scalar dependence for j at line 134,133
         Loop carried scalar dependence for i at line 108
         Loop carried scalar dependence for j at line 115
    110, Accelerator restriction: induction variable live-out from loop: k
    115, Accelerator restriction: induction variable live-out from loop: k
    118, Accelerator restriction: induction variable live-out from loop: k

./MergeSort

How many elements in the array? List Before Sorting...
955 226 763 337 527 478 72 53 212 278 966 707 772 575 581 770 815 181 896 107 230 629 82 754 342 861 608 117 354 434 519 309 12 282 999 891 113 71 944 677 701 911 384 825 486 965 595 653 499 491 113 729 473 195 484 167 56 444 284 763 230 804 72 242 86 71 134 551 495 78 229 548 989 965 374 827 931 969 481 782 813 594 511 286 141 347 805 549 144 90 312 374 246 737 617 684 160 751 236 655 

Sorted array:  12 53 56 71 71 72 72 78 82 86 90 107 113 113 117 134 141 144 160 167 181 195 212 226 229 230 230 236 242 246 278 282 284 286 309 312 337 342 347 354 374 374 384 434 444 473 478 481 484 486 491 495 499 511 519 527 548 549 551 575 581 594 595 608 617 629 653 655 677 684 701 707 729 737 751 754 763 763 770 772 782 804 805 813 815 825 827 861 891 896 911 931 944 955 965 965 966 969 989 999 


Array is sorted
ACC
Time: 0.00296903

Size of the array is 100

