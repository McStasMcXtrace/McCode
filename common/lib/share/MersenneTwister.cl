/*
 * Copyright 1993-2010 NVIDIA Corporation.  All rights reserved.
 *
 * Please refer to the NVIDIA end user license agreement (EULA) associated
 * with this source code for terms and conditions that govern your use of
 * this software. Any use, reproduction, disclosure, or distribution of
 * this software and related documentation outside the terms of the EULA
 * is strictly prohibited.
 *
 * Modified by Jinyan LIU
 * Date: March 2014
 */


#define   MT_RNG_COUNT 1
#define          MT_MM 397
#define          MT_NN 624
#define       MT_WMASK 0xFFFFFFFFU
#define       MT_UMASK 0xFFFFFFFEU
#define       MT_LMASK 0x1U
#define      MT_SHIFT0 12
#define      MT_SHIFTB 7
#define      MT_SHIFTC 15
#define      MT_SHIFT1 18
#define PI 3.14159265358979f

////////////////////////////////////////////////////////////////////////////////
// OpenCL Kernel for Mersenne Twister RNG
////////////////////////////////////////////////////////////////////////////////

__kernel void MersenneTwister(__global unsigned long* d_Rand, 
			      int nPerRng, int seed)
{
    int globalID = get_global_id(0);
    int iState, iState1, iStateM, iOut;
    unsigned int mti, mti1, mtiM, x;
    unsigned int mt[MT_NN], matrix_a, mask_b, mask_c; 

    //Load bit-vector Mersenne Twister parameters
    matrix_a = 0x9908b0dfU;
    mask_b   = 0x9d2c5680U;
    mask_c   = 0xefc60000U;
        
    //Initialize current state
    mt[0] = seed & MT_WMASK;
    for (iState = 1; iState < MT_NN; iState++)
        mt[iState] = (1812433253U * (mt[iState - 1] ^ (mt[iState - 1] >> 30)) + iState) & MT_WMASK;

    iState = 0;
    mti1 = mt[0];
    for (iOut = 0; iOut < nPerRng; iOut++) {
        iState1 = iState + 1;
        iStateM = iState + MT_MM;
        if(iState1 >= MT_NN) iState1 -= MT_NN;
        if(iStateM >= MT_NN) iStateM -= MT_NN;
        mti  = mti1;
        mti1 = mt[iState1];
        mtiM = mt[iStateM];

	    // MT recurrence
        x = (mti & MT_UMASK) | (mti1 & MT_LMASK);
	      x =  mtiM ^ (x >> 1) ^ ((x & 1) ? matrix_a : 0);

        mt[iState] = x;
        iState = iState1;

        //Tempering transformation
        x ^= (x >> MT_SHIFT0);
        x ^= (x << MT_SHIFTB) & mask_b;
        x ^= (x << MT_SHIFTC) & mask_c;
        x ^= (x >> MT_SHIFT1);
	
        //Convert to (0, 1] float and write to global memory
        d_Rand[globalID /*+ iOut * MT_RNG_COUNT */] = x;
    }
}

