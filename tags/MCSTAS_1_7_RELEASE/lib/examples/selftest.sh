#! /bin/sh
#
# This script contains the commands needed to run the example simulations.
# These examples are provided for both validating the McStas package (cross
# comparison between releases versions or with other packages), as well as new 
# components from the library.
#
# The script must be run from the examples/ directory.
n_single=1e5
n_scan=1e4
echo "# Welcome to the Instrument examples (self-test) for the McStas package" > test.log
echo "# Welcome to the Instrument examples (self-test) for the McStas package"
mcstas -v
date
mcstas -v >> test.log
date >> test.log
echo "# Performed on $HOST [`uname -a`]" >> test.log
echo "# Cleaning previous C code and executable files"
rm -r -f *.out *.gif *.c
echo "# Cleaning previous tests"
rm -r -f prisma2a prisma2b linup_* *.sim V_test* h8_test
echo "# Testing prisma2" >> test.log
echo "# Testing prisma2"
mcrun --dir=prisma2a prisma2.instr --ncount=$n_single TT=-30 PHA=22 PHA1=-3 PHA2=-2 PHA3=-1 PHA4=0 PHA5=1 PHA6=2 PHA7=3 TTA=44 >> test.log
mcrun prisma2.instr --dir=prisma2b --ncount=$n_single TT=-30 PHA=22 PHA1=3 PHA2=2 PHA3=1 PHA4=0 PHA5=-1 PHA6=-2 PHA7=-3 TTA=44 >> test.log
echo "# Testing Vanadium sample" >> test.log
echo "# Testing Vanadium sample"
mcrun --dir=V_test vanadium_example.instr --ncount=$n_single ROT=0 >> test.log
echo "# Testing former TAS1 (Risoe): lineup_1" >> test.log
echo "# Testing former TAS1 (Risoe): lineup_1"
mcrun --numpoints=41 -n $n_scan --dir=linup_1_45 linup-1.instr PHM=-39,-35 TTM=-74 C1=0 >> test.log
mcrun --numpoints=41 -n $n_scan --dir=linup_2_45 linup-1.instr PHM=-39,-35 TTM=-74 C1=30 >> test.log
echo "# Testing former TAS1 (Risoe): lineup_2" >> test.log
echo "# Testing former TAS1 (Risoe): lineup_2"
mcrun --numpoints=41 -n $n_scan  --dir=linup_3_45 linup-2.instr PHM=-37.077 TTM=-74 C1=30 OMC1=-50,50 >> test.log
mcrun --numpoints=41 -n $n_scan --dir=linup_4_45 linup-2.instr PHM=-39,-35 TTM=-74 C1=30 OMC1=-1.81715 >> test.log
mcrun --numpoints=31 -n $n_scan --dir=linup_5_m5 linup-2.instr PHM=-38.5,-35.5 TTM=-74 C1=30 OMC1=-5 >> test.log
mcrun --numpoints=31 -n $n_scan --dir=linup_5_m6 linup-2.instr PHM=-38.5,-35.5 TTM=-74 C1=30 OMC1=-6 >> test.log
mcrun --numpoints=31 -n $n_scan --dir=linup_5_m10 linup-2.instr PHM=-38.5,-35.5 TTM=-74 C1=30 OMC1=-10 >> test.log
echo "# Testing former TAS1 (Risoe): lineup_3" >> test.log
echo "# Testing former TAS1 (Risoe): lineup_3"
mcrun --numpoints=41 -n $n_scan --dir=linup_6_0 linup-3.instr PHM=-37.077 TTM=-74 TT=-1.5,1.5 C1=30 OMC1=-5.5 C2=0 C3=0 >> test.log
echo "# Testing former TAS1 (Risoe): lineup_4" >> test.log
echo "# Testing former TAS1 (Risoe): lineup_4"
mcrun --numpoints=41 -n $n_scan --dir=linup_7 linup-4.instr PHM=-37.077 TTM=-74 TT=33.52 TTA=-3,3 C1=30 OMC1=-5.5 C2=28 C3=0 >> test.log
mcrun --numpoints=41 -n $n_scan --dir=linup_8 linup-4.instr PHM=-37.077 TTM=-74 TT=33.52 TTA=-3,3 C1=30 OMC1=-5.5 C2=28 C3=67 >> test.log
echo "# Testing former TAS1 (Risoe): lineup_5" >> test.log
echo "# Testing former TAS1 (Risoe): lineup_5"
mcrun --numpoints=41 -n $n_scan --dir=linup_9 linup-5.instr PHM=-37.077 TTM=-74 TT=32,35 TTA=0 C1=30 OMC1=-5.5 C2=28 C3=67 >> test.log
mcrun --numpoints=41 -n $n_scan --dir=linup_10 linup-5.instr PHM=-37.077 TTM=-74 TT=-32,-35 TTA=0 C1=30 OMC1=-5.5 C2=28 C3=67 >> test.log
echo "# Testing former TAS1 (Risoe): lineup_6" >> test.log
echo "# Testing former TAS1 (Risoe): lineup_6"
mcrun --numpoints=21 -n $n_scan --dir=linup_11 linup-6.instr PHM=-37.077 TTM=-74 TT=33.57 OMA=-16.44,-18.44 TTA=-34.883 C1=30 OMC1=-5.5 C2=28 C3=67 >> test.log
echo "# Testing former TAS1 (Risoe): lineup_7" >> test.log
echo "# Testing former TAS1 (Risoe): lineup_7"
mcrun --numpoints=21 -n $n_scan --dir=linup_13 linup-7.instr PHM=-37.077 TTM=-74 TT=32.5,34.5 OMA=-17.45 TTA=-34.9 C1=30 OMC1=-5.5 C2=28 C3=67 >> test.log
echo "# Testing former H8 (Brookhaven): h8_test" >> test.log
echo "# Testing former H8 (Brookhaven): h8_test"
mcrun  -n $n_single --dir=h8_test  h8_test.instr Lambda=2.359  >> test.log
echo "# End of tests"
echo "# End of tests" >> test.log
date >> test.log
date
