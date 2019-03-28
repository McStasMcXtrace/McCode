# test # of neutrons before virtual source
rm -rf RITA-II_40
mcrun RITA-II.instr -n1e6  BPL=0.97 BPH=1.03 EI=5 EN=0 COLL_MS=40 SAMPLE=1 OUTFILTER=0 REP=10 -d  RITA-II_40

# generating a virtual source
rm -rf RITA-II_Vsource40
mcrun RITA-II.instr -n3e6  BPL=0.97 BPH=1.03 EI=5 EN=0 COLL_MS=40 SAMPLE=1 OUTFILTER=0 REP=10 VIRTUALOUT=1 -d  RITA-II_Vsource40


# Seeing 1-3rd order and using Vanadium sample
rm -rf RITA-II_Vanadium
mcrun RITA-II.instr -n3e6 BPL=0.30 BPH=1.03 EI=5 EF=5 SAMPLE=1 INFILTER=0 OUTFILTER=0  REP=10 -d RITA-II_Vanadium


# Using a virtual source
rm -rf RITA-II_VINsource40_powder

mcrun RITA-II.instr EI=5 EF=5 SAMPLE=2 SAMPLEFILE="Al2O3_sapphire.lau" BARNS=0 SOURCEFILE="RITA-II_Vsource40/Vin_default.dat" VIRTUALIN=1 REP=3 -d RITA-II_VINsource40_powder
# Al2O3_sapphire.laz is much different from the  Al2O3_sapphire.lau in the development tree

# test without VIN
rm -rf RITA-II_40_powder

mcrun RITA-II.instr -n3e6 EF=5 EN=0 SAMPLE=2 COLL_MS=40 SAMPLEFILE="Al2O3_sapphire.lau" BARNS=0 SOURCEFILE="RITA-II_Vsource40/Vin_default.dat" REP=10 -d RITA-II_40_powder

# test powder in 2-axis mode
mcrun RITA-II.instr -n 1e7 ITAR=0.71 COLL_MS=19.6 BPL=0.97 BPH=1.03 EI=5 EF=5 QH=0 QK=0 QL=0 QM=1 A3=0.001 A4=-71 AA5=-90 A6=1e-3 MST=25 MSB=25 MSL=10 MSR=10 SAMPLE=2 SAMPLEFILE="Al2O3_sapphire.lau" SST=25 SSB=25 SSL=10 SSR=10 OUTFILTER=0 OUTFILTERFILE=Be.trm COARSE=1 REP=5 LC=6 RC=4 REP=1 BARNS=0 ANAFORCE=1 -d RITA-II_Al2O3_2axis

# test single crystal elastic
rm -rf SXPb200
mcrun RITA-II.instr -n1e6 BPL=0.97 BPH=1.03 EI=5 EF=5 SAMPLE=3 REP=10 QH=2 QM=0 AS=4.95 BS=4.95 CS=4.95 AAX=-4.95 BBZ=4.95 CCY=4.95 AH=0 AK=1 AL=0  BH=1 BK=0 BL=0 SAMPLEFILE="Pb.laz" -d SXPb200

# Make inelastic test
rm -rf SXPb200_2meV
mcrun RITA-II.instr -n1e6 L0=3.418 BPL=0.97 BPH=1.03 EI=7 EF=5 SAMPLE=4 REP=10 QH=2 QK=0.16 QM=0 AS=4.95 BS=4.95 CS=4.95 AAX=-4.95 BBZ=4.95 CCY=4.95 AH=0 AK=1 AL=0  BH=1 BK=0 BL=0 SAMPLEFILE="Pb.laz" -d SXPb200_2meV

