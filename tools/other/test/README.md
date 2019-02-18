This folder contains two Matlab scripts for visualising differences between simulated datasets, see https://jira.esss.lu.se/browse/MCSTAS-82 and https://github.com/McStasMcXtrace/McCode/issues/752

To compare two selftests, run e.g.:
    McTest_compare('selftest_kMgn','selftest_5ntz')

To compare two normal datasets run e.g.:
    McCode_compare('selftest_kMgn/Test_Collimator_Radial_1','selftest_5ntz/Test_Collimator_Radial_1')

Prerequisites:
Matlab 2018b or newer, iFit installed
