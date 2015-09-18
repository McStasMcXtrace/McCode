To enable R as plotter for the mcwww simulation worker:

1) Rename this folder from rplot-inactive to rplot

2) Install r-base and r-base-devel

3) Launch R and run
install.packages("fields")

[ If step 4 fails, you may need to find an older version of fields at e.g. 
  https://cran.r-project.org/src/contrib/00Archive/fields/

  Compile the downloaded package using e.g. 
  install.packages("fields_5.02.tar.gz", repos = NULL, type = "source")]

