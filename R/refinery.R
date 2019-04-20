#**************************************************************************
# Title: refinery.R
# Author: William Murrah
# Description:
# Created: Monday, 15 April 2019
# R version: R version 3.5.3 (2019-03-11)
# Directory: /home/wmmurrah/Projects/LongitudinalDataAnalysis
#**************************************************************************
# packages used -----------------------------------------------------------
library(TSstudio)  

refine <- read.table("data/dat/refinery.dat", header = FALSE)
names(refine) <- c("t", "u", "x")

refine <- as.ts(refine)

ts_info(refine)

plot(refine)

ts_plot(refine)

length(t)
length(refine$t)
names(refine)
