#**************************************************************************
# Title: collect_rundata.R
# Author: William Murrah
# Description:
# Created: Saturday, 17 November 2018
# R version: R version 3.5.1 (2018-07-02)
# Directory: /home/wmmurrah/Projects/LongitudinalDataAnalysis
#**************************************************************************
# packages used -----------------------------------------------------------
library(tidyverse)
library(trackeR)
library(TSstudio)

run1 <- readTCX("data/tcx/activity_3146091492.tcx")

r1 <- run1 %>% 
  select(-distance)

plot.ts(run1$time, run1$heart_rate, xy.lines = TRUE)  

ts_plot(r1, type = "single")
