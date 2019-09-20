#**************************************************************************
# Title: ILD_Intro_Ram.R
# Author: William Murrah
# Description:
# Created: Saturday, 25 May 2019
# R version: R version 3.6.0 (2019-04-26)
# Directory: /home/wmmurrah/Projects/LongitudinalDataAnalysis
#**************************************************************************
# packages used -----------------------------------------------------------
library(tidyverse)
library(psych)

#set filepath for data file
filepath <- "https://quantdev.ssri.psu.edu/sites/qdev/files/AMIBbrief_raw_daily1.csv"
#read in the .csv file using the url() function
daily <- read.csv(file=url(filepath),header=TRUE)
names(daily) <- tolower(names(daily))

head(daily, 12)

length(unique(daily$id))

daily %>% 
  group_by(id) %>% 
  summarize(max = max(day)) %>% 
  ggplot(aes(max)) + geom_histogram()


