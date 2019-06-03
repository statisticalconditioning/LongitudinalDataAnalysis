#**************************************************************************
# Title: import_CCES2018.R
# Author: William Murrah
# Description: CCES 2018 data from:
#  https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi%3A10.7910/DVN/ZSBZ7K
#               See documentation in Data_documentation/CCES/CCES_2018.
# Created: Monday, 03 June 2019
# R version: R version 3.6.0 (2019-04-26)
# Directory: C:/Users/wmm0017/Projects/LongitudinalDataAnalysis
#**************************************************************************
# packages used -----------------------------------------------------------
suppressPackageStartupMessages(library(tidyverse))
# Load data from Box Data file.
load("~/Box Sync/6_Data/CooperativeCongressionalElectionStudy/CCES_2018/CCES2018_OUTPUT.RData")  

# Rename data.
cces2018 <- table
rm(table)


# TODO: Below here to  be moved to report ---------------------------------------
library(sjPlot)

names(cces2018)


prop.table(table(cces2018$gender))

cces2018 <- cces2018 %>% 
  mutate(female = as_factor(gender))

cces2018f <- as_factor(cces2018)
cces2018f <-  cces2018f %>% 
  droplevels()
plot_frq(cces2018f$gender)


sjp.grpfrq(cces2018f$pew_churatd, cces2018f$gender)


