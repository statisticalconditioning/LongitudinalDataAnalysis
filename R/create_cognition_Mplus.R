#**************************************************************************
# Title: create_cognition_Mplus.R
# Author: William Murrah
# Description:
# Created: Thursday, 04 April 2019
# R version: R version 3.5.3 (2019-03-11)
# Directory: C:/Users/wmm0017/Projects/LongitudinalDataAnalysis
#**************************************************************************
# packages used -----------------------------------------------------------
  
library(MplusAutomation)

cog <- read_dta(file = "data/dta/STATA_Chapter2.dta")
str(cog)


prepareMplusData(cog, filename = "mplus/Cognition/cognition.dat",
                 inpfile = "mplus/Cognition/cogTemplate.inp")
