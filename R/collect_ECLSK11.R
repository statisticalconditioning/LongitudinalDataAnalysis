#**************************************************************************
# Title: collect_ECLSK11.R
# Author: William Murrah
# Description:
# Created: Sunday, 14 April 2019
# R version: R version 3.5.3 (2019-03-11)
# Directory: C:/Users/wmm0017/Projects/LongitudinalDataAnalysis
#**************************************************************************
# packages used -----------------------------------------------------------
library(tidyverse)
library(psych)


# Import Data -------------------------------------------------------------

path <- "../../Dropbox/Data/ECLSK/ECLSK_2011/"
eclsk11 <- haven::read_sav(paste0(path,"eclsk2011_firstLook.sav"), 
                           user_na = FALSE)
rm(path)


# Clean Data --------------------------------------------------------------

# Names to lowercase for ease of typing in R.
names(eclsk11) <- tolower(names(eclsk11))

# Recoded missing data values less than 0 as NA.

eclsk11 <- eclsk11 %>% 
  mutate_at(.vars = vars(childid:p8schrwk),
             .funs = funs(ifelse(. < 0, NA, .)))

# Recode missing data values.
eclsk11 <- eclsk11 %>% 
  mutate_at(.vars = vars(x1rthetk4:x6dccsscr, x8dccsscr:x8locale,
                         x12par1ed_i:p1chdoby, p1brtwgt:p2bfcare,
                         p6nocare, p8selfca, p8schrwk),
            .funs = funs(ifelse(. == -9, NA, .))) %>% 
  mutate_at(.vars = vars(x3region:x8region, p1selfca, p1schrwk,
                         p4selfca, p4schrwk, p6selfca:p7schrwk),
            .funs = funs(ifelse(. == -2, NA, .)))

describe(eclsk11$x1rthetk4, fast = TRUE)

hist(eclsk11$x1rthetk4)


eclsk11 %>% 
  select(x1rthetk4:x8rthetk4) %>% 
  pairs()
