#**************************************************************************
# Title: sentenceChap12_Hoffman.R
# Author: William Murrah
# Description:
# Created: Friday, 22 February 2019
# R version: R version 3.5.2 (2018-12-20)
# Directory: /home/wmmurrah/Projects/LongitudinalDataAnalysis
#**************************************************************************
# packages used -----------------------------------------------------------
library(tidyverse)  
library(lme4)
library(haven)
library(texreg)
library(car)

sentence <- read_dta("data/dta/STATA_Chapter12.dta")

sentence <- sentence %>% 
  mutate(subjectid = factor(subjectid),
         itemid = factor(itemid))

sentenceID <- sentence %>% 
  group_by(subjectid) %>% 
  summarise(older = mean(older),
         age = mean(age))

sentenceID %>% 
  group_by(older) %>% 
  summarise(mean = mean(age),
            sd = sd(age),
            n = n()) 
 


amod0 <- aov(rt ~ subjectid + itemid, data = sentence)
summary(amod0)

mod0 <- lmer(rt ~ 1 + (1 | subjectid) + (1 | itemid), sentence)

summary(mod0)
screenreg(mod0)

table(older ~ age, sentence)
table(sentence$older, sentence$age)
