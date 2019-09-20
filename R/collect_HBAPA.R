#**************************************************************************
# Title: collect_HBAPA.R
# Author: William Murrah
# Description:
# ReadMe for "How health behaviors relate to academic performance via affect: 
# An intensive longitudinal study" by Flueckiger L, Lieb R, Meyer AH, Mata J 
# 
# 
# *********** CONTENT ***********
#   1. README.txt - includes a description of the variables
# 2. Dataset_HealthBehavAcadPerfAffect.xls - data file
# 
# 
# *********** VARIABLE NAMES ***********
#   
# ID					Subject number
# Day					Survey day
# Sex					Participants' sex
# Age					Participants' age
# Sem					Semester: Number of semesters studied
# SQ					Sleep quality 1 (very bad) to 4 (very good)
# PhysAct			Physical activity: Number of minutes engaged in mild, 
#             moderate and strenuous 	exercise weighted by metabolic 
#             equivalents and then summed to produce a total	daily leisure 
#             activity score.
# PA					Positive affect 1 (not at all) to 7 (extremely)
# NA					Negative affect 1 (not at all) to 7 (extremely)
# LGA					Learning goal achievement 0 (not at all) to 4 (completely)
# Exam				Examination success 0 (fail) 1 (pass)
# HSG					High school grades 1 (lowest grade) to 6 (highest grade)
# BDI					Beck Depression Inventory 1(not) 2 (mild to moderate) 3 
#             (clinically relevant symptoms)
# -99					Missing value
# 
# 
# *********** PROCESSING STEPS ***********
# 
# Physical activity was square root transformed to approximate a normal 
# distribution 
# Negative affect was log transformed to approximate a normal 
# distribution
# 
# 
# *********** CONTACT ***********
# 
# For any further queries please contact:
# 
# Jutta Mata
# University of Basel
# Department of Psychology
# Division of Clinical Psychology and Epidemiology
# Missionsstrasse 62a
# Switzerland
# Phone: + 41 61 267 06 54
# e-mail: jutta.mata@unibas.ch

# Created: Thursday, 11 April 2019
# R version: R version 3.5.3 (2019-03-11)
# Directory: C:/Users/wmm0017/Projects/LongitudinalDataAnalysis
#**************************************************************************
# packages used -----------------------------------------------------------
library(tidyverse)  
library(data.table)
hba <- haven::read_sav("../../Box Sync/6_Data/HealthBehavAcadPerfAffect/Dataset_HealthBehavAcadPerfAffect.sav")
hba <- setDT(hba)

hba[, .(ID = factor(ID), 
         Day = factor(Day),
         Sex = factor(Sex),
         Sem = factor(Sem))]

library(TSstudio)

hbats <- as.ts(hba)

matplot(hbats[ , 7], type = "l")

ts_plot(hbats)

ggplot(hba, aes(x = Day, y = PhysAct, group = ID)) + 
  geom_point(size = .3, color = "red") +
  geom_line() + facet_wrap(~ID)


