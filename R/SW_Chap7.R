#**************************************************************************
# Title: SW_Chap7.R
# Author: William Murrah
# Description:
# Created: Saturday, 10 November 2018
# R version: R version 3.5.1 (2018-07-02)
# Directory: /home/wmmurrah
#**************************************************************************
# packages used -----------------------------------------------------------
library(tidyverse)
library(nlme)
library(texreg)

opposites <- read.table("https://stats.

library(mosaic)
library(mosaicCalc)


fx <- makeFun(

library(mosaic)
library(mosaicCalc)


fx <- makeFun(

library(mosaic)
library(mosaicCalc)


fx <- makeFun(

library(mosaic)
library(mosaicCalc)


fx <- makeFun(

library(mosaic)
library(mosaicCalc)


fx <- makeFun(

library(mosaic)
library(mosaicCalc)


fx <- makeFun(

library(mosaic)
library(mosaicCalc)


fx <- makeFun(

library(mosaic)
library(mosaicCalc)


fx <- makeFun(

library(mosaic)
library(mosaicCalc)


fx <- makeFun(

library(mosaic)
library(mosaicCalc)


fx <- makeFun(

library(mosaic)
library(mosaicCalc)


fx <- makeFun(

library(mosaic)
library(mosaicCalc)


fx <- makeFun(idre.ucla.edu/stat/r/examples/alda/data/opposites_pp.txt",header=TRUE,sep=",")

opposites <- opposites %>% 
  mutate(id = factor(id),
         time = factor(time))

ggplot(opposites, aes(time, opp, group = id)) + geom_line()

opp0 <- lme(opp ~ 1, opposites, random = ~  1 |id)
summary(opp0)

opp.reml <- lme(opp~time*ccog, opposites, random= ~time | id)
summary(opp.reml)

screenreg(list(opp0, opp.reml))


corandcov <- function(glsob,cov=T,...){
  corm <- corMatrix(glsob$modelStruct$corStruct)[[5]]
  print(corm)
  varstruct <- print(glsob$modelStruct$varStruct)  
  varests <- coef(varstruct, uncons=F, allCoef=T)
  covm <- corm*glsob$sigma^2*t(t(varests))%*%t(varests)
  return(covm)}

unstruct <- gls(opp~time*ccog,opposites, correlation=corSymm(form = ~ 1 |id),  
                weights=varIdent(form = ~ 1|wave),method="REML")
corandcov(unstruct)

comsym <- gls(opp~time*ccog,opposites, 
              correlation=corCompSymm(,form = ~ 1 |id),
              method="REML")
cc <- corMatrix(comsym$modelStruct$corStruct)[[5]]
print(cc)

cc * comsym$sigma^2

hetercom <- gls(opp~time*ccog,opposites, 
                correlation=corCompSymm(,form = ~ 1 |id),
                weights=varIdent(form = ~1|wave), 
                method="REML")
corandcov(hetercom)

auto1 <- gls(opp~time*ccog,opposites, 
             correlation=corAR1(,form = ~ 1 |id), 
             method="REML")
cc <- corMatrix(auto1$modelStruct$corStruct)[[5]]
print(cc)