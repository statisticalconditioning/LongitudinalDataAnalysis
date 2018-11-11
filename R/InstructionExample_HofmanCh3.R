#**************************************************************************
# Title: InstructionExample_HofmanCh3.R
# Author: William Murrah
# Description:
# Created: Sunday, 21 October 2018
# R version: R version 3.5.1 (2018-07-02)
# Directory: /home/wmmurrah/Projects/GLMsimulation
#**************************************************************************
# packages used -----------------------------------------------------------
library(haven)  
library(tidyverse)
library(psych)
library(car)
library(mosaic)
library(lme4)
library(texreg)
library(effects)
library(emmeans)


dat <- read_dta("data/dta/STATA_Chapter3a.dta")


datlong <- dat %>% 
  gather(time, outcome, outcome1, outcome2) %>% 
  mutate(group = factor(group, labels = c("control", "treatment")),
         time = factor(time, labels = c("time1", "time2")))


datlong %>% 
  summarise(mean = mean(outcome),
            sd = sd(outcome),
            var = var(outcome))

# BP empty model
mod0bp <- lm(outcome ~ 1, datlong )

mod0wp <- lmer(outcome ~ 1 + (1 | personid), datlong)

(icc <- 12.25/(12.25 + 28.21))
var(datlong$outcome)

screenreg(list(mod0bp, mod0wp))

plot(predict(mod0bp))
plot(predict(mod0wp))


mod1bp <- lm(outcome ~ time*group, datlong)
mod1wp <- lmer(outcome ~ time*group + (1 |personid), datlong)
screenreg(list(mod0bp, mod0wp, mod1bp, mod1wp))

plot(allEffects(mod1),
     lines = list(multiline = TRUE))

emmeans(mod1, ~ time + group)
emmip(mod1, group ~ time)

Anova(mod1bp)
