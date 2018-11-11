#**************************************************************************
# Title: CognitionRepeatedMeasuresExample_HoffmanCh3.R
# Author: William Murrah
# Description:
# Created: Wednesday, 24 October 2018
# R version: R version 3.5.1 (2018-07-02)
# Directory: /home/wmmurrah/Projects/GLMsimulation
#**************************************************************************
# packages used -----------------------------------------------------------
library(tidyverse)  
library(car)
library(psych)
library(lme4)
library(effects)
library(emmeans)

cdat <- haven::read_dta("data/dta/STATA_Chapter3b.dta")
cdat <- cdat %>% 
  mutate(session = factor(session),
         personid = factor(personid))

ggplot(cdat, aes(session, rt, group = personid, color = personid)) +
  geom_line() + theme(legend.position="none")


# ANOVA
caov <- lm(rt ~ session, data = cdat)
Anova(caov)
vcov(caov)
-2*logLik(caov)
AIC(caov)
BIC(caov)

summary(caov)

emmeans(caov, ~ session)

# Univariate RM ANOVA

umod <- lmer(rt ~ session + (1 | personid), cdat, REML = TRUE)
Anova(umod)
vcov(umod)
-2*logLik(umod)
summary(umod)
AIC(umod)
BIC(umod)
emmeans(umod, ~ session)


cdatwide <- cdat %>% 
  spread(session, rt)

rmod <- lm(cbind(1, 2, 3, 4, 5, 6) ~ 1, cdatwide)

summary(rmod)
screenreg(rmod)
vcov(rmod)
vcov(cdat)
cov(cdatwide)

cdatwide %>% 
  select(-personid) %>% 
  cov() %>% 
  round()


emmeans(rmod, ~ 1)
mean(cdat$rt)
