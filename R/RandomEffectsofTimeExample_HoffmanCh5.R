#**************************************************************************
# Title: RandomEffectsofTimeExample_HoffmanCh5.R
# Author: William Murrah
# Description:
# Created: Saturday, 08 December 2018
# R version: R version 3.5.1 (2018-07-02)
# Directory: /home/wmmurrah/Projects/LongitudinalDataAnalysis
#**************************************************************************
# packages used -----------------------------------------------------------
library(tidyverse)
library(lme4)
library(rstanarm)
library(texreg)
library(sjstats)
library(effects)
library(emmeans)

# Data --------------------------------------------------------------------

temp <- tempfile()
download.file("http://www.pilesofvariance.com/Chapter5/STATA/STATA_Chapter5.zip",
              temp)
dt <- haven::read_dta(unz(temp, "STATA_Chapter5/STATA_Chapter5.dta"))
unlink(temp)
rm(temp)

dt <- dt %>% 
  mutate(#personid = factor(personid),
         wave = wave - 1)


# Describe Data -----------------------------------------------------------

dt %>% 
  summarize(Mean = mean(outcome),
            SD = sd(outcome),
            n = n()) 

dt %>% 
  group_by(wave) %>% 
  summarize(Mean = mean(outcome),
            SD = sd(outcome),
            n = n())


ggplot(dt, aes(wave, outcome, color = personid, group = personid)) +
  geom_line() +ylim(4, 24) + 
  theme(legend.position = "none") +
  ylab("Outcome Y") + xlab("Years in Study") + 
  ggtitle("Figure 5.2: Individual trajectories for four-occasion example data")


# Models ------------------------------------------------------------------

mod00 <- lmer(outcome ~ 1 + (1 | personid), dt, REML = TRUE)
mod0 <- lmer(outcome ~ wave + (1 | personid), dt, REML = TRUE)
mod.rs <- lmer(outcome ~ wave + (wave | personid), dt, REML = TRUE)



rmod00 <- stan_lmer(outcome ~ 1 + (1 | personid), dt)
rmod0 <- stan_lmer(outcome ~ wave + (1 | personid), dt)
summary(rmod00, digits = 3)
screenreg(list(mod00, mod0, mod.rs))
icc(mod00)
icc(mod0)


plot(effect("wave", mod.rs))


deff(n = 250)

emm(mod.rs)

plot(ggeffect(mod.rs, terms = ~ wave))

REMLcrit(mod.rs)
