#**************************************************************************
# Title: simulate_crossoverDesign.R
# Author: William Murrah
# Description:
# Created: Thursday, 04 July 2019
# R version: R version 3.6.0 (2019-04-26)
# Directory: /home/wmmurrah/Projects/LongitudinalDataAnalysis
#**************************************************************************
# packages used -----------------------------------------------------------
library(ggplot2)
library(texreg)
library(lme4)
library(effects)
library(emmeans)
library(rstanarm)
n <- 100
sigma <- 5
b0 <- 10
btx <- -10
bpd <- 0
bsq <- 0

id <- rep(1:(n), each = 2)
sq <- rep(0:1, each = n)
pd <- rep(0:1, n)
tx <- c(rep(0:1, n/2), rep(1:0, n/2))        

set.seed(123)
y <-  b0 + btx*tx + bpd*pd + bsq*sq + rnorm(n, 0, sigma)

df <- data.frame(id = factor(id),
                 y = y,
                 tx = factor(tx, labels = c("control", "treatment")),
                 pd = factor(pd, labels = c("period 1", "period 2")),
                 sq = factor(sq, labels = c("delayed", "immediate")))

ggplot(df, aes(tx, y, group = id, color = id, shape = pd )) + geom_point() +
  geom_line() + facet_grid(~ sq) + theme(legend.position = "none")


mod <- lm(y ~ tx + pd + sq, data = df)
screenreg(mod)

av <- aov(y ~ tx + pd + sq + Error(id), df)
summary(av)
av

memod <- lmer(y ~ tx +  pd  + sq + (1 |id), df)
summary(memod)
bmod <- stan_lmer(y ~ tx + pd + sq + (1|id), df)
bmod
