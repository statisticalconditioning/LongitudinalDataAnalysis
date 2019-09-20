#**************************************************************************
# Title: fda_giatExample.R
# Author: William Murrah
# Description:
# Created: Monday, 08 July 2019
# R version: R version 3.6.1 (2019-07-05)
# Directory: /home/wmmurrah/Projects/LongitudinalDataAnalysis
#**************************************************************************
# packages used -----------------------------------------------------------
  
library(fda)
library(ggplot2)
library(data.table)

data(gait)

plot(gait[ ,1,1], gait[, 1, 2], type = "b")
g1 <- as.data.frame.table(gait)
names(g1) <- c("time", "id", "location", "angle")

g1 <- within(g1, {
  id <- factor(id)
  location <- factor(location)
})


g1 <- dcast(g1, time + id ~ location , value.var = "angle")
names(g1) <- gsub(" ", "", names(g1))

ggplot(g1, aes(time, HipAngle, group = id, color = id)) + geom_line() +
  theme(legend.position = "none")
ggplot(g1, aes(time, KneeAngle, group =id, color = id)) + geom_line() +
  theme(legend.position = "none")


ggplot(g1, aes(time, angle, group = id)) + geom_line() + facet_grid(location ~ 1)

ggplot(g1, aes(time, angle, group = id, linetype = location)) + geom_line() + 
  facet_grid(id ~ 1)
