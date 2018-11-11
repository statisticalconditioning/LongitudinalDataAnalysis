#**************************************************************************
# Title: cognitionExample_Hoffman.R
# Author: William Murrah
# Description:
# Created: Wednesday, 03 October 2018
# R version: R version 3.5.1 (2018-07-02)
# Directory: /home/wmmurrah/Projects/GLMsimulation
#**************************************************************************
# packages used -----------------------------------------------------------
library(haven)  
library(texreg)
library(emmeans)
library(mosaic)
library(car)
library(effects)
cog <- read_dta(file = "data/dta/STATA_Chapter2.dta")
cog$sexmw <- factor(cog$sexmw, labels = c("Men", "Women"))
cog$demgroup <- factor(cog$demgroup, labels = c("None", "Future", "Current"))

# c1 <- c(1, -1, 0)
# c2 <- c(1, 0, -1)
# c3 <- c(0, 1, -1)
# mat <- cbind(c1, c2, c3)
# contrasts(cog$demgroup) <- mat

mod0 <- lm(cognition ~ 1, data = cog)
summary(mod0)

mod1 <- lm(cognition ~ I(age - 85), data = cog)
mod2 <- update(mod1, . ~ . + I(grip - 9))
mod3 <- update(mod2, . ~ . + sexmw)
mod4 <- update(mod3, . ~ . + demgroup)
mod5 <- update(mod4, . ~ . +  I(age - 85):I(grip - 9))
screenreg(list(mod0, mod1, mod2, mod3, mod4, mod5))

(em5 <- emmeans(mod5, ~ age:grip | demgroup))

favstats(cog$age)
favstats(cog$grip)
ep <- emmip(mod5, age ~ grip, at = list(age = c(80,85,  90),
                                  grip = c(6,9, 12)))
ep <- ep + ylim(0, 40)
ep

ep2 <- emmip(mod5, grip ~ age, at = list(grip = c(6, 9, 12),
                                         age = c(80, 85, 90)))
(ep2 <- ep2 + ylim(0, 40))

par(mfrow = c(2, 1))
ep
ep2
gl <- list(ep, ep2)

GGally::ggmatrix(gl, 2, 1, title = "Interactions")

mod6 <- lm(cognition ~ I(age - 85)*I(grip - 9) + sexmw*demgroup,
           data = cog)
# cog$demgroup <- relevel(cog$demgroup, ref = "3")
mod7.2 <- lm(cognition ~ I(age - 85)*I(grip - 9) + 
               relevel(sexmw, ref = "Women")*demgroup, data = cog)
mod7.3 <- lm(cognition ~ I(age - 85)*I(grip - 9) + 
               sexmw*relevel(demgroup, ref = "Future"), data = cog)
mod7.4 <- lm(cognition ~ I(age - 85)*I(grip - 9) + 
               relevel(sexmw, ref = "Women")*relevel(demgroup, ref = "Future"), 
             data = cog)



screenreg(list(mod5, mod6, mod7.2, mod7.3, mod7.4))

emmeans(mod6, ~ sexmw + demgroup)
effects::effect("sexmw:demgroup", mod6)



# Three way interaction ---------------------------------------------------

mod8 <- update(mod6, . ~ . + I(age - 85)*I(grip - 9)*sexmw)
screenreg(list(mod6, mod8))

plot(allEffects(mod8))

em8 <- emmeans(mod8, ~ age*grip*sexmw)
em8

emp8 <- emmip(mod8, grip ~ age | sexmw , at = list(age = c(80, 85, 90),
                                         grip = c(6, 9, 12)))
emp8 <- emp8 + ylim(0,40)
emp8

plot(predictorEffects(mod5, age ~ grip,
     xlevels = list(age = c(80, 85, 90),
                    grip = c(6, 9, 12))), 
     lines = list(multiline = TRUE),
     confint = list(style = "bars"), 
     lattice = list(ylim = c(0,40)) )

emmip(mod5, age ~ grip, at = list(age = c(80, 85, 90),
                                  grip = c(6, 9, 12)))
