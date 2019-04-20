###########################################################
## File Name: CH3_PIAT_Math_NLME.R                       ## 
## Read data, Fit NLME Models                            ##
## Plot longitudinal data in R                           ##
## By Kevin J. Grimm                                     ##
###########################################################

library(nlme)

# Load the data
nlsy_math_long<-read.table("../../Dropbox/Data/Grimm_Data/nlsy_math_long_R.dat", na.strings='.')

# Give the variable names
names(nlsy_math_long)<-c('id', 'female', 'low_birth_weight', 'anti_k1', 'math', 'grade', 'occ', 'age', 'men', 'spring', 'anti')

# attach(nlsy_math_long)

#Plot of the Longitudinal Data
#interaction.plot(grade,id,math,xlab='Grade',ylab='PIAT Mathematics',legend=F)

library(ggplot2)

plot_obs <- ggplot(data=nlsy_math_long, aes(x = grade, y = math, group = id)) + 
  geom_point() +
                geom_line(color = "gray", alpha = .) + 
                theme_bw() + 
                scale_x_continuous(breaks = 2:8, name = "Grade") +  
                scale_y_continuous(name = "PIAT Mathematics")


print(plot_obs)

library(nlme)

#Using lme to fit No Growth Model to Mathematics Data
ng.math.lme <- lme(math ~ 1, random= ~1 |id, data = nlsy_math_long, method="ML")
summary(ng.math.lme)

#Linear mixed-effects model fit by maximum likelihood
# Data: nlsy_math_long 
#      AIC      BIC    logLik
#  17497.9 17515.02 -8745.952
#
#Random effects:
# Formula: ~1 | id
#        (Intercept) Residual
#StdDev:    6.849591 10.80195
#
#Fixed effects: math ~ 1 
#               Value Std.Error   DF  t-value p-value
#(Intercept) 45.91468 0.3237961 1289 141.8012       0
#
#Standardized Within-Group Residuals:
#        Min          Q1         Med          Q3         Max 
#-2.79834566 -0.57828004  0.05407981  0.57885522  2.52690025 
#
#Number of Observations: 2221
#Number of Groups: 932

intervals(ng.math.lme)


#Using lme to fit Linear Growth Model to Mathematics Data
grade_c2 <- grade - 2

lg.math.lme <- lme(math ~ grade_c2, random= ~ grade_c2 |id, data = nlsy_math_long, method="ML")
summary(lg.math.lme)

#Linear mixed-effects model fit by maximum likelihood
# Data: nlsy_math_long 
#       AIC      BIC    logLik
#  15949.39 15983.62 -7968.693
#
#Random effects:
# Formula: ~grade_c2 | id
# Structure: General positive-definite, Log-Cholesky parametrization
#            StdDev    Corr  
#(Intercept) 8.0350399 (Intr)
#grade_c2    0.8558998 -0.026
#Residual    6.0191136       
#
#Fixed effects: math ~ grade_c2 
#               Value Std.Error   DF  t-value p-value
#(Intercept) 35.26736 0.3551568 1288 99.30081       0
#grade_c2     4.33933 0.0873767 1288 49.66231       0
# Correlation: 
#         (Intr)
#grade_c2 -0.532
#
#Standardized Within-Group Residuals:
#         Min           Q1          Med           Q3          Max 
#-3.030277742 -0.525876492  0.001632966  0.540751651  2.542250502 
#
#Number of Observations: 2221
#Number of Groups: 932

#Using nlme to fit No Growth Model to Mathematics Data
ng.math.nlme <- nlme(math ~ beta_1 + d_1i,
                     data=nlsy_math_long,                      
                     fixed=beta_1~1,                      
                     random=d_1i~1,
                     group=~id,                      
                     start=c(beta_1=40),
                     na.action = "na.omit")                       
                     
summary(ng.math.nlme)

#Nonlinear mixed-effects model fit by maximum likelihood
#  Model: math ~ beta_1 + d_1i 
# Data: nlsy_math_long 
#      AIC      BIC    logLik
#  17497.9 17515.02 -8745.952
#
#Random effects:
# Formula: d_1i ~ 1 | id
#            d_1i Residual
#StdDev: 6.849581 10.80196
#
#Fixed effects: beta_1 ~ 1 
#          Value Std.Error   DF  t-value p-value
#beta_1 45.91468  0.323796 1289 141.8013       0
#
#Standardized Within-Group Residuals:
#        Min          Q1         Med          Q3         Max 
#-2.79834502 -0.57827917  0.05408079  0.57885591  2.52690020 
#
#Number of Observations: 2221
#Number of Groups: 932 
  


#Alternative Specification
ng.math.nlme <- nlme(math~b_1i,
                     data=nlsy_math_long,
                     fixed=b_1i~1,
                     random=b_1i~1|id,
                     start=c(b_1i=40))
                     
summary(ng.math.nlme)

#Nonlinear mixed-effects model fit by maximum likelihood
#  Model: math ~ b_1i 
# Data: nlsy_math_long 
#      AIC      BIC    logLik
#  17497.9 17515.02 -8745.952
#
#Random effects:
# Formula: b_1i ~ 1 | id
#            b_1i Residual
#StdDev: 6.849582 10.80196
#
#Fixed effects: b_1i ~ 1 
#        Value Std.Error   DF  t-value p-value
#b_1i 45.91468  0.323796 1289 141.8013       0
#
#Standardized Within-Group Residuals:
#        Min          Q1         Med          Q3         Max 
#-2.79834512 -0.57827919  0.05408079  0.57885592  2.52690029 
#
#Number of Observations: 2221
#Number of Groups: 932 


#Using nlme to fit Linear Growth Model to Mathematics Data
lg.math.nlme <- nlme(math~(beta_1+d_1i)+(beta_2+d_2i)*(grade-2),  
                   data=nlsy_math_long,                      
                   fixed=beta_1+beta_2~1,                      
                   random=d_1i+d_2i~1,
                   group=~id,                     
                   start=c(beta_1=35,beta_2=4),
                   na.action = "na.omit")

summary (lg.math.nlme)

#Nonlinear mixed-effects model fit by maximum likelihood
#  Model: math ~ (beta_1 + d_1i) + (beta_2 + d_2i) * (grade - 2) 
# Data: nlsy_math_long 
#       AIC      BIC    logLik
#  15949.39 15983.62 -7968.693
#
#Random effects:
# Formula: list(d_1i ~ 1, d_2i ~ 1)
# Level: id
# Structure: General positive-definite, Log-Cholesky parametrization
#         StdDev    Corr  
#d_1i     8.0350382 d_1i  
#d_2i     0.8558994 -0.026
#Residual 6.0191140       
#
#Fixed effects: beta_1 + beta_2 ~ 1 
#          Value Std.Error   DF  t-value p-value
#beta_1 35.26736 0.3551568 1288 99.30082       0
#beta_2  4.33933 0.0873767 1288 49.66231       0
# Correlation: 
#       beta_1
#beta_2 -0.532
#
#Standardized Within-Group Residuals:
#         Min           Q1          Med           Q3          Max 
#-3.030277731 -0.525876489  0.001632883  0.540751587  2.542250204 
#
#Number of Observations: 2221
#Number of Groups: 932 
 


b_1i_hat = ranef(lg.math.nlme)[,1] + fixef(lg.math.nlme)[1]
b_2i_hat = ranef(lg.math.nlme)[,2] + fixef(lg.math.nlme)[2] 
child_id = as.numeric(rownames(ranef(lg.math.nlme)))

estimates <- data.frame(child_id, b_1i_hat, b_2i_hat)
estimates1 = merge(x = nlsy_math_long, y = estimates,
                by.x = c('id'), by.y = c('child_id'),
                all = TRUE)

estimates1$pred = estimates1$b_1i_hat + estimates1$b_2i_hat * (estimates1$grade - 2) 
estimates1$resid = estimates1$math - estimates1$pred 

#Plotting Predicted Values
plot_pred <- ggplot(data=estimates1, aes(x = grade, y = pred, group = id)) + 
                geom_line() + 
                theme_bw() + 
                scale_x_continuous(breaks = 2:8, name = "Grade") +
                scale_y_continuous(name = "PIAT Mathematics - Predictions")
print(plot_pred)

#Plotting Residual Values
plot_resid <- ggplot(data=estimates1, aes(x = grade, y = resid, group = id)) + 
                geom_line() + 
                theme_bw() + 
                scale_x_continuous(breaks = 2:8, name = "Grade") +
                scale_y_continuous(name = "PIAT Mathematics - Predictions")
print(plot_resid)

#fitted (lg.math.nlme)
#residuals (lg.math.nlme)

#Alternative Specification using nlme to fit Linear Growth Model to Mathematics Data
lg.math.nlme <- nlme(math~b_1i+b_2i*(grade-2),
                     data=nlsy_math_long,
                     fixed=b_1i+b_2i~1,
                     random=b_1i+b_2i~1|id,
                     start=c(b_1i=35, b_2i=4))

summary(lg.math.nlme)

#Nonlinear mixed-effects model fit by maximum likelihood
#  Model: math ~ b_1i + b_2i * (grade - 2) 
# Data: nlsy_math_long 
#       AIC      BIC    logLik
#  15949.39 15983.62 -7968.693
#
#Random effects:
# Formula: list(b_1i ~ 1, b_2i ~ 1)
# Level: id
# Structure: General positive-definite, Log-Cholesky parametrization
#         StdDev    Corr  
#b_1i     8.0350377 b_1i  
#b_2i     0.8558995 -0.026
#Residual 6.0191139       
#
#Fixed effects: b_1i + b_2i ~ 1 
#        Value Std.Error   DF  t-value p-value
#b_1i 35.26736 0.3551567 1288 99.30083       0
#b_2i  4.33933 0.0873767 1288 49.66231       0
# Correlation: 
#     b_1i  
#b_2i -0.532
#
#Standardized Within-Group Residuals:
#         Min           Q1          Med           Q3          Max 
#-3.030277750 -0.525876492  0.001632883  0.540751591  2.542250220 
#
#Number of Observations: 2221
#Number of Groups: 932 



##LME4 
library(lme4)

ng.math.lmer <- lmer(math ~ 1 + (1 | id), data = nlsy_math_long, REML = FALSE)
summary(ng.math.lmer)

#Linear mixed model fit by maximum likelihood  ['lmerMod']
#Formula: math ~ 1 + (1 | id)
#   Data: nlsy_math_long
#
#     AIC      BIC   logLik deviance df.resid 
# 17497.9  17515.0  -8746.0  17491.9     2218 
#
#Scaled residuals: 
#     Min       1Q   Median       3Q      Max 
#-2.79835 -0.57828  0.05408  0.57886  2.52690 
#
#Random effects:
# Groups   Name        Variance Std.Dev.
# id       (Intercept)  46.92    6.85   
# Residual             116.68   10.80   
#Number of obs: 2221, groups:  id, 932
#
#Fixed effects:
#            Estimate Std. Error t value
#(Intercept)  45.9147     0.3237   141.8


lg.math.lmer <- lmer(math ~ 1 + grade_c2 + (1 + grade_c2 | id), data = nlsy_math_long, REML = FALSE)
summary(lg.math.lmer)

#Linear mixed model fit by maximum likelihood  ['lmerMod']
#Formula: math ~ 1 + grade_c2 + (1 + grade_c2 | id)
#   Data: nlsy_math_long
#
#     AIC      BIC   logLik deviance df.resid 
# 15949.4  15983.6  -7968.7  15937.4     2215 
#
#Scaled residuals: 
#     Min       1Q   Median       3Q      Max 
#-3.03028 -0.52588  0.00163  0.54075  2.54225 
#
#Random effects:
# Groups   Name        Variance Std.Dev. Corr 
# id       (Intercept) 64.5618  8.0350        
#          grade_c2     0.7326  0.8559   -0.03
# Residual             36.2297  6.0191        
#Number of obs: 2221, groups:  id, 932
#
#Fixed effects:
#            Estimate Std. Error t value
#(Intercept) 35.26736    0.35500   99.35
#grade_c2     4.33933    0.08734   49.68
#
#Correlation of Fixed Effects:
#         (Intr)
#grade_c2 -0.532