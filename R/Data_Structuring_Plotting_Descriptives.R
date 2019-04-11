wght_long = read.table('C:/GRE2016/Data/wght_data.dat', na.strings='.')

names(wght_long) = c('id','occ','occ_begin','year','time_in_study','grade','age','gyn_age', 'wght')

## restructuring data from long to wide ##

head(wght_long)

wght_long$age_r = round(wght_long$age)

wght_wide = reshape(wght_long, v.names='wght', idvar='id',
           timevar='age_r', direction='wide')

head(wght_wide)

wght_wide1 = wght_wide[ , c('id','wght.5','wght.6','wght.7','wght.8','wght.9','wght.10',
                         'wght.11','wght.12','wght.13','wght.14','wght.15','wght.16',
                         'wght.17','wght.18','wght.19')]

head(wght_wide1)

names(wght_wide1) = c('id','wght5','wght6','wght7','wght8','wght9','wght10',
                          'wght11','wght12','wght13','wght14','wght15','wght16',
                          'wght17','wght18','wght19')


## Writing out File ##
write.table(wght_wide1, 'C:/GRE2016/Data/wght_wide.dat', sep=" ", row.names=FALSE, col.names=FALSE, na='.')


## restructuring data from wide back to long ##
wght_long_new = reshape(wght_wide1, idvar='id', 
        varying=c('wght5','wght6','wght7','wght8','wght9','wght10',
                  'wght11','wght12','wght13','wght14','wght15','wght16',
                  'wght17','wght18','wght19'),
        times=c(5,6,7,8,9,10,11,12,13,14,15,16,17,18,19),
        v.names='wght', direction='long')

wght_long_new = wght_long_new[order(wght_long_new$id, wght_long_new$time),]

wght_long_new1 = wght_long_new[which(!is.na(wght_long_new$wght)), ]

head(wght_long_new1)


## Plotting ##

wght_long1 <- wght_long[which(wght_long$id>1300 & wght_long$id<1600), ]

##install.packages('ggplot2')
library(ggplot2)

plot_obs <- ggplot(data=wght_long1, aes(x=age, y=wght, group=id)) +
                 geom_line() +
                 theme_bw() +
                 scale_x_continuous(breaks = c(5,7,9,11,13,15,17), name = "Chronological Age") + 
                 scale_y_continuous(breaks = c(25,50,75,100,125,150,175,200,225), name = "Weight")

print(plot_obs)

plot_obs <- ggplot(data=wght_long1, aes(x=age, y=wght, group=id)) +
                 geom_line() +
                 geom_point(size=2) +
                 theme_minimal() +
                 scale_x_continuous(breaks = c(5,7,9,11,13,15,17), name = "Chronological Age") + 
                 scale_y_continuous(breaks = c(25,50,75,100,125,150,175,200,225), name = "Weight")

print(plot_obs)


## Descriptive Statistics ##

## Using Wide Daaset 
head(wght_wide1)

wght_vars = wght_wide1[ , c('wght5','wght6','wght7','wght8','wght9','wght10','wght11','wght12',
                            'wght13','wght14','wght15','wght16','wght17','wght18','wght19')]

head(wght_vars)

##install.packages('psych')
library(psych)

## Univariate Descriptives
describe(wght_vars)

## Bivariate Descriptives
cor(wght_vars, use='pairwise.complete.obs')
cov(wght_vars, use='pairwise.complete.obs')

## Basic Scatterplot Matrix
panel.hist <- function(x, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}

pairs(~wght5+wght6+wght7+wght8+wght9+
       wght10+wght11+wght12+wght13+wght14+
       wght15+wght16+wght17+wght18+wght19,
      data=wght_wide1, diag.panel=panel.hist)

pairs(~wght5+wght6+wght7+wght8+wght9+wght10, data=wght_wide1, diag.panel=panel.hist)

