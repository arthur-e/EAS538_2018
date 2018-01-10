data("airquality")
head(airquality, 15)
library(dplyr)
airquality = airquality %>%
  subset(is.na(Solar.R)==FALSE & is.na(Ozone)==FALSE)

pairs(airquality[,c(1:4)])
abline
cor(airquality[,c(1:4)], use = "na.or.complete")


Y = seq(from=0, to=20, by=0.1)
X1 = (Y-2)/3 + runif(length(Y), min=0.5, max=2)
X2 = (Y+3)/2 + runif(length(Y), min=0.5, max=2)
df = as.data.frame(cbind(Y, X1, X2))
summary(lm(Y~X1, data=df))
summary(lm(Y~X2, data=df))
plot(X1~X2)

cor(X1, X2)
#0.9641

#Exercise 1
Y = seq(from=0, to=20, by=0.1)
X1 = (Y-100)/3 + runif(length(Y), min=0.5, max=2)
X2 = (Y+500)/2 + runif(length(Y), min=0.5, max=2)
df = as.data.frame(cbind(Y, X1, X2))
summary(lm(Y~X1, data=df))
summary(lm(Y~X2, data=df))
plot(X1~X2)
cor(X1, X2)
#correlation coefficient between X1 and X2 is 0.1385

mod1 = lm(Temp~Wind, data=airquality)
summary(mod1)
mod2 = lm(Temp~Wind+Solar.R, data=airquality)
summary(mod2)

#Exercise 2
mod3 = lm(Temp~Wind+Solar.R+Ozone, data=airquality)
summary(mod3)

#Calculating RSS and F statistics of mod1 and mod2
data("airquality")
head(airquality, 15)
library(dplyr)
airquality = airquality %>%
  subset(is.na(Solar.R)==FALSE & is.na(Ozone)==FALSE)
res1=residuals(mod1)
head(res1)
RSS1=sum(res1^2)
RSS1
P1=2
n1=111
n1-p1=111-2=109
res2=residuals(mod2)
head(res2)
RSS2=sum(res2^2)
RSS2
P2=3
P2
n2=111
n2-P2=111-3=108
FF=RSS1-RSS2
FF
PP=RSS2/(108)
PP
F=FF/PP
F
#F statisics is 8.3739. 

#Calculating F statistic for mod3
airquality = airquality %>%
  subset(is.na(Solar.R)==FALSE & is.na(Ozone)==FALSE & is.na(Wind)==FALSE)
res3=residuals(mod3)
head(res3)
RSS3=sum(res3^2)
RSS3
P3=4
n3=111
FF=RSS3-RSS2
FF
PP=RSS3/(111-4)
PP
F=FF/PP
F
#F statistics is 35.65. It explains more variance becuase the F value is bigger. It gives me resonable estimate of regressional coefficients.

#Exercise 3
AIC(mod1)
AIC(mod2)
AIC(mod3)
#Given the AIC of mod3 is the smallest, mod3 is a better performed model. 