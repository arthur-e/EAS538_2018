## set working directory
setwd("/docs/umich/coursework/538/labs/lab07")


## begin exercise 4
## import data
data("airquality")

## remove rows with empty ozone observations manually to match model behavior
air = airquality[which(is.na(airquality$Ozone)==FALSE),]

## generate linear model for temperature and ozone
mod_air = lm(Temp~Ozone, data=air)

## calculate residuals and RSS
res = residuals(mod_air)
RSS = sum(res^2)

## calculate variance of dependent variable
SSY = var(air$Temp)

## adjust for number of parameters
RSS_adj = RSS/(length(air$Ozone)-2)

## calculate R2
R2 = 1 - (RSS_adj/SSY)
print(paste0("R2 = ", R2))


## begin excerise 5
## install packages
install.packages("https://cran.r-project.org/bin/macosx/mavericks/contrib/3.3/zoo_1.7-14.tgz",
                 repos=NULL, method="libcurl")
library(zoo)

install.packages("https://cran.r-project.org/bin/macosx/mavericks/contrib/3.3/lmtest_0.9-35.tgz",
                 repos=NULL, method="libcurl")
library(lmtest)

## test for residual independency
dwtest(mod_air, alternative=c("two.sided"))
## the test indicates that the residuals are temporally autocorrelated
## the data should be manipulated to remove the temporal trend
## before the linear model is constructed

## test for residual homoscedasticity
bptest(mod_air)
## the test indicates that the residuals are homoscedastic

## test for residual normality
shapiro.test(residuals(mod_air))
## the test indicates that the residuals are not normally distributed