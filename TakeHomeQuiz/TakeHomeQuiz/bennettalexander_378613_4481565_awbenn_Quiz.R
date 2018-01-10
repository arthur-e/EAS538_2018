## useful functions
## ttest, shapiro - week 4
## standardize based on sd
## aov - week 5
## lm, dwtest, bptest (independent residuals) - week 7

library(dplyr)
library(ggplot2)

# Energy data for question 1-4
edata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/energy_data.csv", 
                   sep=",", fill=TRUE, header=TRUE)

edata = mutate(edata,cap.e = TotalEnergy / Population)
edata = mutate(edata,cap.c = TotalCoal / Population)
edata = mutate(edata,cap.GDP = TotalGDP / Population)
edata.coast = subset(edata,Coast==1)
edata.int = subset(edata,Coast==0)

## Plot data for 1b.
hist(edata.coast$cap.e, breaks=20, xlim=c(0,1), ylim=c(0,9), col="light blue", xlab="per capita energy", main="")
par(new=TRUE)
hist(edata.int$cap.e, breaks=20, xlim=c(0,1), ylim=c(0,9), col="light green", xlab="", main="")
abline(v=mean(edata.coast$cap.e), lwd=2, col="blue")
abline(v=mean(edata.int$cap.e), lwd=2, col="dark green")
legend("topright", legend = c("Coastal Mean", "Non-coastal Mean"),text.col=c("blue", "dark green"))

## Test assumptions for 1c.
var.test(edata.coast$cap.e,edata.int$cap.e)
## The variances are not equal, but this is only weakly significant

shapiro.test(edata.coast$cap.e)
shapiro.test(edata.int$cap.e)
## The data are not normally distributed

## Run test for 1d.
t.test(edata.coast$cap.e,edata.int$cap.e)
## There is no significant difference between the means


## Generate plot for 2b.
hist(edata.int$cap.c, breaks=10, xlim=c(0,1), ylim=c(0,15), col="light green", xlab="per capita coal", main="")
par(new=TRUE)
hist(edata.coast$cap.c, breaks=10, xlim=c(0,1), ylim=c(0,15), col="light blue", xlab="", main="")
abline(v=mean(edata.coast$cap.c), lwd=2, col="blue")
abline(v=mean(edata.int$cap.c), lwd=2, col="dark green")
legend("topright", legend = c("Coastal Mean", "Non-coastal Mean"),text.col=c("blue", "dark green"))

## Test assumptions for 2c.
var.test(edata.coast$cap.c,edata.int$cap.c)
## The variances are not equal, but this is only weakly significant

shapiro.test(edata.coast$cap.c)
shapiro.test(edata.int$cap.c)
## The data are not normally distributed

## Run test for 2d.
t.test(edata.coast$cap.c,edata.int$cap.c)
## The non-coastal mean is significantly greater than the coastal mean


## Plot data for 3b.
boxplot(cap.c~Region, data=edata, xlab="Region", ylab="per capita coal consumption")

## Test assumptions for 3c.
library(car)
leveneTest(cap.c~Region, center=mean, data=edata)
## The variances are not equal


## Run test for 3d.
summary(lm(cap.c~Region, data=edata))
## There is no significant difference between the regions


## Run test for 4.
cor.test(edata$cap.c,edata$cap.GDP)
## There is no significant correlation between the variables

# Housing data for question 5-9
hdata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/housingdata.csv", 
                   sep=",", fill=TRUE, header=TRUE)

h.df = select(hdata,crim,rm,b,medv)

## Generate linear model
lm.h = lm(medv~rm+crim+b, data=h.df)

## Check correlations for 5.
cor(h.df)

## Calculate VIF
1 / (1-summary(lm.h)$r.squared)
## The VIF looks good

## Plot data for 6.
plot(h.df$crim,h.df$medv)
plot(h.df$rm,h.df$medv)
plot(h.df$b,h.df$medv)

## Check assumptions for 7.
plot(lm.h)
## The residuals appear to be heteroscedastic

res.h = residuals(lm.h)
shapiro.test(res.h)
## The residuals are not normal

## Box-Cox transformation
box.v = boxCoxVariable(h.df$medv)
h.df = cbind(h.df,box.v)

## Rerun linear model
lm.h = lm(box.v~rm+crim+b, data=h.df)
## This doesn't look any better

## Return to original model for easier interpretation
lm.h = lm(medv~rm+crim+b, data=h.df)
summary(lm.h)
