# ex4 
summary(lm(Temp~Ozone, data=airquality))

dat.new=airquality[!is.na(airquality$Ozone),]
dat.new=subset(airquality, Ozone!="NA")
n = nrow(dat.new)
SSY = deviance(lm(Temp~1, data=dat.new)) 
RSS = sum(residuals(lm(Temp~Ozone, data=dat.new))^2) # calculate RSS manually
adjR2 = 1 - ((RSS/(n-2))/(SSY/(n-1)))

# ex5.2_homoskedasticity

mod4=lm(Temp~Ozone, data=airquality)
plot(residuals(mod4)~fitted(mod4))
abline(lm(residuals(mod4)~fitted(mod4)), col="red")
##it looks like there is a pattern in the varience of the residuals
library(lmtest)
bptest(mod4)
#The p value is about 0.07 so there is a fair chance that there is a pattern