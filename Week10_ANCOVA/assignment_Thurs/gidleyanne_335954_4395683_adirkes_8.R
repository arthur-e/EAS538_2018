# adirkes_8

install.packages("RCurl")
library(RCurl)

# Exercise 1
gz = read.table(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_ANCOVA_n_Interaction/ipomopsis.txt"), sep="", header=T,comment.char="#")
head(gz)
str(gz)
# execute ANCOVA and make interaction plots
install.packages('interplot')
library(interplot)
library(dplyr)
mod = lm(Fruit~Root*Grazing, data=gz)
#summary(mod)
interplot(m=mod, var1="Root", var2="Grazing")+labs(x="Root depth", y="Grazing treatment",title=
                                                     "Estimated Coefficient of Root depth on Grazing")
interplot(m=mod, var1="Grazing", var2="Root")+labs(x="Grazing treatment", y="Root depth",
                                                   title="Estimated coefficient of Grazing on Root depth")
# the effect of grazing on fruit produced is higher with deeper roots, but the difference is not significant
 

library(ggplot2)
# Exercise 2
# Use F-test to compare mod.1 and mod.2.  Choose one over other to explain data.  Why?
# Plot interaction plots with interplot for model with wind and solar.r
# Interpret the two interaction plots
data("airquality")

qqnorm(airquality[,"Wind"])
qqline(airquality[,"Wind"],col="red")
qqnorm(airquality[,"Solar.R"])
qqline(airquality[,"Solar.R"],col="red")
qqnorm(airquality[,"Temp"])
qqline(airquality[,"Temp"],col="red")
shapiro.test(airquality[,"Wind"]) # normal
shapiro.test(airquality[,"Solar.R"]) # not normal, filter out some data
airquality = airquality %>%
  filter(Solar.R>200)
ggplot(data=airquality,mapping=aes(x=Solar.R))+
  geom_histogram(aes(y=..density..))+geom_density(color="red")
qqnorm(airquality[,"Solar.R"])
qqline(airquality[,"Solar.R"],col="red")
shapiro.test(airquality[,"Solar.R"]) # normal!

mod1=lm(Temp~Wind + Ozone, data=airquality)
#summary(mod1)
mod2 = lm(Temp~Wind * Ozone, data=airquality)
#summary(mod2)
mod = lm(Temp~Wind*Solar.R, data=airquality)
interplot(mod, var1="Wind",var2="Solar.R")+
  labs(x="Wind", y="Coefficient for Solar Rad")
interplot(mod, var1="Solar.R",var2="Wind")+
  labs(x="Solar Rad", y="Coefficient for Wind")
