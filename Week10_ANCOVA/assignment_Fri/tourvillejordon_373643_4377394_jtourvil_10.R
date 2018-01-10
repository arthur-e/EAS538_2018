###Exercise 1
library(RCurl)#Library packages
library(dplyr)
library(ggplot2)
gz = read.table(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_ANCOVA_n_Interaction/ipomopsis.txt"), sep="", header=T,comment.char="#")
head(gz)#Read in a view data
qqnorm(gz[, 2])
qqline(gz[, 2], col="red")
shapiro.test(gz[, 2])
qqnorm(gz[, 1])
qqline(gz[, 1], col="red")
shapiro.test(gz[, 1])#Test for normaility for both Root and Fruit variables. Both qqplots and shapiro tests show normaility.
hist(gz[, 1])
hist(gz[, 2])#histograms of both Root and Fruit again show normal distribution.
gz %>%#ploting effect of root depth on fruit weight in grazed treatments.
  filter(Grazing=="Grazed") %>%
  ggplot()+
  geom_point(aes(x=Root, y=Fruit), color="red")+
  labs(x="Rooting Depth", y="Fruit Weight)", title="Grazed")+
  geom_smooth(aes(x=Root, y=Fruit), method="lm", color="black", se=FALSE)
gz %>%#ploting effect of root depth on fruit weight in ungrazed treatments.
  filter(Grazing=="Ungrazed") %>%
  ggplot()+
  geom_point(aes(x=Root, y=Fruit), color="blue")+
  labs(x="Rooting Depth", y="Fruit Weight)", title="Ungrazed")+
  geom_smooth(aes(x=Root, y=Fruit), method="lm", color="black", se=FALSE)
gz_mod = lm(Fruit~ Root*Grazing, data=gz)#linear model with Root and Grazing as interacting independent variables. 
gz_aov = aov(Fruit~ Root*Grazing, data=gz)#analysis of variance.
summary(gz_mod)#intercept and slope for Root and grazed are significant while estimate of ungrazed:Root intercept and slope are not significant.
#Lines are not significantly different, which means that there is no change in fruit weight across different grazing treatments.
anova(gz_mod)
summary(gz_aov)
library(interplot)
interplot(m=gz_mod, var1="Root", var2="Grazing")+
  labs(x="Grazing Type", y="Estimated coefficient for Rooting Depth", title="Estimated Coefficient of Rooting Depth on Grazing Type")
#Coefficient for rooting depth between grazing treatments are not significantly different.
interplot(m=gz_mod, var1="Grazing", var2="Root")+
  labs(x="Root", y="Estimated coefficient for Grazing Type", title="Estimated Coefficient of Grazing Type on Rooting Depth")
#Regression (slope) coefficient for grazing type does not change significantly as rooting depth increases.
###
###Exercise 2
data("airquality")#Read in data
head(airquality, 15)
mod1 = lm(Temp~Wind + Ozone, data=airquality)
summary(mod1)
mod2 = lm(Temp~Wind * Ozone, data=airquality)
summary(mod2)
#Create and summarize models
var.test(mod1, mod2)
aov1 = anova(mod1)
aov2 = anova(mod2)
summary(aov1)
summary(aov2)
#Comparing variances and looking at results of F-test
#P-values for F-stat for both models are the same so I would instead chose the model with the highest R2, which in this case is model 2.
mod3 = lm(Temp~Wind * Solar.R, data = airquality)#New linear model with interaction wind and solar.R independent variables.
summary(mod3)
interplot(m=mod3, var1="Wind", var2="Solar.R")+
  labs(x="Wind", y="Estimated coefficient for Solar.R", title="Estimated Coefficient of Wind on Solar.R")
interplot(m=mod3, var1="Solar.R", var2="Wind")+
  labs(x="Solar.R", y="Estimated coefficient for Wind", title="Estimated Coefficient of Solar.R on Wind")
#Both estimates of solar.R intercept and slope are not significantly different from 0, so are essentailly the same as the coeffiients for the wind variable
#regression coefficients. The lines are statistically the same.