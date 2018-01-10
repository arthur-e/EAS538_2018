##EX 1
library("RCurl")
gz = read.table(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_ANCOVA_n_Interaction/ipomopsis.txt"), sep="", header=T,comment.char="#")
head(gz)
ggplot(data=gz, mapping=aes(x=Fruit, y=Root, color=factor(Grazing)))+
  geom_point()
#Checking assumption of normality
ggplot(data=gz, mapping=aes(x=Root))+
  geom_histogram(aes(y = ..density..)) + geom_density(color="red")
qqnorm(gz[,"Root"])
qqline(gz[,"Root"], col="red")
shapiro.test(gz[,"Root"])
#Since p>0.05 (p=0.6104), the Root variable is normally distributed
ggplot(data=gz, mapping=aes(x=Fruit))+
  geom_histogram(aes(y = ..density..)) + geom_density(color="red")
qqnorm(gz[,"Fruit"])
qqline(gz[,"Fruit"], col="red")
shapiro.test(gz[,"Fruit"])
#Since p>0.05 (p=0.7798), the Fruit variable is normally distributed
mod=lm(Fruit~Root*Grazing, data=gz)
summary(mod)
#Checking assumption of independent residuals
res=residuals(mod)
plot(residuals(mod))
install.packages(lmtest)
library(lmtest)
dwtest(mod, alternative=c("two.sided"))
#Since p=0.9758, true autocorrelation is close to 0 and the residuals are independent of each other 
#Checking assumption of homoscedasticity
bptest(mod)
#Since p=0.6456, our residuals are homoscedastic.
#Checking assumption of normally distributed residuals
shapiro.test(residuals(mod))
#Since p=0.559, the residuals are normally distributed. 

#Intercept estimate: Fruit weight is predicted to be -125.17 when root depth is 0 and there's grazing treatment
#Root estimate: For every unit increase in root depth in a grazed environment, fruit weight is predicted to increase by 23.24 units.  
#Root p value: Since p<0.05, root depth does have a significant effect on fruit weight in a grazed environment
#GrazingUngrazed estimate: When root depth is 0, a change from grazing to ungrazed treatment results in a fruit weight unit increase of 30.806.
#Our interaction term is not significant (p>0.05), so the effect of one variable does not depend on the other
#The relationship between root depth and fruit weight is not significantly different from ungrazed to grazed treatment environments
#The slope between root depth and fruit weight is 23.24+0.756 = 23.99 under an ungrazed treatmnet
#Since our Adjusted R2 = 0.92, our model explains 92% of the variability in fruit weight
#Since the p value is very small (<2.2e-16), there's a small probability (<2.2e-16) that the null model explains as much variance as our model does by chance
library(interplot)
interplot(m=mod, var1="Root", var2="Grazing")+
  labs(x="Treatment", y="Estimated coefficient for Root", title="Estimated Coefficient of Root Depth on Treatment")
#Our interaction plot means that the effect of root depth on fruit weight is higher in ungrazed v. grazed treatments but the difference is NOT significant
interplot(m=mod, var1="Grazing", var2="Root")+
  labs(x="Root Depth", y="Estimated coefficient for Treatment", title="Estimated Coefficient of Treatment on Root Depth")
#Fruit weight is higher in ungrazed v. grazed plots and that difference increases with increasing root depth. However, that difference is not statistically significant. 

#EX 2
library(dplyr)
data("airquality")
head(airquality, 15)
mod1=lm(Temp~Wind + Ozone, data=airquality)
summary(mod1)
mod2=lm(Temp~Wind*Ozone, data=airquality)
summary(mod2)
cor(airquality[,"Wind"], airquality[,"Ozone"], use="na.or.complete")
#-0.6015465
anova(mod1, mod2)
#Since p<0.05 (6.927e-5), the two models significantly differ in terms of the amount of variation in temperature that they explain
#Model 2 explains variability in temperature significantly better than Model 1 (RSS is lower for mod2)
moda=lm(Temp~Wind*Solar.R, data=airquality)
summary(moda)
library(interplot)
interplot(m=moda, var1="Solar.R", var2="Wind")+
  labs(x="Wind", y="Estimated coefficient for solar radiation", title="Estimated Coefficient of Solar Radiation on Wind")
#There is a slight negative slope of the drawn line, indicating some small amount of interaction. However, this is not significant, since our ANOVA shows that the p value for Wind:Solar.R is >0.05 (p=0.7537).
interplot(m=moda, var1="Wind", var2="Solar.R")+
  labs(x="Solar Radiation", y="Estimated coefficient for wind", title="Estimated Coefficient of Wind on Solar Radiation")
#There is a slight negative slope of the interaction line, indicating that as solar radiation increases, the wind explains However, this is not significant, since our ANOVA shows that the p value for Wind:Solar.R is >0.05. 
