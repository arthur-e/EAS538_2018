install.packages("Lahman")
library(Lahman)
library(dplyr)
data("Salaries")
data("Batting")

money15 = subset(Salaries, yearID==2015)
bat14 = subset(Batting, yearID==2014 & AB>100) %>% 
  battingStats(idvars = c("playerID", "stint", "teamID", "lgID"), cbind = FALSE)

dat = inner_join(money15, bat14, by="playerID") %>%
  filter(BA != "NA") %>%
  subset(select=c("lgID.x", "salary", "BA"))
head(dat, 15)
str(dat)
library("ggplot2")

ggplot(data=dat, mapping=aes(x=BA, y=salary, color=factor(lgID.x)))+
  geom_point()
ggplot(data=dat, mapping=aes(x=BA)) + 
  geom_histogram(aes(y = ..density..)) + geom_density(color="red")
qqnorm(dat[,"BA"])
qqline(dat[,"BA"], col="red")
shapiro.test(dat[,"BA"])

ggplot(data=dat, mapping=aes(x=salary)) + 
  geom_histogram(aes(y = ..density..)) + geom_density(color="red")
qqnorm(dat[,"salary"])
qqline(dat[,"salary"], col="red")
shapiro.test(dat[,"salary"])
dat = dat %>%
  mutate(sal.log=log(salary))
ggplot(data=dat, mapping=aes(x=sal.log)) + 
  geom_histogram(aes(y = ..density..)) + geom_density(color="red")
qqnorm(dat[,"sal.log"])
qqline(dat[,"sal.log"], col="red")
shapiro.test(dat[,"sal.log"])
dat = dat %>%
  filter(sal.log>13.5) 
ggplot(data=dat, mapping=aes(x=sal.log)) + 
  geom_histogram(aes(y = ..density..)) + geom_density(color="red")
qqnorm(dat[,"sal.log"])
qqline(dat[,"sal.log"], col="red")
shapiro.test(dat[,"sal.log"])
dat %>%
  filter(lgID.x=="AL") %>%
  ggplot()+
  geom_point(aes(x=BA, y=sal.log), color="red")+
  labs(x="Batting Average", y="Log(salary)", title="American League")+
  geom_smooth(aes(x=BA, y=sal.log), method="lm", color="black", se=FALSE)
dat.AL = dat %>%
  filter(lgID.x=="AL")
summary(lm(sal.log~ BA, data=dat.AL))
dat %>%
  filter(lgID.x=="NL") %>%
  ggplot(aes(x=BA, y=sal.log))+
  geom_point(color="blue")+
  labs(x="Batting Average", y="Log(salary)", title="National League")+
  geom_smooth(aes(x=BA, y=sal.log), method="lm", color="black", se=FALSE)
dat.NL = dat %>%
  filter(lgID.x=="NL")
summary(lm(sal.log~ BA, data=dat.NL))
mod = lm(sal.log~ BA*lgID.x, data=dat)
aov = aov(sal.log~ BA*lgID.x, data=dat)
summary(mod)
anova(mod)
summary(aov)
library(interplot)
interplot(m=mod, var1="BA", var2="lgID.x")+
  labs(x="League ID", y="Estimated coefficient for BA", title="Estimated Coefficient of BA on League ID")
interplot(m=mod, var1="lgID.x", var2="BA")+
  labs(x="BA", y="Estimated coefficient for League ID", title="Estimated Coefficient of League ID on BA")

#Exercise 1
gz = read.table(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_ANCOVA_n_Interaction/ipomopsis.txt"), sep="", header=T,comment.char="#")
head(gz)
plot(gz$Root)
qqnorm(gz[,"Root"])
qqline(gz[,"Root"], col="purple")
shapiro.test(gz[,"Root"])
plot(gz$Fruit)
qqnorm(gz[,"Fruit"])
qqline(gz[,"Fruit"], col="purple")
shapiro.test(gz[,"Fruit"])
plot(gz$Grazing)
#I was about to check normality for this variable... but looking at the plot, there's no need to is there?
#Also, the first two variables look normal, however the p value is quite high. Is that ok?
aov2 = lm(Fruit~ Root*Grazing, data=gz)
summary(aov2)
interplot(m=aov2, var1 = "Root", var2 = "Grazing")
interplot(m=aov2, var1 = "Grazing", var2 = "Root")
#the ancova popped out two significant p values, although grazing seems much more significant.
#So, For the first plot, the effect of the roots on the fruit weight is higher without grazinng compared to grazing, I think.
#For the second plot- the effect of grazing gradually decreases until the root weight reached about 9 (I'm estimating), at that point the effect of not grazing (ungrazing) is higher

data("airquality")
head(airquality, 15)
mod = lm(Temp~Wind, data=airquality)
summary(mod)
plot(Temp~Wind, data=airquality)
abline(lm(Temp~Wind, data=airquality), col="red")
mod1 = lm(Temp~Wind + Ozone, data=airquality)
summary(mod1)
mod2 = lm(Temp~Wind * Ozone, data=airquality)
summary(mod2)
interplot(mod2, var1="Wind", var2="Ozone")+
  labs(x="Wind", y="coefficient for Ozone")
interplot(mod2, var1="Ozone", var2="Wind")+
  labs(x="Ozone", y="coefficient for Wind")
#Exercise 2
var.test(mod1, mod2)
#So, with the f test we can reject the null, the variances are not equal
# I would go with model 2. Simple because there is a significant interaction between Wind and Ozone
# If there wasn't a significant interaction, I may have picked one, but the fact that they do affect each other should be noted in a model
# We can see this interaction has an effect on the results, because the f test found unequal variances
#For the next part... the models don't include an interaction with Solar.R- that was an error that came up

modsol= lm(Temp~Wind*Ozone, data=airquality)
summary(modsol)
interplot(modsol, var1="Wind", var2="Solar.R")
# I attempted to fix this by making a model with Solar.R in it... it didn't work
#So I guess there's not going to be an interaction plot for that.
interplot(mod2, var1 = "Wind", var2 = "Ozone")
interplot(mod2, var1 = "Ozone", var2 = "Wind")
#To make up for it I made these, which look so much alike I almost thought it didn't work
#So for this plot 1, the effect of Wind on Temperature is low until you get to about 15 in Ozone, then it has a much greater effect
#Plot 2 is identical, only the confidence interval is smaller. 


cor(airquality[,"Ozone"], airquality[,"Wind"], use="na.or.complete")
mod3 = lm(Temp~Wind + Wind:Ozone, data=airquality)
summary(mod3)
anova(mod2, mod3)
