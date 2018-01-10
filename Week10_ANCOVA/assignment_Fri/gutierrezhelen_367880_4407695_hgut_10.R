setwd("C:/Users/helen/Documents/winter_2017/538_stats/lab/W10")

library(RCurl)
gz = read.table(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_ANCOVA_n_Interaction/ipomopsis.txt"), sep="", header=T,comment.char="#")
head(gz)
str(gz)

#Checking for assumptions:
#1. Normality
ggplot(data=gz, mapping=aes(x=Root)) + 
  geom_histogram(aes(y = ..density..)) + geom_density(color="red")
qqnorm(gz[,"Root"])
qqline(gz[,"Root"], col="red")
shapiro.test(gz[,"Root"])
#The data passes the Shapiro test and lies normally on the qq-plot

ggplot(data=gz, mapping=aes(x=Fruit)) + 
  geom_histogram(aes(y = ..density..)) + geom_density(color="red")
qqnorm(gz[,"Fruit"])
qqline(gz[,"Fruit"], col="red")
shapiro.test(gz[,"Fruit"])
#The data passes the Shapiro test and lies normally on the qq-plot

library(dplyr)
#Fruit production with grazing
gz %>%
  filter(Grazing=="Grazed") %>%
  ggplot()+
  geom_point(aes(x=Root, y=Fruit), color="red")+
  labs(x="Root depth", y="Fruit weight", title="Fruit Production with grazing")+
  geom_smooth(aes(x=Root, y=Fruit), method="lm", color="black", se=FALSE)

Fruit.grazing = gz%>%
  filter(Grazing=="Grazed")
summary(lm(Fruit~ Root, data=Fruit.grazing))
#There is a significant positive relationship between fruit weight and root length 
#with grazing.

#Fruit production without grazing
gz %>%
  filter(Grazing=="Ungrazed") %>%
  ggplot()+
  geom_point(aes(x=Root, y=Fruit), color="blue")+
  labs(x="Root depth", y="Fruit weight", title="Fruit Production without grazing")+
  geom_smooth(aes(x=Root, y=Fruit), method="lm", color="black", se=FALSE)

Fruit.grazing = gz%>%
  filter(Grazing=="Ungrazed")
summary(lm(Fruit~ Root, data=Fruit.grazing))
#There is a significant positive relationship between fruit weight and root length 
#without grazing. 
#These tests and graphs show that the relationship between fruit weight and root 
#length does vary between samples that have experienced grazing and 
#samples that haven't.

#ANCOVA test
mod = lm(Fruit~ Root*Grazing, data=gz)
aov = aov(Fruit~ Root*Grazing, data=gz)
summary(mod)
anova(mod)
summary(aov)
#The output of the linear model shows that the effect of root size on fruit weight in
#samples that experienced grazing is significant (fruit weight increases 23.24 units
#for every unit increase in root size). The estimate of "GrazingUngrazed" is the average
#difference in fruit weight between grazed and ungrazed samples. The interaction term between
#grazing and root length is not significant, which means that the relationship between
#fruit weight and root length is not significantly different in the presence or absence 
#of grazing. 

library(interplot)
interplot(m=mod, var1="Root", var2="Grazing")+
  labs(x="Grazing", y="Estimated coefficient for Root Length", title="Estimated Coefficient of root length on Grazing")
#The plot shows that the effect (regression coefficient) of root length on fruit weight
#is higher in samples that weren't grazed than in samples that were. However, the difference
#is not significant because the confidence intervals overlap.