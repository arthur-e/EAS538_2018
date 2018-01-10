library(RCurl)
gz = read.table(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_ANCOVA_n_Interaction/ipomopsis.txt"), sep="", header=T, comment.char="#")
head(gz)
str(gz)

# Exercise 1
library(ggplot2)
ggplot(data=gz, mapping=aes(x=Root, y= Fruit, color=factor(Grazing)))+
  geom_point()
## From the plot, it looks like root depth and fruit weight have a positive relationship with average higher fruit weight when ungrazed than grazed.

ggplot(data=gz, mapping=aes(x=Root))+
  geom_histogram(aes(y=..density..))+
  geom_density(color="red")
qqnorm(gz[,"Root"])
qqline(gz[,"Root"],col="red")
shapiro.test(gz[,"Root"]) # p-value=0.6104
## The Root variable looks normal distributed and passes the shapiro test.

ggplot(data=gz, mapping=aes(x=Fruit))+
  geom_histogram(aes(y=..density..))+
  geom_density(color="red")
qqnorm(gz[,"Fruit"])
qqline(gz[,"Fruit"],col="red")
shapiro.test(gz[,"Fruit"]) # p-value=0.7798
## The Fruit variable looks normal distributed and passes the shapiro test.

library(dplyr)
# Grazed
gz %>%
  filter(Grazing=="Grazed") %>%
  ggplot()+
    geom_point(aes(x=Root,y=Fruit),color="red")+
    labs(x="Root Depth",y="Fruit Weight",title="Grazed")+
    geom_smooth(aes(x=Root,y=Fruit),method="lm",color="black",se=FALSE)

gz.grazed=gz %>%
  filter(Grazing=="Grazed")
summary(lm(Fruit~Root,data=gz.grazed))
## There is a significant positive relationship between fruit weight and root depth in the grazed treatment.

# Ungrazed
gz %>%
  filter(Grazing=="Ungrazed") %>%
  ggplot(aes(x=Root,y=Fruit))+
    geom_point(color="blue")+
    labs(x="Root Depth",y="Fruit Weight",title="Ungrazed")+
    geom_smooth(aes(x=Root,y=Fruit),method="lm",color="black",se=FALSE)

gz.ungrazed=gz %>%
  filter(Grazing=="Ungrazed")
summary(lm(Fruit~Root,data=gz.ungrazed))
## There is a significant positive relationship between fruit weight and root depth in the ungrazed treatment.

# ANCOVA
mod=lm(Fruit~Root*Grazing, data=gz)
aov=aov(Fruit~Root*Grazing, data=gz)
summary(mod)
anova(mod)
summary(aov)
## The intercept (= -125.173) is the fruit weight in grazed treatment when root depth is 0.
## The estimate of Root means the effects of root depth on fruit weight in grazed treatment, which is significant.
## The estimate of Grazing is the average fruit weight difference between grazed and ungrazed treatments, which is not significant.
## The interaction term means whether the effect of root depth differs from grazed to ungrazed treatment, which is not significant.
### The relationship between fruit weight and root depth (the effect of root depth on fruit weight) is not significantly different from grazed to ungrazed treatment.

# Interaction Plot
library(interplot)
interplot(m=mod,var1="Root",var2="Grazing")+
  labs(x="Grazing Treatment",y="Estimated Coefficient for Root",title="Estimated Coefficient of Root on Grazing Treatment")
## The plot shows the effect of root depth on fruit weight might be slightly higher in the ungrazed than in the grazed treatment, 
## but the difference is not significant because one estimate falls inside the confidence interval of the other estimate.

interplot(m=mod,var1="Grazing",var2="Root")+
  labs(x="Root Depth",y="Estimated Coefficient for Grazing Treatment",title="Estimated Coefficient of Grazing Treatment on Root Depth")
## The plot shows the fruit weight in the ungrazed treatment is always almost significantly higher than in the grazed treatment,
## and the difference gradually increase with the increase of root depth.
## However, this change of fruit weight difference between ungrazed and grazed treatments is not significant.

# Exercise 2
data("airquality")
head(airquality,15)
mod=lm(Temp~Wind,data=airquality)
summary(mod)
plot(Temp~Wind,data=airquality)
abline(lm(Temp~Wind,data=airquality),col="red")

mod1=lm(Temp~Wind+Ozone,data=airquality)
summary(mod1)
mod2=lm(Temp~Wind*Ozone,data=airquality)
summary(mod2)

interplot(mod2,var1="Wind",var2="Ozone")+
  labs(x="Ozone",y="Coefficient for Wind")
interplot(mod2,var1="Ozone",var2="Wind")+
  labs(x="Wind",y="Coefficient for Ozone")

## Exercise 2-1
# F-test
anova(mod1,mod2)
### RSS of mod1 = 5166.5, RSS of mod2 = 4482.7, F=17.085, p-value=6.927e-05***
### I'll choose mod2 to explain the data.
### Including the interaction term is important because it significantly reduces the RSS (p-value<0.05).
### mod2 explains significantly greater amount of variance comparing to mod1
### because the probability for mod1 to explain the same amount of variance as mod2 is 6.927e-05
cor(airquality[,"Ozone"], airquality[,"Wind"], use="na.or.complete") # cor=-0.6015465
### However, the independent variables are highly correlated; therefore, the estimate of regreassion coefficients provided by the model is not so reasonable.

## Exercise 2-2
mod3=lm(Temp~Wind*Solar.R,data=airquality)
summary(mod3)
### B means Beta
### Temp = B0 + B.wind*Wind + B.solar.r*Solar.R + B.inter*(Wind*Solar.R)
### Temp = 83.5922238 + (-1.0384133)*Wind + 0.0328498*Solar.R + (-0.0006492)*(Wind*Solar.R)

#### When looking at the effect of Solar.R on Wind
#### Temp = 83.5922238 + [(-1.0384133) + (-0.0006492)*Solar.R]*Wind + 0.0328498*Solar.R

#### When looking at the effect of Wind on Solar.R
#### Temp = 83.5922238 + (-1.0384133)*Wind + [0.0328498 + (-0.0006492)*Wind]*Solar.R

##### Based on the p-value, the coefficient of intercept and Wind are significant,
##### but the coefficient of Solar.R and interaction are not significant
##### => Temp = B0 + B.wind*Wind
##### => Temp = 83.5922238 + (-1.0384133)*Wind

interplot(mod3,var1="Wind",var2="Solar.R")+
  labs(x="Solar.R",y="Coefficient for Wind")
### The plot means when Solar.R gradually increases, the negative effect of Wind on Temp will gradually increase.
### When Solar.R increases one unit, the intercept will become 83.6250736 (=83.5922238+0.0328498*1) and the negative effect of Wind on Temp will increase from -1.0384133 to -1.0390625 [= -1.0384133+(-0.0006492*1)]
### When Solar.R increases two units, the intercept will become 83.6579234 (=83.5922238+0.0328498*2) and the negative effect of Wind on Temp will increase from -1.0384133 to -1.0397117 [= -1.0384133+(-0.0006492*2)]
### and so on
### In other word, as Solar.R increases, the effect of Wind on Temp (the coefficient of Wind = B.wind) will decrease by the slope of (-0.0006492)
### => B.wind = -1.0384133 -0.0006492*Solar.R

interplot(mod3,var1="Solar.R",var2="Wind")+
  labs(x="Wind",y="Coefficient for Solar.R")
### The plot means when Wind gradually increases, the positive effect of Solar.R on Temp will gradually decrease.
### When Wind increases one unit, the intercept will become 82.5538105 [=83.5922238+(-1.0384133*1)] and the positive effect of Solar.R on Temp will decrease from 0.0328498 to 0.0322006 [= 0.0328498+(-0.0006492*1)]
### When Wind increases two units, the intercept will become 81.5153972 [=83.5922238+(-1.0384133*2)] and the positive effect of Solar.R on Temp will decrease from 0.0328498 to 0.0315514 [= 0.0328498+(-0.0006492*2)]
### and so on
### In other word, as Wind increases, the effect of Solar.R on Temp (the coefficient of Solar.R = B.solar.r) will decrease by the slope of (-0.0006492)
### => B.solar.r = 0.0328498 -0.0006492*Wind

#### Both plots have the lines with the same slope of -0.0006492, which means
#### the first plot: Solar.R influences the effect of Wind on Temp (B.wind) by -0.0006492 as Solar.R increases every one unit
#### the second plot: Wind influences the effect of Solar.R on Temp (B.solar.r) by -0.0006492 as Wind increases every one unit
#### However, these influences (interaction effects) are not significant based on the p-value (0.7537>0.05) of the interaction term.
#### The insignificant interaction effects can also be seen from both interaction plots.
#### A horizontal line in the interaction plot represents that there is no interaction between the independent variables.
#### Therefore,
#### from the first plot, we can draw a horizontal line that completely falls within the grey area (Confidence Interval of the interaction term)
#### from the second plot, we can also draw a horizontal line that completely falls within the grey area (Confidence Interval of the interaction term)
#### Which demonstrates that the estimated line (slope= -0.0006492) does not significantly different from the horizontal line (slope=0)
#### Which means the interaction effects of both Solar.R on Wind and Wind on Solar.R are not significant.
