## set working directory
setwd("/docs/umich/coursework/538/labs/lab09")

## install ggplot2 and import data
install.packages("ggplot2")
library(ggplot2)
install.packages("interplot")
library(interplot)
library(dplyr)
library(RCurl)
gz = read.table(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_ANCOVA_n_Interaction/ipomopsis.txt"), 
                sep="", header=T,comment.char="#")


## begin exercise 1
## preliminary visualization of data comparison
ggplot(data=gz, mapping=aes(x=Root, y=Fruit, color=factor(Grazing)))+
  geom_point()
## the data show linear relationships and there appears to be a difference
## between the groups

## investigate distributions of continuous variables
ggplot(data=gz, mapping=aes(x=Root)) + 
  geom_histogram(aes(y = ..density..)) + geom_density(color="red")

qqnorm(gz[,"Root"])
qqline(gz[,"Root"], col="red")

shapiro.test(gz[,"Root"])
## the root data appear reasonably normal and have passed the Shapiro test

ggplot(data=gz, mapping=aes(x=Fruit)) + 
  geom_histogram(aes(y = ..density..)) + geom_density(color="red")

qqnorm(gz[,"Fruit"])
qqline(gz[,"Fruit"], col="red")

shapiro.test(gz[,"Fruit"])
## the fruit data also appear reasonably normal and have passed the Shapiro test

## plot fruit against root for the grazed group
gz %>%
  filter(Grazing=="Grazed") %>%
  ggplot()+
  geom_point(aes(x=Root, y=Fruit), color="red")+
  labs(x="Root", y="Fruit", title="Grazed")+
  geom_smooth(aes(x=Root, y=Fruit), method="lm", color="black", se=FALSE)

## subset grazed data
gz.Gr = gz %>%
  filter(Grazing=="Grazed")
summary(lm(Fruit~Root, data=gz.Gr))
## there is a significant linear relationship between fruit and root in the grazed data


## repeat plotting and subsetting for the ungrazed group
gz %>%
  filter(Grazing=="Ungrazed") %>%
  ggplot()+
  geom_point(aes(x=Root, y=Fruit), color="red")+
  labs(x="Root", y="Fruit", title="Ungrazed")+
  geom_smooth(aes(x=Root, y=Fruit), method="lm", color="black", se=FALSE)

gz.Un = gz %>%
  filter(Grazing=="Ungrazed")
summary(lm(Fruit~Root, data=gz.Un))
## there is a similar significant linear relationship between fruit and root in the 
## ungrazed data, with a different intercept


## generate linear model for interaction plots
mod.gz = lm(Fruit~Root*Grazing, data=gz)
summary(mod.gz)
## the summary does not show a significant different between the groups

## visualize this relationship with interplots
interplot(m=mod.gz, var1="Root", var2="Grazing")+
  labs(x="Grazing", y="Estimated coefficient for Root", 
       title="Estimated Coefficient of Root on Grazing")
## the intervals clearly overlap, as expected

interplot(m=mod.gz, var1="Grazing", var2="Root")+
  labs(x="Root", y="Estimated coefficient for Grazing", 
       title="Estimated Coefficient of Grazing on Root")
## there is a weak positive interaction by grazing on root, but it is not significant


## begin exercise 2.1
## import data
data("airquality")

## generate linear models
mod.air.1 = lm(Temp~Wind+Ozone, data=airquality)
mod.air.2 = lm(Temp~Wind*Ozone, data=airquality)

## compare models
anova(mod.air.1,mod.air.2)
## the models are significantly different
summary(mod.air.1)
summary(mod.air.2)
## based on the R-squared values, the second model explains more of the variation
## in the data


## begin exercise 2.2
## generate third linear model
mod.air.3 = lm(Temp~Wind*Solar.R, data=airquality)
summary(mod.air.3)
## the interactions between Wind and Solar.R are not significant

## visualize relationships with interplots
interplot(m=mod.air.3, var1="Wind", var2="Solar.R")+
  labs(x="Solar.R", y="Estimated coefficient for Wind", 
       title="Estimated Coefficient of Wind on Solar.R")
## there is a weak negative interaction by Wind on Solar.R, but it is not significant

interplot(m=mod.air.3, var1="Solar.R", var2="Wind")+
  labs(x="Wind", y="Estimated coefficient for Solar.R", 
       title="Estimated Coefficient of Solar.R on Wind")
## there is a weak negative interaction by Solar.R on Wind, but it is not significant