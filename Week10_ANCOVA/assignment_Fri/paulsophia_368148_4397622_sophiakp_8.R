##Excercise 1
library(Lahman)
library(dplyr)
library(ggplot2)
library(RCurl)
library(interplot)
gz = read.table(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_ANCOVA_n_Interaction/ipomopsis.txt"), sep="", header=T,comment.char="#")
head(gz)
##first check it out/test assumptions
ggplot(data=gz, mapping=aes(x=Root, y=Fruit, color=factor(Grazing)))+
  +     geom_point()
##it looks like there is a difference betweem grazed and ungrazed
hist(gz$Root)
shapiro.test(gz$Root)
hist(gz$Fruit)
shapiro.test(gz$Fruit)
##both are normally distributed

model = lm(Fruit~ Root*Grazing, data=gz)
summary(model)
##Root has a significant impact on fruit
##however the interaction between root and grazed/ungrazed does not have a significant impact on fruit
interplot(m=model, var1="Root", var2="Grazing") 
##this shows the estimated effect of roots (the x axis) in grazed vs ungrazed (the y axis)
##and the confidence intervals of both
##the estimated coefficients are very similar
##Roots have a greater effect in ungrazed but that is not significant
interplot(m=model, var1="Grazing", var2="Root")+
  labs(x="Root", y="Estimated coefficient for Grazing", title="Estimated Coefficient of Grazing on Roots")
##The y axis is the effect of grazed/ungrazed on fruit, the x is root
##the grey area is the error
##The effect of grazed/ungrazed on fruit gets larger as root values increase, but only slightly
##the least error is present for middle-values for root
