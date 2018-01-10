

## Exercise 1

## There isn't a significant interaction between
## the Root and Grazying coefficients. The line of
## the second interplot shows an almost parallel line
## on the graph. The first interplot graph shows
## significant overlap. It does show that there is
## a higher effect of plants with roots with ungrazed
## plants because the line further to the right
## is higher.

gz = read.table(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_ANCOVA_n_Interaction/ipomopsis.txt"), sep="", header=T,comment.char="#")
head(gz)
qqnorm(gz[,"Root"])
qqline(gz[,"Root"], col="red")
shapiro.test(gz[,"Root"])

qqnorm(gz[,"Fruit"])
qqline(gz[,"Fruit"])
shapiro.test(gz[,"Fruit"])

modelfruit= lm(Fruit~ Root*Grazing, data=gz)
summary(modelfruit)
interplot(m=modelfruit, var1="Root", var2="Grazing")+
labs(x="Grazing", y="Estimated coefficient for Root", title="Estimated Coefficient of Root on Grazing")
interplot(m=modelfruit, var1="Grazing", var2="Root")+
labs(x="Root", y="Estimated coefficient for Grazing", title="Estimated Coefficient of Grazing on Root")


## Exercise 2

mod3 = lm(Temp~Wind * Solar.R, data=airquality)
summary(mod3)
interplot(mod3, var1="Wind", var2="Solar.R")+
labs(x="Wind", y="Coefficient for Solar Radiation")
interplot(mod3, var1="Solar.R", var2="Wind")+
labs(x="Solar Radiation", y="coefficient for Wind")

## From the model summary, there is not a significant
## interaction between Wind and Solar Radiation. This
## is also confirmed in the interplots. The slope
## for both plots is not very steep, and there is 
## very little change as the line moves across the
## graph. Also, the shaded areas are pretty think.

