library(RCurl)
###Exercise 1
gz = read.table(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_ANCOVA_n_Interaction/ipomopsis.txt"), sep="", header=T,comment.char="#")
head(gz)
qqnorm(gz[,"Root"])
qqline(gz[,"Root"], col="red")
qqnorm(gz[,"Fruit"])
qqline(gz[,"Fruit"], col="red")
mod=lm(Fruit~Root*Grazing, data=gz)
aov= aov(Fruit~Root*Grazing, data=gz)
summary(mod)
anova(mod)
summary(aov)
library(interplot)
interplot(m=mod, var1="Root", var2="Grazing")+labs(x="Grazing", y="Estimated coefficient for Root depth", title="Estimated Coefficient of Root depth on Grazing")
interplot(m=mod, var1="Grazing", var2="Root")+labs(x="Root depth", y="Estimated coefficient for Grazing", title="Estimated Coefficient of Grazing on root depth")



