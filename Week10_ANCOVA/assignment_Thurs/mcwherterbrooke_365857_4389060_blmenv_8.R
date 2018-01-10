#Exercise 1
#Try to use the following data set to execute ANCOVA and make 
#interaction plots This data set contains the fruit weight (Fruit) 
#of different plants with different root depth (root) and different treatment (Grazing).
install.packages("RCurl")
library(RCurl)
library(ggplot2)
gz = read.table(text=getURL
                ("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_ANCOVA_n_Interaction/ipomopsis.txt"), 
                sep="", header=T,comment.char="#")
head(gz)

ggplot(data=gz, mapping=aes(x=gz$Root, y=gz$Fruit, color=factor(gz$Grazing)))+
  geom_point()
qqnorm(gz[,"Root"])  #looks at normality
qqline(gz[,"Root"], col="red")

ggplot(data=gz, mapping=aes(x=)) + 
  geom_histogram(aes(y = ..density..)) + geom_density(color="red")

#Plot demonstrates what appears to be a positive correlation. Where the size
# of root appears to correlate with larger fruits in both grazed and ungrazed plots.
#The largestfruit and larger root depths appeared to be correlated more highly with grazed plots
#Plot also shows that for a given root size, grazed plants produced fewer fruits (further tests are needed to determine
#significance)

shapiro.test(gz[,"Root"])
#p.value = 0.6104 and w=0.977 signifying that there is normality in the data
shapiro.test(gz[, "Fruit"])
#p value = 0.7798 and w=0.98247 signifying that there is normality in the data


mod = lm(Fruit~ Root*Grazing, data=gz)
summary(mod)
anova(mod)
#There is a significant difference between Fruit size and Root (p=2e-16).
# However there is no difference in the slope of the relationship between the two grazing treatments  (t=0.321, P=0.7500)

modb = lm(Root~Fruit*Grazing, data=gz)
summary(modb)

# There is still significant difference between fruit size and root  (p=2e-16)
# Grazing type is now significant in relation to root depth (p=2e-16), though the estimate indicates that it is a small 
# difference (estimate -1.5, t.value=-6.439)

install.packages("interplot")
library(interplot)
interplot(m=mod, var1="Root", var2="Grazing")+
  labs(x="Grazing Treatment", y="Estimated coefficient for Root Diameter",
       title="Estimated Coefficient of Root Diameter on Grazing Treatment")
interplot(m=mod, var1="Grazing", var2="Root")+
  labs(x="Root", y="Estimated coefficient for Grazing Treatment", 
       title="Estimated Coefficient of Grazing Treatment on Root Diameter")

#By looking at the interaction terms on the relationship of root diameter on grazing treatment. As demonstrated by the line, as 
# Root diameter increases the differences between Grazing and Ungrazed gets smaller and then ungrazed appears to increse on root production.


# interaction plots indicate that for a given root size the grazed plants produce less fruit than that
# of the ungrazed plants. 


#Exercise 2 (BONUS)
#1  #Use F-test to compare mod.1 and mod.2. 
    #Explain why you chose one over another to explain the data.

data("airquality")
head(airquality, 20)
mod1 = lm(Temp~Wind * Ozone, data=airquality)
summary(mod1)
mod2 = lm(Temp~Wind + Ozone, data=airquality)
summary(mod2)
anova(mod1, mod2)

#The p value of the f statistic is significant (p-6.927e-05), indicating
#that the addition of the new variables significantly improved the model. 

#2  #Plot two interaction plots with interplot() for the model 
    #with wind and solar radiation as the independent variables. 
    #Interprete the two interaction plots.

mod3 = lm(Temp~Wind * Solar.R, data=airquality)
summary(mod2)

interplot(mod3, var1="Wind", var2="Solar.R")+
  labs(x="Wind", y="coefficient for Solar Radiation")


#Interaction plot shows a slight decrease in solar radiation as wind increases

interplot(mod3, var1="Solar.R", var2="Wind")+
  labs(x="Solar.R", y="coefficient for Wind")

#Interaction plot shows a decrease in solar radiadion as wind increases