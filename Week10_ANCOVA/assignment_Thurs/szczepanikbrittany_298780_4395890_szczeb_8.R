##Exercise 1##
gz0=Lab.8.data
head(gz0)

gz=na.omit(gz0)
head(gz)

str(gz)
#There are two numbers and a factor (categorical variable), as we would expect

ggplot(data=gz, mapping=aes(x=Root, y=Fruit, color=factor(Grazing)))+
  geom_point()
##There appears to be a pattern in the data

#Test Root variable for normal distribution
ggplot(data=gz, mapping=aes(x=Root)) + 
  geom_histogram(aes(y = ..density..)) + geom_density(color="red")
##The data doesn't really look normally distributed, but the red line does have a sort of bell curve

qqnorm(gz[,"Root"])
qqline(gz[,"Root"], col="red")
##The data points follow the line, but not very well

shapiro.test(gz[,"Root"])
##The Root variable passes the Shapiro test (p-value = 0.6104).

#Test the Fruit variable for normal distrubution
ggplot(data=gz, mapping=aes(x=Fruit)) + 
  geom_histogram(aes(y = ..density..)) + geom_density(color="red")
##Again, the data doesn't really look normally distributed, but the red line does have a sort of bell curve

qqnorm(gz[,"Fruit"])
qqline(gz[,"Fruit"], col="red")
##The data points follow the line, and appears to be a little bit better than the Root variable

shapiro.test(gz[,"Fruit"])
##The Fruit variable passes the Shapiro test (p-value =0.7798).

#Now that we have proven that both the continuous variables are normally distributed, we can look at how the fruit and root relate in the ungrazed fields

#Ungrazed
gz %>%
  filter(Grazing=="Ungrazed") %>%
  ggplot()+
  geom_point(aes(x=Root, y=Fruit), color="red")+
  labs(x="Root", y="Fruit", title="Ungrazed")+
  geom_smooth(aes(x=Root, y=Fruit), method="lm", color="black", se=FALSE)
##Seems to be very little variance and no "outliers"

dat.ungrazed=gz %>%
  filter(Grazing=="Ungrazed")
summary(lm(Fruit~Root, data=dat.ungrazed))
##The adjusted R-squared is very high, and there is a significant positive relationship between Fruit and Root in the Ungrazed land

#Grazed
gz %>%
  filter(Grazing=="Grazed") %>%
  ggplot()+
  geom_point(aes(x=Root, y=Fruit), color="red")+
  labs(x="Root", y="Fruit", title="Grazed")+
  geom_smooth(aes(x=Root, y=Fruit), method="lm", color="black", se=FALSE)
##Same, seems to be little variance (a bit more than the previous "ungrazed graph) and no apparent "outliers"

dat.grazed=gz %>%
  filter(Grazing=="Grazed")
summary(lm(Fruit~Root, data=dat.grazed))
##The adjusted R-squared is again very high, and there is a significant positive relationship between Fruit and Root in the grazed land, but the relationship between the two differs
###Need to conduct ANCOVA to see if the relationship is significantly different

mod = lm(Fruit~ Root*Grazing, data=gz)
summary(mod)
anova(mod)
##The intercept is the Fruit in the Grazed land (due to alphabetical reason), when Root is 0.  
##The effect of Root on the Grazed land is significant (p-value = 2e-16)
##The estimate of Grazing is hte average Fruit difference between Grazed and Ungrazed land (which is significant, p-value = 1.21e-12)
##Additionally, the effect of Root does not significantly differ based on the effect of Ungrazed and Grazed land (p-value = 0.75)
    #This means the effect of one variable does not depend on the other one, and therefore the relationship between Fruit and Root does not based on Grazing

#Interaction Plots
library(interplot)
interplot(m=mod, var1="Root", var2="Grazing")+
  labs(x="Grazing", y="Estimated coefficient for Root", title="Estimated Coefficient of Root on Grazing")
##Here we can see that the effect of Root on Fruit in ungrazed and grazed land differs only slightly (it's slightly higher in Ungrazed land).  
##This difference is not significant, however, as can be shown by the fact that the means are within each other's confidence intervals, and their CI nearly overlap

interplot(m=mod, var1="Grazing", var2="Root")+
  labs(x="Root", y="Estimated coefficient for Grazing", title="Estimated Coefficient of Grazing on Root")
##This shows that the Fruit is higher in Ungrazed when compared to Grazed (positive intercept), and this difference gradually increases as the root gets deeper
##This gradual increase, however, is not significant


################################################################

#Exercise 2

data("airquality")
head(airquality, 15)

mod1 = lm(Temp~Wind + Ozone, data=airquality)
summary(mod1)

mod2 = lm(Temp~Wind * Ozone, data=airquality)
summary(mod2)

interplot(mod2, var1="Wind", var2="Ozone")+
  labs(x="Wind", y="coefficient for Ozone")

interplot(mod2, var1="Ozone", var2="Wind")+
  labs(x="Ozone", y="coefficient for Wind")

##Part 1--Use F-test to compare mod.1 and mod.2

anova(mod1, mod2)
##I would choose  mod2 because it explains a greater amount of variance when compared to mod1
##This is indicated by the probability for mod 1 to explain the same amount of variance as mod 2 (6.927e-05)
##Mod2 also has a smaller RSS, indicating a tighter fit

#Part 2--Plot two interaction plots with wind and solar radiation
mod3 = lm(Temp~Wind * Solar.R, data=airquality)
summary(mod3)

interplot(m=mod3, var1="Wind", var2="Solar.R")+
  labs(x="Wind", y="coefficient for Solar.R")
##With increasing wind, the magnitude of the coefficient for Solar.R slightly decreases
##The coefficient of Solar.R does not largely depend on Wind, as is shown in the graph, as well as the summary table (where the interaction term is not significant--p-value=0.7537)
##In the graph, we can visualize this because the slope of the graph is very modest, but slightly decreasing. 
##Additionally, since we can make a horizontal line in this plot within the confidence interval (gray area), we know that there is a chance that the two aren't interacting
    ##The fact that we can feasibly draw several horizontal lines in this plot, further illustrates the lack of significant for the interaction between Wind and Solar.R

interplot(mod3, var1="Solar.R", var2="Wind")+
  labs(x="Solar.R", y="coefficient for Wind")
##Similarly, the coefficient of Wind does not largely depend on Solar.R, as is shown in the graph
##In the graph, we can visualize this because the slope of the graph is very modest, although slightly decreasing (same as the previous graph).
##Similar to the previous interplot, we can imagine several horizontal lines being drawn within the confidence interval (gray area), therefore further illustrating that the terms are not significantly interacting
