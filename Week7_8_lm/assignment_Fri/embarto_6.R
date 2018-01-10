#Erin Barton 
#NRE538-StatsLab
#2.20.2017

#Lab 6

#Exercise 1: plot and calculate the cor coef btwn the waiting time btwn eruptions and the duration of 
#the eruption of the Old Faithful geysers in YNP. The dataset is built in R named faithul

data("faithful")
head(faithful)

cor(faithful[,c("eruptions","waiting")],use="na.or.complete")
#          eruptions   waiting
#eruptions 1.0000000 0.9008112
#waiting   0.9008112 1.0000000

cor_faith <- cor(faithful$waiting, faithful$eruptions, use="na.or.complete")
#[1] 0.9008112
plot(faithful$waiting~faithful$eruptions)

#This correlation coefficient suggests that the probability of the waiting time btwn eruptions will increase by one unit as the 
#and the duration of the eruption increases by one unit. It is a strong correlation. 



#Exercise 2: 
#1 Build another linear model with another independent variable, Ozone, and split it with estimated 
#regression line
#2 Build a linear model to explain if you see longer eruption time if you wait for longer in the 
#faithful dataset

#1
data("airquality")

lm(Temp~Ozone, data=airquality)
#Coefficients:
#(Intercept)        Ozone  
#  69.4107       0.2008  

ozone_mod <- lm(Temp~Ozone, data=airquality)
summary(ozone_mod)
plot(Temp~Ozone, data=airquality)
abline(lm(Temp~Ozone, data=airquality),col="red")
#This plot shows that there is a positive relationship between temperature and ozone. If Ozone increases by 1 unit, temperature should increase by 0.20081 units. 
#The low p-value and R-squared above # shows the relationship is significant. 


#2
attach(faithful)
faith_mod <- lm(faithful$eruption~faithful$waiting, data=faithful)
summary(faith_mod)

plot(faithful$eruption~faithful$waiting, data=faithful)
abline(lm(faithful$eruption~faithful$waiting, data=faithful),col="red")

#The plot shows a positive relationship between waiting time and eruption length, suggesting that waiting longer will result in you seeing longer eruptions. 
#This is supported by the models low p-value and high r-squared value. 



#Exercise 3: Calculate the RSE and MSE manually for the model you built last time 
#(Ozone as the independent variable). 

res <- residuals(ozone_mod)
RSS <- sum(res^2)
RSS
#[1] 5300.725
str(faithful) 
summary(ozone_mod)
str(summary(ozone_mod))
RSE <- sqrt(RSS/ozone_mod$df)
RSE
#[1] 6.818914

MSE=RSS/ozone_mod$df
MSE
#[1] 46.49759

#check work <- anova has matching MSE and RSS
anova(ozone_mod)

#The MSE (and anova check) show that there is significantly more variation when comparing temperature and ozone than in a null model. 
