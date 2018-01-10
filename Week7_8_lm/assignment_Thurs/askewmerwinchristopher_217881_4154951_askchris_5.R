## Assignment #5 
## Christopher Askew-Merwin
## 2/20/2017

######Exercise 1
#Try to plot and calculate the correlation coefficients 
#between the waiting time between eruptions and the 
#duration of the eruption for the Old Faithful geyser 
#in Yellowstone National Park. The dataset is built in 
#R named faithful.

data(faithful)
head(faithful)

plot(faithful$eruptions, faithful$waiting)

cor(faithful$eruptions, faithful$waiting)
# [1] 0.9008112

######Exercise 2

# 1) Build another linear model with another independent
#   variable, Ozone, and plot it with estimated 
#   regression line.

data("airquality")

mod1 = lm(Temp~Ozone, data=airquality)
summary(mod1)

#Call:
#lm(formula = Temp ~ Ozone, data = airquality)

#Residuals:
#    Min      1Q  Median      3Q     Max 
#-22.147  -4.858   1.828   4.342  12.328 

#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 69.41072    1.02971   67.41   <2e-16 ***
#Ozone        0.20081    0.01928   10.42   <2e-16 ***
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 6.819 on 114 degrees of freedom
#(37 observations deleted due to missingness)
#Multiple R-squared:  0.4877,	Adjusted R-squared:  0.4832 
#F-statistic: 108.5 on 1 and 114 DF,  p-value: < 2.2e-16

plot(Temp~Ozone, data=airquality)
abline(lm(Temp~Ozone, data=airquality), col="red")

# 2) Build a linear model to explain if you see longer 
#   eruption time if you wait for longer in the faithful dataset.

mod2 = lm(eruptions~waiting, data=faithful)
summary(mod2)

#Call:
#lm(formula = eruptions ~ waiting, data = faithful)

#Residuals:
#     Min       1Q   Median       3Q      Max 
#-1.29917 -0.37689  0.03508  0.34909  1.19329 

#Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -1.874016   0.160143  -11.70   <2e-16 ***
#waiting      0.075628   0.002219   34.09   <2e-16 ***
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 0.4965 on 270 degrees of freedom
#Multiple R-squared:  0.8115,	Adjusted R-squared:  0.8108 
#F-statistic:  1162 on 1 and 270 DF,  p-value: < 2.2e-16


plot(eruptions~waiting, data=faithful)
abline(lm(eruptions~waiting, data=faithful), col="red")

######Exercise 3

#Calculat the residual standard error (RSE) and mean 
#square error (MSE) manually for the model you built 
#last time (Ozone as the independent variable).

res = residuals(mod1)
RSS = sum(res^2)
RSE = sqrt(RSS/summary(mod1)$df[2])
RSE # = [1] 6.818914

RMSE = RSS/153
RMSE # = [1] 34.64526

MSE = RSS/summary(mod1)$df[2]
MSE # = [1] 46.49759


