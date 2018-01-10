# Energy data for question 1-4
edata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/energy_data.csv", 
                   sep=",", fill=TRUE, header=TRUE)
head(edata)

# Q1b-visual plot
edata$percapenergy<-edata$TotalEnergy/edata$Population
oncoast=subset(edata,Coast>= 1)
offcoast=subset(edata,Coast<1)

hist(oncoast[,'percapenergy'],breaks=15, col="light blue",xlim=c(0,1),ylim=c(0,8),xlab="per capita energy consumption",main="")
abline(v=mean(oncoast[,'percapenergy']),col="blue")
par(new=TRUE)
hist(offcoast[,'percapenergy'],breaks=15, col="light green",xlim=c(0,1),ylim=c(0,8),xlab="per capita energy consumption",main="")
abline(v=mean(offcoast[,'percapenergy']),col="dark green")
legend("topright",legend = c('oncoast mean','offcoast mean'),text.col=c("blue","dark green"))

# Q1c-assumptions checking
# equal variances
var.test(oncoast[,'percapenergy'],offcoast[,'percapenergy'])
# as p-value = 0.5098 which means the result is not signficant
# we failed to reject null hypothesis and the variances are equal, that follows the assumption

# normality test
shapiro.test(edata$percapenergy)
# since the p-values is 8.808e-08 which is significant, the null hypothesis is rejected
# The samples are not normally distributed, that violates the assumption 

# Bonus treatment of violation
outlier_values<-boxplot.stats(edata$percapenergy)$out
# from the histogram in Q1b, we found that outliers exit after 0.8 per capita energy consumption. 
edata$percapenergy[edata$percapenergy>0.8]=NA
shapiro.test(edata$percapenergy)
# now the p-value is 0.07215 which becomes insignificant, so we failed to reject null hypothesis 
# The samples are normally distributed now which follows the assumption

# independent sampling
# independent sampling cannot be tested here, we assume each observation is sampled independently

# Q1d-results
t.test(oncoast[,'percapenergy'],offcoast[,'percapenergy'],paired = FALSE)
# From the t-test, p-value=0.2245 which is larger than 0.05. So the result is not significant and we failed to reject the null hypothesis.
# Thus, per capita energy consumption has no significant difference on whether a state is found on the coast or not. 

#Q2b-visual plot
edata$percapcoal<-edata$TotalCoal/edata$Population
oncoast=subset(edata,Coast>= 1)
offcoast=subset(edata,Coast<1)

hist(oncoast[,'percapcoal'],breaks=15, col="light blue",xlim=c(0,1),ylim=c(0,10),xlab="per capita coal consumption",main="")
abline(v=mean(oncoast[,'percapcoal']),col="blue")
par(new=TRUE)
hist(offcoast[,'percapcoal'],breaks=15, col="light green",xlim=c(0,1),ylim=c(0,10),xlab="per capita coal consumption",main="")
abline(v=mean(offcoast[,'percapecoal']),col="dark green")
legend("topright",legend = c('oncoast mean','offcoast mean'),text.col=c("blue","dark green"))

# Q2c-assumptions checking
# equal variances
var.test(oncoast[,'percapcoal'],offcoast[,'percapcoal'])
# as p-value = 5.995e-13 which means the result is signficant
# we reject the null hypothesis and the variances are not equal, that violates the assumption

# normality test
shapiro.test(edata$percapcoal)
# since the p-values is 6.024e-11 which is significant, the null hypothese is rejected
# The samples are not normally distributed, that violates the assumption

# independent sampling
# independent sampling cannot be tested here, we assume each observation is sampled independently

# Q2d-results
# Since the variances are not equal, we do the Welch t-test
t.test(oncoast[,'percapcoal'],offcoast[,'percapcoal'],paired = FALSE)
# From the t-test, p-value=0.001936 which is less than 0.05. So the result is significant and we reject the null hypothesis
# Thus, per capita coal consumption has significant difference on whether a state is found on the coast or not

# Q3b-visual plot
boxplot(percapcoal~Region, data=edata, xlab="region", ylab="percapcoal")

# Q3c-assumptinos checking
# equal variances
library(car)
leveneTest(percapcoal~Region,data=edata)
# p-value is 0.5202 which is not significant which means variances are equal, that follows the assumption

# normality test
qqnorm(edata$percapcoal);qqline(edata$percapcoal,col="Red")
# from the QQ-plot test, the sample deviates from the qqline and we cannot assume the samples are normally distributed, that violates the assumption

# independent sampling
# independent sampling cannot be tested here, we assume each observation is sampled independently

# Q3d-results
regioncoal=aov(percapcoal~Region, data=edata)
summary(regioncoal)
# From the one way anova result, the p-value is 0.262 which is not significant
# We failed to reject the null hypothesis, the means of per capita coal have no significant difference in the 4 regions
# Thus, per capita coal consumption has no difference on the region in which a state is found. 

# Q4 
edata$percapgdp<-edata$TotalGDP/edata$Population
cor(edata[,c("percapcoal","percapgdp")])
# the correlation coefficient between per capita coal and per capita GDP is 0.0398.
# it means when percapcoal increase 1unit, the probability of percapgdp increase by 1unit is 3.98%

# Housing data for question 5-9
hdata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/housingdata.csv", 
                   sep=",", fill=TRUE, header=TRUE)

# Q5
# covariates chosen: crim,indus, and age
cor(hdata[,c(14,1,3,7)],use = "na.or.complete")
housevalue=(lm(medv~crim+indus+age, data=hdata))
summary(housevalue)
vif.housevalue=1/(1-summary(housevalue)$r.squared)

# VIF= 1.214 which is small and it means the degree of collinearity among these chosen covariates is small. 
# And thus, the model is applicable for evaluation. 

# Q6
pairs(hdata[,c(14,1,3,7)])
plot(medv~crim,data=hdata)
abline(lm(medv~crim,data=hdata),col="red")
# from the plot, per capita crime rate is negatively correlated to the housing value

plot(medv~indus,data=hdata)
abline(lm(medv~indus,data=hdata),col="red")
# from the plot, proportion of non-retail business acres per town is negatively correlated to the housing value

plot(medv~age,data=hdata)
abline(lm(medv~age,data=hdata),col="red")
# from the plot, proportion of owner-occupied units built prior to 1940 is negatively correlated to the housing value 

# Q7
summary(housevalue)

# assumptions checking 

# residual indpendency 
plot(residuals(housevalue))
# from the residuals plot, there is a clear pattern existed 
library(lmtest)
dwtest(housevalue, alternative=c("two.sided"))
# from the dwtest, the p-value is <2.2e-16 which is significant, it means autocorrelation exist in the model and violates the assumption 

# residual homoscedasticity 
plot(housevalue, which=c(1))
# from just eyeballing the plot, the result seems to be homoscedastic
bptest(housevalue)
# from the bptest, the p-value is 0.008422 which is not significant, so the residuals is homoscedastic and follows the assumption

# residual normality 
qqnorm(residuals(housevalue))
qqline(residuals(housevalue),col="red")
# from just eyeballing the qq plot, the residual seems to be not normally distributed 
shapiro.test(residuals(housevalue))
# from the shapiro test, the p-value is <2.2e-16 which is significant, so the residuals are not normally distributed and it violates the assumption

#bonus 
# From all the results above, the assumptions of residual independency and residual normality are being violated. 

# For the first problem, we can take a look back at the correlation value table.
# From the table, we find that the correlation coefficient between crim and indus, age and indus are both >0.5. 
# That means in general, they are a bit 'too' correlated. We can solve the problem by replacing another variable with indus to make the correlation values in the table all below 0.5. 
# Then run the dwtest again to look for a insignificant p-value (>0.05) 

# For the second problem, we ca n solve it by transforming the data into suitable format (log, square root, or power) in order to get normally distributed residuals. 
# The interpretation will depend on the form of transformation.
  
# Q8
summary(housevalue)
# from the regression result, medv = 29.9-0.226(crim)-0.432(indus)-0.0217(age) 

# for the first line of the coefficients table, the intercept coefficient means when all other variables hold zero, the housing value = 29.9
# the significant p-value (<2e-16) indicates that the coefficient is significantly different from zero 

# for the second line, the coefficient means that when indus and age hold zero, a unit increase in crim will lead to 0.226unit decrease in housing value
# the insignificant p-value (0.227) indicates that the effect of crim towards housing value is negligible 

# for the third line, the coefficient means that when crim and age hold zero, a unit increase in indus will lead to 0.432unit decrease in housing value
# the significant p-value (3.87e-08) indicates that the effect of indus towards housing value is significant 

# for the forth line, the coefficient means that when crim and indus hold zero, a unit increase in age will lead to 0.0217unit decrease in housing value
# the insignificant p-value (0.204) indicates that the effect of age towards housing value is negligible 

# bonus part
housevalue1=(lm(medv~crim+indus+age*dis, data=hdata))
summary(housevalue1)
# from the regression result, medv = 36.7-0.411(crim)-0.706(indus)+0.0401(age)-0.330(dis)-0.0288(age)(dis)

# for the first line of the coefficients table, the intercept coefficient means when all other variables hold zero, the housing value = 36.7
# the significant p-value (<2e-16) indicates that the coefficient is significantly different from zero 

# for the second line, the coefficient means that when indus,age, and dis hold zero, a unit increase in crim will lead to 0.411unit decrease in housing value
# the significant p-value (0.0231) indicates that the effect of crim towards housing value is significant 

# for the third line, the coefficient means that when crim,age, and dis hold zero, a unit increase in indus will lead to 0.706unit decrease in housing value
# the significant p-value (<2e-16) indicates that the effect of indus towards housing value is significant 

# for the forth line, the coefficient means that when crim,indus, and dis hold zero, a unit increase in age will lead to 0.0401unit increase in housing value
# the insignificant p-value (0.303) indicates that the effect of age towards housing value is negligible 

# for the fifth line, the coefficient means that when crim,indus, and age hold zero, a unit increase in dis will lead to 0.330unit decrease in housing value
# the insignificant p-value (0.484) indicates that the effect of dis towards housing value is negligible 

# for the sixth line, the coefficient means that when dis increase by 1unit, the effect of age towards housing value will decrease by 0.0288
# the significant p-value (0.000246) indicates that the interaction effect between age and dis is significant 
# however, in this case since both the individual effect of age and dis are negligible, even though the interaction is significant, 
# the interaction effect still cannot be expressed 

# Q9 
# One way to tell whether the model is good or bad is to examine R-squared 
# which tells how much of the variance is being explained by the independent variables we put in the model.
summary(housevalue)
# from the summary table, R-squared is 0.1764, which means that only 17.64% of the variability of the response data round its mean.
# So this is not really a good model 
# Meanwhile, the p-value of the F-statistic is significant, 2.2e-16, so that means the R-squared is reliable even though it is small.

# As i created a second model with interaction term, we can use model comparison to tell whether it is a good model.
anova(housevalue,housevalue1)
# From the result, the p-value is 5.397e-12 which is significant, that means the probability that model 2 can explain the same variance with model 1 is very small.
# So model 2 is explaining different variances. 
# Also, model 2 has a lower residual sum of squares, which means it has more predictable sum of squares than model 1. So model 2 is a better model.

