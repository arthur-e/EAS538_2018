### EXERCISE 1

data("faithful") #load data
head(faithful) #see first few rows of data
cor(faithful$eruptions, faithful$waiting) #calculate correlation coefficient
#correlation coefficient = 0.900812
#This indicates a high probability that eruptions will increase by one unit when
#waiting increases by one unit.
plot(eruptions~waiting, data=faithful) #plot waiting time vs. eruptions


### EXERCISE 2

#2.1:

data("airquality") #load airquality data
ozonemod  = lm(Wind~Ozone, data=airquality) #create linear model
summary(ozonemod) #view summary of the linear model
plot(Wind~Ozone, data=airquality) #plot data. are these axes right?
abline(ozonemod, col="red") #add linear model to plot
#Here we see a negative correlation between Ozone and Wind - when Ozone decreases,
#so does Wind.

#2.2:
waitlm = lm(eruptions~waiting, data=faithful) #build linear regression model
summary(waitlm) #see summary of linear model
#According to the summary, the linear model creates a line with a y-intercept at
#-1.874016, with a positive slope of 0.075628.
plot(eruptions~waiting, data=faithful) #recreate plot from exercise 1
abline(lm(eruptions~waiting, data=faithful), col="red") #add linear regression line


### EXERCISE 3

ozres = residuals(ozonemod) #calculate residuals in linear model
ozRSS = sum(ozres^2) #calculate residual sum of squares
ozRSE = sqrt(ozRSS/summary(ozonemod)$df[2]) #calculate RSE
ozRSE
#ozRSE = 26.46729
#This RSE can also be seen as representing the standard deviation of the residuals.

ozMSE = ozRSS/summary(ozonemod)$df[2] #calculate MSE
ozMSE
#ozMSE = 700.5177
#This MSE can also be seen as representing the variance of the residuals.

#use summary of linear model and anova to confirm manually calculated RSE and MSE:
anova(ozonemod)
summary(ozonemod)
