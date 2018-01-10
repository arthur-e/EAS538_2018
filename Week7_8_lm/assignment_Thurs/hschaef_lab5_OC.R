## Hannah Schaefer

## Exercise 1 Using dataset "faithful"
data(faithful)
plot(faithful$eruptions,faithful$waiting)
cor(faithful[, c("waiting", "eruptions")], use="na.or.complete")

## There is a positive correlation between waiting time and eruption. When eruption
## increases by a unit of 1, there is a 90 percent chance that waiting will increase 
## by a value of 1. When waiting increases by 1, then there is a 90 percent chance that 
## eruption will increase by 1.


## Exercise 2
## part 1
airqualitymodel = lm(Temp~Ozone, data=airquality)
plot(airquality$Ozone, airquality$Temp)
abline(lm(Temp~Ozone, data=airquality), col="red")
summary(airqualitymodel)

## There is a positive correlation between Ozone and Temperature, so as Ozone increases, 
## temperature will increase. As temperature increases, ozone also increases. The data fit the
## graph with an R^2 value of 0.49. So the model explains almost 50% of the data. There was
## also a significant p value where p<2.2e-16.

## part 2
model2 = lm(eruptions~waiting, data=faithful)
plot(faithful$waiting, faithful$eruptions)
abline(lm(eruptions~waiting, data=faithful), col="red")
summary(model2)

## There is a positive correlation between waiting time and eruption time. As the waiting
## time increases and becomes longer, the eruption time becomes longer as well. The data fit
## the graph with an R^2 value of 0.81. This means the model explains about 81% of the data.
## The model also had a significant p value <2.2e-16.

## Exercise 3
resairquality = residuals(airqualitymodel)
RSSairquality= sum(resairquality^2)
RSSairquality
## RSS = 5300.725
## This RSS value is large and suggests a poor fit of the model to the data.
RSEairquality = sqrt(RSSairquality/summary(airqualitymodel)$df[2])
RSEairquality
## RSE = 6.818914
## The RSE value is greater than 0, suggesting the data do not completely fit the model.
MSEairquality = RSSairquality/summary(airqualitymodel)$df[2]
MSEairquality
## MSE = 46.49759
## MSE tells us how close the regression line is to our data points. This value is quite
## a bit larger than 0, suggesting the model might not fit the data very well.

### Oscar's comments: 
# NO! RES and MSE will always be larger than zero. 
# It is the relative quantity of RSS to SSY (sum of square of response variable) that determines the goodness of fit of the model.
# The MSE can be used to see which model explain the response variable better (by judging the relative ratio of the MSE of the two models).
# Also, if you have a model that fit the data completely. You are probably over fitting the data, which is not a good sign. 




