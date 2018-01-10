# Exercise 1
Y = seq(from=0, to=20, by=0.1)
X1 = (Y-2)/3 + runif(length(Y), min=0.5, max=20)
X2 = (Y+3)/2 + runif(length(Y), min=0.5, max=200)
df = as.data.frame(cbind(Y, X1, X2))

#summary(lm(Y~X1+X2, data=df))
plot(X1~X2)
cor(X1,X2)

##### Oscar: I would expect to see more discussion on this exercise. 

# Exercise 2
#install.packages('dplyr')
data('airquality')
airquality = airquality %>%
  subset(is.na(Solar.R)==FALSE & is.na(Ozone)==FALSE)
mod1 = lm(Temp~Wind, data=airquality)
mod2 = lm(Temp~Wind+Solar.R, data=airquality)
mod3 = lm(Temp~Wind+Solar.R+Ozone, data=airquality)
summary(mod3)
anova(mod2,mod3)
# yes it explains more variance because it has a lower RSS value than model 2
# (model 2 was already shown to explain more variance than model 1, so if mod3>mod2, then mod3>mod1)

# Exercise 3
AIC(mod1)
AIC(mod2)
AIC(mod3)
# all of these are much greater than 10, so there isn't much information to compare the models, but 
# mod3's lower AIC suggests that it is not performing as well as the other models.

##### Oscar: The lower the AIC value is, the higher the probability of seeing the data given the model (likelihood), which is a good sign!