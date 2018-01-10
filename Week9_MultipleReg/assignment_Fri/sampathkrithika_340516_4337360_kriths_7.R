data("airquality")

###Exercise 1
y= seq(from=0, to=20, by=0.1)
x1= (y-5)/2 + runif(length(y), min= 0.2, max= 6)
x2= (y-7)/1.5 + runif(length(y), min= 0.3, max= 5)
df= as.data.frame(cbind(y, x1, x2))
summary(lm(y~x1, data= df))
summary(lm(y~x2, data= df))
summary(lm(y~x1+x2, data= df))
plot(x1~x2)
cor(x1,x2)
#note: as the random noise increases, the correlation decreases. 

###Exercise 2
mod1<- lm(Temp~Wind, data=airquality)
summary(mod1)
mod2<- lm(Temp~Wind+ Solar.R, data= airquality)
summary(mod2)
mod3<- lm(Temp~Wind+ Solar.R+ Ozone, data= airquality)
summary(mod3)
anova(mod1,mod2,mod3)
#I kept getting the error "models were not fitted into the same size dataset. Could you help me out with this?
#However, since the multiple R squared is increasing from mod2 to mod3, I would expect the model with Ozone to explain more of the variance

###Exercise 3
AIC(mod1,mod2,mod3)
#I got the AIC alogn with the same error as above so I'm not sure if the answer is correct. 
#Since mod3 has the least AIC, it has the best performance. 