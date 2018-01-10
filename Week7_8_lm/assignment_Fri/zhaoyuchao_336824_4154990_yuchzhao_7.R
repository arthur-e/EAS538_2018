###Exercise 1
data(faithful)
head(faithful)
cor(faithful$eruptions,faithful$waiting)
### 0.9008112
plot(eruptions~waiting, data=faithful)
abline(lm(eruptions~waiting, data=faithful), col="red")



###Exercise 2-1
mod = lm(Temp~Ozone, data=airquality)
summary(mod)
plot(Temp~Ozone, data=airquality)
abline(lm(Temp~Ozone, data=airquality), col="red")

###Exercise 2-2
mod = lm(eruptions~waiting, data=faithful)
summary(mod)
         Coefficients:
                    Estimate   Std. Error t value   Pr(>|t|)    
       (Intercept) -1.874016   0.160143  -11.70   <2e-16 ***
       waiting      0.075628   0.002219   34.09   <2e-16 ***
plot(eruptions~waiting, data=faithful)
abline(lm(eruptions~waiting, data=faithful), col="red")
###  the data result in the second line indicate when "wating" change how "eruption" time will change. The estimate value is 0.075, such positive corelationship indicate when the waiting is increase the eruption time will also increase. The P-value is very samll incidate such corelatonship is reliable.



###Exercise 3
mod = lm(Temp~Ozone, data=airquality)
res = residuals(mod)
RSS = sum(res^2)
summary(mod) ###114DF
str(summary(mod)) ### df:int [1:3] 2 114 2 
### so we know we are looking for the second df value which is 114

RSE = sqrt(RSS/summary(mod)$df[2])
RSE ### 6.818914
MSE= RSS/summary(mod)$df[2]
MSE ### 46.4975






      

