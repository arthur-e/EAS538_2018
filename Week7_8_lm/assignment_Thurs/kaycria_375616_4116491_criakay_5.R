##Excersize One#
data("faithful")
pairs(faithful[, c("eruptions", "waiting")])
cor(faithful[, c("eruptions", "waiting")], use="na.or.complete")
plot(faithful$eruptions, faithful$waiting) ##Same basic result as the pairs function, pairs just repeats the same graph since there are only two variables
##Any time waiting changes one unit, there is 90% chance that eruptions will change one unit (correlation coifeient is 0.9008112)


##Excersize Two## How do we expect y to change
#Part One#
data("airquality")
mod1 = lm(Temp~Ozone, data= airquality)
summary(mod1)
plot(Temp~Ozone, data= airquality)
abline(lm(Temp~Ozone, data= airquality), col="red")
##The intecept is 69.41072, the slope is 0.20081. If you increase ozone by 1, the Temperature will increase by 0.20081. The P value is significant, so the relationship between temperature and ozone is significant

##Excersize Two #Part Two#
mod3 = lm(eruptions~waiting, data= faithful)
summary(mod3)
plot(eruptions~waiting, data= faithful)
abline(lm(eruptions~waiting, data=faithful), col="red")
##The intercept is -1.874016, the slope is 0.075628. If you increase waiting by 1, eruptions will increase by 0.075628. P value is signficant, so the relationship between eruptions and waiting is significant.

##Excersize Three##
res = residuals(mod1)
RSS = sum(res^2)
RSE = sqrt(RSS/summary(mod1)$df[2])
RSE
##Residual Standard Error (the square root of the residual sum of squares divided by the degrees of freedom = 6.818914
MSE = RSS/summary(mod1)$df[2]
MSE
##Mean Squared Error(Residual standard error divided by the degrees of freedom (i.e. Residual stanadard error squared)) = 46.49759