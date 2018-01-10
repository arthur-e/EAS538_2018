#Ho Hsieh_NRE538_wk7_lab

#Exercise 1####
data("faithful")
head(faithful)
cor(faithful[,c("eruptions", "waiting")]) #0.9008112
#the results suggest that when waiting time change one unit, there is a 90.08112% probability that the eruption would change one unit
lm_e=lm(eruptions~waiting, data = faithful)
lm_w=lm(waiting~eruptions, data = faithful)
plot(eruptions~waiting, data = faithful)
abline(lm_e)
plot(waiting~eruptions, data = faithful)
abline(lm_w)


#Exercise 2####
#1
data("airquality")
head(airquality)
lm_o=lm(Temp~Ozone, data = airquality)
summary(lm_o) #Multiple R-squared:  0.4877;  p-value: < 2.2e-16
plot(Temp~Ozone, data = airquality)
abline(lm_o, col="red")
#the result suggests that the higher the Ozone is, the higher the temperature would be.

#2
head(faithful)
lm_e=lm(eruptions~waiting, data = faithful)
summary(lm_e) #R^2=0.8115  p-value: < 2.2e-16
plot(eruptions~waiting, data = faithful)
abline(lm_e, col = "red")
# the results suggest that the longer you wait the longer eruption time you would see


#Exercise 3####
lm_o=lm(Temp~Ozone, data = airquality)
summary(lm_o) 
#Residual standard error: 6.819 on 114 degrees of freedom
str(summary(lm_o)$df[2])
res_o=residuals(lm_o)
RSS_o = sum(res_o^2)
RSE_o = sqrt(RSS_o/summary(lm_o)$df[2]) #6.818914
MSE_o = RSS_o/(summary(lm_o)$df[2]) #46.49759
RSE_o
MSE_o