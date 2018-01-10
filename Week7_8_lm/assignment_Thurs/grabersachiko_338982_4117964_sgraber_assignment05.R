####Exercise 1
plot(faithful$eruptions~faithful$waiting,xlab="Wait time",ylab="Eruption length",main="Old Faithful geyser eruptions")
#It looks like there is a positive linear relationship between these variables, but there are also clusters near the top and bottom of the plot that might cause problems with residuals independence
cor(faithful$eruptions,faithful$waiting)
#There is a correlation coefficient of 0.9008112, indicating a high positive correlation between eruption length and wait time.


####Exercise 2
##2.1 Linear model of Ozone
modOz=lm(Temp~Ozone,data=airquality)
summary(modOz)
plot(Temp~Ozone,data=airquality,main="Temperature v. Ozone")
abline(lm(Temp~Ozone,data=airquality),col="green")
mtext("Temp=69.4+0.201*Ozone",side=1,line=-2,at=100,cex=0.9,col="darkgreen")
##2.2 Linear model of eruption time
modFaith=lm(faithful$eruptions~faithful$waiting)
summary(modFaith)
#For each one minute you wait, the your eruption time will be 0.076 minutes longer. This is highly significant. 
#Variation in wait time explains 81% of variation in eruption length


####Exercise 3. RSE, MSE for Ozone model
res.O = residuals(modOz)
RSS.O = sum(res.O^2)
RSE.O = sqrt(RSS.O/summary(modOz)$df[2]) #6.818914
RMSE.O = RSS.O/116
RMSE.O #45.6959
MSE.O = RSS.O/summary(modOz)$df[2]
MSE.O #46.49759
anova(modOz) #residuals/mean square is 46.5, so this checks out


