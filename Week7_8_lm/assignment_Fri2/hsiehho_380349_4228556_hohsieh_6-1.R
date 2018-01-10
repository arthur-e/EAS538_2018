#Ho Hsieh_NRE538_Lab6

#Exercise 4####
data("airquality")
lm_o=lm(Temp~Ozone, data = airquality)


res_o = residuals(lm_o)
RSS_o = sum(res_o^2)
n = length(res_o)
k=2

airquality.oz = subset(airquality,Ozone!="NA" & Temp!="NA")
SSY.oz.a = deviance(lm(Temp~1, data=airquality.oz)) #1
SSY1.oz.a = var(airquality.oz$Temp)*115 #2

R2.a = 1-(RSS_o/(n-k))/(SSY.oz.a/(n-1)) #1
R2.a #0.4832134

R2.a1 = 1-(RSS_o/(n-k))/(SSY1.oz.a/(n-1)) #2
R2.a1 #0.4832134


#Exercise 5####

#Ex5_Independency####
dwtest(lm_o, alternative=c("two.sided")) #p-value = 7.276e-06 <0.05
#reject the hull hypothesis, and true autocorrelation is not 0


#Ex5_Homoscedasticity####
library(lmtest)
plot(residuals(lm_o)~fitted(lm_o))
abline(lm(residuals(lm_o)~fitted(lm_o)), col="red")
bptest(lm_o) #p-value = 0.0721 > 0.05
#cannot reject the null hypothesis that the variance of the residuals is constant

#Ex5_Normality####
qqnorm(residuals(lm_o))
qqline(residuals(lm_o), col="red")
shapiro.test(residuals(lm_o)) #p-value = 0.000226 <0.05
#the distribution of the residuals of the model 
#is significantly different from normal distribution
