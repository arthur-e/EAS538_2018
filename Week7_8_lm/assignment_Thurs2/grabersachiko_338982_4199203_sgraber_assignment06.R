####Exercise 4
####Exercise 4. Calculate adjusted R2 manually
set.seed(1)
adjR2 = function(x,y) {
  new=cbind(x,y)
  new2=new[complete.cases(new),]
  x=new2[,1]
  y=new2[,2]
  k = ncol(as.data.frame(x)) + 1 #x + intercept makes 2 parameters estimated
  n = nrow(as.data.frame(x))
  model=lm(y~x)
  e=residuals(model)
  ybar=mean(y)
  numerator=c()
  denominator=c()
  numerator=(sum(e^2))/(n-k)
  denominator=(sum((y-ybar)^2))/(n-1)
  return(1-(numerator/denominator))
}
adjR2(airquality$Ozone,airquality$Temp) #0.4832134




####Exercise 5
model5=lm(airquality$Temp~airquality$Ozone)
#Residual Interdependency
dwtest(model5,alternative=c("two.sided"))
#p<<0.01 so there is an autocorrelation, our data are time-dependent and interdependent.

#homoskedasticity
bptest(model5)
#p=0.07 so we can say results are homoskedastic at alpha=5%

#normality
qqnorm(residuals(model5))
qqline(residuals(model5),col="red")
shapiro.test(residuals(model5))
#in the qqplot, our data skew significantly from the line in the middle.
#the shapiro test also yields significant results, p=0.000226
#so ataOT NORMALLY DISTRIBUTED

