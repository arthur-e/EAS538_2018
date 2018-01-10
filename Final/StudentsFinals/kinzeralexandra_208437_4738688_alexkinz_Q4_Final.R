#4)	What factors are significantly associated with a country’s 
#corruption levels (Corruption)? Choose three continuous independent 
#variables to include in your model.

happy = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/happy.csv",header=TRUE, sep=",")

#corruption level = continuous
#other variables = continuous

#multiple linear regression

#check that dependent variable is normally dist--NO
hist(happy$Corruption)
qqnorm(happy$Corruption); qqline(happy$Corruption, col="Red")
shapiro.test(happy$Corruption)

#SQRT TRANSFORM
happy$Ctrans <- sqrt(happy$Corruption)
hist(happy$Ctrans)
#skewed slightly right
qqnorm(happy$Ctrans); qqline(happy$Ctrans, col="Red")
#QQ is better
shapiro.test(happy$Ctrans)
#p-value is higher, but still not above .05 on Shapiro-Wilks.

#LOG TRANSFORM
happy$Ctrans1 <- log(happy$Corruption)
hist(happy$Ctrans1)
#better
qqnorm(happy$Ctrans1); qqline(happy$Ctrans1, col="Red")
#much better
shapiro.test(happy$Ctrans1)
#passes with a p-value of .08!
#I went ahead with the log transform

#check for linear relationship (with transformed data)
plot(Ctrans1~Life, data = happy)
cor(happy$Ctrans1,happy$GDP) #.17

plot(Ctrans1~Generosity, data = happy)
cor(happy$Ctrans1,happy$Generosity) #.24

plot(Ctrans1~Freedom, data = happy)
cor(happy$Ctrans1,happy$Freedom) #.45

happyMOD2 = lm(happy$Ctrans1~happy$Life + happy$Generosity + happy$Freedom)
summary(happyMOD2)
#Call:
#  lm(formula = happy$Ctrans1 ~ happy$Life + happy$Generosity + 
#       happy$Freedom)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-2.36070 -0.41907  0.06779  0.53881  1.27486 

#Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
#(Intercept)       -3.1091     0.1870 -16.625  < 2e-16 ***
#happy$Life        -0.0733     0.2578  -0.284    0.777    
#happy$Generosity   0.5186     0.4458   1.163    0.246    
#happy$Freedom      2.2601     0.4347   5.199 6.31e-07 ***
#  ---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.6933 on 153 degrees of freedom
#Multiple R-squared:  0.2107,	Adjusted R-squared:  0.1952 
#F-statistic: 13.61 on 3 and 153 DF,  p-value: 6.445e-08


#Heteroscedasticity #PASSES
bptest(happyMOD2)
#studentized Breusch-Pagan test
#data:  happyMOD2
#BP = 5.42, df = 3, p-value = 0.1435

#Normality of errors
res = residuals(happyMOD2)
plot(res)
hist(res) #slightly skewed
qqnorm(res); qqline(res, col="Red") #looks ok
shapiro.test(res) #fails

#Independence of errors #PASSES
install.packages(lmtest)
library(lmtest)
dwtest(happyMOD2, alternative=c("two.sided"))
#Durbin-Watson test

#data:  happyMOD2
#DW = 1.861, p-value = 0.3436
#alternative hypothesis: true autocorrelation is not 0

#Check for multicollinearity
cor(happy[,c(7,8,10)])



