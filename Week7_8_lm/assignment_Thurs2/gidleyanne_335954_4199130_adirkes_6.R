# adirkes
# adj R^2*(n-k) = R^2*(n-1)


# Exercise 4

is.na(airquality$Ozone)
airqual = subset(airquality, is.na(Ozone)==FALSE)
mod = lm(Temp~Ozone, data=airqual)
res = residuals(mod)
RSS = sum(res^2)

SSY = deviance(lm(airqual$Temp~1))
SSY1 = var(airqual$Temp)*115
R2 = 1 - RSS/SSY

#summary(mod)

n = 116
k = 2 # intercept and slope
frac = 1-R2
adjFrac = frac*(n-1)/(n-k)
adjR2 = 1-adjFrac
adjR2 # 0.4832

# Exercise 5

plot(res)
#install.packages(lmtest)
#library(lmtest)
dwtest(mod, alternative=c("two.sided"))
# p-val << 0.001, DW = 1.1833
plot(residuals(mod)~fitted(mod))
abline(lm(residuals(mod)~fitted(mod)),col="red")
bptest(mod)
# BP = 3.2346, p-val = 0.0721
qqnorm(res)
qqline(res, col="blue")
#plot(mod2, which=c(2))
shapiro.test(res)
# w = 0.94867, p-val = 0.000226
#plot(mod2)

# The residuals do not appear to be independent based on the dwtest
# They are homoscedastic based on the BP Test
# They are not normally distributed based on the Shapiro-Wilk test.
# A linear model is not appropriate for this dataset because it fails these tests
