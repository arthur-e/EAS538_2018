### Ex 1
Y = seq(from=0, to=20, by=0.1)
X1 = (Y-2)/3 + runif(length(Y), min=5, max=10)
X2 = (Y+3)/2 + runif(length(Y), min=5, max=10)
df = as.data.frame(cbind(Y, X1, X2))
summary(lm(Y~X1, data=df))
summary(lm(Y~X2, data=df))

summary(lm(Y~X1+X2, data=df))

plot(X1~X2)
cor(X1, X2)


### Ex 2_Bonus
aq = airquality %>%
  subset(is.na(Solar.R)==FALSE & is.na(Ozone)==FALSE)
mod1 = lm(Temp~Wind, data=aq)
summary(mod1)
mod2 = lm(Temp~Wind+Solar.R, data=aq)
summary(mod2)
RSS1 = sum(residuals(mod1)^2)
RSS2 = sum(residuals(mod2)^2)

f = ((RSS1-RSS2)/(3-2))/(RSS2/(nrow(airquality)-3))

f.p = 1-pf(f, df1=(3-2), df2=(nrow(airquality)-3))

### Ex 3
mod3 = lm(Temp~Wind+Solar.R+Ozone, data=aq)
summary(mod2)
anova(mod1, mod2, mod3)