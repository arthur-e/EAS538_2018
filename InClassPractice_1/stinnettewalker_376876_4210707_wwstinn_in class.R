install.packages("fivethirtyeight")
library(fivethirtyeight)

head(tennis_events_time, 15)
Time = subset(tennis_events_time, select=c(sec_added))

Hard = subset(tennis_events_time, surface=="Hard")
Clay = subset(tennis_events_time, surface=="Clay")
Carpet = subset(tennis_events_time, surface=="Carpet")
Grass = subset(tennis_events_time, surface=="Grass")

boxplot(sec_added~surface, data=tennis_events_time, main="Distribution of seconds added per point", xlab="Surface", ylab="Seconds Added")

# test for normality
shapiro.test(Hard$sec_added)
shapiro.test(Clay$sec_added)
shapiro.test(Carpet$sec_added)
shapiro.test(Grass$sec_added)
# data are normally distributed

# test for equal variance
var.test(Hard$sec_added, Clay$sec_added)
var.test(Clay$sec_added, Carpet$sec_added)
# samples have equal variance

# Two sample t-test
t.test(Hard$sec_added, Clay$sec_added, paired = FALSE, var = TRUE)
# the two samples are significantly different (p-value = 2.2e-16)

# Two sample t-test
t.test(Clay$sec_added, Carpet$sec_added, paired = FALSE, var = TRUE)
# the two samples are significantly different (p-value = 2.2e-16)

#ANOVA
aov.sec = aov(sec_added~surface, data=tennis_events_time)
summary(aov.sec)
# there is a significant difference between the surface types (p-value<2e-16)

TukeyHSD(aov.sec)
# all surface types are significantly different from each other


