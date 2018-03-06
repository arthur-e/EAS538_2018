library(car)
library(lmtest)

# question 1
data(PlantGrowth)
boxplot(weight~group,data=PlantGrowth)
leveneTest(weight~group,data=PlantGrowth) # variances equal
anomod=aov(weight~group,data=PlantGrowth)
summary(anomod) # result significant
shapiro.test(resid(anomod)) # normal
TukeyHSD(anomod) # treatment 2 is significantly different from treatment 1

# question 2
test <- read.csv(file = "https://raw.githubusercontent.com/OscarFHC/EAS538_2018/master/ProblemSet1/test.csv", sep = ",", header = T, comment.char = "#")
boxplot(score~textbook,data=test)
var.test(score~textbook,data=test) # variances are equal
shapiro.test(test$score) # data are normal
t.test(score~textbook,var.equal=TRUE,data=test) # results are significantly different. the group who read the textbook had higher scores 
# it's okay if students use welch's t test because we said this gives simliar results to regular t test when variances are equal and to be on safe side students can always run welch's t test

# question 3
rate <- read.csv(file = "https://raw.githubusercontent.com/OscarFHC/EAS538_2018/master/ProblemSet1/rate.csv", sep = ",", header = T, comment.char = "#")
boxplot(rate~when,data=rate)
var.test(rate~when,data=rate) 
# technically students should not check the normality of the full dataset. They should only consider the normality of the differences between the two groups. give them 1 point extra credit if they checked the normality of the difference as opposed to the normality of the 'rate' column as is. Do not take off points if they checked the normality of the 'rate' column, even though this is wrong.
before=subset(rate,when=='before')
after=subset(rate,when=='after')
diff=after$rate-before$rate
shapiro.test(diff) # data are normally distributed
t.test(rate~when,data=rate,paired=TRUE) # results are significantly different. students had a higher heart rate when opening R studio after taking EAS 538. 

# question 4
house <- read.csv(file = "https://raw.githubusercontent.com/OscarFHC/EAS538_2018/master/ProblemSet1/housingdata.csv", sep = ",", header = T, comment.char = "#")
cor(house$nox,house$medv) # they are negatively correlated meaning that as nox increases, medv decreases (or vice versa). The correlation is neither very strong nor very weak (medium correlation).
plot(house$nox,house$medv,xlab='nox',ylab='medv') # you can give 1 point extra credit if students labeled axes in plots/made nice figures throughout the problem set
linmod=lm(medv~nox,data=house) 
resids=resid(linmod)
qqPlot(resids)
shapiro.test(resids) # errors are not normally distributed. Give one point extra credit if they try a transform and describe whether it improved/made worse normality and homoscedasticity.
bptest(linmod) # errors are homoscedastic
