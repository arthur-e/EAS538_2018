setwd("/afs/umich.edu/user/w/a/wayihan")
#create data of the pupolation
Truemean=5
truesd=2
population=rnorm(10000, Truemean, truesd)
hist(population)

# get data for the sample
N=100
> sampleN = sample(population, N)
> mean = mean(sampleN)
> sd = sd(sampleN)
> se = sd/(sqrt(N))
> error = qnorm(0.975)*se # 95% confidence interval
> left = mean-error
> right = mean+error
> range = c(lef, right)
Error: object 'lef' not found
> range = c(left, right)
> range
[1] 4.735266 5.469727
> ####
  fert=read.csv("lecture6_fertilizer.csv")
setwd("/Users/yihanwang/Desktop")
fert=read.csv("lecture6_fertilizer.csv")
manual = subset(fert, Type=="m")
spreader = subset(fert, Type=="s")

 # two-tailed t test
t. test(flintlead, mu=15) # one sample, two-tailed t test
t. test(flintlead, mu=15, alte
        rnative = "greater") # one sample, one-tailed t test

#Week4 Lab
setwd("/Users/yihanwang/Desktop")
library(RCurl)
guns=read.csv("/Users/yihanwang/Downloads/guns_NRE538.csv",sep=",", header=T, comment.char="#")
guns.MI = subset(guns, state=="Michigan")

library(RCurl)
guns=read.csv(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_Lab2/guns.csv"), sep=",", header=T, comment.char="#")
guns.MI = subset(guns, state=="Michigan")
t.test(guns.MI[, "murder"], mu=9)
hist(guns.MI[, "murder"], breaks=20)
abline(v=mean(guns.MI[, "murder"]), col="blue")
abline(v=9, col="dark green")
legend("topleft", legend=c("sample mean", "specified value=9"), test.col=c"blue", "dark green")

#Excersie 1
#one-tail:
#H0: The robbery rate is significantly greater than 300 per 10^5 people in Michigan state. 
guns.MI = subset(guns, state=="Michigan")
t.test(guns.MI$robbery, mu=300, alternative="less")
#the t-test shows that we are confident to reject H0, which means that the robbery rate is significantly less than 300 per 10^5 people in michgian state.

#two-tail:
#H0: The number of prisoners is not different from 350 per 10^5 people in Michigan state
guns.MI = subset(guns, state=="Michigan")
t.test(guns.MI$prisoners, mu=350)
#The result shows that the mean of our sample is significantly different from 350 per 10^5 people in Michigan State. We reject H0. 

#Exercise 2:
#two sample t-test:
shapiro.test(guns.WAS[, "murder"])
shapiro.test(guns.MIN[, "murder"])
#Shapiro test shows that the two data frame follow normal distribution
var.test(guns.WAS[, "murder"], guns.MIN[, "murder"])
#the F test shows P value is greater than 0.05. We fail to reject the null hypothesis, which means that the two variances are the same
#The assumptions for t-test are met, so the results from this unpaired t-test should be robust. guns.WAS = subset(guns, state=="Washington")
guns.MIN = subset(guns, state=="Minnesota")
t.test(guns.WAS[, "murder"], guns.MIN[, "murder"], paired = FALSE)
#the p value is less than 0.05, so we reject H0, which means that the murder rate in two states are significantly different from each other.

#Lecture 7
#one-way Anova (one dependent variable and one catergory)
#Manova(multiple dependent variables and one independent variable)
data=read.csv('/Users/yihanwang/Desktop/anova_data.csv', sep=",", header=T, comment.char="#")
model = aov(wbc~as.factor(group), data = data)
summary(model)

TukeyHSD(model)
boxplot(wbc~as.factor(group), data=data)

model = aov(bgl~as.factor(group), data = data)
summary(model)

TukeyHSD(model)
boxplot(bgl~as.factor(group), data = data)

model=aov(sun~as.factor(group), data=data)
summary(model)

model=aov(rats~as.factor(group), data = data)
summary(model)

model=aov(meat~as.factor(group), data = data)
summary(model)

model=aov(fruit~as.factor(group), data = data)
summary(model)

TukeyHSD(model)

#Two-way Anova (two indenpendt variables and one dependent variable)
#two factors of interests

#5.10 Textbook exercise
library(RCurl)
url<-"https://raw.githubusercontent.com/NicoleRadziwill/Data-for-R-Examples/master/anova-textbooks.txt"
score.data <- getURL(url,ssl.verifypeer=FALSE)
scores <- read.table(text=score.data, header = T)

shapiro.test(scores$textbook)
var(scores[scores$textbook==1,]$score)
var(scores[scores$textbook==3,]$score)
52.78333/52.5625
data=read.csv('/Users/yihanwang/Library/Application support/Microsoft/Office/Office 2011 AutoRecovery/income.csv')
model=aov(Income~as.factor(Villages), data = data)
model = aov(total.income~as.factor(Villages), data=data)
summary(model)

model=aov(income~as.factor(Villages), data = data)
model = aov(Education~as.factor(Villages), data = data)
summary(model)
TukeyHSD(model)

model=aov(Cast~as.factor(Villages), data = data)
summary(model)
TukeyHSD(model)
var(data[data$Villages==1,]$Income)
var(data[data$Villages==1,]$total.income)
var(data[data$Villages==2,]$total.income)

var(data[data$group==1,]$fruit)
var(data[data$group==2,]$fruit)
var(data[data$group==3,]$fruit)

#Chi-Square test 
row1<-c(6,7,7,2,7,5,11,11,9)
row2<-c(5,2,2,8,8,5,3,2,1)
my.ctable<-rbind(row1, row2)
dimnames(my.ctable)<-list(state=c("Satisfied","Unsatisfied"), villages=c(1,2,3,4,5,6,7,8,9))
my.ctable
chisq.test(my.ctable)
#Satisfaction of CFUG is dependent of villages 

Xsq.results<-chisq.test(my.ctable)
((Xsq.results$observed-Xsq.results$expected)^2)/Xsq.results$expected
sum(((Xsq.results$observed-Xsq.results$expected)^2)/Xsq.results$expected)

contin.coeff<-function(xsq){sqrt(xsq$statistic/(sum(xsq$observed)+xsq$statistic))}
contin.coeff(Xsq.results)

hist(data$Income)
hist(data$Education)
qqplot(data$Income)

qchisq(0.975, df=8)

model = aov(participation~as.factor(total.income), data = data)
summary(model)
TukeyHSD(model)

model=aov(total.income~as.factor(Villages), data = data)
summary(model)
TukeyHSD(model)

mean(data$total.income)
mean(data$Income)
mean(data$Education)

#ANOVA for factorial design
data("CO2")
head(CO2, 10)
str(CO2)
boxplot(uptake~Treatment*Type, data = CO2, xlab="treatment", ylab="decrease mean")
CO2.aov = aov(uptake~Treatment + Type, data=CO2)
summary(CO2.aov)

#Exercise 1
CO2.aov = aov(uptake~Treatment*Type, data = CO2)
summary(CO2.aov)
model.tables(CO2.aov, "effect", se=TRUE)
model.tables(CO2.aov, "mean", se=TRUE)
TukeyHSD(CO2.aov)
#The effect table shows that the difference of the mean of each treatment model comparing to the grand mean. For example, the grand mean is 27.2131, and the mean of nonchilled treatment(regardless of type) is 30.643, which is 3.43 difference to the grand mean

#The mean table shows the mean effect size of each treatment model, without calculating the difference of the grand mean. For example, the effect size of chilled treatment(regardless of Type) is 23.783.

#Exercise 2
CO2.aov2.1 = lm(uptake~Treatment*Type, data = CO2)
summary (CO2.aov2.1)
CO2.aov2.2 = lm(uptake~Treatment*Type-1, data = CO2)
summary(CO2.aov2.2)
#lm() without -1 displays effect size model of each treatment model comparing to the first treatment. For example, the first treatment model is nonchilled Quebec, and the estimate effect size is 35.333. The rest of the estimate of each treatment model is the difference of tha treatment effects comparing to nonchilled Quebec.
#lm() with -1 displays the mean model, so the estimate of each treatment model is the mean effect size of that treatment.
