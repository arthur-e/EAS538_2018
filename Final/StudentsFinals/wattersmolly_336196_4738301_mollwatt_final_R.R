###TAKEHOME FINAL EXAM
##Question 1--------------------------------------
#1)	Is there a significant association between gender (gender)
#and whether people think it's rude to bring an unruly child on the plane (unruly_child)? 
#If yes, which gender tends to think that bringing an unruly child is more rude?

flying = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/flying.csv",header=TRUE, sep=",")

chisq.test(flying$gender,flying$unruly_child)
#p value = 0.001193, so there is a significant association between gender and whether people think it's rude to bring an unruly child on the plane.

#MALES
male=subset(flying, gender=="Male")

length(which(male$unruly_child=="No"))
length(which(male$unruly_child=="Somewhat"))
length(which(male$unruly_child=="Very"))
#see what percentage of men think it's very rude as compared with the total for all 3 answers

length(which(male$unruly_child=="Very"))/(length(which(male$unruly_child=="No"))+length(which(male$unruly_child=="Somewhat"))+length(which(male$unruly_child=="Very")))
#percent males who think it's very rude = 47.38%

#FEMALES
female=subset(flying, gender=="Female")

length(which(female$unruly_child=="No"))
length(which(female$unruly_child=="Somewhat"))
length(which(female$unruly_child=="Very"))
#see what percentage of women think it's very rude as compared with the total for all 3 answers

length(which(female$unruly_child=="Very"))/(length(which(female$unruly_child=="No"))+length(which(female$unruly_child=="Somewhat"))+length(which(female$unruly_child=="Very")))
#percent females who think it's very rude = 35.75%

##Males tend to think it is more rude (more males think it's very rude) to bring an unruly child on an airplane than do females.

###Question 2---------------------------------------
college = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/college.csv",header=TRUE, sep=",")

type.pub = subset(college, type=="Public")
type.priv = subset(college, type=="Private nonprofit")
t.test(type.pub[,"tuition"], type.priv[,"tuition"], paired=FALSE)

#plot histograms
hist(type.priv[,"tuition"], breaks=15, col="light green", xlim=c(3000, 50000), ylim=c(0,200), xlab="", main="")
abline(v=mean(type.priv[,"tuition"]), col="dark green")
par(new=TRUE)
hist(type.pub[,"tuition"], breaks=15, col="light blue", xlim=c(3000, 50000), ylim=c(0,200), xlab="tuition", main="")
abline(v=mean(type.pub[,"tuition"]), col="blue")
legend("topright", legend = c("Public mean", "Private mean"),text.col=c("blue", "dark green"))

#test assumptions
#normal
shapiro.test(college$tuition)
qqPlot(college$tuition)

#variance
var.test(type.pub[,"tuition"], type.priv[,"tuition"])

###Question 3----------------------------------------------
happy = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/happy.csv",header=TRUE, sep=",")
## Is there a significant difference in happiness (Hscore) by region (Region)?

#one-way anova
#test assumptions
#normality
shapiro.test(happy$Hscore)
#p-value is smaller than 0.05 (0.0123) which means we rejec the null of normality 
hist(happy$Hscore)
#based on the histogram the data looks normally distributed...
library(car)
qqPlot(happy$Hscore)
#qqplot looks good so we can assume normally distributed enough

#equal variance
library(Rcmdr)
library(car)
leveneTest(Hscore~Region,data=happy)
#p-value is large so we cannot reject the null of different variances


AsiaAust=subset(happy, Region=="AsiaAustralia")
Europe=subset(happy, Region=="Europe")
AmCar=subset(happy, Region=="AmericasCarribean")
AfMidE=subset(happy, Region=="AfricaMideast")

boxplot(AsiaAust$Hscore, Europe$Hscore, AmCar$Hscore, AfMidE$Hscore)
#boxplot shows that AmericasCarribean has the highest mean happiness score

#one way anova
happy.aov = aov(Hscore~Region, data=happy)
summary(happy.aov)

###Question 4-------------------------------------------
##4)	What factors are significantly associated with a country's corruption levels (Corruption)? Choose three continuous independent variables to include in your model. 
cor(happy[,c("Corruption", "GDP","Family","Life","Freedom","Generosity")])
pairs(happy[,c("Corruption", "GDP","Family","Life","Freedom","Generosity")])
##Let's test freedom, generosity, and GDP

#test asusmptions
#linear relationship
#make plots
plot(happy$Freedom, happy$Corruption)
plot(happy$Generosity, happy$Corruption)
plot(happy$GDP, happy$Corruption)
#look linear

#make model
corrup=lm(formula=Corruption~Freedom+Generosity+GDP,data=happy)
summary(corrup)

#test assumption of autocorrelation
plot(residuals(corruplog))
library(lmtest)
dwtest(corruplog, alternative=c("two.sided"))

#test assumption of homoscedasticity
plot(residuals(corruplog)~fitted(corruplog))
abline(lm(residuals(corruplog)~fitted(corruplog)), col="red")
library(lmtest)
bptest(corruplog)

#normal distribution of corruption
shapiro.test(happy$Corruption)
hist(happy$Corruption)

#not normal so need to transform--log transform dependent
library(magrittr)
library(plyr)
#make new variable called CorrupLog
happy2=mutate(happy, CorrupLog=log(Corruption))

hist(happy2$CorrupLog)
shapiro.test(happy2$CorrupLog)

CorrupRes1=residuals(corruplog)
plot(CorrupRes1)
shapiro.test(CorrupRes1)

qqPlot(CorrupRes1)
#normal enough

#run new model with log transformed variable
corruplog=lm(formula=CorrupLog~Freedom+Generosity+GDP,data=happy2)
summary(corruplog)

corruplog1=lm(formula=CorrupLog~Freedom+Generosity,data=happy2)
corruplog2=lm(formula=CorrupLog~Freedom+GDP,data=happy2)
corruplog3=lm(formula=CorrupLog~Generosity+GDP,data=happy2)
corruplog4=lm(formula=CorrupLog~Freedom,data=happy2)
corruplog5=lm(formula=CorrupLog~GDP,data=happy2)
corruplog6=lm(formula=CorrupLog~Generosity,data=happy2)

AIC(corruplog,corruplog1,corruplog2,corruplog3,corruplog4,corruplog5,corruplog6)

###Question 5--------------------------------------
##Choose one of the continuous independent variables that was significant in the model for Question 4 and interact it with region (Region) to predict corruption (Corruption). 
#This model should only include one continuous independent variable and its interaction with region. Does the influence of your continuous variable on corruption vary by region? 
#If yes, how do you interpret the interaction? 
#I choose freedom
#new model-ANCOVA
corrup2 = lm(CorrupLog~Freedom*Region, data=happy2)
summary(corrup2)

#visualize and check normality
ggplot(data=happy2, mapping=aes(x=Freedom, y=CorrupLog, color=factor(Region)))+
  geom_point()

#freedom first
ggplot(data=happy2, mapping=aes(x=Freedom)) + 
  geom_histogram(aes(y = ..density..)) + geom_density(color="red")

qqnorm(happy2[,"Freedom"])
qqline(happy2[,"Freedom"], col="red")

shapiro.test(happy2[,"Freedom"])

#now corruption2

ggplot(data=happy2, mapping=aes(x=CorrupLog)) + 
  geom_histogram(aes(y = ..density..)) + geom_density(color="red")

qqnorm(happy2[,"CorrupLog"])
qqline(happy2[,"CorrupLog"], col="red")

shapiro.test(happy2[,"CorrupLog"])
#check normal residuals
CorrupRes=residuals(corrup2)
plot(CorrupRes)
shapiro.test(CorrupRes)

qqPlot(CorrupRes)
#normal enough

#test assumption of autocorrelation
plot(residuals(corrup2))
library(lmtest)
dwtest(corrup2, alternative=c("two.sided"))

#test assumption of homoscedasticity
plot(residuals(corrup2)~fitted(corrup))
abline(lm(residuals(corrup2)~fitted(corrup2)), col="red")
library(lmtest)
bptest(corrup2)

##run actual model
corrup2 = lm(CorrupLog~Freedom*Region, data=happy2)
summary(corrup2)

####QUESTION 6------------------------------------------
#6)	Which factors are significantly associated with whether a breast cancer tumor is malignant or not? 
#Choose three continuous independent variables to include in your model. cancer

cancer = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/cancer.csv",header=TRUE, sep=",")

cor(cancer)
pairs(cancer)

##make model
mod1 = glm(malignant~perimeter_mean+texture_mean+smoothness_mean, data=cancer, family=binomial(link="logit"))
summary(mod1)

#model checking
mod0 = glm(malignant~1, data=cancer, family=binomial(link=logit))
summary(mod0)
anova(mod0, mod1, test="Chi")
AIC(mod0, mod1)

mod2 = glm(malignant~perimeter_mean, data=cancer, family=binomial(link=logit))
summary(mod2)
anova(mod1, mod2, test="Chi")
AIC(mod1, mod2)

mod3 = glm(malignant~texture_mean, data=cancer, family=binomial(link=logit))
summary(mod3)

mod4 = glm(malignant~smoothness_mean, data=cancer, family=binomial(link=logit))
summary(mod4)

mod5=glm(malignant~perimeter_mean+smoothness_mean, data=cancer, family=binomial(link=logit))
summary(mod5)

mod6=glm(malignant~texture_mean+smoothness_mean, data=cancer, family=binomial(link=logit))
summary(mod6)

mod7=glm(malignant~texture_mean+perimeter_mean, data=cancer, family=binomial(link=logit))
summary(mod7)

anova(mod0,mod1,mod2,mod3,mod4,mod5,mod6,mod7)
AIC(mod0,mod1,mod2,mod3,mod4,mod5,mod6,mod7)

#extra credit




