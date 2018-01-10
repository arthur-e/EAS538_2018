###Question1
flying = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/flying.csv",header=TRUE, sep=",")
newfly= transform(flying, unrulybaby = as.numeric(unruly_child) )
male=subset(newfly,gender=="Male")
female=subset(newfly,gender=="Female")
shapiro.test(newfly$unrulybaby)
var.test(male$unrulybaby,female$unrulybaby)
t.test(male$unrulybaby,female$unrulybaby,alternative='greater',var.equal=TRUE)
### Because P-value<0.05, I will accept the alternative hypothesis that true 
###differences in means is greater than 0. Which means there is a significant   
###association between gender and whether people think it???s rude to bring an unruly 
###child on the plane. And male tends to think its more rude.



###Question2
college = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/college.csv",header=TRUE, sep=",")
shapiro.test(college$tuition) ###p-value = 3.193e-15
public=subset(college,type=="Public")
private=subset(college,type=="Private nonprofit")
var.test(public$tuition, private$tuition) ###p-value < 2.2e-16
t.test(public$tuition, private$tuition,alternative='less')
###p-value < 2.2e-16, indicate we can reject the null hypothesis and accept the alternative that
###true difference in means is less than 0, which indicate the mean of tuition in pulic college is 
###less than the tuition mean in private nonprofit college 

###Question3
happy = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/happy.csv",header=TRUE, sep=",")
shapiro.test(happy$Hscore) ### p-value = 0.01248
library(Rcmdr)
leveneTest(Hscore~Region,data=happy) ### p-value=0.5179
question3 = aov(Hscore~Region,data=happy)
summary(question3)

###Question4
shapiro.test(happy$Corruption)
shapiro.test(sqrt(happy$Corruption))
cor(happy[,c("GDP","Generosity","Freedom")], use="na.or.complete")
vif1 = 1/(1 - summary(lm(GDP~Generosity+Freedom, data=happy))$r.squared)
vif1
vif2 = 1/(1 - summary(lm(Freedom~Generosity+GDP, data=happy))$r.squared)
vif2
vif3 = 1/(1 - summary(lm(Generosity~GDP+Freedom, data=happy))$r.squared)
vif3
plot(Corruption~GDP, data=happy)
abline(lm(Corruption~GDP, data=happy), col="red")
plot(Corruption~Freedom, data=happy)
abline(lm(Corruption~Freedom, data=happy), col="red")
plot(Corruption~Generosity, data=happy)
abline(lm(Corruption~Generosity, data=happy), col="red")

question4 = lm(sqrt(Corruption)~GDP+Generosity+Freedom, data=happy)
summary(question4)

shapiro.test(residuals(question4))
library(lmtest)
dwtest(question4, alternative=c("two.sided"))
bptest(question4)

question4.1 = lm(log(Corruption)~GDP+Generosity+Freedom, data=happy)
shapiro.test(residuals(question4.1))
dwtest(question4.1, alternative=c("two.sided"))
bptest(question4.1)

AIC(question4, question4.1)

###Question5
question5= lm(sqrt(Corruption)~Freedom+Freedom*Region, data=happy)
summary(question5)
shapiro.test(residuals(question5))
dwtest(question5, alternative=c("two.sided"))
bptest(question5)
question5.1= lm(log(Corruption)~Freedom+Freedom*Region, data=happy)
shapiro.test(residuals(question5.1))
dwtest(question5.1, alternative=c("two.sided"))
bptest(question5.1)
AIC(question5,question5.1)

###Question6
cancer = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/cancer.csv",header=TRUE, sep=",")
cor(cancer[,c("radius_mean","texture_mean","smoothness_mean")], use="na.or.complete")
vif1 = 1/(1 - summary(lm(radius_mean~texture_mean+smoothness_mean, data=cancer))$r.squared)
vif1
vif2 = 1/(1 - summary(lm(texture_mean~radius_mean+smoothness_mean, data=cancer))$r.squared)
vif2
vif3 = 1/(1 - summary(lm(smoothness_mean~texture_mean+radius_mean, data=cancer))$r.squared)
vif3
question6=glm(malignant~radius_mean+texture_mean+smoothness_mean, data=cancer, family=binomial(link="logit"))
summary(question6)
dwtest(question6, alternative=c("two.sided"))
question6.1=glm(malignant~radius_mean, data=cancer, family=binomial(link="logit"))
question6.2=glm(malignant~radius_mean+texture_mean, data=cancer, family=binomial(link="logit"))
anova(question6.1, question6.2, question6, test="Chi")
AIC(question6.1, question6.2, question6)

###Question7
install.packages("relaimpo")
library(relimp)
radiusvstexture=relimp(question6, set1=2,set=3) 
radiusvstexture
radiusvssmooth=relimp(question6, set1=2,set=4) 
radiusvssmooth
texturevssmooth=relimp(question6, set1=3,set=4) 
texturevssmooth
