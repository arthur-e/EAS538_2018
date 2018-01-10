library(dplyr)
library(plyr)
library(ggplot2)
library(lmtest)

flying = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/flying.csv",header=TRUE, sep=",")
college = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/college.csv",header=TRUE, sep=",")
happy = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/happy.csv",header=TRUE, sep=",")
cancer = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/cancer.csv",header=TRUE, sep=",")

###1. Is there a significant difference between gender and whether people think it's rude to bring
#an unruly child on the plane?
flying2 = flying %>% subset(gender!="NA",) %>% subset(unruly_child!="NA")
duplications = count(duplicated(flying2$respondent_id))
tbl = table(flying2$unruly_child, flying2$gender)
chisq.test(tbl) #p=0.001193

###2. Is there a significant difference in tuition by type of institution?
ggplot(college, aes(x=type, y=tuition, fill=type)) + geom_boxplot() + labs(y="Tuition",x="Institution type", title = "Tuition by institution type")
hist(college$tuition)
qqnorm(college[,"tuition"])
qqline(college[,"tuition"], col="red")
shapiro.test(college$tuition)
length(college$tuition)
public = college %>% subset(type=="Public",)
private = college %>% subset(type=="Private nonprofit",)
var.test(public[,"tuition"], private[,"tuition"]) #p<0.001
t.test(public$tuition,private$tuition,paired=FALSE,var.equal=FALSE) #p<0.001

###3. Is there a significant difference in happiness by region? 
head(happy)
ggplot(happy, aes(x=Region, y=Hscore)) + geom_boxplot() + labs(y="Happiness score",x="Region", title = "Happiness by region")
#check for normality
qqnorm(happy$Hscore)
qqline(happy$Hscore,col="red")
shapiro.test(happy$Hscore)
#subset regions
Africa = happy %>% subset(Region=="AfricaMideast")
America = happy %>% subset(Region == "AmericasCarribean")
Asia = happy %>% subset(Region == "AsiaAustralia")
Europe = happy %>% subset(Region == "Europe")
#check equal variance
var.test(Africa$Hscore,America$Hscore)
var.test(Africa$Hscore,Asia$Hscore)
var.test(Africa$Hscore,Europe$Hscore)
var.test(America$Hscore,Asia$Hscore)
var.test(America$Hscore,Europe$Hscore)
var.test(Asia$Hscore,Europe$Hscore)
#run ANOVA
mod3=aov(Hscore~Region,data=happy)
summary(mod3)

###4. What factors are significantly associated with a country's corruption levels?
hist(happy$Corruption)
hist(log(happy$Corruption))
happy2 = happy %>% mutate(Corruption=log(Corruption))
qqnorm(happy2$Corruption,main="QQ plot of log(Corruption)")
qqline(happy2$Corruption,col="red")
shapiro.test(happy2$Corruption)
happy2 = happy %>% subset(,select=c("Corruption","Hscore","Freedom","Generosity"))
pairs(happy2)
cor(happy2)
mod4=lm(Corruption~Hscore+Freedom+Generosity,data=happy2)
summary(mod4)
plot(mod4)
plot(mod4$residuals,main="Residuals of linear model")
dwtest(mod4, alternative=c("two.sided"))
bptest(mod4)
shapiro.test(mod4$residuals)

###5. Interact one significant factor from above and interact with Region to predict Corruption.
#Include one continuous independent variable and its interaction with region.
#Does the influence of the continuous variable differ by region?
hist(log(happy$Corruption))
happy5=happy %>% mutate(Corruption=log(Corruption)) %>% subset(,select=c("Corruption","Freedom","Region"))
ggplot(data=happy5, mapping=aes(x=Freedom, y=Corruption, color=factor(Region)))+geom_point()+labs(title="Effect of Freedom on Corruption, by region")+ylab("log(Corruption)")
shapiro.test(happy5$Corruption) #p = 0.09 -> normally distributed
mod5 = lm(Corruption~Freedom+Freedom:Region,data=happy5)
summary(mod5)
plot(mod5$residuals,main="Residuals of ANCOVA model")
dwtest(mod5,alternative=c("two.sided"))
bptest(mod5)
qqnorm(mod5$residuals)
qqline(mod5$residuals,col="red")
shapiro.test(mod5$residuals)



###6. Which factors are significantly associated with malignancy of breast tumors? 
#3 continuous independent variables
cor(cancer)

mod6=glm(malignant~texture_mean+area_mean+smoothness_mean,data=cancer,family=binomial(link="logit"))
summary(mod6)

mod6.0 = glm(malignant~1, data=cancer, family=binomial(link="logit"))
summary(mod6.0)
anova(mod6,mod6.0,test="Chi")
AIC(mod6,mod6.0)

###7. which independent variables are most important in explaining malignancy?
t=mean(cancer$texture_mean)
ts=sd(cancer$texture_mean)
a=mean(cancer$area_mean)
as=sd(cancer$area_mean)
s=mean(cancer$smoothness_mean)
ss=sd(cancer$smoothness_mean)
cancer2 = cancer %>% mutate(texture_mean=((texture_mean-t)/ts)) %>% mutate(area_mean=((area_mean-a)/as)) %>% mutate(smoothness_mean=((smoothness_mean-s)/ss))

mod7=glm(malignant~texture_mean+area_mean+smoothness_mean,data=cancer2,family=binomial(link="logit"))
summary(mod7)
