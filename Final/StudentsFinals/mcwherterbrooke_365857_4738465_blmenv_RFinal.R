#flying 

fly <-read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/flying.csv",header=TRUE, sep=",")

#college 

college <- read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/college.csv",header=TRUE, sep=",")

#happy
happy<- read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/happy.csv",header=TRUE, sep=",")

#Cancer
cancer<- read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/cancer.csv",header=TRUE, sep=",")

##############################################################
library(gmodels)
library(lattice)
library(rcompanion)
library(Matrix)
library(fifer)
library(dplyr)
library(RCurl)
library(ggplot2)
library(gplots)
library(corrplot)
library(vcd)
library(userfriendlyscience)
library(car)
library(ggplot2)
library(GGally)
library(mvnormtest)
library(lmtest)
library(sandwich)
library(plyr)
library(robustbase)
###############################################################3
  #Q1 - association between gender and perception of unruly children?
Fly2 <- subset(fly, select=c(gender, unruly_child))
summary(Fly2)
Fly3<-na.omit(Fly2)
fly4 <- table(Fly3$gender,Fly3$unruly_child)
summary(Fly3)
xtab<-xtabs(~gender+unruly_child, data=Fly3)

flychi <- chisq.test(fly4)
flychi
 #Pearson's Chi-squared test
 #data:  fly4
 #X-squared = 13.463, df = 2, p-value = 0.001193
################################################################################3

#Q2 Which gender thinks its more rude ("Very")

#check contingency table and chisq test
Fly6<- CrossTable(Fly3$unruly_child, Fly3$gender,chisq = TRUE)
# check indpendence
fisher.test(fly4) 
  # p = 0.001166

#Pairwise Post Hoc - Crosstabulation reviewing adjust standard residuals
pairwiseNominalIndependence(fly4,
                            fisher = FALSE,
                            gtest  = FALSE,
                            chisq  = TRUE,
                            method='fdr')


str(Fly3)
flychi$residuals
#       Very
#Female -1.8109908
#Male    1.9013199

str(Fly3)

-1.810*-1.810
#3.271  p value= 0.1948
1.90*1.90
#3.61   p value = 0.1645

###########################################################################
# Question 2 : Difference between tuition (num) and type of institution(cat)

boxplot(T_tuk~type, data=college)
#large error bars for private, but with the exception of some large outliers in the public
# appears significant

Tuit.Public = subset(college, type=="Public")
Tuit.Private =subset(college, type=="Private nonprofit")

#Test for normality
shapiro.test(Tuit.Private[,"tuition"]) #p-value= 1.147e-06
shapiro.test(Tuit.Public[,"tuition"]) # p-value= 2.58e-10
shapiro.test(college[,"T_tuk"]) # p-value= 3.193e-15

qqplot(college$type, college$tuition, plot.it = TRUE, xlab = deparse(""),
       ylab = "")
qqnorm(college$tuition);qqline(college$tuition, col="Red")

#Can reject the null that the data comes from a normal distribution

#transforming data

college$T_tuk =transformTukey(college$tuition,
                 plotit=FALSE)
plotNormalHistogram(T_tuk)

boxplot(tuition ~ type,
        data = college,
        ylab="Tukey-transformed Turbidity",
        xlab="Location")

#Test for variance

var.test(Tuit.Public[,"tuition"], Tuit.Private[,"tuition"])
var.test(Tuit.Public[,"T_tuk"], Tuit.Private[,"T_tuk"])

# p-value = 2.2e-16, F=0.43469  -> 3.687e-09, F=0.62526
# reject the null of equal variance

#Non-parametric Welch's t-test

t.test(Tuit.Public[,"T_tuk"], Tuit.Private[,"T_tuk"], paired=FALSE)
#p-value = 2.2e-16


#Post Hoc Test - Games Howell

oneway<-oneway(college$type, y=college$T_tuk, posthoc = 'games-howell')
oneway

#                          diff  ci.lo  ci.hi    t      df p p.adjusted
#Public-Private nonprofit -16.46 -17.95 -14.97 21.7 1318.06 0      <.001

TukeyHSD(aov(T_tuk~type, data= college))

########################################################################################
#Q3 differences in happiness by region
head(happy)
pairs (happy, main="happy data")

#visualize
boxplot(Hscore~Region, data=happy)

#Test for normality
qqplot(happy$Region, happy$Hscore, plot.it = TRUE, xlab = deparse(""),
       ylab = "")
qqnorm(happy$Hscore);qqline(happy$Hscore, col="Red")
shapiro.test (happy[,"Hscore"])
# p=0.01248, fail to reject the null that the data is normally distributed

#Test for Homogeneity of variances
leveneTest(Hscore~Region, data=happy)
    #F Value= 0.7605 and p value= 0.5179
    # Fail to reject the null that the group variances are equal

#TEST
Happy.mod<-lm(Hscore ~ Region, happy)
anova(Happy.mod)
# p=1.279e-12

#Ad hoc- 
TukeyHSD(Happy.mod)

#####################################################################################3
#Q4 factors assoicated with a countries corruption levels
#chosen factors; Hrank, Family, Freedom

Happynew <-happy[1:157, 3:9]
Happynew <- happy[c(-1,-2,-4,-5,-7)]

#Test
lm_fit<-lm(Corruption~ Hrank+ Family+ Freedom, data=happy)
summary(lm_fit)

#Assumptions

#Linear Relationsip
crPlots(happy, columns=c("Corruption","Hrank", "Family","Freedom"))


#Multivariate normality

ggpairs(happy, columns=c("Corruption","Hrank", "Family","Freedom"),
        diag=list
        (continuous="density", discrete="bar"), axisLabels="show")
roystonTest(Happynew)
#pvalue = 2.864516e-09, H=39
#Data are non multivariate normal

shapiro.test (happy[,"Hrank"]) #p 5.33e-05
shapiro.test (happy[,"Family"]) # p= 1.263e-06
shapiro.test (happy[,"Corruption"])# p =5.128e-11
shapiro.test (happy[,"Freedom"]) # p=0.0002907


#Multicollinearity
vif(lm_fit)


#correlation
pairs(happy[, c("Corruption","Hrank", "Family","Freedom")])
cor(happy[, c("Corruption","Hrank", "Family","Freedom")], use="na.or.complete")

#Homoscedasticity
bp <-bptest(lm_fit)
bp # p value=0.002892
#data is heteroskedastistic


plot(lm_fit)

#Check Model Fit
lm_fit<-lm(Corruption~ Hrank+ Family+ Freedom, data=happy)
summary(lm_fit)

lm_fit2<-lm(Corruption~ Family+ Freedom, data=happy)
summary(lm_fit2)
bp2<-bptest(lm_fit2)
bp2 = # P = 5.388e-05
  
lm_fit3<-lm(Corruption~ Hrank+ Freedom, data=happy)
summary(lm_fit3)
bp3<-bptest(lm_fit3)
bp3 = #P=0.0008721
  
lm_fit4<-lm(Corruption~ Hrank+ Family, data=happy)
summary(lm_fit4)
bp4<-bptest(lm_fit4)
bp4 # p =0.09519

anova(lm_fit, lm_fit2, lm_fit3, lm_fit4)
#########################################################
#Q5: Choose a significant factor (Freedom) and interact it with Region to predict Corruption

NewMod<-lm(Corruption~Freedom*Region, data=happy)
summary(NewMod)

Newmod2<-lm(Corruption~Region, data=happy)
summary(Newmod2)

AIC(NewMod, Newmod2)
###############################################################
#Q6 factors associated with breast cancer and tumor malignancy

head(cancer)
#VIF
vif()

#correlation
pairs(cancer[, c("malignant","radius_mean", "perimeter_mean","smoothness_mean")])
ggpairs(cancer, columns=c("malignant","radius_mean", "perimeter_mean","smoothness_mean"),diag=list
  (continuous="densityDiag", discrete="barDiag"), axisLabels="show")

#multicol
lm.modC <-lm(malignant~radius_mean + perimeter_mean + smoothness_mean, data=cancer)
summary(lm.modC)
vif(lm.modC)

#normality
roystonTest(cancer)

#homoscedasticity
bptest(lm.modC)

lm.modc1 = glm(malignant~radius_mean + perimeter_mean+ smoothness_mean, data=cancer,family=binomial(link="logit"))
summary(lm.modc2)

lm.modc2 = glm(malignant~ perimeter_mean+ smoothness_mean, data=cancer,family=binomial(link="logit"))
summary(lm.modc2)

lm.modc3 = glm(malignant~radius_mean + smoothness_mean, data=cancer,family=binomial(link="logit"))
summary(lm.modc2)

lm.modc4 = glm(malignant~radius_mean + perimeter_mean, data=cancer,family=binomial(link="logit"))
summary(lm.modc2)

anova(lm.modC,lm.modc2,lm.modc3,lm.modc4)

lm.modc5=lm (malignant~perimeter_mean, data=cancer)
summary (lm.modc5)

lm.modc6=lm(malignant~smoothness_mean, data=cancer)
summary(lm.modc6)

AIC(lm.modc5, lm.modc6)
