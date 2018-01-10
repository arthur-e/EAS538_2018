#3) Is there a significant difference in happiness (Hscore) by region (Region)? 

happy = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/happy.csv",header=TRUE, sep=",")

#happiness score = continuous
#region = categorical

#one-way ANOVA (one factor (region), many levels)

#visualize the data
boxplot(Hscore~Region, data=happy, xlab="Region", ylab="Hscore")

#CHECK FOR NORMALITY
hist(happy$Hscore)
shapiro.test(happy$Hscore)
#Ho = data is normally distributed
#p-value less than .01248--accept the Ho. 
#Passes the test, data is normall distributed

#Check qqplot
qqnorm(happy$Hscore); qqline(happy$Hscore, col="Red")
#looks good

#CHECK FOR EQUAL VARIANCE
library(car)
leveneTest(Hscore~Region, data = happy)
#Ho = variances are equal
#p-value of .5179--fail to reject the null

#run the anova
happyMOD1 = aov(Hscore~Region, data = happy)
summary(happyMOD1)

#           Df      Sum Sq    Mean Sq     F value     Pr(>F)    
#Region        3    64.37     21.456      23.62       1.28e-12 ***
#Residuals   153    138.96    0.908                     
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Based on the p-value (1.28e-12), there is a significant difference in happiness score by region.