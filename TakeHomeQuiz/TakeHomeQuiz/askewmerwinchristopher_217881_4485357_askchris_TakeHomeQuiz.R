# Take-Home Quiz
# Christopher Askew-Merwin
# 3/26/2017

# Energy data for question 1-4
edata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/energy_data.csv", 
                   sep=",", fill=TRUE, header=TRUE)

edata$PerCapitaEnergy = (edata$TotalEnergy/edata$Population)
edata$PerCapitaCoal = (edata$TotalCoal/edata$Population)
edata$PerCapitaGDP = (edata$TotalGDP/edata$Population)

coast = subset(edata, Coast == "1")
inland = subset(edata, Coast == "0")

#__________________________________________________________________________
# Question 1

boxplot(edata$PerCapitaEnergy ~ edata$Coast, main="Per Capita Energy Consumption by Coastal Status")

shapiro.test(coast$PerCapitaEnergy)
# Shapiro-Wilk normality test
#
# data:  coast$PerCapitaEnergy
# W = 0.66467, p-value = 5.037e-06

shapiro.test(inland$PerCapitaEnergy)
# Shapiro-Wilk normality test
#
# data:  inland$PerCapitaEnergy
# W = 0.77249, p-value = 3.627e-05

var.test(coast$PerCapitaEnergy, inland$PerCapitaEnergy)
# F test to compare two variances
#
# data:  coast$PerCapitaEnergy and inland$PerCapitaEnergy
# F = 1.3021, num df = 22, denom df = 27, p-value = 0.5098
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#   0.5861139 2.9930256
# sample estimates:
# ratio of variances 
#          1.302084 

wilcox.test(coast$PerCapitaEnergy,inland$PerCapitaEnergy, paired=FALSE)
# Wilcoxon rank sum test
#
# data:  coast$PerCapitaEnergy and inland$PerCapitaEnergy
# W = 184, p-value = 0.008417
# alternative hypothesis: true location shift is not equal to 0

#__________________________________________________________________________
# Question 2

boxplot(edata$PerCapitaCoal ~ edata$Coast, main="Per Capita Coal Consumption by Coastal Status")

shapiro.test(coast$PerCapitaCoal)
# Shapiro-Wilk normality test
#
# data:  coast$PerCapitaCoal
# W = 0.82584, p-value = 0.001023

shapiro.test(inland$PerCapitaCoal)
# Shapiro-Wilk normality test
#
# data:  inland$PerCapitaCoal
# W = 0.651, p-value = 6.211e-07

var.test(coast$PerCapitaCoal, inland$PerCapitaCoal)
# F test to compare two variances
#
# data:  coast$PerCapitaCoal and inland$PerCapitaCoal
# F = 0.025045, num df = 22, denom df = 27, p-value = 5.995e-13
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#  0.01127368 0.05756970
# sample estimates:
#  ratio of variances 
#          0.02504508 

wilcox.test(coast$PerCapitaCoal,inland$PerCapitaCoal, paired=FALSE)
# Wilcoxon rank sum test with continuity correction
#
# data:  coast$PerCapitaCoal and inland$PerCapitaCoal
# W = 96.5, p-value = 2.051e-05
# alternative hypothesis: true location shift is not equal to 0

#________________________________________________________________________
# Question 3

south = subset(edata, Region == "South")
west = subset(edata, Region == "West")
midwest = subset(edata, Region == "Midwest")
east = subset(edata, Region == "East")

boxplot(edata$PerCapitaCoal ~ edata$Region, main="Per Capita Coal Consumption by Region")

library(car)
leveneTest(PerCapitaCoal ~ Region, data=edata)
#Levene's Test for Homogeneity of Variance (center = median)
#      Df F value Pr(>F)
#group  3  0.7635 0.5202
#      47  

shapiro.test(east$PerCapitaCoal)
# Shapiro-Wilk normality test
#
# data:  east$PerCapitaCoal
# W = 0.49468, p-value = 5.535e-06
shapiro.test(midwest$PerCapitaCoal)
# Shapiro-Wilk normality test
#
# data:  midwest$PerCapitaCoal
# W = 0.62528, p-value = 0.0001758
shapiro.test(south$PerCapitaCoal)
# Shapiro-Wilk normality test
#
# data:  south$PerCapitaCoal
# W = 0.67217, p-value = 5.689e-05
shapiro.test(west$PerCapitaCoal)
# Shapiro-Wilk normality test
#
# data:  west$PerCapitaCoal
# W = 0.50477, p-value = 1.103e-05

kruskal.test(edata$PerCapitaCoal, edata$Region, paired=FALSE)
#Kruskal-Wallis rank sum test
#
#data:  edata$PerCapitaCoal and edata$Region
#Kruskal-Wallis chi-squared = 18.097, df = 3, p-value = 0.00042

#_________________________________________________________________________
# Question 4

plot(edata$PerCapitaGDP,edata$PerCapitaCoal)
cor(edata[c("PerCapitaGDP","PerCapitaCoal")], use="na.or.complete")
#                PerCapitaGDP PerCapitaCoal
#PerCapitaGDP    1.00000000    0.03598182
#PerCapitaCoal   0.03598182    1.00000000

#__________________________________________________________________________
#__________________________________________________________________________

# Housing data for question 5-9
hdata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/housingdata.csv", 
                   sep=",", fill=TRUE, header=TRUE)

#__________________________________________________________________________
# Question 5

cor(hdata[c("crim","age","lstat")], use="na.or.complete")
#            crim       age     lstat
# crim  1.0000000 0.4476638 0.4247886
# age   0.4476638 1.0000000 0.5732663
# lstat 0.4247886 0.5732663 1.0000000

library(car)
vif(lm(medv~crim+age+lstat, data=hdata))
#     crim      age    lstat 
# 1.320170 1.611567 1.572323

#__________________________________________________________________________
# Question 6

plot(hdata$crim, hdata$medv)
plot(hdata$age, hdata$medv)
plot(hdata$lstat, hdata$medv)

#__________________________________________________________________________
# Question 7

qqnorm(hdata$crim)
qqnorm(hdata$age)
qqnorm(hdata$lstat)


summary(lm(medv~crim+age+lstat, data=hdata))
# Call:
# lm(formula = medv ~ crim + age + lstat, data = hdata)
#
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -16.773  -4.060  -1.420   2.151  23.177 
#
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 33.44194    0.78362  42.676  < 2e-16 ***
# crim        -0.08460    0.13333  -0.634 0.526080    
# age          0.05125    0.01307   3.921 0.000102 ***
# lstat       -1.13017    0.05899 -19.158  < 2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
# Residual standard error: 6.151 on 448 degrees of freedom
# Multiple R-squared:  0.5156,	Adjusted R-squared:  0.5124 
# F-statistic:   159 on 3 and 448 DF,  p-value: < 2.2e-16












