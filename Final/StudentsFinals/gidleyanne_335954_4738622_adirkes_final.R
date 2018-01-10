library(dplyr)

flying = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/flying.csv",header=TRUE, sep=",")

fly1 = subset(flying,gender!='NA',select=c(gender,unruly_child))
fly = subset(fly1,unruly_child!='NA',select=c(gender,unruly_child))
head(fly)
tbl1 = table(fly$gender,fly$unruly_child)
tbl1
chisq.test(tbl1) # p = 0.001193

college=read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/college.csv",header=TRUE, sep=",")
head(college)
hist(college$tuition) # roughly normal
boxplot(tuition~type, data=college, xlab="Type", ylab="Tuition")
qqnorm(college$tuition) # deviates from normal at the top end
shapiro.test(college$tuition) # p = 3.193e-15
public=subset(college,type=='Public',select=tuition)
private=subset(college,type=='Private nonprofit',select=tuition)
var.test(public$tuition,private$tuition)# check variances, p < 2.2e-16, unequal variances, ratio 0.4347
t.test(public,private,alternative='two.sided', paired=FALSE) # p < 2.23e-16

happy = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/happy.csv",header=TRUE, sep=",")
head(happy)
hist(happy$Hscore)
boxplot(Hscore~Region, data=happy, xlab="Region", ylab="Hscore")
reg1 = subset(happy,Region=="AfricaMideast")
reg2 = subset(happy,Region=="AmericasCarribean")
reg3 = subset(happy,Region=="AsiaAustralia")
reg4 = subset(happy,Region=="Europe")
region.aov1 = aov(Hscore~Region, data=happy)
region.aov2.1 = lm(Hscore~Region, data=happy)
summary(region.aov2.1) # adj r2 = 0.3032, all regions significant
kruskal.test(happy$Hscore, happy$Region)
TukeyHSD(region.aov1)

pairs(happy[, c("Hscore","GDP","Family","Life","Freedom","Corruption","Generosity")])
cor(happy[, c("Hscore","GDP","Family","Life","Freedom","Corruption","Generosity")],use="na.or.complete")
# freedom, hscore, generosity
plot(happy$Corruption)
hist(happy$Corruption)
plot(happy$Freedom, happy$Corruption)
plot(happy$Hscore, happy$Corruption)
plot(happy$Generosity, happy$Corruption)
# no nonlinear relationships found (appear linear)
# disclaimer: explanatory vars are correlated with each other
summary(lm(Corruption~Freedom+Hscore+Generosity, data=happy))


# this time with transformed dependent variable
corruptexp = log(happy$Corruption)
plot(corruptexp)
hist(corruptexp)
fit4 = lm(corruptexp~happy$Freedom+happy$Hscore+happy$Generosity)
summary(fit4)
# Freedom is significant
plot(residuals(fit4))
shapiro.test(residuals(fit4))


# interaction with region: freedom
fit5=lm(corruptexp~happy$Region*happy$Freedom)
summary(fit5)
plot(residuals(fit5))
qqnorm(residuals(fit5))# homooscedasticity!!!
shapiro.test(residuals(fit5))


cancer = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/cancer.csv",header=TRUE, sep=",")
head(cancer)
cor(cancer[, c("malignant","radius_mean","texture_mean","perimeter_mean","area_mean","smoothness_mean")],use="na.or.complete")
rad = cancer$radius_mean
per = cancer$perimeter_mean
area = cancer$area_mean
fit6 = glm(cancer$malignant~rad+per+area, family=binomial(link="logit"))
summary(fit6)
plot(residuals(fit6))
qqnorm(residuals(fit6))
shapiro.test(residuals(fit6))
