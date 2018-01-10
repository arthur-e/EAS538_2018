# Energy data for question 1-4
edata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/energy_data.csv", 
                   sep=",", fill=TRUE, header=TRUE)

########### Problem 1 #############

coast=subset((edata$TotalEnergy/edata$Population), edata$Coast==1)
not_coast=subset((edata$TotalEnergy/edata$Population), edata$Coast==0)

summary(coast)
summary(not_coast)

plot(coast)
plot(not_coast)



##Part b--creating a visual

hist(coast, breaks=30, col="light blue", xlim=c(0, 1), ylim=c(0,5), xlab="TotalEnergy", main="")
abline(v=mean(coast), col="blue")
par(new=TRUE)
hist(not_coast, breaks=30, col="light green", xlim=c(0, 1), ylim=c(0,5), xlab="TotalEnergy", main="")
abline(v=mean(not_coast), col="dark green")
legend("topright", legend = c("Coast Mean", "Not Coast Mean"),text.col=c("blue", "dark green"))



##Part c--testing assumptions

#Normality
shapiro.test(edata$TotalEnergy/edata$Population)

qqnorm(edata$TotalEnergy/edata$Population)
qqline(edata$TotalEnergy/edata$Population, col="red")


#Variance
var.test(coast, not_coast)


##Part d--run the t-test
t.test(coast, not_coast, paired=FALSE)


########### Problem 2 #############

coast_coal=subset((edata$TotalCoal/edata$Population), edata$Coast==1)
not_coast_coal=subset((edata$TotalCoal/edata$Population), edata$Coast==0)

summary(coast_coal)
summary(not_coast_coal)

plot(coast)
plot(not_coast)


##Part b--creating a visual

hist(coast_coal, breaks=5, col="light blue", xlim=c(0, 1), ylim=c(0,15), xlab="TotalCoal", main="")
abline(v=mean(coast_coal), col="blue")
par(new=TRUE)
hist(not_coast_coal, breaks=30, col="light green", xlim=c(0, 1), ylim=c(0,15), xlab="TotalCoal", main="")
abline(v=mean(not_coast_coal), col="dark green")
legend("topright", legend = c("Coast Mean", "Not Coast Mean"),text.col=c("blue", "dark green"))


##Part c--testing assumptions


#Normality
shapiro.test(edata$TotalCoal/edata$Population)

qqnorm(edata$TotalCoal/edata$Population)
qqline(edata$TotalCoal/edata$Population, col="red")


#Variance
var.test(coast_coal, not_coast_coal)


##Part d--run the t-test
t.test(coast_coal, not_coast_coal, paired=FALSE, var.equal=FALSE)



############ Problem 3 ##############

##Part b--creating a visual

CCPC=edata$TotalCoal/edata$Population

CCPC_East=subset((edata$TotalCoal/edata$Population), edata$Region=="East")
CCPC_Midwest=subset((edata$TotalCoal/edata$Population), edata$Region=="Midwest")
CCPC_South=subset((edata$TotalCoal/edata$Population), edata$Region=="South")
CCPC_West=subset((edata$TotalCoal/edata$Population), edata$Region=="West")

summary(CCPC_East)
summary(CCPC_Midwest)
summary(CCPC_South)
summary(CCPC_West)

plot(CCPC_East)
plot(CCPC_Midwest)
plot(CCPC_South)
plot(CCPC_West)

boxplot(CCPC~edata$Region, xlab="Region", ylab="CPPC (effect size)")


##Part c--testing asummptions

#Test for normality
shapiro.test(CCPC)
qqnorm(CCPC)
qqline(CCPC, col="red")


#Variance
library(Rcmdr)
leveneTest(CCPC, edata$Region)


##part d -- run the test
aov=lm(CCPC~edata$Region-1)
summary(aov)

aov.1=aov(CCPC~edata$Region)
summary(aov.1)

TukeyHSD(aov.1)



########## Problem 4 ############

GDPPC=(edata$TotalGDP/edata$Population)

cor(CCPC,GDPPC)

plot(CCPC,GDPPC)


#################################### Part 2 ################################


# Housing data for question 5-9
hdata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/housingdata.csv", 
                   sep=",", fill=TRUE, header=TRUE)

######### Problem 5 ############

cor(hdata[,c(1,8,13)], use="na.or.complete")

pairs(hdata[,c(1,8,13)])

#VIF for crim
vif.crim = 1/(1 - summary(lm(medv~crim, dat=hdata))$r.squared)
vif.crim

vif.dis = 1/(1 - summary(lm(medv~dis, dat=hdata))$r.squared)
vif.dis

vif.lstat = 1/(1 - summary(lm(medv~lstat, dat=hdata))$r.squared)
vif.lstat

vif.medv = 1/(1 - summary(lm(medv~crim + dis + lstat, dat=hdata))$r.squared)
vif.medv

######### Problem 6 ###########
plot(medv~crim, dat=hdata)
abline(lm(medv~crim, dat=hdata), col="red")

plot(medv~dis, dat=hdata)
abline(lm(medv~dis, dat=hdata), col="red")

plot(medv~lstat, dat=hdata)
abline(lm(medv~lstat, dat=hdata), col="red")

pairs(hdata[, c("crim", "dis", "lstat")])


######### Problem 7 ############

#Assumptions#
medv_lm = lm(medv~crim+dis+lstat, dat=hdata)
summary(medv_lm)

#Test for normality

qqnorm(residuals(medv_lm))
qqline(residuals(medv_lm), col="red")

shapiro.test(residuals(medv_lm))
plot(medv_lm)

shapiro.test(hdata$crim)
shapiro.test(hdata$dis)
shapiro.test(hdata$lstat)

#try out log transform
crim_log=log(hdata$crim)
plot(crim_log)
plot(hdata$crim)

dis_log=log(hdata$dis)
plot(dis_log)
plot(hdata$crim)

lstat_log=log(hdata$lstat)
plot(lstat_log)
plot(hdata$lstat)

shapiro.test(crim_log)
shapiro.test(dis_log)
shapiro.test(lstat_log)

medv_log=log(hdata$medv)
plot(medv_log)
plot(hdata$medv)

medv_lm_log=lm(medv_log~crim_log+dis_log+lstat_log)
summary(medv_lm_log)

shapiro.test(residuals(medv_lm_log))
plot(medv_lm_log)


#Test for residual independency
plot(residuals(medv_lm_log))

library(lmtest)

dwtest(medv_lm, alternative=c("two.sided"))

dwtest(medv_lm_log, alternative=c("two.sided"))

#Homoscedasticity

plot(residuals(medv_lm_log))

library(lmtest)
bptest(medv_lm)
bptest(medv_lm_log)

medv_lm_log=lm(medv_log~crim_log+dis_log+lstat_log)
summary(medv_lm_log)

exp(4.394248)
exp(-0.021527)
exp(-0.120698)
exp(-0.507930)
exp(-0.046768)

medv_lm_log_2=lm(medv_log~dis_log+crim_log*lstat_log)
summary(medv_lm_log_2)


############  Problem 9 ############

medv_glm_guassian=glm(hdata$medv~hdata$crim + hdata$dis + hdata$lstat, family='gaussian')
summary(medv_glm_guassian)

AIC(medv_glm_guassian)
AIC(medv_lm_log)
AIC(medv_lm)







