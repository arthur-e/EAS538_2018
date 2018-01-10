library(RCurl)
library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(lmtest)
##Define panel functions for correlation analysis
"panel.lm" <-  function (x, y, pch = par("pch"),  col.lm = "red", ...) 
{
  ymin <- min(y)
  ymax <- max(y)
  xmin <- min(x)
  xmax <- max(x)
  ylim <- c(min(ymin,xmin),max(ymax,xmax))
  xlim <- ylim
  points(x, y, pch = pch,ylim = ylim, xlim= xlim,...)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) 
    abline(lm(y[ok]~ x[ok]), 
           col = col.lm, ...)
}
"panel.cor" <- function(x, y, digits=2, prefix="", cex.cor,...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y,use="pairwise")
  txt <- format(c(round(r,digits), 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) {cex <- 0.8/strwidth(txt)} else {cex <- cex.cor}
  text(0.5, 0.5, txt,cex=cex)
}


### Question 1
flying = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/flying.csv",header=TRUE, sep=",")
flying
ctable<- CrossTable(flying$gender, flying$unruly_child, chisq= TRUE)
x<-rchisq(100,5)
curve(dchisq(x,df=5), xlim=c(0,20), main="Chi-Square Distribution")
abline(v=13.463, lwd=2)
#Result: There is a significant association between gender and if people think its rude to bring an unruly child on the plane. According to the data, men tend to think that bringing an unruly child is more rude. 

### Question 2
college = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/college.csv",header=TRUE, sep=",")
college
shapiro.test(college$tuition)
qqnorm(college$tuition)
qqline(college$tuition)
var.test(college$type, college$tuition)
t.test(college$tuition~college$type, alternative="greater", paired=FALSE, var.equal= TRUE )
#Result: There is a difference in the tuition paid by the type of institution (p-value<2.2e-16). Average tution rates in private non- profits is greater. 

### Question 3
happy = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/happy.csv",header=TRUE, sep=",")
happy
var.test(happy$Region, happy$Hscore)
qqnorm(happy$Hscore)
qqline(happy$Hscore)
shapiro.test(happy$Hscore)
anova=aov(Hscore~Region, data= happy)
summary(anova)
#Result: there is a significant difference in happiness by region (p-value= 1.28e-12)

### Question 4
pairs(happy, main="Happiness data")
cor.test(happy$Corruption, happy$Hrank)
cor.test(happy$Corruption, happy$Hscore)
cor.test(happy$Corruption, happy$GDP)
cor.test(happy$Corruption, happy$Family)
cor.test(happy$Corruption, happy$Life)
cor.test(happy$Corruption, happy$Freedom)
cor.test(happy$Corruption, happy$Generosity)

### Question 5
qqnorm(happy$Corruption)
qqline(happy$Corruption)
hist(happy$Corruption)
mod<-lm(Corruption~Region*Hscore, data=happy)
summary(mod)
par(mfrow=c(2,2))
plot(mod)
# the model shows a significant interaction between log odds of corruption and a region's Hscore, but is not the best model to predict corruption. 

### Question 6
cancer = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/cancer.csv",header=TRUE, sep=",")
cancer
pairs(cancer, main="Breast- cancer data")
cor.test(cancer$malignant, cancer$radius_mean)
cor.test(cancer$malignant, cancer$texture_mean)
cor.test(cancer$malignant, cancer$perimeter_mean)
cor.test(cancer$malignant, cancer$area_mean)
cor.test(cancer$malignant, cancer$smoothness_mean)
# All parameters have significant association with malignancy of tumors. 

### Question 7
qqnorm(cancer$malignant)
qqline(cancer$malignant)
hist(cancer$malignant)
mod1.g=glm(malignant~area_mean+texture_mean+smoothness_mean, data=cancer, family=binomial(link="logit"))
mod2.g=glm(malignant~area_mean+texture_mean, data=cancer, family=binomial(link="logit"))
mod3.g=glm(malignant~area_mean, data=cancer, family=binomial(link="logit"))
AIC(mod1.g, mod2.g, mod3.g)
anova(mod1.g, mod2.g, mod3.g)
summary(mod1.g)
#Model 1 with all three parameters can explain the log odds of a malignant tumor occuring. 