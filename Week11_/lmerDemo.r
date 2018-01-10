##### read in data
ready = read.table(file="D:/Course/UM/2017_WN/NRE538_2017Fall/Week11_/lmerDemo.csv", sep=",", header=TRUE)

##### assign cruise number
ready.1719=as.data.frame(ready[which(ready[,1]==1),])
ready.1766=as.data.frame(ready[which(ready[,1]==2),])
ready.1796=as.data.frame(ready[which(ready[,1]==3),])
ready.1813=as.data.frame(ready[which(ready[,1]==4),])
ready.966=as.data.frame(ready[which(ready[,1]==5),])
ready.980=as.data.frame(ready[which(ready[,1]==6),])

##### plot growth rate versus body size #####
par(lwd=2,cex=1.5,cex.lab=1,cex.main=1)
plot(ready.1719[,3], ready.1719[,13],main = "" , xlab="log2[biomass(pg)]", ylab="log2[size-specific growth rate(d-1)]", col="black", type="p", xlim=c(5.5,18.5), ylim=c(14,22))
par(new="T")
plot(ready.1766[,3], ready.1766[,13],main = "" , xlab="", ylab="", col="green", type="p", xlim=c(5.5,18.5), ylim=c(14,22))
par(new="T")
plot(ready.1796[,3], ready.1796[,13],main = "" , xlab="", ylab="", col="red", type="p", xlim=c(5.5,18.5), ylim=c(14,22))
par(new="T")
plot(ready.1813[,3], ready.1813[,13],main = "" , xlab="", ylab="", col="orange", type="p", xlim=c(5.5,18.5), ylim=c(14,22))
par(new="T")
plot(ready.966[,3], ready.966[,13],main = "" , xlab="", ylab="", col="blue", type="p", xlim=c(5.5,18.5), ylim=c(14,22))
par(new="T")
plot(ready.980[,3], ready.980[,13],main = "" , xlab="", ylab="", col="purple", type="p", xlim=c(5.5,18.5), ylim=c(14,22))

mod1719u=lm(ready.1719[,13]~ready.1719[,3])
summary(mod1719u)
abline( mod1719u$coefficients, col="black", lwd=2, lty=2)
mod1766u=lm(ready.1766[,13]~ready.1766[,3])
summary(mod1766u)
abline( mod1766u$coefficients, col="green", lwd=2)
mod1796u=lm(ready.1796[,13]~ready.1796[,3])
summary(mod1796u)
abline( mod1796u$coefficients, col="red", lwd=2, lty=2)
mod1813u=lm(ready.1813[,13]~ready.1813[,3])
summary(mod1813u)
abline( mod1813u$coefficients, col="orange", lwd=2, lty=2)
mod966u=lm(ready.966[,13]~ready.966[,3])
summary(mod966u)
abline( mod966u$coefficients, col="blue", lwd=2)
mod980u=lm(ready.980[,13]~ready.980[,3])
summary(mod980u)
abline( mod980u$coefficients, col="purple", lwd=2)
##### plot growth rate versus body size #####

library(lme4)
library(arm)
modu.crmm1=lmer(T_loggrowth1~ size+(1+size|cr), ready, control=lmc)
summary(modu.crmm1)

se.coef(modu.crmm1)

random.effects(modu.crmm1)
se.ranef(modu.crmm1)

fixed.effects(modu.crmm1)
se.fixef(modu.crmm1)


