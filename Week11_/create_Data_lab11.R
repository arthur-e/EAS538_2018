networking=sample(c(0:10),100,replace=TRUE)
networking=sort(networking)
degree=rep('PhD',52)
degree=c(degree,rep('Business',48))

wages=corfun(.7,networking[1:52],mean=100000,sd=10000)
wages=c(wages,corfun(.5,gradyrs[53:100],mean=200000,sd=10000))

df=cbind(networking,wages,degree)
df=as.data.frame(df)
for(i in c(1:2)){df[,i]=as.numeric(as.character(df[,i]))}

summary(lm(wages~networking,data=df))
summary(lm(wages~networking+degree,data=df))
write.csv(df,'/Users/mehajain/Desktop/networking.csv',row.names=FALSE)

irrigation=sample(1:6,100,replace=TRUE)
irrigation=sort(irrigation)
state = c(rep('Bihar',32),rep('Punjab',4),rep('Bihar',14),rep('Punjab',48),rep('Bihar',2))
yield = corfun(.9,irrigation,mean=4000,sd=4000)

irrdata=cbind(irrigation,yield,state)
irrdata=as.data.frame(irrdata)
for(i in c(1:2)){irrdata[,i]=as.numeric(as.character(irrdata[,i]))}
write.csv(irrdata,'/Users/mehajain/Desktop/irrdata.csv',row.names=FALSE)

diversity = sample(1:20,100,replace=TRUE)
lake=corfun(.5,diversity,mean=5,sd=20)
lake=round(lake)
table(lake)
lake = lake + 1
lake = paste('lake',lake,sep='')
table(lake)
prod = corfun(0.7,diversity,mean=1000,sd=500)
temp = corfun(0.3,prod,mean=17,sd=3)
rain = corfun(0.4,prod,mean=150,sd=100)
sun = corfun(0.5,prod,mean=4,sd=1)
lakedf = cbind(lake,diversity,prod,temp,rain,sun)  
lakedf = as.data.frame(lakedf)
for(i in c(2:6)){lakedf[,i]=as.numeric(as.character(lakedf[,i]))}
head(lakedf)
write.csv(lakedf,'/Users/mehajain/Desktop/lakedf.csv',row.names=FALSE)

# corfun = function(corr,xvar,mean,sd){
#   n     <- length(xvar)                   # length of vector
#   rho   <- corr                     # desired correlation = cos(angle)
#   theta <- acos(rho)             # corresponding angle
#   x1    <- xvar                   # fixed given data
#   x2    <- rnorm(n, 2, 0.5)      # new random data
#   X     <- cbind(x1, x2)         # matrix
#   Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)
#   Id   <- diag(n)                               # identity matrix
#   Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
#   P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
#   x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
#   Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
#   Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1
#   final <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
#   orig = final*sd + mean
#   return(orig)}
