library(RCurl)
guns=read.csv(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_Lab2/guns.csv"), sep=",", header=T, comment.char="#") 
guns.MN = subset(guns, state=="Minnesota")
guns.WA = subset(guns, state=="Washington")
dif.g=c()
for (i in 1:10000){
  dif.g[i]=mean(sample(guns.WA[,"murder"], nrow(guns.WA), replace=TRUE))-mean(sample(guns.MN[,"murder"], nrow(guns.WA), replace=TRUE))
}
dif.g.mean=mean(dif.g)
dif.g.sd=sd(dif.g)
CI95.g.lo=sort(dif.g) [10000*0.025]
CI95.g.lo=sort(dif.g) [10000*0.925]
##what is the likelihood dif.g contains zero?
dif.g.se=dif.g.sd/(sqrt(10000))
t.val=dif.g.mean-0/dif.g.se

dif.g.mean+t.val*dif.g.se
##or
dif.g.mean-t.val*dif.g.se

qt(0.95,df=9,999)

##it is outside of the distribution

