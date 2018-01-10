#Exercise 1
#One-tailed t-test, H0:if prisoners rate is significantly lower than 200 in Michigan. Alternative hypothesis: prisoners rate is significantly higher or equal to 200 in Michigan
guns=read.csv("/Users/jacaranda/Desktop/NRE_538/Lab/Week4_Assignment3/guns_NRE538.csv",sep=",",header = T,comment.char="#")
pri.MI=subset(guns,state=="Michigan")
t.test(pri.MI[,"prisoners"],mu=200,alternative = "greater")
hist(pri.MI[,"prisoners"],breaks=20)
abline(v=mean(pri.MI[,"prisoners"]), col="blue")
abline(v=200, col="dark green")
legend("topright", legend = c("sample mean", "specified value=200"),text.col=c("blue", "dark green"))
#p-value=0.00176, reject the null hypothesis, so the prisoners rate is significantly higher or equal to 200 in Michigan 

#Two-tailed t-test; null hypothesis: prisoners rate is equal to 300 in Michigan; Alternative hypothsis: prisoners rate is not equal to 300 in Michigan
guns=read.csv("/Users/jacaranda/Desktop/NRE_538/Lab/Week4_Assignment3/guns_NRE538.csv",sep=",",header = T,comment.char="#")
pri.MI=subset(guns,state=="Michigan")
t.test(pri.MI[,"prisoners"],mu=300)
hist(pri.MI[,"prisoners"],breaks=20)
abline(v=mean(pri.MI[,"prisoners"]), col="blue")
abline(v=300, col="dark green")
legend("topright", legend = c("sample mean", "specified value=300"),text.col=c("blue", "dark green"))
#p-value=0.5544,cannot rejest the null hypothesis,so the prisoners rate equals to 300 in Michigan


#Exercise 2
#method 1
#Null hypothsis: the murder rate of Washington State equals to Minnesota state
guns=read.csv("/Users/jacaranda/Desktop/NRE_538/Lab/Week4_Assignment3/guns_NRE538.csv",sep=",",header = T,comment.char="#")
guns.WAS=subset(guns,state=="Washington")
guns.MIN=subset(guns,state=="Minnesota")
shapiro.test(guns.WAS[,"murder"])
#samples of murders rate in Washington can be a normal distribution
shapiro.test(guns.MIN[,"murder"])
#samples of murders rate in Minnesota can be a normal distribution
var.test(guns.WAS[,"murder"],guns.MIN[,"murder"])
#p-value=0.6626;variance of these two samples can be equal.
mean.WAS=mean(guns.WAS[,"murder"])
mean.MIN=mean(guns.MIN[,"murder"])
SD.WAS=sd(guns.WAS[,"murder"])
SD.MIN=sd(guns.MIN[,"murder"])
WAS=rnorm(23,mean=mean.WAS,sd=SD.WAS)
MIN=rnorm(23,mean=mean.MIN,sd=SD.MIN)
dif=c()
for(i in 1:10000){
  dif[i]=mean(sample(WAS,23,replace=TRUE))-mean(sample(MIN,20,replace=TRUE))
}
dif.mean=mean(dif)
dif.sd=sd(dif)
CI95.low=sort(dif)[10000*0.025]
CI95.hig=sort(dif)[10000*0.975]
p=2*(length(which(dif<0))/length(dif))
p
#p=0,we can reject our null hypothesis,so the murder rate of Washington is significantly different from the murder rate of Minnesota

#method 2
mean.WAS=mean(guns.WAS[,"murder"])
mean.MIN=mean(guns.MIN[,"murder"])
SD.WAS=sd(guns.WAS[,"murder"])
SD.MIN=sd(guns.MIN[,"murder"])
n.was=length(guns.WAS[,"murder"])
n.was
n.min=length(guns.MIN[,"murder"])
n.min
sp=sqrt(((n.was-1)*(SD.WAS^2)+(n.min-1)*(SD.MIN^2))/(n.was+n.min-2))
sp
t=(mean.WAS-mean.MIN-0)/(sp*sqrt((1/n.was)+(1/n.min)))
t
p=2*(1-pt(t,44))
p
#because p=1.332268e-15<0.05,so we can reject null hypothesis. Therefore,the murder rate of Washington is significantly different from the murder rate of Minnesota
