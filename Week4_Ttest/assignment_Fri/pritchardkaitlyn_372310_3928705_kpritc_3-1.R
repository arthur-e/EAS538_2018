## EXERCISE 1
guns=read.csv("/Users/katiepritchard/Downloads/guns_NRE538.csv", sep=",", header=T, comment.char="#")
guns.MI=subset(guns,state=="Michigan")

## One sample, two tail t-test (alpha = 0.05)
## Ho: The average annual incidence of robbery in MI is equal to 200
t.test(guns.MI[,"robbery"], mu=200)
hist(guns.MI[,"robbery"], breaks=20)
abline(v=mean(guns.MI[,"robbery"]), col="blue")
abline(v=200, col="dark green")
legend("topright", legend = c("sample mean", "specified value=200"),text.col=c("blue", "dark green"))
## p = 0.002085. Since p<0.05, we reject the null hypothesis.

## One sample, one tail t-test (alpha = 0.05)
## Ho: The average annual incidence of robbery in MI is less than or equal to the standard (200 per 10^5 people/yr)
t.test(guns.MI["robbery"], mu=200, alternative='greater')
## p = 0.001043. Since p<0.05, we reject the null hypothesis.

## EXERCISE 2
guns.WA=subset(guns,state=="Washington")
guns.MN=subset(guns,state=="Minnesota")
## Ho:The murder rate of Washington state is not significantly different than that of Minnesota state. 
## Alpha = 0.05
shapiro.test(guns.WA[,"murder"])
shapiro.test(guns.MN[,"murder"])
## Since p for both >0.05, the datasets are both normally distributed
var.test(guns.WA[,"murder"], guns.MI[,"murder"])
## Since p>0.05, we don't have enough evidence to reject H0 (which is that the two variances are the same)
t.test(guns.WA[,"murder"], guns.MN[,"murder"], paired=FALSE, var.equal=TRUE)
## p = 1.347e-15. Since p<0.05, we reject the null hypothesis and conclude that the murder rates of WA and MN are significantly different. 

hist(guns.WA[,"murder"], breaks=15, col="light blue", xlim=c(0, 7), ylim=c(0,5), xlab="murder rate", main="")
abline(v=mean(guns.WA[,"murder"]), col="blue")
par(new=TRUE)
hist(guns.MN[,"murder"], breaks=15, col="light green", xlim=c(0, 7), ylim=c(0,5), xlab="", main="")
abline(v=mean(guns.MN[,"murder"]), col="dark green")
legend("topright", legend = c("Washington mean", "Minnesota mean"),text.col=c("blue", "dark green"))