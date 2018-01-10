#2) Is there a significant difference in tuition (tuition) by type of 
#institution (type)? If yes, which type has a higher tuition?

college = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/college.csv",header=TRUE, sep=",")

#tuition is continuous
#type of institution is categorical
#run a two-sample, unpaired t-test

hist(college$tuition)
#skewed slightly right
shapiro.test(college$tuition)
#Ho = data is normally distributed
#p-value less than .05--reject the Ho.

#Check qqplot
qqnorm(college$tuition); qqline(college$tuition, col="Red")
#not bad, not great. Since the histogram looks good and n>30, I am tempted to go with the original data.

#But I will also test out a square root transform, just to see if it gets better
college$Ttrans <- sqrt(college$tuition)

#test normality of the transform
hist(college$Ttrans)
#now it is skewed slightly left.
shapiro.test(college$Ttrans)
#HO=normally distributed
#p-value less than .05--reject the Ho. Still fails this
qqnorm(college$Ttrans); qqline(college$Ttrans, col="Red")
#but the data fits the line more closely, so I will use this.

#subset by type
public = subset(college, type=="Public")
private = subset(college, type=="Private nonprofit")

#test for equal variance
var.test(public$Ttrans, private$Ttrans)
#F test to compare two variances

#data:  public$Ttrans and private$Ttrans
#F = 0.60438, num df = 534, denom df = 871, p-value = 2.743e-10
#alternative hypothesis: true ratio of variances is not equal to 1
#95 percent confidence interval:
#  0.5196380 0.7049248
#sample estimates:
#  ratio of variances 
#0.6043774 

#Based on the p-value less than .05, we reject the null that the variances are equal.

#run Welch's t-test with the transformed data, with equal variance marked as false
t.test(public$Ttrans, private$Ttrans, var.equal=FALSE)
#Welch Two Sample t-test
#data:  public$Ttrans and private$Ttrans
#t = -21.847, df = 1328.9, p-value < 2.2e-16
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -32.97451 -27.54052
#sample estimates:
# mean of x   mean of y 
# 135.4532    165.7107 

#based on p-value, we reject the null. There is a difference in tuition based on institution type. Private schools have a higher average tuition.

#What would the results be if I used the original data instead of the transformed?
t.test(public$tuition, private$tuition, var.equal=FALSE)
#Basically the same...
#Welch Two Sample t-test

#data:  public$tuition and private$tuition
#t = -22.79, df = 1397.9, p-value < 2.2e-16
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -10258.878  -8632.749
#sample estimates:
# mean of x   mean of y 
# 18855.88    28301.69 

