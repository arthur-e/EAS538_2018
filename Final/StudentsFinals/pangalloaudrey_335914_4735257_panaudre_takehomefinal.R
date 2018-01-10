#Question 1
flying=read.csv('C:/Users/Audrey Pangallo/Desktop/r/flying.csv')
flytab=table(flying$gender, flying$unruly_child)
head(flytab)
chisq.test(flytab)
chisq.test(flytab)$expected

#Question 2
college=read.csv('C:/Users/Audrey Pangallo/Desktop/r/college_final.csv')
public=subset(college$tuition, college$type=="Public")
head(public)
private=subset(college$tuition, college$type=="Private nonprofit")
hist(public)
abline(v=mean(public), col="salmon")
hist(private)
abline(v=mean(public), col="hot pink")
hist(college$tuition)
abline(v=mean(public), col="hot pink")
shapiro.test(public)
qqnorm(public);qqline(public, col="Red")
shapiro.test(private)
qqnorm(private);qqline(private, col="purple")
shapiro.test(college$tuition)
qqnorm(college$tuition);qqline(public, col="Red")
var.test(public,private)
t.test(public, private, var.equal = FALSE)
wilcox.test(public, private)
wilcox.test(public, private, var.equal=FALSE)
#I'm not sure how you'll grade this honestly

#Question 3
happy=read.csv('C:/Users/Audrey Pangallo/Desktop/r/happy.csv')
pairs(happy)
boxplot(Hscore~Region, data=happy, xlab="Region", ylab="Hscore")
qqnorm(happy$Hscore);qqline(happy$Hscore, col="green")
happyaov= aov(Hscore~Region, data = happy)
summary(happyaov)
TukeyHSD(happyaov)

#Question 4
cor(happy[, c("Corruption", "GDP", "Generosity", "Family")], use="na.or.complete")
plot(Corruption~GDP, data=happy)
abline(lm(Corruption~GDP, data=happy), col="red")
plot(Corruption~Generosity, data=happy)
abline(lm(Corruption~Generosity, data=happy), col="red")
plot(Corruption~Family, data=happy)
abline(lm(Corruption~Family, data=happy), col="red")
abline(lm(Corruption~sqrtgen, data=happy), col="red")
corrupt= lm(Corruption~ GDP+ Generosity + Family, data=happy)
summary(corrupt)
plot(corrupt)
bptest(corrupt)

#Question 5
corrupt2 = lm(Corruption~ GDP*Region, data=happy)
summary(corrupt2)

interplot(m=corrupt2, var1="Region", var2="GDP")+
  labs(x="GDP", y="Estimated coefficient for Region")

#Question 6
cancer=read.csv('C:/Users/Audrey Pangallo/Desktop/r/cancer.csv')
qqnorm(cancer$radius_mean);qqline(cancer$radius_mean, col="green")
lograd=log(cancer$radius_mean)
qqnorm(lograd);qqline(lograd, col="purple")
qqnorm(cancer$texture_mean);qqline(cancer$texture_mean, col="green")
qqnorm(cancer$area_mean);qqline(cancer$area_mean, col="pink")
logar=log(cancer$area_mean)
qqnorm(logar);qqline(logar, col="blue")
cor(cancer[, c("malignant", "radius_mean", "texture_mean", "area_mean")], use="na.or.complete")
fit <- glm(F~x1+x2+x3,data=mydata,family=binomial())
#the code above is just example code I pulled from the internet
tumor= lm(malignant~lograd+texture_mean+logar, data=cancer)
summary(tumor)
plot(tumor)
tumor3=lm(malignant~radius_mean+texture_mean+radius_mean, data=cancer)
summary(tumor3)
plot(tumor3)
tumor2= glm(malignant~lograd+texture_mean+logar, data=cancer, family = binomial)
summary(tumor2)
plot(tumor2)

