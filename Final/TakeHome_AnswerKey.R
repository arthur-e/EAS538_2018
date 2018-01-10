library(dplyr)
library(fivethirtyeight)
library(car)

# load in datasets
flying = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/flying.csv", 
                    header=TRUE, sep=",")
college = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/college.csv", 
                     header=TRUE, sep=",")
<<<<<<< HEAD
happy = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/happy.csv",
=======
happy = read.table(file="https://github.com/OscarFHC/NRE538_2017Fall/blob/master/Final/happy.csv", 
>>>>>>> origin/master
                   header=TRUE, sep=",")
cancer = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/cancer.csv", 
                    header=TRUE, sep=",")

# Q1 chi-square - flying
chisq.test(flying$gender,flying$unruly_child)

# Q2 ttest - college
qqPlot(college$sat)
var.test(college$sat~college$type)
<<<<<<< HEAD
t.test(tuition~type,var.equal=FALSE,data=college)
=======
t.test(sat~type,var.equal=FALSE,data=college)
>>>>>>> origin/master

# Q3 ANOVA - happiness
leveneTest(Hscore~Region,data=happy)
summary(aov(Hscore~Region,data=happy))
TukeyHSD(aov(Hscore~Region,data=happy))
## OR
summary(lm(Hscore~Region,data=happy))

# Q4 multiple linear regression - happiness
hist(happy$Corruption)
qqPlot(happy$Corruption)
hist(log(happy$Corruption))
qqPlot(log(happy$Corruption))

model=lm(log(Corruption)~GDP+Freedom+Generosity,data=happy)
vif(model)
summary(model)
plot(model)

# Q5 multiple linear regression w interaction term - happiness
model2=lm(log(Corruption)~Freedom*Region,data=happy)
summary(model2)
plot(model2)

# Q6 glm - breast cancer
model=glm(malignant~radius_mean+texture_mean+smoothness_mean,data=cancer,family='binomial')
summary(model)

# Q7 glm - breast cancer (extra credit)
# here it is key that they standardize their variables otherwise you cannot tell variable importance from the direct coefficients in Q6
