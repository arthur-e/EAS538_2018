data(CO2)
head(CO2,10)
str(CO2)

# Exercise 1
CO2.aov1=aov(uptake~Treatment*Type,data=CO2)
summary(CO2.aov1)
a=model.tables(CO2.aov1,"means",se=TRUE)
b=model.tables(CO2.aov1,"effect",se=TRUE)

## means
a$tables$`Grand mean` #a grand mean=27.2131 -> grand mean of all uptake
a$tables$Treatment    #b nonchilled=30.64286 -> mean of uptake in the nonchilled treatment 
                      #c chilled=23.78333 -> mean of uptake in the chilled treatment
a$tables$Type         #d Quebec=33.54286 -> mean of uptake in the Quebec type
                      #e Mississippi=20.88333 -> mean of uptake in the Mississippi type

## effects -> the difference between the "mean" of a specific group & the "grand mean" (the "mean" - the "grand mean")
b$tables$Treatment    # nonchilled=3.429762 -> the difference between the "mean" of uptake in the nonchilled treatment & the "grand mean" of uptake (#b-#a=30.64286-27.2131=3.429762)
                      # chilled= -3.429762 -> the difference between the "mean" of uptake in the chilled treatment & the "grand mean" of uptake" (#c-#a=23.78333-27.2131= -3.429762)
b$tables$Type         # Quebec=6.329762 -> the difference between the "mean" of uptake in Quebec type & the "grand mean" of uptake (#d-#a=33.54286-27.2131=6.329762)
                      # Mississippi= -6.329762 -> the difference between the "mean" of uptake in Mississippi type & the "grand mean" of uptake" (#e-#a=20.88333-27.2131= -6.329762)

# Exercise 2
x= CO2.aov2=lm(uptake~Treatment,data=CO2)
y= CO2.aov3=lm(uptake~Type,data=CO2)
z= CO2.aov4=lm(uptake~Treatment*Type,data=CO2)
summary(x)
summary(y)
summary(z)

## x: the linear model of treatment (nonchilled & chilled) impacts on uptake
## The model: y=30.643-6.86x
## Mean uptake of nonchilled =30.643 (x=0) which is the intercept
## Mean uptake of chilled =23.783 (x=1) =30.643-6.86
## Estimate std. of chilled = -6.86 = mean of chilled - mean of nonchilled = 23.783-30.643
## The p-value of impacts on uptake by chilled comparing to nonchilled =0.0031 (<0.05)
## which means impacts of both treatments on uptake are significantly different

### y: the linear model of type (Quebec & Mississippi) impacts on uptake
### The model: y=33.543-12.66x
### Mean upatke of Quebec =33.543 (x=0) which is the intercept
### Mean uptake of Mississippi =20.883 (x=1) = 33.543-12.66
### Estimate std. of Mississippi = -12.66 = mean of Mississippi - mean of Quebec = 20.883-33.543
### The p-value of impacts on uptake by Mississippi comparing to Quebec =3.83e-09 (<<0.05)
### which means impacts of both types on uptake are significantly different

#### z: the linear model of interaction impacts of treatment and type on uptake
#### The model: y=35.333-3.581(x1)-9.381(x2)-6.557(x3)
#### The p-value of impacts on uptake by (baseline: nonchilled + Quebec) =2e-16 (<<0.05), which is significantly different
#### The p-value of impacts on uptake by (chilled + Quebec) comparing to the baseline (nonchilled + Quebec) =0.151141 (>0.05), which is not significantly different
#### The p-value of impacts on uptake by (nonchilled + Mississippi) comparing to the baseline (nonchilled + Quebec) =0.000284 (<0.05), which is significantly different
#### The p-value of impacts on uptake by (Mississippi:nonchilled->chilled) comapring to (Quebec:nonchilled->chilled) =0.064213 (>0.05), which is not significantly different
#### Both the p-value>0.05 show that in the same type (Quebec or Mississippi), there is no significantly different between the impacts on uptake by nonchilled and chilled treatment

##### We then decide what variables should be put in the linear model
lmx1=lm(uptake~Type,data=CO2)
lmx2=lm(uptake~Type*Treatment,data=CO2)
m=anova(lmx1,lmx2) #p-value<0.05

lmx3=lm(uptake~Treatment,data=CO2)
lmx4=lm(uptake~Type*Treatment,data=CO2)
n=anova(lmx3,lmx4) #p-value<0.05

lmx5=lm(uptake~Type+Treatment,data=CO2)
lmx6=lm(uptake~Type*Treatment,data=CO2)
o=anova(lmx5,lmx6) #p-value>0.05
##### Therefore, the linear model will be y=35.333-3.581(x1)-9.381(x2) because interaction (x3) has no significant impacts on uptake (y value)
##### x1=0: treatment "nonchilled", x1=1: treatment "chilled", x2=0: type "Quebec", x2=1: type "Mississippi"
##### When x1=0, x2=0, the uptake of (nonchilled + Quebec) =35.333
##### When x1=0, x2=1, the uptake of (nonchilled + Mississippi) =35.333-9.381=25.952
##### When x1=1, x2=0, the uptake of (chilled + Quebec) =35.333-3.581=31.752
##### when x1=1, x2=1, the uptake of (chilled + Mississippi) =35.333-3.581-9.381=22.371
