#hohsieh_NRE538_FinalR

flying = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/flying.csv",header=TRUE, sep=",")

college = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/college.csv",header=TRUE, sep=",")

happy = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/happy.csv",header=TRUE, sep=",")

cancer = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/cancer.csv",header=TRUE, sep=",")


library(plyr) 
library(reshape2)
library(magrittr)

library(RCurl) 
library(ggplot2) 

library(car)
library(lmtest)

#1. flying_unruly child_Chi-square####
flying2 = flying %>%
  subset(unruly_child != "NA") %>%
  subset(gender != "NA")

  #Check for assumptions:
  #1) Random sampled: unknown.
  #2) Independent observation: unknown.
  #3) >20% of the groups have enough expected and observed values (>= 5 or 10): Yes. The least number of observations is 56.
  #4) No structured 0: Yes.

tb = table(flying2$gender, flying2$unruly_child)
chisq.test(flying2$gender, flying2$unruly_child) #p-value = 0.001193
#the result of the chi-square test indicates that gender is significant associated with 
#whether people think it's rude to bring an unruly child on the plane (p-value < 0.05)

#Based on the table, we can tell that males tend to think it's more rude to bring an unruly child on the plane.


#2. tuition_T-test####

  #Check for assumption: 
  #1) Random sampled: unknown.
  #2) Independent observations: unknown.
  #3) Normal distribution OR sample size > 30: Yes.
    
    hist(college$tuition)
    shapiro.test(college$tuition) #p-value = 3.193e-15
    qqnorm(college$tuition)
    qqline(college$tuition, col = "red")
    length(college$tuition) #1407

    #The result of the shapiro test indicates that the distribution of the data is significant different than normal distribution (p-value < 0.05).
    #But in the qq plot, the distribution is not deviated from the line a lot. Most of the values in the middle fit the line.
    #As well, the sample size is larger than 30 (sample size = 1407).
    #Therefore, we can assume the normality of the data.
  
  #4) Equal variance: No.
    var.test(college$tuition~college$type, data = college) #p-value < 2.2e-16
    #p-value < 2.2e-16 < 0.05 --> reject null hypothesis that the variance between the groups is equal
    #---> the variance between the groups is not equal.

t.test(tuition~type, var.equal = FALSE, data = college) #p-value < 2.2e-16
    #p-value < 2.2e-16 < 0.05 --> reject H0 that the tuition between the two types of institution are the same.
    #--> the tuition between the two types of institution are significantly different.

    #The mean of "Private nonprofit" is 28301.69, and the mean of "Public" is 18855.88.
    #--->The Private nonprofit has a higher tuition.

#3. Happiness~Region_1-wayANOVA####
  
  #Test: One-way ANOVA
  
  #Check for the assumptions:
  #1) Random samples: unknown
  #2) Independent observations: unknown.
  #3) Normal distribution OR sample size > 30: Yes.
    
    hist(happy$Hscore)
    shapiro.test(happy$Hscore) #p-value = 0.01248
    qqnorm(happy$Hscore)
    qqline(happy$Hscore, col = "red")
    length(happy$Hscore) #157
    
    #The result of the shapiro test indicates that the distribution of the data is significant different than normal distribution (p-value < 0.05).
    #But in the qq plot, the distribution is not deviated from the line a lot. Most of the values in the middle fit the line.
    #As well, the sample size is larger than 30 (sample size = 157).
    #Therefore, we can assume the normality of the data.
    
  #4) equal variance: Yes.
    
    leveneTest(happy$Hscore~happy$Region, data = happy) #p-value = 0.5179
    #The p value > 0.05 ---> we cannot reject the H0 that the variance among the groups are the same.
    #---> equal variance among the groups.

aov(Hscore~Region, data = happy) #Estimated effects may be unbalanced
lm3 = lm(Hscore~Region, data = happy)
summary(lm3)
  #I use the "lm" function to run the ANOVA. The result suggests that the happiness of RegionAmericasCarribean, RegionAsiaAustralia, and RegionEurope are significantly different than that of AfricaMideast (each of the regions has a significant effect (p value < 0.05) on the happiness).
  #Based on the result, we can say that there is at least one region (AfricaMideast) that is significantly different than other regions in terms of happiness.


#4. Corruption_Multiple linear regression####
  
  #Test: Multiple linear regression

  #Check for collinearity:
  pairs(happy[,c(3:8, 10)])
  cor(happy[,c(3:8, 10)], use="na.or.complete")
    #Based on the results of the correlation table, I choose three continuous variables that are not highly correlated to each other (correlation cefficient < 0.5).
    #The continuous independent variables I choose are "Generosity", "Freedom", and "GDP".

  #Check for the assumptions:
  #1) Independent observations: unknown.
  #2) Normal distribution of dependent variable: No. --> Transform the data.
      
    hist(happy$Corruption)
    shapiro.test(happy$Corruption) #p-value = 5.128e-11
    qqnorm(happy$Corruption)
    qqline(happy$Corruption, col = "red")
    
    #The shapiro test indicates that the data is not normally distributed (p-value < 0.05 --> reject H0 that the data is normally distributed). 
    #In the qq plot, the data does not fit the line well.
    #Therefore, I would try to transform the data.
  
    #---> log transform the data
  
    happy = mutate(happy, Corruption.log = log(Corruption))
    hist(happy$Corruption.log)
    shapiro.test(happy$Corruption.log) #p-value = 0.08535
    qqnorm(happy$Corruption.log)
    qqline(happy$Corruption.log, col = "red")
    
    #The shapiro test indicates that the data is normally distributed (p-value > 0.05 --> cannot reject H0 that the data is normally distributed). 
    #In the qq plot, the data fit the line better.
    
  #3) Linear relationship between the dependent variable and the independent variables: assumed.
    pairs(happy[,c(5, 8:10)])
    plot(Corruption.log~Generosity, data = happy)
    abline(lm_G, col = "red")
    lm_G=lm(Corruption.log~Generosity, data = happy)
    summary(lm_G) #p-value = 0.00226 ** 
    
    plot(Corruption.log~GDP, data = happy)
    abline(lm_GDP, col = "red")
    lm_GDP=lm(Corruption.log~GDP, data = happy)
    summary(lm_GDP) #p-value =  0.0349 *  
    
    plot(Corruption.log~Freedom, dat = happy)
    abline(lm_F, col = "red")
    lm_F=lm(Corruption.log~Freedom, data = happy)
    summary(lm_F) #p-value = 3.18e-09 ***
    
lm4 = lm(Corruption.log~Generosity+Freedom+GDP, data = happy)
summary(lm4)
    
    #(Assume we set the level of significance as 0.05) The result indicates that 
      #Intercept: The intercept is significant (p-value < 2e-16) which means that controlled for the effect of Generosity, Freedom, and GDP, the average log(Corruption) level is -3.16388 and is significant different from 0.(that is, the average corruption level is e^-3.16388 = 0.04226145) 
      #Generosity: For every unit increase in Generosity, the log(Corruption) would increase by 0.54962 unit. But this effect is not significant (p-value = 0.226 > 0.05).
      #Freedom: For every unit increase in Freedom, the log(Corruption) would increase by 2.16578 unit. That is, for every unit increase in Freedom, the Corruption would be e^2.16578 (= 8.721402) times the origninal value. This effect is significant (p value = 2.95e-06).
      #GDP: For every unit increase in GDP, the log(Corruption) would increase by 0.04341 unit. But this effect is not significant (p value = 0.786).
    #The multiple R-squared is 0.2107, which means the model can explain 21.07% of the variance in the data. 
    #The adjusted R-squared is 0.1952 which suggests that the model fit the data okay.
    #The p value of the F test is 6.424e-08 < 0.05, which indicate that the model is significant different than null model or randomness. 
    
  
  #Check model fit:
  
  par(mfrow=c(2,2))
  plot(lm4)
  
  #1) Normality of residuals
    shapiro.test(residuals(lm4)) #p-value = 0.0006382
    #The p-value is smaller than 0.05 indicating that we reject the H0 that the residual distribution is not different from normal distribution.
    #--> the residuals are not normally distributed.
    #However, from the qq plot, the residuals fit the normal line fairly and are not hugely deviated from normal distribution. 
    #Therefore, I would think the residuals are normally distributed.
    
  #2) Independence of residuals
    dwtest(lm4, alternative = c("two.sided")) #p-value = 0.3701
    #The dwtest shows that the p-value = 0.3701 > 0.05. We cannot reject the H0 that the residuals are independent.---> The residuals are independent.
    
  #3) Homoscedasticity
    bptest(lm4) #p-value = 0.06878
    #The bptest shows that the p-value = 0.06878 > 0.05 indicating that we cannot reject the H0 that the residual distribution is homoscedasticity.
    #---> The residuals are homoscedasticity.
    
    
  #Based on the results of model fit checks, I would say the model fit the data pretty well.
  


#5. Corruption~Region*Freedom_ANCOVA####
    
    #Based on the assumption checks done in the last question, I would log transform the dependent variable (Corruption).
    
    lm5 = lm(Corruption.log~Region*Freedom, data = happy)
    summary(lm5)
    
    #(Assume that we set 0.05 as the level of significance)
    #The result of the linear model show that:
    
    #Intercept: The estimate of the intercept is -2.6096 which means that controlled for other independent variables, the average log(Corruption) in AfricaMideast is -2.6096. The intercept is significant (p-value < 2e-16). 
    #RegionAmericasCarribean: The estimate of the RegionAmericasCarribean is 0.3985 which means that controlled for other independent variables, the average log(Corruption) in AmericasCarribean is -2.6096 + 0.3985 = -2.2111. But this effect of RegionAmericasCarribean is not significant (p-value = 0.500360 > 0.05).
    #RegionAsiaAustralia: The estimate of the RegionAsiaAustralia is -0.5177 which means that controlled for other independent variables, the average log(Corruption) in AsiaAustralia is -2.6096 - 0.5177 = -3.1273. But this effect of AsiaAustralia is not significant (p-value = 0.340110 > 0.05).
    #RegionEurope: The estimate of the RegionEurope is 0.3985 which means that controlled for other independent variables, the average log(Corruption) in Europe is -2.6096 - 1.3755 = -3.9851. This effect of Europe is significant (p-value = 2.42e-05 < 0.05).
    
    #Freedom: The estimate of Freedom is 1.5929 which means for every unit increase in Freedom, the log(Corruption) in AfricaMideast would increase by a rate of 1.5929. This effect is significant (p-value = 0.009360 < 0.05).
    #RegionAmericasCarribean:Freedom: The estimate of RegionAmericasCarribean:Freedom is -1.6566 which means that for every unit increase in Freedom in AmericasCarribean, the log(Corruption) would increase by a rate of 1.5929 - 1.6566 = -0.0637. But this effect is not significant (p-value = 0.234643 > 0.05).
    #RegionAsiaAustralia:Freedom: The estimate of RegionAsiaAustralia:Freedom is  0.6657 which means that for every unit increase in Freedom in AsiaAustralia, the log(Corruption) would increase by a rate of 1.5929 + 0.6657 = 2.2586. But this effect is not significant (p-value = 0.600949 > 0.05).
    #RegionEurope:Freedom: The estimate of RegionEurope:Freedom is 2.9826 which means that for every unit increase in Freedom in Europe, the log(Corruption) would increase by a rate of 1.5929 + 2.9826 = 4.5755. This effect is significant (p-value = 0.000494 < 0.05).
    
    #log(Corruption) = -2.6096 + 0.3985*RegionAmericasCarribean + (-0.5177)*RegionAsiaAustralia + (-1.3755)*RegionEurope + (1.5929 + (-1.6566*RegionAmericasCarribean + 0.6657*RegionAsiaAustralia + 2.9826*RegionEurope))*Freedom
    #Consideing the significance of the effects--->
    #log(Corruption) = -2.6096 + (-1.3755)*RegionEurope + (1.5929 + (2.9826*RegionEurope))*Freedom
    
    #The multiple R-squared is 0.3365, which means the model can explain 33.65% of the variance in the data. 
    #The adjusted R-squared is 0.3053, which suggests that the model fit the data okay.
    #The p value of the F test is 5.739e-11 < 0.05, which indicate that the model is significant different than null model or randomness. 
    
    
    #Yes, the influence of the continuous variable (Freedom) is different among regions. Compared to in AfricaMideast, the influence of Freedom on log(Corruption) in Region Europe is higher (1.5929 + 2.9826 = 4.5755). For AmericasCarribean and AsiaAustralia, the influence of Freedom on log(Corruption) is the same as that of AfricaMideast.

    
    #Check model fit:
    
    par(mfrow=c(2,2))
    plot(lm5)
    
    #1) Normality of residuals
    shapiro.test(residuals(lm5)) #p-value = 0.01481
    #The p-value is smaller than 0.05 indicating that we reject the H0 that the residual distribution is not different from normal distribution.
    #--> the residuals are not normally distributed.
    #However, from the qq plot, the residuals fit the normal line fairly and are not hugely deviated from normal distribution. 
    #Therefore, I would think the residuals are normally distributed.
    
    #2) Independence of residuals
    dwtest(lm5, alternative = c("two.sided")) #p-value = 0.5627
    #The dwtest shows that the p-value = 0.5627 > 0.05. We cannot reject the H0 that the residuals are independent.---> The residuals are independent.
    
    #3) Homoscedasticity
    bptest(lm5) #p-value = 0.2501
    #The bptest shows that the p-value = 0.2501 > 0.05 indicating that we cannot reject the H0 that the residual distribution is homoscedasticity.
    #---> The residuals are homoscedasticity.
    
    #Based on the results of model fit checks, I would say the model fit the data pretty well.
    
#6. Cancer_GLM_Bionomial####
    
    #Test: Generalized linear regression
    
    #Check for collinearity:
    pairs(cancer[,c(2:6)])
    cor(cancer[,c(2:6)], use="na.or.complete")
    #Based on the results of the correlation table, I choose three continuous variables that are not highly correlated to each other (correlation cefficient < 0.5).
    #The continuous independent variables I choose are "smoothness_mean", "texture_mean", and "radius_mean".
    
    #Check for the assumptions:
    #1) Independent observations: unknown.
    #2) Linear relationship between the transformed dependent variable and the independent variables: assumed.
    
    
    #The data of whether a breast cancer tumor is malignant or not is a binomial distribution. Therefore I would apply a logit transformation on the data.
    #logit transformation: p = the probability that the breast cancer tumor is malignant; odds = (p/(1-p)); y = log(odds)
    
    glm6 = glm(malignant~smoothness_mean+texture_mean+radius_mean, data = cancer, family = binomial(link="logit"))
    summary(glm6)
    
    #Intercept: When controlled for independent variables, the average log(odds) would be -42.01941. 
    #That is, when controlled for other independent variables, the average probability that the breast cancer tumor would be malignant is exp(-42.01941)/(1+exp(-42.01941)) = 5.639e-19
    #The intercept is significant different than 0 (p value < 2e-16).
    
    #smoothness_mean: When controlled for other independent variables, for every unit increase in smoothness_mean, the average log(odds) would increase by a rate of 144.67423. 
    #That is, when controlled for other independent variables, every unit increase in smootheness_mean would increase the odds by exp(144.67423) = 6.779845e+62 times.
    #This effect is significant (p value = 3.06e-14).
    
    #texture_mean: When controlled for other independent variables, for every unit increase in texture_mean, the average log(odds) would increase by a rate of 0.38056. 
    #That is, when controlled for other independent variables, every unit increase in texture_mean would increase the odds by exp(0.38056) = 1.463104 times.
    #This effect is significant (p value = 2.68e-11).
    
    #radius_mean: When controlled for other independent variables, for every unit increase in radius_mean, the average log(odds) would increase by a rate of 1.39699. 
    #That is, when controlled for other independent variables, every unit increase in radius_mean would increase the odds by exp(1.39699) = 4.043012 times.
    #This effect is significant (p value < 2e-16).
    
    #In conclusion, the smoothness_mean, texture_mean, and radius_mean all are significantly associated with whether a breast cancer tumor is mlignant or not.
    
    
    #Model fit check
    glm0 = glm(malignant~1, data = cancer, family = binomial(link = logit))
    summary(glm0)
        
    anova(glm0, glm6, test = "Chi") #p-value < 2.2e-16
    AIC(glm0, glm6)
    #AIC for glm0 = 753.4400
    #AIC for glm6 = 195.2902
    
    #The results of the two tests indicate that the smoothness_mean, texture_mean, and radius_mean as independent variables (glm6) could increase the likelihood of the model and the increase is significant (p-value < 0.05).
    
    
#7. Cancer_GLM_Bionomial_BONUS####

    glm7_1 = glm(malignant~smoothness_mean, data = cancer, family = binomial(link="logit"))
    glm7_2 = glm(malignant~texture_mean, data = cancer, family = binomial(link="logit"))
    glm7_3 = glm(malignant~radius_mean, data = cancer, family = binomial(link="logit"))
  
    AIC(glm7_1, glm7_2, glm7_3)
    
    #glm7_1 = 677.9485
    #glm7_2 = 650.5191
    #glm7_3 = 334.0108
    
    #Based on the results of the AIC, we can see that radius_mean is the most important variable among the three variables in explaining whether a breast cancer tumor is malignant or not.
    #(glm7_3 has the lowest AIC, which implies that the model glm7_3 would lost least amount of information)

