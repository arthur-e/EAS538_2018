#### read in data:  
  ##cancer=read.csv("C:/Users/Owner/Desktop/U of M Masters Program/Winter 2017/NRE 538/Final Exam/cancer.csv")
  ##college=read_csv("C:/Users/Owner/Desktop/U of M Masters Program/Winter 2017/NRE 538/Final Exam/college_final.csv")
  ##happy=read_csv("C:/Users/Owner/Desktop/U of M Masters Program/Winter 2017/NRE 538/Final Exam/happy.csv")
  ##flying=read_csv("C:/Users/Owner/Desktop/U of M Masters Program/Winter 2017/NRE 538/Final Exam/flying.csv")


##############################################################################################
##### Q1. Is there a significant association between gender (gender) and whether people  ##### 
#####        think it's rude to bring an unruly child on the plane (unruly_child)?       #####  
##############################################################################################

# a. write null and alternate hypothesis

## Ho: The two categorical variables, gender and rudeness are independent of one another.
## Ha: The two categorical variables, gender and rudeness are not independent of one another.

# b. create a visual
      
      ###   tbl=table(flying$gender, flying$unruly_child)
      ###   tbl
      ###   barplot(t(prop.table(tbl,1)))
      ###   barplot(t(prop.table(tbl,1)), main="Gender and Feelings of Rudeness", xlab="color", ylab="proportion", ylim=c(0,1.4), col=names(unruly_child), legend=TRUE, names.arg=names(unruly_child))

      ###   Contingency Table
      ###   CrossTable(flying$gender, flying$unruly_child, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE, prop.chisq=TRUE, chisq=TRUE)

      ###      Cell Contents
      ###   |-------------------------|
      ###   |                       N |
      ###   | Chi-square contribution |
      ###   |           N / Row Total |
      ###   |           N / Col Total |
      ###   |         N / Table Total |
      ###   |-------------------------|
  
  
      ###   Total Observations in Table:  843 


      ###   | flying$unruly_child 
      ###   flying$gender |        No |  Somewhat |      Very | Row Total | 
      ###   --------------|-----------|-----------|-----------|-----------|
      ###   Female        |        91 |       193 |       158 |       442 | 
      ###                 |     2.516 |     0.609 |     3.280 |           | 
      ###                 |     0.206 |     0.437 |     0.357 |     0.524 | 
      ###                 |     0.619 |     0.555 |     0.454 |           | 
      ###                 |     0.108 |     0.229 |     0.187 |           | 
      ###               --------------|-----------|-----------|-----------|-----------|
      ###   Male          |        56 |       155 |       190 |       401 | 
      ###                 |     2.773 |     0.671 |     3.615 |           | 
      ###                 |     0.140 |     0.387 |     0.474 |     0.476 | 
      ###                 |     0.381 |     0.445 |     0.546 |           | 
      ###                 |     0.066 |     0.184 |     0.225 |           | 
      ###   --------------|-----------|-----------|-----------|-----------|
      ###    Column Total |       147 |       348 |       348 |       843 | 
      ###                 |     0.174 |     0.413 |     0.413 |           | 
      ###   --------------|-----------|-----------|-----------|-----------|
  
  
      ###     Statistics for All Table Factors

      ###     Pearson's Chi-squared test 
      ###     ------------------------------------------------------------
      ###     Chi^2 =  13.46306     d.f. =  2     p =  0.001192704


# c. check assumptions
# check normality - no, but sample size is > 30 with about 15 per group so I won't worry
# equal variance - yes
# d. run chi-square test

      ###   chisq.test(flying$gender, flying$unruly_child)

      ###   Pearson's Chi-squared test
      
      ###   data:  flying$gender and flying$unruly_child
      ###   X-squared = 13.463, df = 2, p-value = 0.001193

# e. interpretation
      
      ###   Because the p-value 0.001193 < .05, we cannot reject the null: Therefore, there is no relationship
      ###   between gender and feelings of rudeness about unruly children when flying.      


######################################################################################################
#####  Q2. Is there a significant difference in tuition (tuition) y type of institution (type)?  #####
######################################################################################################

# a. write null and alternate hypothesis

## Ho: ?(Private nonprofit) - ?(Public) = 0
## Ha: ?(Private nonprofit) - ?(Public) ??? 0

# b. CREATE A VISUAL
   
      ###  boxplot(tuition~type, data=college)

# c. CHECK ASSUMPTIONS
      
      # 1)  Random Sample: I'm assuming each tuition associated with the type of institution was randomply sampled.
      
      # 2)  Independent Observations:  I'm assuming the tuition values for each type of college are independent of each other.
      
      # 3)  Sample size:  The sample size (1407) is about 33% of the total 4207 colleges in the US. 
      #     Sample size, proportionately, is not less than 10%, however, the sample size is 
      #     greater than 30, I will proceed with the test.      
      
      # 4)  Check normality
      
      ### pnp<-hist(college[college$type=="Private nonprofit",]$tuition,breaks=12)
      ### pub<-hist(college[college$type=="Public",]$tuition,breaks=6)
      
      ### shapiro.test(college$tuition)
      ### Shapiro-Wilk normality test

          ###  data:  college$tuition
          ###  W = 0.97407, p-value = 3.193e-15

      ### qqnorm(college$tuition)
      ### qqline(college$tuition)

     ### Shapiro-Wilk shows p-value 3.193e-15 which is < .05 but qqnorm
     ### doesn't show a perfectly straight line so normality isn't exactly met but sample size is>30
      
      # EQUAL VARIANCE
     
      ###   var.test(college[college$type=="Private nonprofit",]$tuition, college[college$type=="Public",]$tuition)

      ###   F test to compare two variances
      
          ### data:  college[college$type == "Private nonprofit", ]$tuition and college[college$type == "Public", ]$tuition
          ### F = 2.3005, num df = 871, denom df = 534, p-value < 2.2e-16
          ### alternative hypothesis: true ratio of variances is not equal to 1
          ### 95 percent confidence interval:
          ### 1.97238 2.67567
          ### sample estimates:
          ### ratio of variances 
          ### 2.300516 
      
      ### Variances are not equal between types of colleges and their tuition;
      ### p-value < 202e-16, proceed to two-sample t-test with unequal variances ( )
      
      
      
# d. run two-sample t-test with unequal variances
      
      ###  t.test(college[college$type=="Private nonprofit",]$tuition, college[college$type=="Public",]$tuition, var.equal=FALSE,data=college)
      
      ###   Welch Two Sample t-test
      
      ###   data:  college[college$type == "Private nonprofit", ]$tuition and college[college$type == "Public", ]$tuition
      ###   t = 22.79, df = 1397.9, p-value < 2.2e-16
      ###   alternative hypothesis: true difference in means is not equal to 0
      ###   95 percent confidence interval:
      ###     8632.749 10258.878
      ###     sample estimates:
      ###     mean of x mean of y 
      ###     28301.69  18855.88 
      
      ###   tcritical value
      ###   qt(0.025, 1397)
      ###   [1] -1.961664

# e. interpretation
      
      ### t(stat) = 22.79 is > t(crit) = -1.961664
      ### Because the t-stat is greater than t-critical, we can reject the null
      ### that there is no difference between the tuition for Private nonprofit and Public colleges.
      
      ### p < 2.2e-16, this is less than .05, confirms we can reject the null.


# f. check model fit - Non-applicable



###########################################################################################
##### Q3. Is there a significant difference in happiness (Hscore) by region(Region)?  #####
###########################################################################################

# a. write null and alternate hypothesis

    ## Ho: ?(AsAu) = ?(Eu) = ?(AmCa) = ?(AfMi)
    ## Ha: At least one of the group means is not the same

# b. create a visual
      
    ##boxplot(Hscore~Region, data=happy)
      
# c. check assumptions
      
      # 1)  Random Sample: I'm assuming each HScore associated with the different Region was randomply sampled.
      
      # 2)  Independent Observations:  I'm assuming the HScore values for each Region are independent of each other.
      
      # 3)  Sample size:  The sample size (157) is more than likely less than 10% of the total populations of the Regions. 
      #     Sample size is also greater than 30.      
      
      # 4)  Check normality
      
      ### hist(happy$Hscore)
      
      ### shapiro.test(happy$Hscore)
      ### Shapiro-Wilk normality test
      
      ###  data:  happy$Hscore
      ###  W = 0.97783, p-value = 0.01248
      
      ### qqnorm(happy$Hscore)
      ### qqline(happy$Hscore)
      
      ### Shapiro-Wilk p-value is 0.01248 which is < .05 indicating I can't reject the null
      ### hypothesis that the sample is coming from a population which is normally distributed.
      ### However, the qqnorm shows a slight "s" curve, normality assumption is not met but sample size is>30 with 
      ### at least 15 per group.
      
      # EQUAL VARIANCE
      
      ###   leveneTest(Hscore~Region,data=happy)      
      
      ###   Levene's Test for Homogeneity of Variance (center = median)
      ###           Df F value Pr(>F)
      ###   group   3  0.7605 0.5179
      ###   153  
      
      ### Variances are equal; Levene's test p-value = 0.5179 is > .05.
      
      
# d. run One-Way Anova
      
      ###   anova(aov(Hscore~Region,data=happy))
      
      ###   Analysis of Variance Table
      
      ###   Response: Hscore
      ###   Df  Sum Sq Mean Sq F value    Pr(>F)    
      ###   Region      3  64.369 21.4562  23.623 1.279e-12 ***
      ###   Residuals 153 138.965  0.9083                      
      ###   ---
        
      ###   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
# e. interpretation
      
      ### 
# f. check model fit

##### OSCAR: Actually, the sample size in unequal, so Kruskal-wallis test should be used, and TukeyHSD can't be used here...


###########################################################################################################
##### Q4. What factors are significantly associated with a country's corruption levels (Corruption)?  #####
###########################################################################################################

# a. This is not a hypothesis test:  Linear model:

    ###   Corpt<- lm(Corruption ~ GDP + Freedom + Hscore, data=happy)

# b. create a visual
      
      ###   plot(Corruption~GDP,data=happy)
      ###   plot(Corruption~Freedom,data=happy)
      ###   plot(Corruption~Hscore,data=happy)

      ###  pairs(Corruption~GDP+Freedom+Hscore, data=happy)
      
# c. check assumptions
      
    ##  hist(happy$Corruption)
    ##  shapiro.test(happy$Corruption)
    ##  qqnorm(happy$Corruption)
    ##  qqline(happy$Corruption)

    ##  Interpretation:   The histogram of raw data, Corruption, shows the data is not normally distributed.
    ##                    This was supported by an extremely low p-value in the Shapiro-Wilkes test:  p-value=5.128e-11.
    ##                    qqnorm wasn't entirely perfect. 

    ##  Transformations:

    ##  Corptlog<-log(Corruption)
    ##  hist(Corptlog)
    ##  shapiro.test(Corptlog)
    ##  qqnorm(Corptlog)
    ##  qqline(Corptlog)

    ##  Interpretation:   Performing a log transformation on the y, dependent variable,
    ##                    Corruption made the data more normally distributed (visualized by the 
    ##                    histogram of the log of Corruption).  This was supported by a much
    ##                    higher p-value in the Shapiro-Wilkes test:  p-value = 0.08535 (but this is still
    ##                    not above .05 so I must reject the null that the sample comes from a population
    ##                    which is normally distributed)
    ##                    The qqnorm still isn't entirely perfect but it does look closer to the line of fit.

      # 1)  Observations are independent: I'm assuming the values for the dependent variable, Corruption, and the values of each independent
      #     variable (GDP, Freedom, and Hscore) are independent of one another.
      
      # 2)  Linear relationship exists: Checked with plot(residualsCorpt) and qqnorm(residualsCorpt); Residuals plot shows pattern in residuals, may not be
      #                                               a linear relationship.qqnorm line of fit was not perfect.      
      
      # 3)  Independent errors: Checked with dwtest(Corptmod, alternative=c("two.sided")) p-value = 0.01573 which is fairly close to 
      #                         to "0"; I'm assuming there's no autocorrelation and that the errors are independent.
      
      # 4)  Normal Distribution of Errors: Checked with shapiro.test(residuals(Corpt)):  p-value = 7.755e-05; Residuals are not normally distributed.
      
      # 5)  Equal variances(homoscedasticity):  Checked with plot(residuals(Corpt)), bptest(Corpt): plot of residuals looks homoscedastic, but bptest
      #                                         was < .05 (bptest(Corpt) p-value = 0.002254) implying there was heteroscadasticity.                                      
      
      # 6)  Check multicollinearity: Checked using vif(Corpt):  
      #             GDP  Freedom   Hscore 
      #           2.742945 1.517115 3.510992

      #           Interpretation:   There isn't multicollinearity between explanatory variables;
      #                             All variables report low VIF scores.

##### OSCAR: 
#####       Residual (error) homoskedasticity, independencey, and normality should be part of model checking. 
#####       The only assumption is the normality of the dependent variable and co-linearity. 
#####       In other word, if the three checks are violated, the model is not good but you are still eligible to do linear regression (with OLS) and use the model. 
#####       If the dependent variable is not normal, you can NOT do linear regression at the beginning.
#####       Why not at least log transform the data? (-0.5)


# d.  RUN MODEL

  #   Corpt<-lm(Corruption~GDP+Freedom+Hscore, data=happy)
  #   summary(Corpt)

    ##  Call:
    ##  lm(formula = Corruption ~ GDP + Freedom + Hscore, data = happy)

    ##  Residuals:
    ##    Min       1Q   Median       3Q      Max 
    ##  -0.17481 -0.06946 -0.01371  0.05455  0.34578 

    ##  Coefficients:
    ##    Estimate Std. Error t value Pr(>|t|)    
    ##    (Intercept) -0.052513   0.041116  -1.277    0.203    
    ##    GDP          0.008169   0.030730   0.266    0.791    
    ##    Freedom      0.311192   0.064804   4.802 3.71e-06 ***
    ##    Hscore       0.014287   0.012565   1.137    0.257    
    ##    ---
    ##    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ##    Residual standard error: 0.09562 on 153 degrees of freedom
    ##    Multiple R-squared:  0.2727,	Adjusted R-squared:  0.2585 
    ##    F-statistic: 19.12 on 3 and 153 DF,  p-value: 1.386e-10
      
# e. INTERPRETATION of OUTPUT
      
      ### The only factor which is significant, according to this model (and knowing all of the
      ### assumptions have not been met) is Freedom.  For every 0.311192 increase in Freedom, 
      ### Corruption decreases 0.052513.

      

# f. CHECK MODEL FIT

      ### The adjusted R-squared=0.2585 which is fairly low; It says 26% of the variance is       
      ### explained by this model.

      ### TRANSFORMATION:   One interesting note, however, is that when the quadratic(Freedom2<-Freedom^2) was added
      ###                   the model (lm(formula = happy$Corruption ~ GDP + Freedom + Freedom2 + Hscore, 
      ###                   data = happy)), Multiple R-squared:  0.3812,	Adjusted R-squared:  0.3649.                         

      ### I would say, however, since we are only looking to see which factors are significantly
      ### associated with a country's corruption levels, AND NOT PREDICTING ANYTHING,
      ### this R-squared value does not necessarily explain anything about the relationship between
      ### variables. 

#########################################################################################################
##### Q5. Choose one of the continuous variables that was siginificant in the modelfor Question 4   #####
#####     and interact it with region(Region) to predict corruption (Corruption).                   ##### 
#####     Does the influence of the continuous variable on corruption vary by region?               ##### 
#########################################################################################################
      
    # a. This is not a hypothesis test:  Linear model:
      
      ###   Cormod2<-lm(Corruption~Hscore*Region, data=happy)

# b. create a visual

###   plot(Corruption~Freedom,data=happy)
      #  Looks heteroscadastic

# c. check assumptions

##  hist(happy$Corruption)
##  shapiro.test(happy$Corruption)
##  qqnorm(happy$Corruption)
##  qqline(happy$Corruption)

##  Interpretation:   The histogram of raw data, Corruption, shows the data is not normally distributed.
##                    This was supported by an extremely low p-value in the Shapiro-Wilkes test:  p-value=5.128e-11.
##                    qqnorm wasn't entirely perfect. 

##  Transformations:

##  Corptlog<-log(Corruption)
##  hist(Corptlog)
##  shapiro.test(Corptlog)
##  qqnorm(Corptlog)
##  qqline(Corptlog)

##  Interpretation:   Performing a log transformation on the y, dependent variable,
##                    Corruption made the data more normally distributed (visualized by the 
##                    histogram of the log of Corruption).  This was supported by a much
##                    higher p-value in the Shapiro-Wilkes test:  p-value = 0.08535 (but this is still
##                    not above .05 so I must reject the null that the sample comes from a population
##                    which is normally distributed)
##                    The qqnorm still isn't entirely perfect but it does look closer to the line of fit.

# 1)  Observations are independent: I'm assuming the values for the dependent variable, Corruption, and the values of each independent
#     variable (Freedom and Region) are independent of one another.

# 2)  Linear relationship exists: Checked with plot(residualsCorpt) and qqnorm(residualsCorpt); Residuals plot shows pattern in residuals, may not be
#                                               a linear relationship.qqnorm line of fit was not perfect.      

# 3)  Independent errors: Checked with dwtest(Corptmod, alternative=c("two.sided")) p-value = 0.01573 which is fairly close to 
#                         to "0"; I'm assuming there's no autocorrelation and that the errors are independent.

# 4)  Normal Distribution of Errors: Checked with shapiro.test(residuals(Corpt)):  p-value = 7.755e-05; Residuals are not normally distributed.

# 5)  Equal variances(homoscedasticity):  Checked with plot(residuals(Corpt)), bptest(Corpt): plot of residuals looks homoscedastic, but bptest
#                                         was < .05 (bptest(Corpt) p-value = 0.002254) implying there was heteroscadasticity.                                      

# 6)  Check multicollinearity: Checked using vif(Corpt):  
#             GDP  Freedom   Hscore 
#           2.742945 1.517115 3.510992

#           Interpretation:   There isn't multicollinearity between explanatory variables;
#                             All variables report low VIF scores.
      # b. create a visual
      
      ###  plot(Cormod2)
      
      # c. check assumptions
      # 1)  Observations are independent: I'm assuming the values for the dependent variable, Corruption, and the values of each independent
      #     variable (Hscore and Region) are independent of one another.
      
      # 2)  Linear relationship exists (plot(Cormod2)): Residuals plot shows pattern in residuals.
      
      # 3)  Independent errors:  
      
      # 4)  Normal Distribution of Errors (plot(Cormod2)):  qqnorm plot of residuals shows residuals aren't normally distributed.
      
      # 5)  Equal variances(homoscedasticity) (plot(Cormod2)): indication of heteroscadasticity
      
# d.  RUN MODEL

#   intmod=(lm(Corruption~Freedom*Region,data=happy))
#   summary(intmod)

##  Call:
##  lm(formula = Corruption ~ Freedom * Region, data = happy)

##  Residuals:
##    Min       1Q   Median       3Q      Max 
##  -0.20801 -0.05591 -0.01298  0.04804  0.29995 

##  Coefficients:
##                                      Estimate    Std. Error   t value Pr(>|t|)    
##   (Intercept)                        0.03558    0.02973   1.197   0.2333     
##    Freedom                           0.35091    0.08559   4.100 6.77e-05 ***    
##    RegionAmericasCarribean           0.07653    0.08344   0.917   0.3605    
##    RegionAsiaAustralia              -0.05444    0.07654  -0.711   0.4780    
##    RegionEurope                     -0.10550    0.04464  -2.363   0.0194 *  
##    Freedom:RegionAmericasCarribean  -0.33785    0.19640  -1.720   0.0875 .  
##    Freedom:RegionAsiaAustralia       0.05666    0.17967   0.315   0.7529    
##    Freedom:RegionEurope              0.25902    0.11845   2.187   0.0303 *  
##    ---
##    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

##    Residual standard error: 0.09112 on 149 degrees of freedom
##    Multiple R-squared:  0.3567,	Adjusted R-squared:  0.3265 
##    F-statistic:  11.8 on 7 and 149 DF,  p-value: 6.547e-12

# e. INTERPRETATION of OUTPUT

### The influence of Freedom on Corruption only varies in Europe; p-value=0.0194 is < .05.

##### OSCAR: No, this is not correct. Check the interpretation of interaction again (-1)


# f. CHECK MODEL FIT

### The adjusted R-squared=0.3265 which is fairly low; It says 33% of the variance is       
### explained by this model.

                 


####################################################################################################################
##### Q6. Which factors are significantly associated with whether a breast cancer tumor is malignant or not?   #####
####################################################################################################################

# a. This is not a hypothesis test:  Linear model with a binomial dependent variable (1 is malignant, 0 is not malignant):

###   malmod=glm(malignant~radius_mean+texture_mean+smoothness_mean, data=cancer, family=binomial(link="logit")))

# b. create a visual

###   pairs(cancer)


# 1)  Observations are independent: I'm assuming the values for the dependent variable, Malignant, and the values of each independent
#     variable (radius_mean, texture_mean, smoothness_mean) are independent of one another.

# 2)  Normal Distribution of dependent variable is from a binomial distribution (assumption met).       

# 3)  Linear relationship between independent variables and transformed dependent variable: 

# 4)  Equal variances(homoscedasticity):  Not applicable                                      

# 6)  Maximum likelihood Estimation:  

# d.  RUN MODEL

#   malmod<-glm(formula = malignant ~ radius_mean + texture_mean + smoothness_mean, family = binomial(link = "logit"), data = cancer)
#   summary(malmod)

    ##  Deviance Residuals: 
    ##    Min        1Q    Median        3Q       Max  
    ##  -2.19102  -0.19403  -0.03799   0.04025   2.92583  

    ##    Coefficients:
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ##    (Intercept)     -42.01941    4.45943  -9.423  < 2e-16 ***
    ##    radius_mean       1.39699    0.15403   9.069  < 2e-16 ***
    ##    texture_mean      0.38056    0.05711   6.663 2.68e-11 ***
    ##    smoothness_mean 144.67423   19.04687   7.596 3.06e-14 ***
    ##    ---
    ##    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ##    (Dispersion parameter for binomial family taken to be 1)

    ##    Null deviance: 751.44  on 568  degrees of freedom
    ##    Residual deviance: 187.29  on 565  degrees of freedom
    ##    AIC: 195.29

    ##    Number of Fisher Scoring iterations: 8

# e. INTERPRETATION of OUTPUT

### All of these independent variables are significantly associated with determining
### whether a breast cancer tumor is significant or not.  

##### OSCAR: It should be more than this, right? (-0.5)


# f. CHECK MODEL FIT

### plot(residuals(malmod)~radius_mean+texture_mean+smoothness_mean, data=cancer)

##### OSCAR: This is not correct though. You should at least plot the model not the regression of residuals.(-0.5)



#######################################################################################################################################
##### Q7. Which independent variables are the most important in explaining whether a breast cancer tumor is malignant or not?     #####
#######################################################################################################################################


# a. This is not a hypothesis test:  Linear model with a binomial dependent variable (1 is malignant, 0 is not malignant):

###   malmod=glm(malignant~radius_mean+texture_mean+smoothness_mean, data=cancer, family=binomial(link="logit")))

# b. create a visual

###   pairs(cancer)


# c. check assumptions

# 1)  Observations are independent: I'm assuming the values for the dependent variable, Malignant, and the values of each independent
#     variable (radius_mean, texture_mean, smoothness_mean) are independent of one another.

# 2)  Normal Distribution of dependent variable is from a binomial distribution (assumption met).       

# 3)  Linear relationship between independent variables and transformed dependent variable: 

# 4)  Equal variances(homoscedasticity):  Not applicable                                      

# 6)  Maximum likelihood Estimation: 

# d.  RUN MODEL

#   malmod<-glm(formula = malignant ~ radius_mean + texture_mean + smoothness_mean, family = binomial(link = "logit"), data = cancer)
#   summary(malmod)

##  Deviance Residuals: 
##    Min        1Q    Median        3Q       Max  
##  -2.19102  -0.19403  -0.03799   0.04025   2.92583  

##    Coefficients:
##                      Estimate Std. Error z value Pr(>|z|)    
##    (Intercept)     -42.01941    4.45943  -9.423  < 2e-16 ***
##    radius_mean       1.39699    0.15403   9.069  < 2e-16 ***
##    texture_mean      0.38056    0.05711   6.663 2.68e-11 ***
##    smoothness_mean 144.67423   19.04687   7.596 3.06e-14 ***
##    ---
##    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

##    (Dispersion parameter for binomial family taken to be 1)

##    Null deviance: 751.44  on 568  degrees of freedom
##    Residual deviance: 187.29  on 565  degrees of freedom
##    AIC: 195.29

##    Number of Fisher Scoring iterations: 8

# e. INTERPRETATION of OUTPUT

### The radius_mean is most important in explaining whether a breast cancer tumor is malignant
### or not because it has the highest z-value at 9.069.

##### OSCAR: You have to standardize it first.

# f. CHECK MODEL FIT

###