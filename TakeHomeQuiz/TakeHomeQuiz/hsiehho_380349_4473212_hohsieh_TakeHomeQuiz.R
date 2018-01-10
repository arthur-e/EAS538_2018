#Ho Hsieh_TakeHomeQuiz

# Energy data for question 1-4####
edata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/energy_data.csv", 
                   sep=",", fill=TRUE, header=TRUE)
library(dplyr)
library(car)

#Q1####
#a.H0: The per capita energy consumptions do not differ between states on the coast and states off the coast;
  #Ha: The per capita energy consumptions are different between states on the coast and states off the coast.

#b.visual plot
  edata.c = subset(edata, Coast == 1, select = c(State, Coast, TotalEnergy, Population, TotalCoal))
  edata.c = mutate(edata.c, PerCapitaE = TotalEnergy/Population)
  edata.nc = subset(edata, Coast == 0, select = c(State, Coast, TotalEnergy, Population, TotalCoal))
  edata.nc = mutate(edata.nc, PerCapitaE = TotalEnergy/Population)
  TE.mean.c = mean(edata.c$PerCapitaE)
  TE.mean.nc = mean(edata.nc$PerCapitaE)
  
  hist(edata.c[,"PerCapitaE"], breaks = 15, col = rgb(0,0,1,0.35), xlim = c(0,1), ylim = c(0, 10),xlab = "Per Capita Energy Consumption", main = "")
  abline(v=TE.mean.c, col = "blue")
  abline(v=c((TE.mean.c+qt(0.975, 22)*sd(edata.c$PerCapitaE)/sqrt(23)),(TE.mean.c-qt(0.975, 22)*sd(edata.c$PerCapitaE)/sqrt(23)) ), col="blue",lty="dashed" )
  par(new=T)
  hist(edata.nc[,"PerCapitaE"], breaks = 15, col = rgb(0,1,0,0.35),xlim = c(0,1), ylim = c(0, 10), xlab = "", main = "")
  abline(v=TE.mean.nc, col = "dark green")
  abline(v=c((TE.mean.nc+qt(0.975, 27)*sd(edata.nc$PerCapitaE)/sqrt(28)),(TE.mean.nc-qt(0.975, 27)*sd(edata.nc$PerCapitaE)/sqrt(28)) ), col="dark green",lty="dashed" )
    legend("topright", legend = c("Coast states mean", "Off coast states mean", "--- 95% CI for coast states", "--- 95% CI for off coast states"), text.col = c("blue", "dark green"))
  
#c.statistics to use: T-test --> Wilcoxon-Mann-Whiteney U test OR resampling (no assumption)
  #Assumpetions for T-test
    #1) Random sampled: Unknown
    #2) independent observation: Unknown
    #3) continuous data: Yes
    #4) Normal Distribution OR sample size > 30: No. The data are not normally distributed and either of the sample sizes is smaller than 30.---> Wilcoxon-Mann-Whiteney U test
    
        shapiro.test(edata.c$PerCapitaE)
        #p-value = 5.037e-06 < 0.05 --> reject the H0 that the distribution is not different from normal distribution
        #-->(the distribution is different from normal distribution)
        shapiro.test(edata.nc$PerCapitaE)
        #p-value = 3.627e-05 < 0.05 --> reject the H0 that the distribution is not different from normal distribution
        #-->(the distribution is different from normal distribution)
        
        #(states on the coast) The sample size is 23 < 30. 
        #(states off the coast) The sample size is 28 < 30
        
    #5) Equal variance: Yes.
        var.test(edata.c$PerCapitaE, edata.nc$PerCapitaE, alternative = "two.sided")
        #p-value = 0.5098 > 0.05, cannot reject the H0 (same variance) 
        #--> (the variances of the two data are not different)

#d.Run the statistics
  
  wilcox.test(edata.c$PerCapitaE, edata.nc$PerCapitaE, paired = FALSE)
  #p-value = 0.008417 < 0.05 --> reject H0 that the total energy consumption are not different between states on the coast and states off the coast
  #--> (the total energy consumption are different between states on the coast and states off the coast)
        
 
             
#Q2####
#a.H0: The per capita coal consumptions do not differ between states on the coast and states off the coast;
  #Ha: The per capita coal consumptions are different between states on the coast and states off the coast.
  
#b.visual plot
  edata.c = mutate(edata.c, PerCapitaC = TotalCoal/Population)
  edata.nc = mutate(edata.nc, PerCapitaC = TotalCoal/Population)
  TC.mean.c = mean(edata.c$PerCapitaC)
  TC.mean.nc = mean(edata.nc$PerCapitaC)
  
  hist(edata.c[,"PerCapitaC"], breaks = 15, col = rgb(0,0,1,0.5), xlim = c(0,1), ylim = c(0, 10),xlab = "Per Capita Coal Consumption", main = "")
  abline(v=TC.mean.c, col = "blue")
  abline(v=c((TC.mean.c+qt(0.975, 22)*sd(edata.c$PerCapitaC)/sqrt(23)),(TC.mean.c-qt(0.975, 22)*sd(edata.c$PerCapitaC)/sqrt(23)) ), col="blue",lty="dashed" )
  par(new=T)
  hist(edata.nc[,"PerCapitaC"], breaks = 15, col = rgb(0,1,0,0.35),xlim = c(0,1), ylim = c(0, 10), xlab = "", main = "")
  abline(v=TC.mean.nc, col = "dark green")
  abline(v=c((TC.mean.nc+qt(0.975, 27)*sd(edata.nc$PerCapitaC)/sqrt(28)),(TC.mean.nc-qt(0.975, 27)*sd(edata.nc$PerCapitaC)/sqrt(28)) ), col="dark green",lty="dashed" )
    legend("topright", legend = c("Coast states mean", "Off coast states mean", "--- 95% CI for coast states", "--- 95% CI for off coast states"), text.col = c("blue", "dark green"))

  
#c.statistics to use: Resampling (no assumption)
  #Assumpetions for Resamping: None
  
#d.Run the statistics
    #Resampling
    dif =c()
    
    for (i in 1:10000){
      dif[i] = mean(sample(edata.c[,"PerCapitaC"], 23, replace = T))-mean(sample(edata.nc[,"PerCapitaC"],28, replace = T))
      } 
    
    dif.mean=mean(dif)
    dif.sd=sd(dif)
    CI95.lo=sort(dif)[10000*0.025]
    CI95.hi=sort(dif)[10000*0.975]
    CI95=c(CI95.lo,CI95.hi) #-0.1929443 -0.0601542
    
    hist(dif, probability = T, xlim = c(-0.4,0) ,main = "distribution of differences between per capita coal consumption in states on the coast and states off the coast")
    lines(density(dif), col="red")
    abline(v=CI95, col="blue",lty="dashed" )
    abline(v=0, col="dark green", lwd=3)
    legend("topleft", legend = c("95% confidence interval", "0"), col = c("blue", "dark green"), text.col = c("blue", "dark green"), lty=c("dashed","solid"), bty="o")
    
    pval= 2*(length(which(dif>0))/length(dif)) 
    pval #p-value = 0
    #p-value<<0.05 which means we can reject H0
    #and the per capita energy consumption are different between the states on the coast and the states off the coast.
  
#Q3####    
#a.H0: The per capita coal consumptions do not differ among the regions;
  #Ha: At least one region has per capita coal consumptions that is different among the regions.
    
#b.visual plot  
  edata = mutate(edata, PerCapitaC = TotalCoal/Population)
  boxplot(PerCapitaC~Region, data = edata, xlab="Region", ylab="Per Capita Coal Consumption", main = "Per Capita Coal Consumption in Different Regions in the US")
    
#c.statistics to use: one-way ANOVA --> Kruskal-Wallis test (data not normally distributed)
  #Assumpetions for one-way ANOVA  
    #1) Random sampled: Unknown
    #2) Independent observation: Unknown
    #3) Normal distribution: No. --> Kruskal-Wallis test
      edata.s = subset(edata, Region == "South", select = c(State, Region, PerCapitaC))
      edata.w = subset(edata, Region == "West", select = c(State, Region, PerCapitaC))
      edata.e = subset(edata, Region == "East", select = c(State, Region, PerCapitaC))
      edata.mw = subset(edata, Region == "Midwest", select = c(State, Region, PerCapitaC))
      
      shapiro.test(edata.s$PerCapitaC) #p-value = 5.689e-05 < 0.05 --> reject the H0 that the distribution is not different from normal distribution
      #-->(the distribution is different from normal distribution)
      shapiro.test(edata.w$PerCapitaC) #p-value = 1.103e-05 < 0.05 --> reject the H0 that the distribution is not different from normal distribution
      #-->(the distribution is different from normal distribution)
      shapiro.test(edata.e$PerCapitaC) #p-value = 5.535e-06 < 0.05 --> reject the H0 that the distribution is not different from normal distribution
      #-->(the distribution is different from normal distribution)
      shapiro.test(edata.mw$PerCapitaC) #p-value = 0.0001758 < 0.05 --> reject the H0 that the distribution is not different from normal distribution
      #-->(the distribution is different from normal distribution)
     
      
    #4) Equal variance (Or equal sample size and variance differ < 3X): Yes.
      leveneTest(PerCapitaC~Region, data = edata)
      #P-value = 0.5202 > 0.05 --> cannot reject H0 (same variance among different regions)
      #--> (the variances of different groups are the same)
      
#d.Run the statistics
      kruskal.test(PerCapitaC~Region, data = edata)
      #P-value = 0.00042 < 0.05 --> reject H0 
      #-->the per capita coal consumption are different among regions
      
      install.packages("dunn.test")
      library(dunn.test)
      dunn.test(edata$PerCapitaC, edata$Region, method = "bonferroni") #dunn.test outputs both z-test-statistics for each pairwise comparison and the p-value = P(Z>|z|) for each.
      #The result indicates that the per capita coal consumption of region East is significantly different from region Midwest (p-value = 0.0001) and South (p-value = 0.0083), but not different from region West (p-value = 0.0673),
      #and the per capita coal consumptions are not different among region Midwest, South, and West.
      
      
#Q4####
#per capita coal consumption and per capita GDP
#a.Strong correlation?
  edata = mutate(edata, PerCapitaGDP = TotalGDP/Population)
  cor(edata$PerCapitaC, edata$PerCapitaGDP) #0.03598182
  #The correlation coefficient of per capita coal consumption and per capita GDP is 0.03598182
  #which means there are a weak, positive relationship between the two variables
  
#b.Why or why not strong?
  #Because the correlation coefficient could range from -1 (strong and negative) to +1 (strong and positive)
  #And 0.03598182 indicates a very weak positive relationship.

  
        
# Housing data for question 5-9####
hdata = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/TakeHomeQuiz/housingdata.csv", 
                   sep=",", fill=TRUE, header=TRUE)

#Q5####
#Check multi-collinearity using correlation and  variance inflation factor (VIF)
  library(lmtest)
  head(hdata)
  lm = lm(medv~crim+rm+age, data = hdata) 
  
  #correlation
  pairs(hdata[,c(1,6,7)])
  cor(hdata[,c(1,6,7)], use = "na.or.complete")
    #correlation coefficient of crim and rm: -0.1424577; |-0.1424577| < 0.5
    #correlation coefficient of crim and age: 0.4476638; 0.4476638 < 0.5
    #correlation coefficient of rm and age: -0.1878709; |-0.1878709| < 0.5
  
  #VIF
  vif = 1/(1-summary(lm)$r.squared)
  vif #2.439356 < 10
  
  #Based on the results of correlation and VIF, 
  #we could say that the potential bias that could result from multi-collinearity is not really high

#Q6####
  #medv~crim
    plot(medv~crim, data = hdata)
    abline(lm(medv~crim, data = hdata), col = "red")
    #Based on the plot, I think crim (per capita crime rate) might have a negative impact on medv (the median value of owner-occupied homes).
    #Because there is a trend in the data that the higher the crim is, the lower the medv is.
  
  #medv~rm
    plot(medv~rm, data = hdata)
    abline(lm(medv~rm, data = hdata), col = "red")
    #Based on the plot, I think rm (average number of rooms per dwelling) would have a positive impact on medv (the median value of owner-occupied homes).
    #Because there is a trend in the data that the higher the rm is, the higher the medv is.
    
  #medv~age
    plot(medv~age, data = hdata)
    abline(lm(medv~age, data = hdata), col = "red")
    #Based on the plot, I think age (proportion of owner-occupied units built prior to 1940) might have a slightly negative impact on medv (the median value of owner-occupied homes).
    #Because there is a trend in the data that the higher the age is, the lower the medv is, but some of the data do not fit the line pretty well.

#Q7####
  #Assumptions
    #1) There is a linear relationship between the dependend variable and the independent variables: Based on the plots in question 6, there seems to be a linear relationship.
    #2) Homoscedasticity of the errors/residuals: No.
        bptest(lm) #p-value = 8.811e-07 < 0.05 --> reject H0 (the errors have constant variance)-->(Heteroscedasticity)
    #3) Independence of the errors/residuals: No.
        dwtest(lm, alternative = c("two.sided")) #p-value < 2.2e-16 --> reject H0 (There are no autocorrelation presented in the errors) --> (There are autocorrelation presented in the errors)
    #4) Normality of the error/residuals distribution: No.
        shapiro.test(residuals(lm)) #p-value < 2.2e-16 --> reject H0 (The errors are normally distributed) --> (The errors are not normally distributed)

#Q8####
  lm = lm(medv~crim+rm+age, data = hdata)
  summary(lm)
  
  #interpretation: (Note that some of the assumptions are violated which could result in inaccurate p-values for the regression coefficients and indicates that there is room for improvement in the model)

  #(Estimate)
    #the estimate for the Intercept is -32.07178, 
      #which is the median value of owner-occupied homes when the per capita crime rate, the average number of rooms per dwelling, and the proportion of owner-occupied units built prior to 1940 all = 0, 
      #and the intercept for the regression line
      #the p-value <2e-16 < 0.05, which means the intercept is significantly different from 0
  
    #the estimate for the crim is -0.49045, 
      #which is the effect of the per capita crime rate on the median value of owner-occupied homes when controlling for other variables 
      #that every unit of increase in per capita crime rate would decrease the median value of owner-occupied homes by 0.49045 unit.
      #the p-value = 4.93e-05 < 0.05, which means this effect is significantly different from 0.
  
    #the estimate for the rm is 9.25306, 
      #which is the effect of the average number of rooms per dwelling on the median value of owner-occupied homes when controlling for other variables 
      #that every unit of increase in the average number of rooms per dwelling would increase the median value of owner-occupied homes by 9.25306 unit.
      #the p-value < 2e-16 < 0.05, which means this effect is significantly different from 0.
  
    #the estimate for the age is -0.03322, 
      #which is the effect of the proportion of owner-occupied units built prior to 1940 on the median value of owner-occupied homes when controlling for other variables 
      #that every unit of increase in the proportion of owner-occupied units built prior to 1940 would decrease the median value of owner-occupied homes by 0.03322 unit.
      #the p-value = 0.00202 < 0.05, which means this effect is significantly different from 0.

#Q9####

  #F-statistic: 214.9 on 3 and 448 DF,  p-value: < 2.2e-16 < 0.05 means that the model is significantly different from "no model"/randomness
  #Or, the probability that "no model" (the intercept) will have the result of the model is < 2.2e-16
  #However, the Adjusted R-squared:  0.5873 means the model explains 58.73% of the variance in the data which indicates the model fit the data fairly bit not very well because 41.27% of the variance of the data cannot be explained by the model
  #In addition, the assumption of homoscedasticity or errors, independence of errors, and normality of the error distribution are all violated which indicate that there is room for improvement in the model.
  #In conclusion, I would say that the model does not fit the data well.
