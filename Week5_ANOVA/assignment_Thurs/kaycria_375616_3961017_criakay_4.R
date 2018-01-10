data(CO2)
CO2.aov = aov(uptake~Treatment + Type, data = CO2)

##Excersize One: 
model.tables(CO2.aov, "effects", se=TRUE)$tables$Treatment
model.tables(CO2.aov, "effects", se=TRUE)$tables$Type
##Each effect is the grand mean minus the mean of each treatment/type (effects are the differences of the mean of each treatment from the grand mean)

##Excersize Two:
CO2.lm = lm(uptake~Treatment + Type, data=CO2)
summary(CO2.lm)

##Results
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)        36.973      1.536  24.065  < 2e-16 ***
  #Treatmentchilled   -6.860      1.774  -3.867 0.000222 ***
  #TypeMississippi   -12.660      1.774  -7.136 3.68e-10 ***
##The treatment Chilled is significantly smaller than non-chilled (found by taking the difference between chilled and non-chilled) and and the Type Mississippi is significantly less than Type Qubec (found by taking the difference between Qubec and Mississippi)