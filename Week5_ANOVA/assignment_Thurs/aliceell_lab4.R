data(CO2)
boxplot(uptake~Treatment*Type, data=CO2, xlab='treatment', ylab='uptake')

CO2.aov = aov(uptake~Treatment + Type, data = CO2) #Treatment and Type are our two independent variables

#Exercise 1
model.tables(CO2.aov, "effects", se=TRUE)$tables #The effects are equal, but influence the uptake in different directions (quebec causes uptake to increase, but mississippi cauess it to decrease)

#Exercise 2
summary(lm(uptake~Treatment + Type - 1, data=CO2)) 
#The first coefficient is treated as the intercept in this model
#The second coefficient shows that moving from nonchilled to chilled causes uptake to change (general trend)
#The third coefficient shows that moving from quebec to mississippi causes uptake to change (general trend)

