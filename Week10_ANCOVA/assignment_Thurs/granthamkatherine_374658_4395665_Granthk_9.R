#Exercise 1
gz=read.table((text=
                 "https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_ANCOVA_n_Interaction/ipomopsis.txt" ), sep = "", header = T, comment.char = "#")
mod1=lm(Fruit~Root*Grazing, data = gz)
summary(mod1)

#When looking at the output of this summary table after running our ANCOVA, we can see a lot of different things. 
#First of all, we see that the interaction between root length and grazing is not significant. 
#Therefore, root lenght does not differ when it is grazed or ungrazed.We also see from this output table that 
#the effect of root length on fruit weight is significant, as can be shown by the small p-value in the output table. 
#We also see from the output table that the effect of grazing is not significant, due to the fact that the p-value is not significant. 

library(interplot)
interplot(m=mod1, var1="Root", var2="Grazing")+
  labs(x="Grazing vs Ungrazing", y="Estimate Coeffcient of Root Length", title="Estimated Coefficient of Root Length on Grazing")

interplot(m=mod1, var1="Grazing", var2="Root")+
  labs(x="Root Length", y="Estimated coefficient for Grazing", title="Estimated Coefficient of Grazing on Root Length")


#When analyzing these plots, we see that the effect of root length on fruit weight is not significant between grazed and ungrazed plots. 
#In the second plot, this can be visualized from the line in the graph being fairly straight. 

#Exercise 2
data("airquality")
mod1 = lm(Temp~Wind + Ozone, data=airquality)
mod2 = lm(Temp~Wind * Ozone, data=airquality)

anova(mod1, mod2)

#When running the F-Test to comapre model 1 to model 2, we see that it may be better to use model 2. 
# The F statistic is high (17.085) and the p-value is significant. 
#From this we see that model 2 explains a lot more variation in the model than model 1 would. 