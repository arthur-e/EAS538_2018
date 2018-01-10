# Exercise 1
data("faithful")
head(faithful,10)

plot(eruptions~waiting,data=faithful)
abline(lm(eruptions~waiting,data=faithful))
cor(faithful[,c("eruptions","waiting")])
# The correlation coefficient between the waiting time between eruptions and the duration of the eruption for the Old Faithful geyser in Yellowstone National Park = 0.9008112
# The correlation coefficient shows that when we observe waiting time that increases one unit, we have the probability of 0.9008112 to observe that the duration of the eruption also increases a specific unit based on the slope of the line

# Exercise 2.1
data("airquality")
head(airquality,15)
lm1=lm(Temp~Ozone,data=airquality)
summary(lm1)
plot(Temp~Ozone,data=airquality)
abline(lm(Temp~Ozone,data=airquality),col="red")

# Exercies 2.2
lm2=lm(eruptions~waiting,data=faithful)
summary(lm2)
plot(eruptions~waiting,data=faithful)
abline(lm(eruptions~waiting,data=faithful),col="red")
# eruptions= -1.874016 + 0.075628*waiting
# The positive correlation (slope=0.075628) shows that if we wait longer, we will see longer eruption time.

# Exercise 3
res=residuals(lm1)
RSS=sum(res^2)                   #RSS=5300.725
RSE=sqrt(RSS/summary(lm1)$df[2]) #RSE=6.818914
MSE=RSS/summary(lm1)$df[2]       #MSE=46.49759