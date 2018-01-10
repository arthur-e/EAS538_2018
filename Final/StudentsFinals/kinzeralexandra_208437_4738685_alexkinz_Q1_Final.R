#1)	Is there a significant association between gender (gender) 
#and whether people think it’s rude to bring an unruly child on the 
#plane (unruly_child)? If yes, which gender tends to think that 
#bringing an unruly child is more rude? flying

flying = read.table(file="https://raw.githubusercontent.com/OscarFHC/NRE538_2017Fall/master/Final/flying.csv",header=TRUE, sep=",")

#gender is categorical (nominal)
#rude to take baby is categorical (ordinal)

#Chi square

tbl = table(flying$gender, flying$unruly_child)
tbl
chisq.test(tbl)

#Pearson's Chi-squared test

#data:  tbl
#X-squared = 13.463, df = 2, p-value = 0.001193

#Reject the null hypothesis, there is an association between gender and whether people think it is rude to bring an unruly child flying
#More men (~47%) than women (~36%) think it is very rude, (~39% of men and ~44% of women say it is somewhat rude, men)