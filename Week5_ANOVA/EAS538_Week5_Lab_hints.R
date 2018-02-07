#############
# Factors...

# Factors are:
#   - Used to represent categorical data;
#   - Can be ordered or unordered.

# Understanding them is necessary for statistical analysis and for plotting.
# Factors are stored as integers, and have labels associated with these unique integers.
# While factors look (and often behave) like character vectors, they are actually integers under the hood,
#   and you need to be careful when treating them like strings.

times <- c('night', 'day', 'night')
study.times <- factor(times)

# Once created, factors can only contain a pre-defined set of values, known as levels.
# By default, R always sorts levels in alphabetical order.
# R assigns the "day" level the integer value 1 even though "night" appears first in the vector.

levels(study.times)
nlevels(study.times)

# By default factors are *unordered,* which makes some kind of comparison and aggregation meaningless.

spice <- factor(c('low', 'medium', 'low', 'high'))
levels(spice)
max(spice)

# The easiest way to create an ordered factor is with the ordered() function;
#   the levels argument is interpreted as the order of the levels

spice <- ordered(spice, levels = c('high', 'medium', 'low'))
max(spice)

####################################
# Conceptual notes (Re: Week 4 lab)

# - The conceptual basis of significance tests: EXTREME values
#   - Flint lead example, a one-tailed t-test
#   - t-distribution a distribution of the possible t-statistics we could have obtained from any sample
# - The null hypothesis as the hypothesis of "no difference"

##########################################
# The perils of multiple testing, example

apdata <- read.csv('https://raw.githubusercontent.com/arthur-e/EAS538_2018/master/Week3_CLT/aps.csv')

set.seed(1492)
samples <- matrix(nrow = 100, ncol = 2)
for (i in 1:100) {
  a.sample <- sample(apdata$concen, size = 5000)
  samples[i,1] <- mean(a.sample, na.rm = T)
  samples[i,2] <- sd(a.sample, na.rm = T)
}

hist(samples[,1])

set.seed(1492)
p.values <- list()
for (i in 1:100) {
  p.values[[i]] <- t.test(x = sample(apdata$concen, size = 5000), mu = 0.1477,
    alternative = 'two.sided')$p.value
}
p.values <- unlist(p.values)

# Bonferroni correction
hist(p.values)
table(p.values < 0.05) # "Significant" how many times?
table(p.values < (0.05 / 100)) # "Significant" how many times?

#############################
# Multiple testing and ANOVA

CO2_sub <- subset(CO2, Plant %in% c("Qn2", "Qc2", "Mn3"))

summary(aov(uptake ~ Plant, data=CO2_sub))

t.test(CO2_sub[which(CO2_sub$Plant %in% c("Qn2")),"uptake"],
       CO2_sub[which(CO2_sub$Plant %in% c("Qc2")),"uptake"])

t.test(CO2_sub[which(CO2_sub$Plant %in% c("Qn2")),"uptake"],
       CO2_sub[which(CO2_sub$Plant %in% c("Mn3")),"uptake"])

t.test(CO2_sub[which(CO2_sub$Plant %in% c("Qc2")),"uptake"],
       CO2_sub[which(CO2_sub$Plant %in% c("Mn3")),"uptake"])

with(droplevels(CO2_sub), boxplot(uptake ~ Plant))