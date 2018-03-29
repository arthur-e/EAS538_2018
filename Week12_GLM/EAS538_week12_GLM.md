---
title: "EAS 538 Generalized Linear Model"
author: "Written by Oscar Feng-Hsun Chang and Meha Jain and modified by Arthur Endsley"
date: "Week12"
output: 
  html_document:
    code_folding: show
    highlight: textmate
    keep_md: yes
    number_sections: true
    theme: flatly
    toc: yes
    toc_float:
      collapsed: false
      smooth_scroll: true
      toc_depth: 4
---

\newcommand\expect[1]{\mathbb{E}\left[{#1}\right]}
\newcommand\var[1]{\mathrm{Var}\left[{#1}\right]}





# Rationale

We talked about linear models when dealing with data that followed a normal distribution. However, mother nature is not always that nice to us... Luckily, we have lots of brilliant mathematicians and statisticians who developed __generalized linear model (GLM)__ that can be used when encountering non-normally distributed data.

The idea of GLM is to use a link function to "link" (or your can think of it as being transformed) the distribution mean $\mu$, that the dependent variable (Y) follows to the outcome of linear predictors (independent variables, or X). The model can be written in the following form:

$$Y_i\ \stackrel{\text{i.i.d.}}{\sim}\ f(\mu,\ \dots)$$

$$ g(\mu)\ = \ X\,\beta_i$$

- The $f$ is the Error distribution, BUT! it is actually the distribution of your dependent variable (i.e. the **data distribution**)... Theoretically, this can be ANY distribution.
- The $g$ is the **link function**. 

To determine what data distribution to use requires you to understand how your data are being generated. For example, it would be logical to use the [binomial distribution](https://en.wikipedia.org/wiki/Binomial_distribution) as the data distribution if you are dealing with a yes-no (binary) data set. To determine what link function to use requires **knowledge of the data**, **theoretical consideration**, and **empirical fit** to the data. Most of the time, there is a typical link function that goes together with the specified data distribution. However, always keep in mind that __chosing the data distribution and the link function are just two pieces of the model__. It is just like figuring out what independent variables to include in your model. 

The following two posts are pretty clear in explaining the link function and some conceptual theory of GLM. I encourage you to read them.  
[GLM](http://stats.stackexchange.com/questions/40876/difference-between-link-function-and-canonical-link-function-for-glm)  
[link function](http://stats.stackexchange.com/questions/20523/difference-between-logit-and-probit-models#30909)  

In the generalized linear model, the dependent variable does not need to follow the normal distribution (i.e. $f$ does not need to be a normal distribution). We will not get into the detailed mathematical theory of the generalized linear function, but just the application of this `glm` function in this class. 

# GLM Application

## Binomial data (logistic regression)

We will use a dataset about whether a household in India switched to using a different well for drinking water after arsenic was detected in their original well. The data set is from @gelman2006 (http://www.stat.columbia.edu/~gelman/arm/). Let's load in the dataset and see what variables are included.


```r
well=read.csv("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_GLM/wells.csv", sep=",", header=T,comment.char="#")
```
- `switch`: Whether the household switched to a new, clean well (1 = yes, 0 = no)  
- `arsenic`: Natural arsenic concentration in well water in ppm.  
- `dist`: The distance of the household to the closest known safe well in 100 meter    
- `assoc`: Whether any members of the household are active in community organizations (1 = yes, 0 = no).  
- `educ`: The education level of the head of household.  

Let's do some exploratory analyses. 

```r
cor(well)
pairs(well)
```

It looks like we don't have any issues with multicollinearity, so let's move on to actually doing the analysis! 

Since the dependent variable is a binary variable (move = 1 or stay = 0), it should follow a binomial distribution. This means the $f$ function in this case is a binomial distribution. The link function (or the 'transform') that is most commonly used with a binomial distribution is the [`logit` function](https://en.wikipedia.org/wiki/Logit). 

Just as we saw in lecture, the logit model transforms Y to be log(odds), and the model formula can be written as the following.

$$ \text{Prob}(Y_i=1) = p_i$$  

$$ \text{logit}(p_i) = log\ (\frac{P_i}{1-P_1})= X_i \ \beta$$  


Here the $X_i$ are the linear predictors. For now, let's develop a model that estimates whether a household moves or not depending on the arsenic level found in their well.

We can use the following code to fit a GLM to the data. 

```r
mod1 = glm(switch~arsenic, data=well, family=binomial(link="logit"))
summary(mod1)
```

The function pretty much looks like an `lm` model in __R__, but we have to specify two additional parameters: the `family` and the `link` function. The family is just the distribution your y variable follows (in this case, the binomial distribution) and the `link` is a logit (the typical 'transform' that is used for a binomial distribution). If you want, you can leave the `link` parameter empty and __R__ will use the default link that is typically associated with a given `family` of distributions. 


```r
mod2 = glm(switch~arsenic,data=well,family=binomial)
summary(mod2)
```
See - same answer!

Remember from lecture that the way we interpret this model is by backtransforming our coefficients using the log(odds) equation shown above? Let's do that now to understand and interpret the coefficient on arsenic properly.


```r
summary(mod1)
```
Okay, so the coefficient on arsenic is 0.37914. What does this mean?? We can interpret this as the log odds of a household moving is 0.38 for every ppm of arsenic detected in the well water. This is a mouthful, but please notice that this interpretation is EXACTLY the same as how you would interpret the coefficient if we were using a normal linear regression, however, instead of predicting y (whether a household moves) we are predicting the log(odds) of y. Let's convert this result into something a bit more interpretable. 

First, let's exponentiate the coefficient to remove the log from log(odds). 

```r
exp(0.37914)
```
What this number now means is that the odds of moving increase by 1.46 for every ppm of arsenic in the water. So you are 1.46 times more likely to move for every ppm increase in arsenic in the water. 

Sometimes people like representing these results as percents because that may be even more interpretable when writing up your results. So right now we know odds = 1.46. Remember the odds are calculated as the probability the household moves/the probability the household does not move (p/(1-p)). So we can think of the odds = 1.46 = 1.46/1, which means the odds of a household moving is 1.46 and the odds of a household staying is 1. How much greater is 1.46 than 1 in percentage terms? That's 1.46 - 1 = 0.46* 100% = 46%. So another way to interpret your result is for every ppm increase in arsenic, a household is 46% more likely to move. 

Alright, now you try!

**Exercise 1 (R output required)**          
1. Please run a logistic regression where you estimate whether a household moves based on `arsenic`, `dist`, `assoc`, and `educ`. Which variables are significantly associated with whether a household moves or not (don't interpret the variables at this stage)?    
2. Now let's do the hard part! Backtransform all of your coefficients (on `arsenic`,`dist`,`assoc`,and `educ`) and write one sentence for each interpreting your coefficient in terms of odds. Please note the unit of x when you are writing out this sentence. As a reminder, `arsenic`, `dist`, and `educ` are continuous variables and `assoc` is a categorical variable (whether a household belonged to a communitity association, coded as 1, or not, coded as 0).    
3. Now modify the sentences above to state the % change in whether a household moves or not per unit change in each of the x variables. I know questions 2 + 3 may be challenging so please ask the GSIs for help! 

---------------------------------------------------------------------------------------------------------------------------------

## Count data (Poisson distribution)

The second example is to model count data using a [Poisson distribution](https://en.wikipedia.org/wiki/Poisson_distribution). The data come from the [**faraway** package](http://people.bath.ac.uk/jjf23/ELM/). Let's load in the data and see what they look like. 


```r
library(faraway) # remember to install this package first!
data(gala)
head(gala)
```
The dataset contains the following variables about the species diversity on the Galapagos Islands:   
- `Species` the number of plant species found on each island    
- `Endemics` the number of endemic species found on each island    
- `Area` the area of each island (km$^2$)    
- `Elevation` the highest elevation on the island (m)   
- `Nearest` the distance to the nearest island (km)   
- `Scruz` the distance from Santa Cruz island (km)   
- `Adjacent` the area of the adjacent island (km$^2$)   

Let's say we want to create a model that estimates the number of species on each island. What happens if we run a simple linear model using these data? First, let's check for multicollinearity. 


```r
cor(gala,use='complete.obs')
pairs(gala)
```
It looks like there are some variables that are highly correlated! In particular, `Endemics`, `Area`, and `Elevation` are very correlated with one another and `Nearest` and `Scruz` are correlated with one another. Let's think through which variables we should drop and which we should include in the model. From the first set of variables, let's select `Area`. This is because it doesn't really make sense to estimate the total number of `Species` based on the number of `Endemics` on an island and I care more about understanding the relationship between `Area` and number of `Species` than the relationship between `Elevation` and `Species.` For the second set of variables, let's select `Nearest` since I care more about how far an island is from *any* island and not just Santa Cruz. 

Okay, let's run the linear model!

```r
mod3 = lm(Species~Area+Nearest+Adjacent,data=gala)
summary(mod3)
```
But wait! Before interpreting the result, let's do some model checking.


```r
plot(mod4)
```
Man - those plots look BAD! This suggests that the linear model isn't fitting the data well. Let's look at what the distribution of the y variable looks like to get a better understanding of why this may be happening. 


```r
hist(gala$Species)
```
Whoa! Those data are NOT normally distributed. Thinking back to the different data distributions Meha showed in class, what distribution does it look like the data follow? They could fit either a Poisson or a Gamma distribution, right? Which one would we choose? Well, let's think about what our y variable is. We are measuring the number of species on each island, which is a *count* variable. We also know that since it is the number of species on an island, the number must be an integer (i.e., we can't have 1.2 species on an island). Since the data are skewed (as shown in the histogram), count data, and integer data, let's go with the Poisson distribution.

We can build a glm with a Poisson distribution.


```r
mod4 = glm(Species ~ Area+Nearest+Adjacent, data=gala, family=poisson(link="log"))
summary(mod4)
```

**Exercise 2 (No R Output Required)**       
1. Compare the results from `mod3` and `mod4` using `summary`. Which variables are significant using the linear model framework? Which variables are significant using the glm with poission distribution? This suggests that making sure you get appropriate model fit is important because it can change the results of your model!      
2. *EXTRA CREDIT* - For 0.5 points extra credit, interpret each of the coefficients in your Poisson model. The interpretation is NOT the same as the logistic regression because we used a different link transform. You can google how to interpret a possion coefficient and read this link (http://environmentalcomputing.net/interpreting-coefficients-in-glms/) to get an idea of how to backtransform the coefficient from your Poisson GLM to get a meaningful interpretation of your coefficient.   

By the way - we did not cover how to check model fit for GLMs to make sure you are using the appropriate model for your data. Doing this is outside of the scope of this course, but if you'd like to use GLMs in any of your work please please please read the Bolker textbook Meha keeps showing in lecture. You can find a free copy of the .pdf here (https://ms.mcmaster.ca/~bolker/emdbook/book.pdf).

