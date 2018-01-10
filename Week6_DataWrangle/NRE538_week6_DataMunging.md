# NRE538_Data Munging
Oscar Feng-Hsun Chang  
Week6  

We have seen some basic functions built in the _base_ package of __R__ (which means you don't have to install anything). However, there are several packages that allows you to clean/reorganize/munge the data in a more elegant and efficient fashion. They are [__reshape2__](https://cran.r-project.org/web/packages/reshape2/reshape2.pdf), [__plyr__](https://cran.r-project.org/web/packages/plyr/plyr.pdf),
[__dplyr__](https://cran.rstudio.com/web/packages/dplyr/vignettes/introduction.html), or 
[__tidyr__](https://cran.r-project.org/web/packages/tidyr/tidyr.pdf) packages. There even exists a cheat sheet for [__dplyr__ and __tidyr__](https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf) packages.

We've seen `order`, `subset`, `r/cbind`, and `merge` before. Let's explore some more handy functions in the __reshape2__ and __plyr__ packages.


```r
library(reshape2)
library(plyr)
library(magrittr)
```





```r
RY=read.csv(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_DataWrangling/RY.csv"), sep=",", header=T,comment.char="#")

id=read.csv(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_DataWrangling/sp_id.csv"), sep=",", header=T,comment.char="#")

trait=read.csv(text=getURL("https://raw.githubusercontent.com/OscarFHC/NRE538_GSRA/master/Labs/NRE538_DataWrangling/sp_stoi_traits.csv"), sep=",", header=T,comment.char="#")
```



Our goal is to organize the three data frames into the `goal` data frame.  
Following are some functions for organizing/cleaning/munging data in R. 


```r
head(goal)
```

```
##   sp2 sp1         RY1       RY2     avgRY    C.N.x    C.P.x   N.P.x
## 1   2   1 0.004167904 1.1385059 0.5713369 18.57258 435.5213 23.4497
## 2   3   1 0.007112204 0.9607456 0.4839289 18.57258 435.5213 23.4497
## 3   4   1 0.519281008 1.6451093 1.0821952 18.57258 435.5213 23.4497
## 4   5   1 0.004955600 0.9128606 0.4589081 18.57258 435.5213 23.4497
## 5   7   1 0.449101255 1.3225352 0.8858182 18.57258 435.5213 23.4497
## 6   8   1 0.883576608 0.1783871 0.5309819 18.57258 435.5213 23.4497
##       C.N.y    C.P.y    N.P.y
## 1 16.437039 365.5173 22.23742
## 2 12.359818 259.7180 21.01310
## 3 10.639614 161.1780 15.14885
## 4  7.129078 146.6461 20.57014
## 5 18.936076 363.5510 19.19885
## 6 10.896064 219.1580 20.11350
```

# mutate

Given a data frame, `mutate()` modifies, adds, or removes variables.


```r
head(RY)
```

```
##   sp1 sp2         RY1       RY2     avgRY
## 1   1   2 0.004167904 1.1385059 0.5713369
## 2   1   3 0.007112204 0.9607456 0.4839289
## 3   1   4 0.519281008 1.6451093 1.0821952
## 4   1   5 0.004955600 0.9128606 0.4589081
## 5   1   7 0.449101255 1.3225352 0.8858182
## 6   1   8 0.883576608 0.1783871 0.5309819
```

```r
RY = RY %>%
  mutate(avgry = (RY1+RY2)/2)
head(RY)
```

```
##   sp1 sp2         RY1       RY2     avgRY     avgry
## 1   1   2 0.004167904 1.1385059 0.5713369 0.5713369
## 2   1   3 0.007112204 0.9607456 0.4839289 0.4839289
## 3   1   4 0.519281008 1.6451093 1.0821952 1.0821952
## 4   1   5 0.004955600 0.9128606 0.4589081 0.4589081
## 5   1   7 0.449101255 1.3225352 0.8858182 0.8858182
## 6   1   8 0.883576608 0.1783871 0.5309819 0.5309819
```

```r
RY = RY %>%
  mutate(avgry = avgry/2)
head(RY)
```

```
##   sp1 sp2         RY1       RY2     avgRY     avgry
## 1   1   2 0.004167904 1.1385059 0.5713369 0.2856684
## 2   1   3 0.007112204 0.9607456 0.4839289 0.2419644
## 3   1   4 0.519281008 1.6451093 1.0821952 0.5410976
## 4   1   5 0.004955600 0.9128606 0.4589081 0.2294540
## 5   1   7 0.449101255 1.3225352 0.8858182 0.4429091
## 6   1   8 0.883576608 0.1783871 0.5309819 0.2654909
```

# subset

This allows you to `subset()` rows or `select` (this is an argument in `subset()`, NOT a function here!) columns.


```r
sp1 = RY %>%
  subset(sp1==1, select=c(RY1))
head(sp1)
```

```
##           RY1
## 1 0.004167904
## 2 0.007112204
## 3 0.519281008
## 4 0.004955600
## 5 0.449101255
## 6 0.883576608
```

This can also be achieved by the `filter()` (for rows) and `select()` (for columns) functions in the __dplyr__ package.

# merge/join

These two, by its name, do the *join* operation.
The `merge()` belongs to the __base__ package, and the `join()` belongs to __plyr__.


```r
RY.m = RY %>%
  merge(id, by.x="sp2", by.y="id") %>%
  merge(id, by.x="sp1", by.y="id") %>%
  arrange(sp1)

colnames(RY.m)[c(7:8)]=c("sp2_name", "sp1_name")
head(RY.m)
```

```
##   sp1 sp2         RY1       RY2     avgRY     avgry               sp2_name
## 1   1   3 0.007112204 0.9607456 0.4839289 0.2419644    Chlamydocapsa ampla
## 2   1  14 0.143032727 1.2666866 0.7048597 0.3524298  Monoraphidium minutum
## 3   1   4 0.519281008 1.6451093 1.0821952 0.5410976 Chlamydomonas moewusii
## 4   1   2 0.004167904 1.1385059 0.5713369 0.2856684 Botryococcus sudeticus
## 5   1  21 0.082254410 0.9185357 0.5003950 0.2501975 Scenedesmus acuminatus
## 6   1  13 0.566499546 0.3548113 0.4606554 0.2303277 Golenkinia minutissima
##                  sp1_name
## 1 Ankistrodesmus falcatus
## 2 Ankistrodesmus falcatus
## 3 Ankistrodesmus falcatus
## 4 Ankistrodesmus falcatus
## 5 Ankistrodesmus falcatus
## 6 Ankistrodesmus falcatus
```

In the `join()`, we can specify the `type`, which is the same with the `all` argument in the `merge()` function. Try `? join` to understand it. 


```r
colnames(trait)[1] = "sp"
trait.1 = trait %>%
  join(id, by="sp", type="left")
  # merge(id, by="sp", all.x=TRUE) This and the above arguments will do the same thing.
head(trait.1)
```

```
##                        sp      C.N      C.P      N.P id
## 1 Actinastrum hantzschii  10.53468 235.5864 22.36293 NA
## 2 Ankistrodesmus falcatus 18.57258 435.5213 23.44970  1
## 3 Arthrodesmus convergens 15.25011 481.3848 31.56598 NA
## 4   Asterococcus superbus 19.00790 357.0712 18.78541 NA
## 5    Botryococcus braunii 17.72712 423.0099 23.86231 NA
## 6  Botryococcus sudeticus 16.43704 365.5173 22.23742  2
```

Let's merge the two data frames but exclude species that have no trait information. We should have the following data frame. 


```
##                         sp       C.N      C.P      N.P id
## 1  Ankistrodesmus falcatus 18.572578 435.5213 23.44970  1
## 2   Botryococcus sudeticus 16.437039 365.5173 22.23742  2
## 3      Chlamydocapsa ampla 12.359818 259.7180 21.01310  3
## 4   Chlamydomonas moewusii 10.639614 161.1780 15.14885  4
## 5    Chlorella sorokiniana  7.129078 146.6461 20.57014  5
## 6 Closteriopsis acicularis 18.936076 363.5510 19.19885  7
```

# ddply

This is a very useful function that allows you to apply a function to a data frame and reture a data frame back.  
I always use this to summarize my own data. 


```r
sum = RY.m %>%
  ddply(~sp1, summarize,
        mean=mean(RY1),
        med=quantile(RY1, 0.5),
        lo=quantile(RY1, 0.05),
        hi=quantile(RY1, 0.95))
sum
```

```
##    sp1      mean        med          lo        hi
## 1    1 0.2898778 0.17751278 0.004916215 0.9034076
## 2    2 0.9827241 0.93150681 0.718204608 1.4055848
## 3    3 0.8541042 0.90856213 0.442758034 1.1683561
## 4    4 0.7166386 0.70336081 0.054399857 1.2965328
## 5    5 0.2924596 0.08802885 0.028070055 0.8381911
## 6    7 0.6433982 0.27215109 0.017227364 1.9947887
## 7    8 0.1618722 0.06555914 0.013586022 0.5179500
## 8    9 0.6887810 0.57784979 0.114829614 1.4363793
## 9   10 0.6938790 0.58021739 0.160945652 1.3804556
## 10  11 0.7865976 0.81150458 0.213351056 1.4043605
## 11  12 0.9732537 0.96997228 0.648571429 1.3483598
## 12  13 0.4190639 0.19459916 0.011743253 1.1569805
## 13  14 0.6739349 0.65758755 0.156050176 1.3208471
## 14  16 0.6139744 0.64942358 0.138329708 1.2701913
## 15  17 0.9132902 0.94729067 0.683282582 1.1070749
## 16  18 0.5401754 0.55012667 0.218512486 0.7728944
## 17  19 0.3379909 0.33799089 0.047359639 0.6286221
## 18  20 1.5665162 1.56651623 1.063076587 2.0699559
## 19  21 0.9117685 0.91176848 0.760318363 1.0632186
## 20  22 1.2263210 1.22632104 1.219591227 1.2330508
## 21  23 0.9479769 0.94797688 0.947976879 0.9479769
```

# melt/cast

`melt` function can make a data frame from "wide" form into a "long" form.


```r
RY.long = RY %>%
  melt(id.vars=c("sp1", "sp2"), measure.vars=c("RY1", "RY2"), variable.name="RY_type", value.name="value")
head(RY.long)
```

```
##   sp1 sp2 RY_type       value
## 1   1   2     RY1 0.004167904
## 2   1   3     RY1 0.007112204
## 3   1   4     RY1 0.519281008
## 4   1   5     RY1 0.004955600
## 5   1   7     RY1 0.449101255
## 6   1   8     RY1 0.883576608
```

`cast` function can do the reverse of `melt`


```r
RY.wide = RY.long %>%
  dcast(sp2+sp1~RY_type)
head(RY.wide)
```

```
##   sp2 sp1         RY1        RY2
## 1   2   1 0.004167904 1.13850585
## 2   3   1 0.007112204 0.96074558
## 3   3   2 0.739350695 0.11359761
## 4   4   1 0.519281008 1.64510930
## 5   5   1 0.004955600 0.91286058
## 6   5   3 0.898026283 0.02287871
```

# The GOAL

1. Your turn. Use what we've learned to generate the `goal` data frame. 


```
##   sp1 sp2         RY1       RY2     avgRY               sp2.name   C.N.sp2
## 1   1   3 0.007112204 0.9607456 0.4839289    Chlamydocapsa ampla 12.359818
## 2   1   5 0.004955600 0.9128606 0.4589081  Chlorella sorokiniana  7.129078
## 3   1  14 0.143032727 1.2666866 0.7048597  Monoraphidium minutum 10.481420
## 4   1  13 0.566499546 0.3548113 0.4606554 Golenkinia minutissima 10.656880
## 5   1  11 0.191188274 0.4217485 0.3064684     Cosmarium botrytis 15.734797
## 6   1   2 0.004167904 1.1385059 0.5713369 Botryococcus sudeticus 16.437039
##    C.P.sp2  N.P.sp2                sp1.name  C.N.sp1  C.P.sp1 N.P.sp1
## 1 259.7180 21.01310 Ankistrodesmus falcatus 18.57258 435.5213 23.4497
## 2 146.6461 20.57014 Ankistrodesmus falcatus 18.57258 435.5213 23.4497
## 3 223.5869 21.33174 Ankistrodesmus falcatus 18.57258 435.5213 23.4497
## 4 147.8191 13.87076 Ankistrodesmus falcatus 18.57258 435.5213 23.4497
## 5 417.7917 26.55209 Ankistrodesmus falcatus 18.57258 435.5213 23.4497
## 6 365.5173 22.23742 Ankistrodesmus falcatus 18.57258 435.5213 23.4497
```

In addition, if you are dealing with large dataset, [__data.table__](https://cran.r-project.org/web/packages/data.table/index.html) package will be your good friend!

2. Conduct some analysis to answer the following questions:   
2.1 Does different species have different relative yield (RY)? _hint_: ANOVA  
2.2 Does the RY of ones species depend on the other species? _hint_: interaction term  
2.3 Does different species have different C:N, C:P, N:P ratio? _hint_: ANOVA or MANOVA

