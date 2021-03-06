# Lab1_Natural Resources Stats
Feng-Hsun Chang  
January 4 and 5, 2018  

# About me, your GSI

* Feng-Hsun (Oscar) Chang @ Bradley Cardinale's lab
* fhchang@umich.edu
* Office hour : Mon/Fri 4:00-5:00 PM; 1556 Dana
    + Conceptual questions
    + R programing 

# You?

* Name (I'll try my best to remember!)
* What do you want to get from this class?

# ["Stereotype Threat: A Summary of the Problem"](https://dynamicecology.wordpress.com/2014/04/28/stereotype-threat-a-summary-of-the-problem/)

Don't stress out...

![](D:/Courses/UM/2018_WN/ESA538/wk1/giphy.gif)

# Some logistics

* Syllabus
* [Piazza](https://piazza.com/class/jbzluswc2kg1h7)
    + enrollment
    + Post questions/answers/comments/ideas
    + Post in the right folder
* Public PC or your own laptops are all good
* [R](https://www.r-project.org/) : Both the programming language and the software that interprets the scripts written using it. 
* [Rstudio](https://www.rstudio.com/) : A very popular way to not only write your R scripts but also to interact with the R software.
* [Rmarkdown](http://rmarkdown.rstudio.com/) : An interface to weave together narrative text and code to produce elegantly formatted output, like the one you are seeing now. 

# Creating Objects

_object_ -> _value_

Now enter my weight in metric system. 


```r
weight_kg <- 75
weight_kg
```

```
## [1] 75
```

Now we can use `weight_kg` to do further calculation.


```r
weight_lb = weight_kg/0.454
```

Now we have my weight in pounds.

> ### Challenge
>
> What are the values after each statement in the following?
>
> 
> ```r
> mass <- 47.5            # mass?
> age  <- 122             # age?
> mass <- mass * 2.0      # mass?
> age  <- age - 20        # age?
> mass_index <- mass/age  # mass_index?
> ```

# Functions and their arguments

Functions are "canned scripts" that automate more complicated sets of commands
including operations assignments, etc. A function 
usually gets one or more inputs called *arguments*. Functions often (but not
always) return a *value*, NOT an *object*.



```r
b <- sqrt(36)
b
```

```
## [1] 6
```

`sqrt()` is a function. It returns a value of 6 (square root of 36). This calue is being assigned to an object, b.


```r
rnorm(n=5, mean=3, sd=1)
```

```
## [1] 4.880516 3.604285 2.140714 4.421854 2.523946
```

`rnorm()` is another function generating random numbers from a normal distribution. The *n*, *mean*, and *sd* are the arguments needed in for this function. 

# Data type / data structure

The basic data types in __R__ are  
* `integer`,   
* `numeric` (real numbers)  
* `logical` (TRUE or FALSE)     
* `character` (alphanumeric strings)  
__R__ organizes these data into __vectors__ of one of these types.  

In addition to __vectors__. Other important data structures are  
* lists (`list`)  
* matrices (`matrix`)  
* data frames (`data.frame`)  
* factors (`factor`)  
* arrays (`array`)

`str()` and `class()` can be used to check the data type/structure.


```r
str(weight_kg)
```

```
##  num 75
```


```r
weights <- c(weight_kg, 80, 100, 120)
weights
```

```
## [1]  75  80 100 120
```


```r
weight_chr <- c("Oscar", "Future Oscar")
weight_chr
```

```
## [1] "Oscar"        "Future Oscar"
```


> ### Challenge
>
>
> * Weâve seen that vectors can be of type character,
>   numeric (or double), integer, and logical. But what happens if we try to mix these types in
>   a single vector?
> <!-- * _Answer_: R implicitly converts them to all be the same type -->
>
> * What will happen in each of these examples? (hint: use `class()`
>   to check the data type of your objects):
>
>     ```r
>     num_char <- c(1, 2, 3, "a")
>     num_logical <- c(1, 2, 3, TRUE)
>     char_logical <- c("a", "b", "c", TRUE)
>     tricky <- c(1, 2, 3, "4")
>     ```
>
> * Why do you think it happens?
> <!-- * _Answer_: Vectors can be of only one data type. R tries to convert (coerce)
>   the content of this vector to find a "common denominator". -->
>
> * You've probably noticed that objects of different types get
>   converted into a single, shared type within a vector. In R, we
>   call converting objects from one class into another class
>   _coercion_. These conversions happen according to a hierarchy,
>   whereby some types get preferentially coerced into other
>   types. Can you draw a diagram that represents the hierarchy of how
>   these data types are coerced?
> <!-- * _Answer_: `logical -> numeric -> character <-- logical` -->

# Subsetting vectors

If we want to extract one or several values from a vector, we must provide one
or several indices in square brackets. For instance:

```r
animals <- c("mouse", "rat", "dog", "cat")
animals[2]
```

```
## [1] "rat"
```

```r
animals[c(3, 2)]
```

```
## [1] "dog" "rat"
```

We can also repeat the indices to create an object with more elements than the
original one:


```r
more_animals <- animals[c(1, 2, 3, 2, 1, 4)]
more_animals
```

```
## [1] "mouse" "rat"   "dog"   "rat"   "mouse" "cat"
```

## Conditional subsetting

Another common way of subsetting is by using a logical vector. `TRUE` will
select the element with the same index, while `FALSE` will not:


```r
weight_g <- c(21, 34, 39, 54, 55)
weight_g[c(TRUE, FALSE, TRUE, TRUE, FALSE)]
```

```
## [1] 21 39 54
```

Typically, these logical vectors are not typed by hand, but are the output of
other functions or logical tests. For instance, if you wanted to select only the
values above 50:


```r
weight_g > 50    # will return logicals with TRUE for the indices that meet the condition
```

```
## [1] FALSE FALSE FALSE  TRUE  TRUE
```

```r
## so we can use this to select only the values above 50
weight_g[weight_g > 50]
```

```
## [1] 54 55
```

You can combine multiple tests using `&` (both conditions are true, AND) or `|`
(at least one of the conditions is true, OR):


```r
weight_g[weight_g < 30 | weight_g > 50]
```

```
## [1] 21 54 55
```

```r
weight_g[weight_g >= 30 & weight_g == 21]
```

```
## numeric(0)
```

Here, `<` stands for "less than", `>` for "greater than", `>=` for "greater than
or equal to", and `==` for "equal to". The double equal sign `==` is a test for
numerical equality between the left and right hand sides, and should not be
confused with the single `=` sign, which performs variable assignment (similar
to `<-`).

A common task is to search for certain strings in a vector.  One could use the
"or" operator `|` to test for equality to multiple values, but this can quickly
become tedious. The function `%in%` allows you to test if any of the elements of
a search vector are found:


```r
animals <- c("mouse", "rat", "dog", "cat")
animals[animals == "cat" | animals == "rat"] # returns both rat and cat
```

```
## [1] "rat" "cat"
```

```r
animals %in% c("rat", "cat", "dog", "duck", "goat")
```

```
## [1] FALSE  TRUE  TRUE  TRUE
```

```r
animals[animals %in% c("rat", "cat", "dog", "duck", "goat")]
```

```
## [1] "rat" "dog" "cat"
```

# Read in data

We are studying the species and weight of animals caught in plots in our study
area. The dataset is stored as a comma separated value (CSV) file.
Each row holds information for a single animal, and the columns represent:

| Column           | Description                        |
|------------------|------------------------------------|
| record\_id       | Unique id for the observation      |
| month            | month of observation               |
| day              | day of observation                 |
| year             | year of observation                |
| plot\_id         | ID of a particular plot            |
| species\_id      | 2-letter code                      |
| sex              | sex of animal ("M", "F")           |
| hindfoot\_length | length of the hindfoot in mm       |
| weight           | weight of the animal in grams      |
| genus            | genus of animal                    |
| species          | species of animal                  |
| taxa             | e.g. Rodent, Reptile, Bird, Rabbit |
| plot\_type       | type of plot                       |

We are going to use the R function `download.file()` to download the CSV file
that contains the survey data from figshare, and we will use `read.csv()` to
load into memory the content of the CSV file as an object of class `data.frame`.

To download the data into the `your desired directory`, run the following:


```r
download.file("https://ndownloader.figshare.com/files/2292169",
              "your desired directory")
```



You are now ready to load the data:




```r
surveys <- read.csv("your desired directory/portal_data_joined.csv")
```

Let's check the top (the first 6 lines) of this data frame using the
function `head()`:


```r
head(surveys)
```

```
##   record_id month day year plot_id species_id sex hindfoot_length weight
## 1         1     7  16 1977       2         NL   M              32     NA
## 2        72     8  19 1977       2         NL   M              31     NA
## 3       224     9  13 1977       2         NL                  NA     NA
## 4       266    10  16 1977       2         NL                  NA     NA
## 5       349    11  12 1977       2         NL                  NA     NA
## 6       363    11  12 1977       2         NL                  NA     NA
##     genus  species   taxa plot_type
## 1 Neotoma albigula Rodent   Control
## 2 Neotoma albigula Rodent   Control
## 3 Neotoma albigula Rodent   Control
## 4 Neotoma albigula Rodent   Control
## 5 Neotoma albigula Rodent   Control
## 6 Neotoma albigula Rodent   Control
```

# What are data frames?

Data frames are the _de facto_ data structure for most tabular data, and what we
use for statistics and plotting.

A data frame can be created by hand, but most commonly they are generated by the
functions `read.csv()` or `read.table()`; in other words, when importing
spreadsheets from your hard drive (or the web).

A data frame is the representation of data in the format of a table where the
columns are vectors that all have the same length. Because the column are
vectors, they all contain the same type of data (e.g., characters, integers,
factors).

# Inspecting `data.frame` Objects

We already saw how the functions `head()` and `str()` can be useful to check the
content and the structure of a data frame. Here is a non-exhaustive list of
functions to get a sense of the content/structure of the data. Let's try them out!

* Size:
    * `dim(surveys)` - returns a vector with the number of rows in the first element,
          and the number of columns as the second element (the **dim**ensions of
          the object)
    * `nrow(surveys)` - returns the number of rows
    * `ncol(surveys)` - returns the number of columns

* Content:
    * `head(surveys)` - shows the first 6 rows
    * `tail(surveys)` - shows the last 6 rows

* Names:
    * `names(surveys)` - returns the column names (synonym of `colnames()` for `data.frame`
	   objects)
    * `rownames(surveys)` - returns the row names

* Summary:
    * `str(surveys)` - structure of the object and information about the class, length and
	   content of  each column
    * `summary(surveys)` - summary statistics for each column

Note: most of these functions are "generic", they can be used on other types of
objects besides `data.frame`.


> ### Challenge
>
> Based on the output of `str(surveys)`, can you answer the following questions?
>
> * What is the class of the object `surveys`?
> * How many rows and how many columns are in this object?
> * How many species have been recorded during these surveys?

# Indexing and subsetting data frames




Our survey data frame has rows and columns (it has 2 dimensions), if we want to
extract some specific data from it, we need to specify the "coordinates" we
want from it. Row numbers come first, followed by column numbers. However, note
that different ways of specifying these coordinates lead to results with
different classes.


```r
surveys[1, 1]   # first element in the first column of the data frame (as a vector)
surveys[1, 6]   # first element in the 6th column (as a vector)
surveys[, 1]    # first column in the data frame (as a vector)
surveys[1]      # first column in the data frame (as a data.frame)
surveys[1:3, 7] # first three elements in the 7th column (as a vector)
surveys[3, ]    # the 3rd element for all columns (as a data.frame)
head_surveys <- surveys[1:6, ] # equivalent to head_surveys <- head(surveys)
```

`:` is a special function that creates numeric vectors of integers in increasing
or decreasing order, test `1:10` and `10:1` for instance.

You can also exclude certain parts of a data frame using the "`-`" sign:


```r
surveys[,-1]          # The whole data frame, except the first column
surveys[-c(7:34786),] # Equivalent to head(surveys)
```

As well as using numeric values to subset a `data.frame` (or `matrix`), columns
can be called by name, using one of the four following notations:


```r
surveys["species_id"]       # Result is a data.frame
surveys[, "species_id"]     # Result is a vector
surveys[["species_id"]]     # Result is a vector
surveys$species_id          # Result is a vector
```

For our purposes, the last three notations are equivalent. RStudio knows about
the columns in your data frame, so you can take advantage of the autocompletion
feature to get the full and correct column name.

> ### Challenge
>
> 1. Create a `data.frame` (`surveys_200`) containing only the observations from
>    row 200 of the `surveys` dataset.
>
> 2. Notice how `nrow()` gave you the number of rows in a `data.frame`?
>
>      * Use that number to pull out just that last row in the data frame.
>      * Compare that with what you see as the last row using `tail()` to make
>        sure it's meeting expectations.
>      * Pull out that last row using `nrow()` instead of the row number.
>      * Create a new data frame object (`surveys_last`) from that last row.
>
> 3. Use `nrow()` to extract the row that is in the middle of the data
>    frame. Store the content of this row in an object named `surveys_middle`.
>
> 4. Combine `nrow()` with the `-` notation above to reproduce the behavior of
>    `head(surveys)` keeping just the first through 6th rows of the surveys
>    dataset.

# Citation and other resources

* All the materials are modified from the ecology workshop of [Data Carpentry](http://www.datacarpentry.org/lessons/#ecology-workshop).

* [Software carpentry lesson](http://swcarpentry.github.io/r-novice-inflammation/)

Others
[https://www.statmethods.net/](https://www.statmethods.net/)
[https://www.rstudio.com/resources/cheatsheets/](https://www.rstudio.com/resources/cheatsheets/)
[https://cran.r-project.org/doc/contrib/Short-refcard.pdf](https://cran.r-project.org/doc/contrib/Short-refcard.pdf)
