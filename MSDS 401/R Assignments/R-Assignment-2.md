------------------------------------------------------------------------

### Test Items starts from here - There are 5 sections - 75 points total

***\#\#\#\# Section 1: (15 points) \#\#\#\#***

##### (1) R has probability functions available for use (Kabacoff, Section 5.2.3). Using one distribution to approximate another is not uncommon.

(1)(a) (6 points) The Poisson distribution may be used to approximate
the binomial distribution if n \> 20 and np \< 7. Estimate the following
binomial probabilities using *dpois()* or *ppois()* with probability p =
0.05, and n = 100. Then, estimate the same probabilities using
*dbinom()* or *pbinom()*. Show the numerical results of your
calculations.

1.  The probability of exactly 0 successes.

``` r
p = 0.05
n = 100
dpois(x=0,lambda=n*p)
```

    ## [1] 0.006737947

``` r
dbinom(x=0,size=n,prob=p)
```

    ## [1] 0.005920529

1.  The probability of fewer than 6 successes. Please note the
    following, taken from the Binomial Distribution R Documentation
    page, regarding the “lower.tail” argument:

lower.tail logical; if TRUE (default), probabilities are P\[X ??? x\],
otherwise, P\[X \> x\].

``` r
x = 0:5
sum(dpois(x,lambda=n*p))
```

    ## [1] 0.6159607

``` r
sum(dbinom(x,size=n,prob=p))
```

    ## [1] 0.6159991

The binomial may also be approximated via the normal distribution.
Estimate the following binomial probabilities using *dnorm()* or
*pnorm()*, this time with probability p = 0.25 and n = 100. Then,
calculate the same probabilities using *dbinom()* and *pbinom()*. Use
continuity correction. Show the numerical results of your calculations.

1.  The probability of exactly 25 successes.

``` r
p = 0.25
q = 1-p
n = 100
dnorm(x=25,mean=n*p,sd=sqrt(n*p*q))
```

    ## [1] 0.09213177

``` r
dbinom(x=25,size=n,prob=p)
```

    ## [1] 0.09179969

1.  The probability of fewer than 20 successes. Please note the
    following, taken from the Normal Distribution R Documentation page,
    regarding the “lower.tail” argument:

lower.tail logical; if TRUE (default), probabilities are P\[X ??? x\],
otherwise, P\[X \> x\].

``` r
pnorm(19.5,mean=n*p,sd=sqrt(n*p*q),lower.tail = TRUE)
```

    ## [1] 0.1020119

``` r
pbinom(19,size=n,prob=p,lower.tail=TRUE)
```

    ## [1] 0.09953041

(1)(b) (3 points) Generate side-by-side barplots using *par(mfrow =
c(1,2))* or *grid.arrange()*. The left barplot will show Poisson
probabilties for outcomes ranging from 0 to 10. The right barplot will
show binomial probabilities for outcomes ranging from 0 to 10. Use p =
0.05 and n = 100. Title each plot, present in color and assign names to
the bar; i.e. x-axis value labels.

``` r
par(mfrow=c(1,2))
p = 0.05
n = 100
x = dpois(0:10,n*p)
y = dbinom(0:10,n,p)
barplot(x,main="Poisson Probabilities",col="cyan1",xlab="Outcome",ylab="Probability")
barplot(y,main="Binomial Probabilities",col="darkorchid1",xlab="Outcome",ylab="Probability")
```

![](R-Assignment-2_files/figure-markdown_github/test1b-1.png)

``` r
par(mfrow=c(1,1))
```

(1)(c) For this problem, refer to Sections 5.2 of Business Statistics. A
discrete random variable has outcomes: 0, 1, 2, 3, 4, 5, 6. The
corresponding probabilities in sequence with the outcomes are: 0.215,
0.230, 0.240, 0.182, 0.130, 0.003, 0.001. In other words, the
probability of obtaining “0” is 0.215.

1.  (3 points) Calculate the expected value and variance for this
    distribution using the general formula for mean and variance of a
    discrete distribution. To do this, you will need to use integer
    values from 0 to 6 as outcomes along with the corresponding
    probabilities. Round your answer to 2 decimal places.

``` r
out = c(0:6)
prob = c(0.215,0.230,0.240,0.182,0.130,0.003,0.001)
exp_val = sum(out*prob)
var = sum(((out-exp_val)**2)*prob)
round(exp_val,2)
```

    ## [1] 1.8

``` r
round(var,2)
```

    ## [1] 1.79

1.  (3 points) Use the *cumsum()* function and plot the cumulative
    probabilities versus the corresponding outcomes. Determine the value
    of the median for this distribution and show on this plot.

``` r
cum_prob = cumsum(prob)
plot(out,cum_prob,main="Cum. Prob vs Outcome",xlab="Outcome",ylab="Prob",pch=19,col="green")
abline(v=exp_val,col="red")
```

![](R-Assignment-2_files/figure-markdown_github/test1cii-1.png)

***\#\#\#\# Section 2: (15 points) \#\#\#\#***

##### (2) Conditional probabilities appear in many contexts and, in particular, are used by Bayes’ Theorem. Correlations are another means for evaluating dependency between variables. The dataset “faithful”" is part of the “datasets” package and may be loaded with the statement *data(faithful)*. It contains 272 observations of 2 variables; waiting time between eruptions (in minutes) and the duration of the eruption (in minutes) for the Old Faithful geyser in Yellowstone National Park.

(2)(a) (3 points) Load the “faithful” dataset and present summary
statistics and a histogram of waiting times. Additionally, compute the
empirical conditional probability of an eruption less than 3.0 minutes,
if the waiting time exceeds 70 minutes.

``` r
data(faithful, package = "datasets")
summary(faithful)
```

    ##    eruptions        waiting    
    ##  Min.   :1.600   Min.   :43.0  
    ##  1st Qu.:2.163   1st Qu.:58.0  
    ##  Median :4.000   Median :76.0  
    ##  Mean   :3.488   Mean   :70.9  
    ##  3rd Qu.:4.454   3rd Qu.:82.0  
    ##  Max.   :5.100   Max.   :96.0

``` r
hist(faithful$waiting,main="Waiting Times",col="lightpink1")
```

![](R-Assignment-2_files/figure-markdown_github/test2a-1.png)

``` r
exceeds_70 = faithful[faithful$waiting > 70,]
less_3 = exceeds_70[exceeds_70$eruptions < 3,]
nrow(less_3)/nrow(exceeds_70)
```

    ## [1] 0.006060606

(2)(a)(i) (3 points) Identify any observations in “faithful” for which
the waiting time exceeds 70 minutes and the eruptions are less than 3.0
minutes. List and show any such observations in a distinct color on a
scatterplot of all eruption (vertical axis) and waiting times
(horizontal axis). Include a horizontal line at eruption = 3.0, and a
vertical line at waiting time = 70. Add a title and appropriate text.

``` r
plot(faithful$waiting,faithful$eruptions,main="Waiting Times Vs Eruptions",xlab="Waiting Time",ylab="Eruption Duration",pch=1, col=ifelse((faithful$waiting > 70 & faithful$eruptions < 3),"deeppink","black"))
abline(h = 3, v = 70, col="deepskyblue")
```

![](R-Assignment-2_files/figure-markdown_github/test2ai-1.png)

(2)(a)(ii) (2 points) What does the plot suggest about the relationship
between eruption time and waiting time?

***Answer: There is a mild positive correlation between waiting time and
eruption duration. As wait time between eruptions increase, the eruption
duration generally increases as well ***

------------------------------------------------------------------------

(2)(b) (4 points) Past research indicates that the waiting times between
consecutive eruptions are not independent. This problem will check to
see if there is evidence of this. Form consecutive pairs of waiting
times. In other words, pair the first and second waiting times, pair the
third and fourth waiting times, and so forth. There are 136 resulting
consecutive pairs of waiting times. Form a data frame with the first
column containing the first waiting time in a pair and the second column
with the second waiting time in a pair. Plot the pairs with the second
member of a pair on the vertical axis and the first member on the
horizontal axis.

One way to do this is to pass the vector of waiting times -
faithful$waiting - to *matrix()*, specifying 2 columns for our matrix,
with values organized by row; i.e. byrow = TRUE.

``` r
pairs = matrix(data = faithful$waiting,nrow=136,ncol=2,byrow=TRUE)
plot(x=pairs[,1],y=pairs[,2],xlab="First Column",ylab="Second Column",main="Paired Plot")
```

![](R-Assignment-2_files/figure-markdown_github/test2b-1.png)

(2)(c) (3 points) Test the hypothesis of independence with a two-sided
test at the 5% level using the Kendall correlation coefficient.

``` r
cor.test(pairs[,1],pairs[,2],alternative = "two.sided",method="kendall",conf.level = 0.95)
```

    ## 
    ##  Kendall's rank correlation tau
    ## 
    ## data:  pairs[, 1] and pairs[, 2]
    ## z = -4.9482, p-value = 7.489e-07
    ## alternative hypothesis: true tau is not equal to 0
    ## sample estimates:
    ##        tau 
    ## -0.2935579

***\#\#\#\# Section 3: (15 points) \#\#\#\#***

##### (3) Performing hypothesis tests using random samples is fundamental to statistical inference. The first part of this problem involves comparing two different diets. Using “ChickWeight” data available in the base R, “datasets” package, we will create a subset of the “ChickWeight” data frame. Specifically, we want to create a data frame that includes only those rows where Time == 21 AND Diet == 1 or 3.

``` r
# load "ChickWeight" dataset
data(ChickWeight, package = "datasets")

# There are multiple ways to approach the subsetting task. The method you choose is up
# to you.

result = ChickWeight[(ChickWeight$Diet == 1 | ChickWeight$Diet ==3) & ChickWeight$Time == 21,]

# The values in your subsetted data frame should match those below:
# > head(df)
#    weight Time Chick Diet
# 12    205   21     1    1
# 24    215   21     2    1
# 36    202   21     3    1
# 48    157   21     4    1
# 60    223   21     5    1
# 72    157   21     6    1
```

##### The data frame, “result”, has chick weights for two diets, identified as diet “1” and “3”. Use the data frame, “result,” to complete the following item.

(3)(a) (3 points) Display two side-by-side vertical boxplots using
par(mfrow = c(1,2)). One boxplot would display Diet “1” and the other
Diet “3”.

``` r
par(mfrow=c(1,2))
diet1 = subset(result,result$Diet==1)
diet3 = subset(result,result$Diet==3)
boxplot(diet1$weight,main="Diet 1", col="yellow",ylab="Weight")
boxplot(diet3$weight,main="Diet 3", col="red",ylab="Weight")
```

![](R-Assignment-2_files/figure-markdown_github/test3a-1.png)

(3)(b) (3 points) Use the “weight” data for the two diets to test the
null hypothesis of equal population mean weights for the two diets. Test
at the 95% confidence level with a two-sided t-test. This can be done
using *t.test()* in R. Assume equal variances. Display the results of
*t.test()*.

``` r
d1_weight = diet1$weight
d3_weight = diet3$weight
t.test(d1_weight,d3_weight,alternative = "two.sided",mu=0,var.equal = TRUE,conf.level = 0.95)
```

    ## 
    ##  Two Sample t-test
    ## 
    ## data:  d1_weight and d3_weight
    ## t = -3.5955, df = 24, p-value = 0.001454
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -145.67581  -39.42419
    ## sample estimates:
    ## mean of x mean of y 
    ##    177.75    270.30

##### Working with paired data is another common statistical activity. The “ChickWeight” data will be used to illustrate how the weight gain from day 20 to 21 may be analyzed. This time, we will look only at those individuals on Diet == “3”. You will need to add code below creating two (2) vectors. One (1) vector should include all the Time == 20 weights of those individuals on Diet == “3”; a second should include all the Time == 21 weights of those individuals on Diet == “3”.

``` r
# There are multiple ways to approach the subsetting task. The method you choose is up
# to you.

t20 = subset(ChickWeight$weight,ChickWeight$Diet=="3" & ChickWeight$Time==20)
t21 = subset(ChickWeight$weight,ChickWeight$Diet=="3" & ChickWeight$Time==21)

# The first six (6) elements of your Time == 20 vector should match those below:
# [1] 235 291 156 327 361 225
```

(3)(c) (3 points) Present a scatterplot of the Time == 21 weights as a
function of the Time == 20 weights. Include a diagonal line with zero
intercept and slope equal to one. Title and label the variables in this
scatterplot.

``` r
plot(t20,t21,main="T20 Weights vs T21 Weights",xlab="T20 Weights",ylab="T21 Weights",pch=1,col="blue2")
abline(a=0,b=1,col="gold")
```

![](R-Assignment-2_files/figure-markdown_github/test3c-1.png)

(3)(d) (6 points) Calculate and present a one-sided, 95% confidence
interval for the average weight gain from day 20 to day 21. Write the
code for the paired t-test and for determination of the confidence
interval endpoints. \*\*Do not use \*t.test()\*\*, although you may
check your answers using this function. Present the resulting test
statistic value, critical value, p-value and confidence interval.

``` r
mean20 = mean(t20)
mean21 = mean(t21)
n = length(t20)
df = n-1

sample_difference = t20-t21
mean_sample_difference = sum(sample_difference)/n

ssq = sum((sample_difference-mean_sample_difference)**2)
std = sqrt(ssq/(n-1))

test_stat = mean_sample_difference/(std/sqrt(n))
crit_val = qt(0.025,n-1)
p_val = 2*pt(test_stat,n-1)
conf_int = c((mean_sample_difference)-(abs(crit_val)*(std/sqrt(n))), (mean_sample_difference)+(abs(crit_val)*(std/sqrt(n))))

t.test(t20,t21,alternative = "two.sided",paired=TRUE,conf.level = 0.95)
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  t20 and t21
    ## t = -3.2253, df = 9, p-value = 0.0104
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -19.3958  -3.4042
    ## sample estimates:
    ## mean of the differences 
    ##                   -11.4

***\#\#\#\# Section 4: (15 points) \#\#\#\#***

##### (4) Statistical inference depends on using a sampling distribution for a statistic in order to make confidence statements about unknown population parameters. The Central Limit Theorem is used to justify use of the normal distribution as a sampling distribution for statistical inference. Using Nile River flow data from 1871 to 1970, this problem demonstrates sampling distribution convergence to normality. Use the code below to prepare the data. Refer to this example when completing (4)(c) below.

``` r
data(Nile, package = "datasets")
m <- mean(Nile)
std <- sd(Nile)

x <- seq(from = 400, to = 1400, by = 1)
hist(Nile, freq = FALSE, col = "darkblue", xlab = "Flow",
     main = "Histogram of Nile River Flows, 1871 to 1970")
curve(dnorm(x, mean = m, sd = std), col = "orange", lwd = 2, add = TRUE)
```

![](R-Assignment-2_files/figure-markdown_github/test4-1.png)

(4)(a) (3 points) Using Nile River flow data and the “moments” package,
calculate skewness and kurtosis. Present a QQ plot and boxplot of the
flow data side-by-side using *qqnorm()*, *qqline()* and *boxplot()*;
*par(mfrow = c(1, 2))* may be used to locate the plots side-by-side. Add
features to these displays as you choose.

``` r
library(moments)
```

    ## Warning: package 'moments' was built under R version 4.0.3

``` r
skewness(Nile)
```

    ## [1] 0.3223697

``` r
kurtosis(Nile)
```

    ## [1] 2.695093

``` r
par(mfrow = c(1,2))
qqnorm(Nile, main="QQ Plot (Flow)",col="coral")
qqline(Nile, col="navy")
boxplot(Nile, main="Boxplot (Flow)", col="lavender")
```

![](R-Assignment-2_files/figure-markdown_github/test4a-1.png)

``` r
par(mfrow=c(1,1))
```

(4)(b) (6 points) Using *set.seed(124)* and the Nile data, generate 1000
random samples of size n = 16, with replacement. For each sample drawn,
calculate and store the sample mean. This can be done with a for-loop
and use of the *sample()* function. Label the resulting 1000 mean values
as “sample1”. **Repeat these steps using *set.seed(127)* - a different
“seed” - and samples of size n = 64.** Label these 1000 mean values as
“sample2”. Compute and present the means, sample standard deviations and
sample variances for “sample1” and “sample2” in a table with the first
row for “sample1”, the second row for “sample2” and the columns labled
for each statistic.

``` r
set.seed(124)
sample1 = c()
for(i in 1:1000) {
  sample1[i] = mean(sample(Nile,16,replace=TRUE))
}


set.seed(127)
sample2 = c()
for(i in 1:1000) {
  sample2[i] = mean(sample(Nile,64,replace = TRUE))
}

row_names<-c("Sample1","Sample2")
col_names<-c("Mean","Standard Deviation","Variance")
matrix(c(mean(sample1),mean(sample2),sd(sample1),sd(sample2),var(sample1),var(sample2)),nrow = 2,ncol = 3,dimnames = list(row_names,col_names))
```

    ##             Mean Standard Deviation  Variance
    ## Sample1 918.7364           42.00156 1764.1312
    ## Sample2 918.5149           20.22883  409.2054

(4)(c) (6 points) Present side-by-side histograms of “sample1” and
“sample2” with the normal density curve superimposed. To prepare
comparable histograms, it will be necessary to use “freq = FALSE” and to
maintain the same x-axis with “xlim = c(750, 1050)”, and the same y-axis
with “ylim = c(0, 0.025).” **To superimpose separate density functions,
you will need to use the mean and standard deviation for each “sample” -
each histogram - separately.**

``` r
par(mfrow=c(1,2))
hist(sample1,freq=FALSE,xlim=c(750,1050),ylim=c(0,0.025),col="bisque")
curve(dnorm(x,mean=mean(sample1),sd=sd(sample1)),add=TRUE,col="firebrick1",lwd=2)
hist(sample2,freq=FALSE,xlim=c(750,1050),ylim=c(0,0.025),col="bisque")
curve(dnorm(x,mean=mean(sample2),sd=sd(sample2)),add=TRUE,col="firebrick1",lwd=2)
```

![](R-Assignment-2_files/figure-markdown_github/test4c-1.png)

``` r
par(mfrow=c(1,2))
```

------------------------------------------------------------------------

***\#\#\#\# Section 5: (15 points) \#\#\#\#***

##### (5) This problem deals with contingency table analysis. This is an example of categorical data analysis (see Kabacoff, pp. 145-151). The “warpbreaks” dataset gives the number of warp breaks per loom, where a loom corresponds to a fixed length of yarn. There are 54 observations on 3 variables: breaks (numeric, the number of breaks), wool (factor, type of wool: A or B), and tension (factor, low L, medium M and high H). These data have been studied and used for example elsewhere. For the purposes of this problem, we will focus on the relationship between breaks and tension using contingency table analysis.

(5)(a)(5 points) warpbreaks is part of the “datasets” package and may be
loaded via *data(warpbreaks)*. Load “warpbreaks” and present the
structure using *str()*. Calculate the median number of breaks for the
entire dataset, disregarding “tension” and “wool”. Define this median
value as “median\_breaks”. Present a histogram of the number of breaks
with the location of the median indicated.

Create a new variable “number” as follows: for each value of “breaks”,
classify the number of breaks as either strictly below “median\_breaks”,
or the alternative. Convert the “above”\|“below” classifications to a
factor, and combine with the dataset warpbreaks. Present a summary of
the augmented dataset using *summary()*. Present a contingency table of
the frequency of breaks using the two variables “tension” and “number”.
There should be six cells in this table.

``` r
data(warpbreaks, package = "datasets")
str(warpbreaks)
```

    ## 'data.frame':    54 obs. of  3 variables:
    ##  $ breaks : num  26 30 54 25 70 52 51 26 67 18 ...
    ##  $ wool   : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ tension: Factor w/ 3 levels "L","M","H": 1 1 1 1 1 1 1 1 1 2 ...

``` r
median_breaks = median(warpbreaks$breaks)
median_breaks
```

    ## [1] 26

``` r
hist(warpbreaks$breaks,main="Histogram of Warpbreaks",xlab="Number of Breaks",ylab="Frequency",col="mediumorchid1")
abline(v=median_breaks,col="black",lwd=2)
```

![](R-Assignment-2_files/figure-markdown_github/test5a-1.png)

``` r
number<-ifelse(warpbreaks$breaks<median_breaks,"below","above")
warpbreaks<-cbind(warpbreaks,number)
summary(warpbreaks)
```

    ##      breaks      wool   tension    number         
    ##  Min.   :10.00   A:27   L:18    Length:54         
    ##  1st Qu.:18.25   B:27   M:18    Class :character  
    ##  Median :26.00          H:18    Mode  :character  
    ##  Mean   :28.15                                    
    ##  3rd Qu.:34.00                                    
    ##  Max.   :70.00

``` r
break_freq = table(warpbreaks$tension,warpbreaks$number)
break_freq
```

    ##    
    ##     above below
    ##   L    14     4
    ##   M    10     8
    ##   H     5    13

(5)(b)(3 points) Using the table constructed in (5)(a), test at the 5%
level the null hypothesis of independence using the uncorrected
*chisq.test()* (Black, Business Statistics, Section 16.2). Show the
results of this test and state your conclusions.

``` r
chisq.test(break_freq,correct=FALSE)
```

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  break_freq
    ## X-squared = 9.0869, df = 2, p-value = 0.01064

``` r
#P Value is lower than alpha (0.05), so we reject the null hypothesis and determine that tension and being above/below median breaks is not independent. 
```

(5)(c) (3 points) ‘Manually’ calculate the chi-squared statistic and
p-value of the table from (5)(a). The *addmargins()* function can be
used to add row and column sums to the table; useful for calculating the
expected values for each cell. You should be able to match the
chi-squared and p-values from (5)(b). The underlying code for the
*chisq.test()* function can be viewed by entering *chisq.test* - without
parentheses - in the Console. You are given code below to create the
table, add row and column sums and calculate the expected values for the
for the first two (2) of three (3) rows. You will need to add code to
calculate the expected values for the third row and the chi-squared. The
*pchisq()* function can be used to return the p-value.

``` r
tbl <- table(warpbreaks$tension, warpbreaks$number)
mar_tbl <- addmargins(tbl)

e11 <- mar_tbl[4, 1] * mar_tbl[1, 3] / mar_tbl[4, 3]
e12 <- mar_tbl[4, 2] * mar_tbl[1, 3] / mar_tbl[4, 3]
e21 <- mar_tbl[4, 1] * mar_tbl[2, 3] / mar_tbl[4, 3]
e22 <- mar_tbl[4, 2] * mar_tbl[2, 3] / mar_tbl[4, 3]
e31 =  mar_tbl[4, 1] * mar_tbl[3, 3] / mar_tbl[4, 3]
e32 =  mar_tbl[4, 2] * mar_tbl[3, 3] / mar_tbl[4, 3]


chisq_test_stat = ((14-e11)**2)/e11 + ((4-e12)**2)/e12 + ((10-e21)**2)/e21 + ((8-e22)**2)/e22 + ((5-e31)**2)/e31 + ((13-e32)**2)/e32
chisq_test_stat
```

    ## [1] 9.086897

``` r
pchisq(chisq_test_stat,df=2,lower.tail = FALSE)
```

    ## [1] 0.01063667

(5)(d) (4 points) Build a user-defined function, using your code for
(5)(c).We want to pass our (5)(a) table to our function and have it
return the chi-squared statistic and p-value. You’re provided with the
‘shell’ of a function and will need to add code to calculate the
expected values, the chi-squared statistic, the p-value and return
(i.e. output) the chi-squared and p-value.

``` r
chisq_function <- function(x) {
  # Code for calculating the expected values
  e11 <- mar_tbl[4, 1] * mar_tbl[1, 3] / mar_tbl[4, 3]
  e12 <- mar_tbl[4, 2] * mar_tbl[1, 3] / mar_tbl[4, 3]
  e21 <- mar_tbl[4, 1] * mar_tbl[2, 3] / mar_tbl[4, 3]
  e22 <- mar_tbl[4, 2] * mar_tbl[2, 3] / mar_tbl[4, 3]
  e31 <- mar_tbl[4, 1] * mar_tbl[3, 3] / mar_tbl[4, 3]
  e32 <- mar_tbl[4, 2] * mar_tbl[3, 3] / mar_tbl[4, 3]
  
  # Code for calculating the chi-squared
  chiSq <-((x[1,1] - e11)**2)/e11 + ((x[1,2] - e12)**2)/e12 + ((x[2,1] - e21)**2)/e21 + ((x[2,2] - e22)**2)/e22 + ((x[3,1] - e31)**2)/e31 + ((x[3,2] - e32)**2)/e32
  
  # Code for calculating the degrees of freedom and p-value
  df <- (nrow(x)-2)*(ncol(x)-2)
  p_val <- pchisq(chiSq,df,lower.tail = FALSE)
    
  # Code to ouput the chi-squared, degrees of freedom and p-value
  return(list("chi-squared" = chiSq,
              "degrees of freedom" = df,
              "p-value" = p_val))
}
chisq_function(mar_tbl)
```

    ## $`chi-squared`
    ## [1] 9.086897
    ## 
    ## $`degrees of freedom`
    ## [1] 2
    ## 
    ## $`p-value`
    ## [1] 0.01063667

You do not need to do anything with the below. It is provided only for
demonstration purposes. In (5)(d), we know the size of the table - 3 x 2
- and write a function to match. Often, though, we’ll want to write
functions that are flexible in some way.

``` r
# Below is a function that should return the same values as chisq.test() and your
# function from (5)(d). Here, though, the function loops over the rows and columns
# to calculate the expected values. Ideally, this function would work for any sized
# table.

chisqfun <- function(t) {
   x <- addmargins(t)
   e <- matrix(0, nrow = nrow(t), ncol = ncol(t), byrow = T)
   r <- matrix(0, nrow = nrow(t), ncol = ncol(t), byrow = T)
   for (i in 1:dim(t)[1]) {
       for (j in 1:dim(t)[2]) {
          e[i,j] = x[nrow(x),j] * x[i,ncol(x)]/x[nrow(x), ncol(x)]
         r[i,j] = ((x[i,j] - e[i,j])^2)/e[i,j]
         }
     }
  chi <- sum(r)
  xdf <- (nrow(t) - 1) * (ncol(t) - 1)
  pv <- pchisq(chi, df = xdf, lower.tail = FALSE)
  return(list("chi-squared" = chi, "degrees_of_freedom" = xdf, "p-value" = pv))
  }
```
