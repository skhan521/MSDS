R Assignment \#1 (50 points)
================
Sameer Khan

-----

### Test Items starts from here - There are 5 sections - 50 points total

Read each question carefully and address each element. Do not output
contents of vectors or data frames unless requested.

***\#\#\#\#\# Section 1: (8 points) \#\#\#***

\#\#\#This problem deals with vector manipulations

(1)(a) Create a vector that contains the following, in this order, and
output the final, resulting vector. Do not round any values, unless
requested. \* A sequence of integers from 0 to 4, inclusive. \* The
number 13 \* Three repetitions of the vector c(2, -5.1, -23). \* The
arithmetic sum of 7/42, 3 and 35/42

``` r
v1 = seq(0,4,by=1)
v2 = 13
v3 = rep(c(2,-5.1,-23),3)
v4 = sum(7/42,3,35/42)
vector = c(v1,v2,v3,v4)
vector
```

    ##  [1]   0.0   1.0   2.0   3.0   4.0  13.0   2.0  -5.1 -23.0   2.0  -5.1 -23.0
    ## [13]   2.0  -5.1 -23.0   4.0

(1)(b) Sort the vector created in (1)(a) in ascending order. Output this
result. Determine the length of the resulting vector and assign to “L”.
Output L. Generate a descending sequence starting with L and ending with
1. Add this descending sequence arithmetically the sorted vector. This
is vector addition, not vector combination. Output the contents. Do not
round any values.

``` r
vector_2 = sort(vector)
L = length(vector_2)
L
```

    ## [1] 16

``` r
L_seq = seq(from=L,to=1,by=-1)
vector_3 = vector_2 + L_seq
vector_3
```

    ##  [1] -7.0 -8.0 -9.0  7.9  6.9  5.9 10.0 10.0 10.0  9.0  8.0  7.0  7.0  7.0  6.0
    ## [16] 14.0

(1)(c) Extract the first and last elements of the vector you have
created in (1)(b) to form another vector of the extracted elements. Form
a third vector from the elements not extracted. Output these vectors.

``` r
extract_v = c(vector_3[1],vector_3[16])
extract_v
```

    ## [1] -7 14

``` r
not_extract_v = vector_3[2:15]
not_extract_v
```

    ##  [1] -8.0 -9.0  7.9  6.9  5.9 10.0 10.0 10.0  9.0  8.0  7.0  7.0  7.0  6.0

(1)(d) Use the vectors from (c) to reconstruct the vector in (b). Output
this vector. Sum the elements and round to two decimal places.

``` r
vector_4 = append(extract_v,not_extract_v, after=1)
vector_4
```

    ##  [1] -7.0 -8.0 -9.0  7.9  6.9  5.9 10.0 10.0 10.0  9.0  8.0  7.0  7.0  7.0  6.0
    ## [16] 14.0

``` r
round(sum(vector_4),digits=2)
```

    ## [1] 84.7

-----

***\#\#\#\#\# Section 2: (10 points) \#\#\#***

\#\#\#The expression y = sin(x/2) + cos(x/2) is a trigonometric function

(2)(a) Create a user-defined function - via *function()* - that
implements the trigonometric function above, accepts numeric values,
“x,” calculates and returns values “y.”

``` r
trig_f = function(x){
  y = sin(x/2) + cos(x/2)
  return(y)
}
```

(2)(b) Create a vector, x, of 4001 equally-spaced values from -2 to 2,
inclusive. Compute values for y using the vector x and your function
from (2)(a). **Do not output x or y.** Find the value in the vector x
that corresponds to the maximum value in the vector y. Restrict
attention to only the values of x and y you have computed; i.e. do not
interpolate. Round to 3 decimal places and output both the maximum y and
corresponding x value.

Finding the two desired values can be accomplished in as few as two
lines of code. Do not use packages or programs you may find on the
internet or elsewhere. Do not output the other elements of the vectors x
and y. Relevant coding methods are given in the *Quick Start Guide for
R*.

``` r
x = seq(from=-2,to=2,by=4/4000)
y = trig_f(x)
round(max(y),3)
```

    ## [1] 1.414

``` r
x[which.max(y)]
```

    ## [1] 1.571

(2)(c) Plot y versus x in color, with x on the horizontal axis. Show the
location of the maximum value of y determined in 2(b). Show the values
of x and y corresponding to the maximum value of y in the display. Add a
title and other features such as text annotations. Text annotations may
be added via *text()* for base R plots and *geom\_text()* or
*geom\_label()* for ggplots.

``` r
plot(x,y,type="l",xlim=c(-2,2),ylim=c(-2,2),main="Plot of Trig Function",col="blue")
points(max(y),pch=16, cex=2)
text(1,1.75,labels = c("(1.571,1.414)"),cex=1)
```

![](R-Assignment-1_files/figure-gfm/test2c-1.png)<!-- -->
![](R-Assignment-1_files/figure-gfm/test2c-1.png)<!-- --> —–

***\#\#\#\#\# Section 3: (8 points) \#\#\#***

\#\#\#This problem requires finding the point of intersection of two
functions. Using the function y = cos(x/2)\*sin(x/2), find where the
curved line y = -(x/2)^3 intersects it within the range of values used
in part (2) (i.e. 4001 equally-spaced values from -2 to 2). Plot both
functions on the same display, and show the point of intersection.
Present the coordinates of this point as text in the display.

``` r
range = seq(from=-2,to=2,by=4/4000)
y1 = cos(x/2)*sin(x/2)
y2 = -(range/2)^3

plot(range,y1, col="paleturquoise1",cex=0.5)
lines(range,y2,col="rosybrown1", lwd = 4)
points(0,0,pch=4, cex=2)
text(0,0.1,label="(0,0)")
```

![](R-Assignment-1_files/figure-gfm/test3-1.png)<!-- -->
![](R-Assignment-1_files/figure-gfm/test3-1.png)<!-- --> —–

***\#\#\#\#\# Section 4: (12 points) \#\#\#***

\#\#\#Use the “trees” dataset for the following items. This dataset has
three variables (Girth, Height, Volume) on 31 felled black cherry trees.

(4)(a) Use *data(trees)* to load the dataset. Check and output the
structure with *str()*. Use *apply()* to return the median values for
the three variables. Output these values. Using R and logicals, output
the row number and the three measurements - Girth, Height and Volume -
of any trees with Girth equal to median Girth. It is possible to
accomplish this last request with one line of code.

``` r
data(trees)
str(trees)
```

    ## 'data.frame':    31 obs. of  3 variables:
    ##  $ Girth : num  8.3 8.6 8.8 10.5 10.7 10.8 11 11 11.1 11.2 ...
    ##  $ Height: num  70 65 63 72 81 83 66 75 80 75 ...
    ##  $ Volume: num  10.3 10.3 10.2 16.4 18.8 19.7 15.6 18.2 22.6 19.9 ...

``` r
apply(trees,2,median)
```

    ##  Girth Height Volume 
    ##   12.9   76.0   24.2

``` r
trees[which(trees==median(trees$Girth)),]
```

    ##    Girth Height Volume
    ## 16  12.9     74   22.2
    ## 17  12.9     85   33.8

(4)(b) Girth is defined as the diameter of a tree taken at 4 feet 6
inches from the ground. Convert each diameter to a radius, r. Calculate
the cross-sectional area of each tree using pi times the squared radius.
Present a stem-and-leaf plot of the radii, and a histogram of the radii
in color. Plot Area (y-axis) versus Radius (x-axis) in color showing the
individual data points. Label appropriately.

``` r
r = trees$Girth/2
area = pi*(r^2)
par(mfrow = c(1,2))
stem(r)
```

    ## 
    ##   The decimal point is at the |
    ## 
    ##    4 | 234
    ##    5 | 34455667779
    ##    6 | 055799
    ##    7 | 013
    ##    8 | 0278
    ##    9 | 000
    ##   10 | 3

``` r
hist(r,col = "orange",main="Histogram of Tree Radii",xlab="Radius" )
plot(r,area, col="seagreen1",main="Area vs Radius of Trees",xlab="Radius",ylab="Area")
```

![](R-Assignment-1_files/figure-gfm/test3b-1.png)<!-- -->

``` r
par(mfrow=c(1,1))
```

![](R-Assignment-1_files/figure-gfm/test3b-1.png)<!-- -->

(4)(c) Present a horizontal, notched, colored boxplot of the areas
calculated in (b). Title and label the axis.

``` r
boxplot(area, col="cyan",horizontal = TRUE,notch=TRUE,main="Boxplot of Tree Areas",xlab="Area",ylab="Tree Areas")
```

![](R-Assignment-1_files/figure-gfm/test3c-1.png)<!-- -->
![](R-Assignment-1_files/figure-gfm/test3c-1.png)<!-- -->

(4)(d) Demonstrate that the outlier revealed in the boxplot of Area is
not an extreme outlier. It is possible to do this with one line of code
using *boxplot.stats()* or ‘manual’ calculation and logicals. Identify
the tree with the largest area and output on one line its row number and
three measurements.

``` r
boxplot.stats(area, coef=3.0)
```

    ## $stats
    ## [1]  54.10608  95.90104 130.69811 183.09595 333.29156
    ## 
    ## $n
    ## [1] 31
    ## 
    ## $conf
    ## [1] 105.9543 155.4420
    ## 
    ## $out
    ## numeric(0)

``` r
trees[which(trees==max(trees$Girth)),]
```

    ##    Girth Height Volume
    ## 31  20.6     87     77

-----

***\#\#\#\#\# Section 5: (12 points) \#\#\#***

\#\#\#The exponential distribution is an example of a right-skewed
distribution with outliers. This problem involves comparing it with a
normal distribution which typically has very few outliers.

5(a) Use *set.seed(124)* and *rexp()* with n = 100, rate = 5.5 to
generate a random sample designated as y. Generate a second random
sample designated as x with *set.seed(127)* and *rnorm()* using n = 100,
mean = 0 and sd = 0.15.

Generate a new object using *cbind(x, y)*. Do not output this object;
instead, assign it to a new name. Pass this object to *apply()* and
compute the inter-quartile range (IQR) for each column: x and y. Use the
function *IQR()* for this purpose. Round the results to four decimal
places and present (this exercise shows the similarity of the IQR
values.).

For information about *rexp()*, use *help(rexp)* or *?rexp()*. **Do not
output x or y.**

``` r
set.seed(124)
y=rexp(n=100,rate=5.5)
set.seed(127)
x=rnorm(n=100,mean=0,sd=0.15)
object=cbind(x,y)
round(apply(object,2,IQR),4)
```

    ##      x      y 
    ## 0.2041 0.2164

(5)(b) This item will illustrate the difference between a right-skewed
distribution and a symmetric one. For base R plots, use *par(mfrow =
c(2, 2))* to generate a display with four diagrams; *grid.arrange()* for
ggplots. On the first row, for the normal results, present a histogram
and a horizontal boxplot for x in color. For the exponential results,
present a histogram and a horizontal boxplot for y in color.

``` r
par(mfrow=c(2,2))
hist(x,col="aquamarine",main = "Histogram of Normal Distribution")
boxplot(x,horizontal = TRUE,col="aquamarine",main="Boxplot of Normal Distribution")
hist(y, col="coral",main="Histogram of Exponential Distribution")
boxplot(y,horizontal = TRUE,col="coral",main="Boxplot of Exponential Distribution")
```

![](R-Assignment-1_files/figure-gfm/test5b-1.png)<!-- -->

``` r
par(mfrow=c(1,1))
```

![](R-Assignment-1_files/figure-gfm/test5b-1.png)<!-- -->

(5)(c) QQ plots are useful for detecting the presence of heavy-tailed
distributions. Present side-by-side QQ plots, one for each sample, using
*qqnorm()* and *qqline()*. Add color and titles. In base R plots, “cex”
can be used to control the size of the plotted data points and text.
Lastly, determine if there are any extreme outliers in either
sample.Remember extreme outliers are based on 3.0*IQR in the box plot. R
uses a default value of 1.5*IQR to define outliers (not extreme) in both
boxplot and boxplot stats.

``` r
par(mfrow=c(1,2))
qqnorm(x,col="deepskyblue",cex=2,main="QQ Plot (Normal)")
qqline(x,col="deeppink")
qqnorm(y,col="springgreen",cex=2,main="QQ Plot (Exponential)")
qqline(y,col="slateblue1")
```

![](R-Assignment-1_files/figure-gfm/test5c-1.png)<!-- -->

``` r
outliers = function(x)
{
  out = 3*IQR(x)
  return(out)
  
}
x[x > outliers(x)] #no outliers in normal sample
```

    ## numeric(0)

``` r
y[y > outliers(y)] #2 outliers exponential sample
```

    ## [1] 1.4486792 0.6677194

![](R-Assignment-1_files/figure-gfm/test5c-1.png)<!-- -->
