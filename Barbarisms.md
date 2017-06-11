Barbarisms in the Russian web-discourse
================
Kirill Milintsevich, Ivan Rodin
2017

Introduction
------------

Here, we're doing cool stuff, analysing the usage of the Barbarisms in the Russian Internet.

Let's load some useful packages and our data:

``` r
library(tidyverse)
library(ggpubr)
```

    ## Warning: package 'ggpubr' was built under R version 3.3.3

``` r
library(dplyr)
library(car)
```

    ## Warning: package 'car' was built under R version 3.3.3

``` r
barbar <- read.csv("https://goo.gl/ggeuW5")
```

Manipulating the data
---------------------

Let's divide our cities into small(population &lt; 100000) and big(population &gt; 100000). Also, we'd like to throw out very small cities, with the population smaller than 10000 inhabitants, due to the small amount of commenters from those ones and the fact that they cannot be called "cities" (A.A.Perederiy. City Classification and Typology. Link: <http://www.mstu.edu.ru/science/conferences/11ntk/materials/section8/section8_35.html>).

Statistics
----------

The time has come to look at some numbers. First, we'd like to look at the means and SDs of the frequencies of the barbarisms' usage in small and big cities.

``` r
group_by(barbar_by_status, status) %>%
  summarise(
    count=n(),
    mean = mean(barbar_frequency, na.rm=TRUE),
    sd = sd(barbar_frequency, na.rm=TRUE)
  )
```

    ## # A tibble: 2 × 4
    ##   status count       mean         sd
    ##    <chr> <int>      <dbl>      <dbl>
    ## 1    Big   159 0.06946605 0.04508912
    ## 2  Small   641 0.15896334 0.09912085

Surprisingly, users from small cities tend to use almost twice as more barbarisms that users from big cities.

We also want to conduct a Shapiro test to see whether our frequencies are normaly distributed:

``` r
with(barbar_by_status, shapiro.test(barbar_frequency[status == "Small"])) 
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  barbar_frequency[status == "Small"]
    ## W = 0.81651, p-value < 2.2e-16

``` r
with(barbar_by_status, shapiro.test(barbar_frequency[status == "Big"])) 
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  barbar_frequency[status == "Big"]
    ## W = 0.37315, p-value < 2.2e-16

We got the p-value way less that our alpha = 0.05, which means that frequencies are not normally distributed. In that case, let's use unpaired two-samples Wilcoxon test to find out whether the means are significanlty different from each other.

``` r
wilcox.test(barbar_frequency ~ status, data = barbar_by_status, exact = FALSE)
```

    ## 
    ##  Wilcoxon rank sum test with continuity correction
    ## 
    ## data:  barbar_frequency by status
    ## W = 9996.5, p-value < 2.2e-16
    ## alternative hypothesis: true location shift is not equal to 0

The p-value of the test is &lt; 2.2e-16, which is less than the significance level alpha = 0.05. We can conclude that small cities' median barbarism frequency is significantly different from big cities' median frequency with a p-value &lt; 2.2e-16.

Plotting the data:

``` r
p <- ggboxplot(barbar_by_status, x = "status", y = "barbar_frequency", 
               color = "status", palette = c("#00AFBB", "#E7B800"),
               ylab = "Frequency", xlab = "City Status")
p + stat_compare_means()
```

![](Barbarisms_files/figure-markdown_github/plot%20wilcoxon-1.png)

Clean our data
--------------

Previously, we took the data from every city, that got into our scope, i.e. that a city with only one registered used might be present there, and if that user left a comment with a barbarism in it, his city would get 1.0 probability of it's inhabitant leaving a fancy comment. That's, obviously, not very good. Let's try to conduct the same analysis on the filtered data, where only cities with at least 200 registered users were allowed in. Now, we also divide our cities into more categiries, which are "Small", "Big", "Large", "Biggest", and "Million".

``` r
barbar_filtered <- read.csv("https://goo.gl/TekJTH")

barbar_small_f <- barbar_filtered[barbar_filtered$population < 100000 & barbar_filtered$population > 10000,]
barbar_big_f <- barbar_filtered[barbar_filtered$population < 250000 & barbar_filtered$population > 100000,]
barbar_large_f <- barbar_filtered[barbar_filtered$population < 500000 & barbar_filtered$population > 250000,]
barbar_biggest_f <- barbar_filtered[barbar_filtered$population < 1000000 & barbar_filtered$population > 500000,]
barbar_million_f <- barbar_filtered[barbar_filtered$population > 1000000,]

barbar_small_f$status <- rep("Small", length(barbar_small_f[,1]))
barbar_big_f$status <- rep("Big", length(barbar_big_f[,1]))
barbar_large_f$status <- rep("Large", length(barbar_large_f[,1]))
barbar_biggest_f$status <- rep("Biggest", length(barbar_biggest_f[,1]))
barbar_million_f$status <- rep("Million", length(barbar_million_f[,1]))

barbar_by_status_detailed <- rbind(barbar_small_f[,c("barbar_frequency","status")], barbar_big_f[,c("barbar_frequency","status")], barbar_large_f[,c("barbar_frequency","status")], barbar_biggest_f[,c("barbar_frequency","status")], barbar_million_f[,c("barbar_frequency","status")])
barbar_by_status_detailed$status <- ordered(barbar_by_status_detailed$status,
                         levels = c("Small", "Big", "Large", "Biggest", "Million"))
```

Now let's look at their means again.

    ## # A tibble: 5 × 4
    ##    status count       mean          sd
    ##     <ord> <int>      <dbl>       <dbl>
    ## 1   Small    39 0.06577130 0.012087895
    ## 2     Big    74 0.06694744 0.017007460
    ## 3   Large    39 0.05762430 0.009003499
    ## 4 Biggest    20 0.05571422 0.006576426
    ## 5 Million    13 0.05867399 0.004343862

Now, we can see, that Small and Big cities are almost equal and only a little bit more that Large, Biggest and Million cities. Also, the diviation redused a lot, meaning that our data is more "compact" now.

Visualising our new data:

![](Barbarisms_files/figure-markdown_github/filtered%20plot-1.png)

No ANOVA here
-------------

To find out whether we can use the ANOVA test to check the significance of differences between our means. Thus, our data have to meet the assumptions homogenity and normality.

We use Levene's Test to check the homogenity of variances.

``` r
leveneTest(barbar_frequency ~ status, data = barbar_by_status_detailed)
```

    ## Levene's Test for Homogeneity of Variance (center = median)
    ##        Df F value    Pr(>F)    
    ## group   4  4.9468 0.0008311 ***
    ##       180                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

P-value is less than 0.05, which means that we cannot assume the homogenity of variances in the different city size groups.

To check our data for normality, we use the Shapito-Wilk test on the ANOVA residuals.

``` r
res.aov <- aov(barbar_frequency ~ status, data = barbar_by_status_detailed)
aov_residuals <- residuals(object = res.aov)
shapiro.test(x = aov_residuals)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  aov_residuals
    ## W = 0.92732, p-value = 5.563e-08

Again, our value is way less that our alpha = 0.05, so we cannot assume the normality of the data.

In this case, we cannot use the ANOVA test because our data failed tests for homogenity and normality. As an alternative, we'll use Kruskal-Wallis rank sum test.

``` r
kruskal.test(barbar_frequency ~ status, data = barbar_by_status_detailed)
```

    ## 
    ##  Kruskal-Wallis rank sum test
    ## 
    ## data:  barbar_frequency by status
    ## Kruskal-Wallis chi-squared = 23.843, df = 4, p-value = 8.586e-05

Looking at the p-value, we can conclude that there is significant differences between the cities of different sizes. However, we have 5 categories, so we might want to look what categories exactly are significantly different. To do so, we use the pair-wise Wilcoxon test.

``` r
pairwise.wilcox.test(barbar_by_status_detailed$barbar_frequency, barbar_by_status_detailed$status, p.adjust.method = "BH")
```

    ## 
    ##  Pairwise comparisons using Wilcoxon rank sum test 
    ## 
    ## data:  barbar_by_status_detailed$barbar_frequency and barbar_by_status_detailed$status 
    ## 
    ##         Small  Big    Large  Biggest
    ## Big     0.9591 -      -      -      
    ## Large   0.0036 0.0036 -      -      
    ## Biggest 0.0021 0.0036 0.6979 -      
    ## Million 0.0312 0.0706 0.5643 0.3568 
    ## 
    ## P value adjustment method: BH

From this, we can see that there's no significant difference between Small and Big cities and Large, Biggest and Million cities. What we are left with is that the difference is significant between Small and Large, Small and Biggest and Big and Large and Big and Biggest cities.

Conclusion
----------

Thus we are left with two groups that are significantly different: Small-Big (pop. less than 250,000) and Large-Biggest (pop. between 250,000 and 1,000,000). Cities with more than million population kind of got out of our way. We suppose that the explanation to this may lie in a fact that many people from smaller cities move to the bigger ones, thus bringing their linguistic habits with them and mixing our samples.

However, even that the diffenece is present, it is very small (less than 1%). We should probably look at other sociolinguistic factors like age, gender or education to get some bigger difference in the usage of Anglophone Barbarisms in Russian web-discourse.