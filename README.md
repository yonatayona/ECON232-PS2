ECON 232 - PS2
================

### 0. Preliminaries

Load required packages.

``` r
library(readxl)
library(car)
```

    ## Loading required package: carData

------------------------------------------------------------------------

### 1. Burger chain

Read the data. Then fit a linear model of the form:

$$ \\\\text{TR} = \\\\beta\_1 + \\\\beta\_2\\\\text{A} + \\\\beta\_3\\\\text{P} + \\\\epsilon $$

``` r
burger <- read_xlsx("chainStore 1.xlsx")

# Fit a linear model
burger_lm <- lm(TR ~ A + P, data = burger)

# Prints summary measures
summary(burger_lm)
```

    ## 
    ## Call:
    ## lm(formula = TR ~ A + P, data = burger)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -11.5574  -4.2439  -0.4033   5.3298  10.1294 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 104.7855     6.4827  16.164   <2e-16 ***
    ## A             2.9843     0.1669  17.877   <2e-16 ***
    ## P            -6.6419     3.1912  -2.081   0.0427 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6.07 on 49 degrees of freedom
    ## Multiple R-squared:  0.8671, Adjusted R-squared:  0.8617 
    ## F-statistic: 159.8 on 2 and 49 DF,  p-value: < 2.2e-16

The fitted model is given by:

*T**R* = 104.7855 + 2.9843*A* − 6.6419*P*
We can also compute the summary measures manually and check if these
coincide with values provided by the `summary()` function.

``` r
# Define components
n <- nrow(burger)
j <- 1
k <- 2

y_mean <- mean(burger$TR)

SSR <- sum(residuals(burger_lm)^2)
TSS <- sum((burger$TR - y_mean)^2)
ESS <- sum((fitted(burger_lm) - y_mean)^2)


# Compute the measures
SER <- sqrt(1/(n-k-1) * SSR)
Rsq <- 1 - (SSR/TSS)
adj_Rsq <- 1 - (n-1)/(n-k-1) * SSR/TSS

c("SER" = SER,
  "Rsq" = Rsq,
  "adj_Rsq" = adj_Rsq)
```

    ##       SER       Rsq   adj_Rsq 
    ## 6.0696105 0.8670848 0.8616597

We see that the manually computed values coincide with the `summary()`
output.

Next, we want to know the overall significance of the model. The
`summary()` function already reports an F-statistic and its
corresponding p-value. The null hypothesis belonging to this F-test is
that all of the coefficients in the model (except for the intercept) are
zero.

The `linearHypothesis()` function performs a Wald Test on a specified
hypothesis. We test the following null hypothesis, and check if the
result coincides with what `summary()` reports.

$$ \\\\begin{aligned} & H\_0: \\\\beta\_2 = 0,  \\\\beta\_3 = 0 \\\\ & H\_1: \\\\text{There is a } \\\\beta\_j \\\\neq 0 \\\\text{ for at least one } j \\\\end{aligned} $$

``` r
# Wald Test
linearHypothesis(burger_lm, c("P = 0", "A = 0"))
```

    ## Linear hypothesis test
    ## 
    ## Hypothesis:
    ## P = 0
    ## A = 0
    ## 
    ## Model 1: restricted model
    ## Model 2: TR ~ A + P
    ## 
    ##   Res.Df     RSS Df Sum of Sq      F    Pr(>F)    
    ## 1     51 13581.4                                  
    ## 2     49  1805.2  2     11776 159.83 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

The F-statistic for this particular joint hypothesis is `159.83` and the
corresponding p-value is `2.2e-16`. This is enough to **reject the null
hypothesis** for any practical level of significance.

------------------------------------------------------------------------

### 2. Investment data

``` r
inv <- read_excel("investment.xlsx")
head(inv)
```

    ## # A tibble: 6 x 8
    ##    YEAR RealGNP INVEST GNPDefl Interest  Infl Trend RealInv
    ##   <dbl>   <dbl>  <dbl>   <dbl>    <dbl> <dbl> <dbl>   <dbl>
    ## 1  2000    87.1   2.03    81.9     9.23   3.4     1    2.48
    ## 2  2001    88     1.93    83.8     6.91   1.6     2    2.31
    ## 3  2002    89.5   1.92    85       4.67   2.4     3    2.26
    ## 4  2003    92     2.03    86.7     4.12   1.9     4    2.34
    ## 5  2004    95.5   2.28    89.1     4.34   3.3     5    2.56
    ## 6  2005    98.7   2.53    91.9     6.19   3.4     6    2.75

Estimate an investment function:

$$ \\\\text{RealInv} = \\\\beta\_1 + \\\\beta\_2\\\\text{Trend} + \\\\beta\_3\\\\text{RealGNP} + \\\\beta\_4\\\\text{Interest} + \\\\beta\_5\\\\text{Infl} + \\\\epsilon $$

``` r
inv_lm <- lm(RealInv ~ Trend + RealGNP + Interest + Infl,
             data = inv)

summary(inv_lm)
```

    ## 
    ## Call:
    ## lm(formula = RealInv ~ Trend + RealGNP + Interest + Infl, data = inv)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.23597 -0.07658  0.02551  0.07897  0.15815 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept) -6.21967    1.93045  -3.222  0.00915 **
    ## Trend       -0.16089    0.04724  -3.406  0.00670 **
    ## RealGNP      0.09908    0.02413   4.106  0.00212 **
    ## Interest     0.02017    0.03369   0.599  0.56268   
    ## Infl        -0.01166    0.03977  -0.293  0.77538   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1427 on 10 degrees of freedom
    ## Multiple R-squared:  0.7878, Adjusted R-squared:  0.7029 
    ## F-statistic: 9.282 on 4 and 10 DF,  p-value: 0.002125

The fitted model is given by:

*R**e**a**l**I**n**v* =  − 6.2197 − 0.1609*T**r**e**n**d* + 0.0991*R**e**a**l**G**N**P* + 0.0202*I**n**t**e**r**e**s**t* − 0.0117*I**n**f**l*

It might be appropriate to formulate the regression in terms of real
interest rather than to treat interest rate and inflation rate
separately. If investors were only interested in the real interest rate,
equal increases in interest rate and inflation rate would have no effect
on investment.

To test this, perform a Wald test with the following null hypothesis:

$$ H\_0: \\\\beta\_4 + \\\\beta\_5 = 0 $$

``` r
linearHypothesis(model = inv_lm,
                 hypothesis.matrix = c("Trend = 0",
                                       "RealGNP = 1",
                                       "Interest + Infl = 0"))
```

    ## Linear hypothesis test
    ## 
    ## Hypothesis:
    ## Trend = 0
    ## RealGNP = 1
    ## Interest  + Infl = 0
    ## 
    ## Model 1: restricted model
    ## Model 2: RealInv ~ Trend + RealGNP + Interest + Infl
    ## 
    ##   Res.Df    RSS Df Sum of Sq     F    Pr(>F)    
    ## 1     13 669.55                                 
    ## 2     10   0.20  3    669.35 10954 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

The F-statistic for this particular joint hypothesis is `159.83` and the
corresponding p-value is `2.2e-16`. This is enough to **reject the null
hypothesis** for any practical level of significance.

------------------------------------------------------------------------

### 3. Macro data

``` r
library(AER)
```

    ## Loading required package: lmtest

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

    ## Loading required package: sandwich

    ## Loading required package: survival

``` r
library(dynlm)
library(readxl)
```

``` r
con <- read_excel("macroeconomics data set.xlsx")
head(con)
```

    ## # A tibble: 6 x 14
    ##    YEAR   QTR REALGDP REALCONS REALINVS REALGOVT REALDPI CPI_U    M1 TBILRATE
    ##   <dbl> <dbl>   <dbl>    <dbl>    <dbl>    <dbl>   <dbl> <dbl> <dbl>    <dbl>
    ## 1  1950     1   1610.    1059.     198.     361    1186.  70.6  110.     1.12
    ## 2  1950     2   1659.    1076.     220.     366.   1178.  71.4  112.     1.17
    ## 3  1950     3   1723     1131      240.     360.   1196.  73.2  113.     1.23
    ## 4  1950     4   1754.    1098.     272.     382.   1210   74.9  114.     1.35
    ## 5  1951     1   1774.    1123.     243.     422.   1208.  77.3  115.     1.4 
    ## 6  1951     2   1804.    1091.     249.     480.   1226.  77.6  116.     1.53
    ## # … with 4 more variables: UNEMP <dbl>, POP <dbl>, INFL <dbl>, REALINT <dbl>

Consider the distributed lag model of consumption:

$$ \\\\text{log}(C\_t) = \\\\beta\_1 + \\\\beta\_2\\\\text{log}(Y\_t) + \\\\beta\_3\\\\text{log}(C\\\_{t-1}) + \\\\epsilon $$

where

$$ \\\\begin{aligned} C\_t &= \\\\text{real consumption expenditure}, \\\\ Y\_t &= \\\\text{real disposable personal income}, \\\\ \\\\delta &= \\\\frac{\\\\beta\_2}{1 - \\\\beta\_3} \\\\qquad \\\\text{(long-run MPC)} \\\\end{aligned} $$

Fit the model using the macro data.

``` r
# Convert macro data into time series
con_ts <- ts(data = con[, 3:ncol(con)],
             start = c(1950, 1),
             end = c(2000, 4),
             frequency = 4)

# Fit a linear model
con_lm <- dynlm(formula = log(REALCONS) ~ log(REALDPI) + log(L(REALCONS)),
                data = con_ts,
                start = c(1950, 1),
                end = c(2000, 4))

summary(con_lm)
```

    ## 
    ## Time series regression with "ts" data:
    ## Start = 1950(2), End = 2000(4)
    ## 
    ## Call:
    ## dynlm(formula = log(REALCONS) ~ log(REALDPI) + log(L(REALCONS)), 
    ##     data = con_ts, start = c(1950, 1), end = c(2000, 4))
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.035243 -0.004606  0.000496  0.005147  0.041754 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      0.003142   0.010553   0.298  0.76624    
    ## log(REALDPI)     0.074958   0.028727   2.609  0.00976 ** 
    ## log(L(REALCONS)) 0.924625   0.028594  32.337  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.008742 on 200 degrees of freedom
    ## Multiple R-squared:  0.9997, Adjusted R-squared:  0.9997 
    ## F-statistic: 3.476e+05 on 2 and 200 DF,  p-value: < 2.2e-16

Then use the Wald Test to confirm if the long-run MPC $\\\\delta$ is
equal to 1. We test the following hypothesis:

$$ \\\\begin{aligned} H\_0: \\\\delta = 1 &\\\\implies \\\\frac{\\\\beta\_2}{1 - \\\\beta\_3} = 1 \\\\ &\\\\implies \\\\beta\_2 + \\\\beta\_3 = 1 \\\\end{aligned} $$

``` r
linearHypothesis(model = con_lm,
                 hypothesis.matrix = c("log(REALDPI) + log(L(REALCONS)) = 1"))
```

    ## Linear hypothesis test
    ## 
    ## Hypothesis:
    ## log(REALDPI)  + log(L(REALCONS)) = 1
    ## 
    ## Model 1: restricted model
    ## Model 2: log(REALCONS) ~ log(REALDPI) + log(L(REALCONS))
    ## 
    ##   Res.Df      RSS Df  Sum of Sq      F Pr(>F)
    ## 1    201 0.015295                            
    ## 2    200 0.015286  1 9.1197e-06 0.1193 0.7301

``` r
linearHypothesis(model = con_lm,
                 hypothesis.matrix = c("log(REALDPI) = 0",
                                       "log(L(REALCONS)) = 1"))
```

    ## Linear hypothesis test
    ## 
    ## Hypothesis:
    ## log(REALDPI) = 0
    ## log(L(REALCONS)) = 1
    ## 
    ## Model 1: restricted model
    ## Model 2: log(REALCONS) ~ log(REALDPI) + log(L(REALCONS))
    ## 
    ##   Res.Df      RSS Df  Sum of Sq      F  Pr(>F)  
    ## 1    202 0.015843                               
    ## 2    200 0.015286  2 0.00055706 3.6442 0.02789 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
