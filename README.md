# Data Wrangling

``` r
# Data wrangling. (tidyverse)

TData <- read.csv("Data/TBillData.csv") %>% # load

         rename(TBillRate = DTB3) %>% # renaming column 

         subset(
           
           DATE < "2023-01-01" & 
             
           DATE <= "1991-10-01") # making vectors equal length


InfData <- read.csv("Data/PriceDeflator(SAdjust).csv") %>% # load
 
         subset(
           
           DATE >= "1954-01-01" & 
             
           DATE <= "1991-10-01") %>% # only after 1954

         rename(Deflator = A191RI1Q225SBEA) # rename column

###

TData_ts <- ts(TData$TBillRate, 
               
               start = c(year(TData$DATE[1]), 
                         
                         quarter(TData$DATE[1])),
               
               frequency = 4) # convert to ts

decomp <- decompose(TData_ts) # Perform seasonal decomposition on data.

TData_ts <- TData_ts - decomp$seasonal #seasonally adjust 

rm(decomp)

###

InfData_ts <- ts(InfData$Deflator, 
               start = c(year(InfData$DATE[1]), 
                         
                         quarter(InfData$DATE[1])),
               
               frequency = 4)

# At this point, R interprets the data as Time series, seasonally adjusted, vectors of equal length, indexed by recognizable date columns. This means I can begin doing actual tests. 

# For Johansen: 

# Step 1: check that all variables are I(1). [y] 
# Step 2: Unrestricted VAR. Test whether white noise errors. [n]
# Step 3: Estimate specific VECM, do trace and max eigenvalue tests. 
# Step 4: Impose r, reduced rank regression. 
# Step 5: forecasts; model adequacy. 
```

# Data Plotting and Comparison

``` r
plot(TData_ts, main = "GDP Deflator and US 3-Month Treasury Bill Rate",
               xlab = "",
               ylab = "",
               col = "red3")
lines(InfData_ts, 
      col = "blue3")
legend("topright",
       c("T-Bill", "Deflator"), 
       col = c("red3", "blue3"), 
       lty = 1,
       cex = 0.6)
```

![](README_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
# must compare this with graph from their actual paper. 
```

``` r
# Comparing with original paper. 

knitr::include_graphics("Images/FirstPlot.png", 
                         dpi = 300)
```

<img src="Images/FirstPlot.png" width="85%" height="85%" style="display: block; margin: auto;" />

# Formal Dickey-Fuller tests for a Unit Root

# Vector Autoregression and Tests for White Noise Errors

``` r
plot(density(StdResid), 
     main = "Distribution of Unrestricted VAR Residuals", 
     col = "red3")
lines(density(ActNorm), 
      col = "blue3")
legend("topright",
       c("Residuals", "Standard Normal Distribution"),
       col = c("red3", "blue3"),
       lty = 1, 
        cex = 0.6)
```

<img src="README_files/figure-markdown_github/unnamed-chunk-8-1.png" width="75%" height="75%" style="display: block; margin: auto;" />

``` r
# mean zero
# Speak about how Kurtosis affects Maximum Likelihood estimations.
```

<img src="README_files/figure-markdown_github/unnamed-chunk-9-1.png" width="75%" height="75%" style="display: block; margin: auto;" />

# Vector Error Correction Mechanism and Eigenvalue Tests

## Trace and Maximum Eigenvalue Tests

## Imposing Restrictions and Estimating the Model

## Model Adequacy

``` r
plot(density(VecRes), 
     main = "Distribution of Restriced VECM Residuals", 
     col = "red3")
lines(density(ActNorm), 
      col = "blue3")
legend("topright",
       c("Residuals", "Standard Normal Distribution"),
       col = c("red3", "blue3"),
       lty = 1,
        cex = 0.6)
```

<img src="README_files/figure-markdown_github/unnamed-chunk-12-1.png" width="75%" height="75%" style="display: block; margin: auto;" />

``` r
# Kurtosis once again. Very high. Maximum Likelihood unlikely to be appropriate.
```

``` r
acf(VecRes, 
    col="red3", 
    main = NA, 
    lag.max = 20)
mtext("Autocorrelation Functions for the Restricted VECM", 
      line=2, 
      cex=1)
```

<img src="README_files/figure-markdown_github/M-1.png" width="85%" height="85%" style="display: block; margin: auto;" />

# Impulse Response Functions

``` r
# Creating IRFS. 

IrfVec <- vec2var(Johansen21, r = 1)

TempIRF1 <- vars::irf(IrfVec, 
                      impulse = "Interest", 
                      response = "Inflation", 
                      n.ahead = 40, 
                      boot = TRUE, runs = 100)

TempIRF2 <- vars::irf(IrfVec, 
                      impulse = "Interest", 
                      response = "Interest", 
                      n.ahead = 40, 
                      boot = TRUE, runs = 100)

PermIrf1 <- vars::irf(IrfVec, 
                      impulse = "Inflation", 
                      response = "Interest",
                      n.ahead = 40, 
                      boot = TRUE, runs = 100)

PermIrf2 <- vars::irf(IrfVec, 
                      impulse = "Inflation", 
                      response = "Inflation",
                      n.ahead = 40, 
                      boot = TRUE, runs = 100)
```

``` r
# lots and lots of very ugly code here that you can painstakingly read through in the Code folder. Hideous code though, I had not yet, at this point in my programming journey, learned how to code in a pretty way. 

source("code/IRFs.R")

grid.arrange(plot2, plot3, plot1, plot4, 
             ncol = 2)
```

![](README_files/figure-markdown_github/unnamed-chunk-14-1.png)

``` r
rm(plot1, plot2, plot3, plot4)
```

``` r
knitr::include_graphics("Images/Impulses.png", 
                         dpi = 300)
```

<img src="Images/Impulses.png" width="80%" height="80%" style="display: block; margin: auto;" />

# Variance Decomposition

<img src="Images/Variance.png" width="100%" />

``` r
source("Code/VarianceDecomp.R")

grid.arrange(plot5, plot6, ncol = 1)
```

<img src="README_files/figure-markdown_github/unnamed-chunk-17-1.png" width="80%" height="80%" style="display: block; margin: auto;" />

``` r
## Nominal Interest is Red and Inflation is Blue. 
```

# Bibliography

Crowder, W.J & Hoffman, D.L. 1996. The Long-Run Relationship between
Nominal Interest Rates and Inflation: The Fisher Equation Revisited.
*Journal of Money, Credit and Banking*, 28(1):102-118.

Enders, W. 2015. Applied Econometric Time Series. 4th edition. New
Jersey: John Wiley & Sons, Inc. 

Gross Domestic Product: Implicit Price Deflator \[Online\]. \[n.d.\].
Available: <https://fred.stlouisfed.org/series/A191RI1Q225SBEA> \[2023,
May 10\].

Monetary Policy Principles and Practice \[Online\]. \[n.d.\]. Available:
<https://www.federalreserve.gov/monetarypolicy/historical-approaches-to-monetary-policy.htm>
\[2023, May 10\].

3-Month Treasury Bill Secondary Market Rate, Discount Basis \[Online\].
\[n.d.\] Available: <https://fred.stlouisfed.org/series/DTB3> \[2023,
May 10\].
