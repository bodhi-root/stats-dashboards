# File: univariate.R
# Created: 2020-06-22
#
# Functions for univariate analysis.  These are intended to produce output
# similar to PROC UNIVARIATE in SAS.

source("funcs/stats.R")

#' Returns statistics similar to what PROC UNIVARIATE returns
#' 
proc_univariate <- function(x) {
  
  v_moments <- c(
    `N` = length(x),
    `Mean` = mean(x),
    `Std Deviation` = sd(x),
    `Skewness` = skewness(x, type=2),
    `Uncorrected SS` = sum(x^2),
    `Coeff Variation` = sd(x) / mean(x) * 100,
    `Sum` = sum(x),
    `Variance` = var(x),
    `Kurtosis` = kurtosis(x, type=2),
    `Corrected SS` = sum((x-mean(x))^2),
    `Std Error Mean` = sd(x) / sqrt(length(x))
  )
  
  v_quantiles <- quantile(x, c(0, 0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99, 1))
  
  v_location <- c(
    `Mean` = mean(x),
    `Median` = v_quantiles["50%"],
    `Mode` = Mode(x)
  )
  names(v_location) <- c("Mean", "Median", "Mode")
  
  v_variability <- c(
    `Std Deviation` = v_moments["Std Deviation"],
    `Variance` = v_moments["Variance"],
    `Range` = diff(range(x)),
    `Interquartile Range` = v_quantiles["75%"] - v_quantiles["25%"]
  )
  names(v_variability) <- c("Std Deviation", "Variance", "Range", "Interquartile Range")
  
  result <- list(
    moments = v_moments,
    location = v_location,
    variability = v_variability,
    quantiles = v_quantiles
  )
  
  result
}


