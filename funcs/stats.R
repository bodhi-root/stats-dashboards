# File: stats.R
# Created: 2020-06-22

#' Calculates the nth moment of a series.  This is modeled after the
#' same function from the e1071 package.
moment <- function(x, order, center=TRUE) {
  
  x_center <- ifelse(center, mean(x), 0)
  mean((x - x_center)^order)
  
}

#' skewness calculation from package e1071.  Note that type=2
#' is the one used by SAS and SPSS.
#' see: https://www.rdocumentation.org/packages/e1071/versions/1.7-3/topics/skewness
skewness <- function(x, type=3) {
  
  g1 <- moment(x, 3, center=T) / (moment(x, 2, center=T)^(3/2))
  
  if (type == 1) {
    
    return(g1)
    
  } else if (type == 2) {
  
    n <- length(x)  
    return(g1 * sqrt(n * (n-1)) / (n-2))
  
  } else if (type == 3) {
    
    n <- length(x)
    return(g1 * (((n-1)/n)^(3/2)))
  
  } else {
    stop(paste0("Invalid valid for type: ", type))
  }
  
}

#' kurtosis calculation from package e1071.  Note that type=2
#' is the one used by SAS and SPSS.
#' see: https://www.rdocumentation.org/packages/e1071/versions/1.7-3/topics/kurtosis
kurtosis <- function(x, type=3) {
  
  g2 <- moment(x, 4, center=T) / (moment(x, 2, center=T)^2) - 3
  
  if (type == 1) {
    
    return(g2)
    
  } else if (type == 2) {
  
    n <- length(x)  
    return((g2 * (n+1) + 6) * (n-1)/((n-2)*(n-3)))
  
  } else if (type == 3) {
    
    n <- length(x)
    return((g2 + 3) * (1-1/n)^2 - 3)
  
  } else {
    stop(paste0("Invalid valid for type: ", type))
  }
  
}

#' Mode
#' source: https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode
#' 
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#' Performs a sign test to determine whether the mean is equal to the given value
#' http://www.r-tutor.com/elementary-statistics/non-parametric-methods/sign-test
#' 
sign.test <- function(x, mu0 = 0) {
  
  n_higher <- sum(x > mu0)
  n_lower  <- sum(x < mu0)
  
  b_result <- binom.test(n_higher, n_higher+n_lower)
  
  list(
    statistic = (n_higher - n_lower) / 2,
    p.value = b_result$p.value
  )
}

