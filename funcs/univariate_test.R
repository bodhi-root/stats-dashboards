# File: univariate_test.R
# Created: 2020-06-22

source("funcs/univariate.R")

df <- read.csv("data/sample/hsb2.csv", stringsAsFactors=F)
x <- df$write

### Basic Stats ###############################################################

uni_stats <- proc_univariate(x)

# The UNIVARIATE Procedure
# Variable:  write  (writing score)
#                             Moments
# N                         200    Sum Weights                200
# Mean                   52.775    Sum Observations         10555
# Std Deviation      9.47858602    Variance             89.843593
# Skewness           -0.4820386    Kurtosis            -0.7502476
# Uncorrected SS         574919    Corrected SS         17878.875
# Coeff Variation    17.9603714    Std Error Mean      0.67023725

moments_expected <- c(
  `N` = 200,
  `Mean` = 52.775,
  `Std Deviation` = 9.47858602,
  `Skewness` = -0.4820386,
  `Uncorrected SS` = 574919,
  `Coeff Variation` = 17.9603714,
  #`Sum Weights` = 200,
  `Sum Observations` = 10555,
  `Variance` = 89.84359,
  `Kurtosis` = -0.7502476,
  `Corrected SS` = 17878.875,
  `Std Error Mean` = 0.67023725
)

(uni_stats$moments - moments_expected) < 0.0000001
max(uni_stats$moments - moments_expected) < 0.0000001
# variance is close, but larger than expected error for some reason

### More Stats ################################################################

#               Basic Statistical Measures
#     Location                    Variability
# Mean     52.77500     Std Deviation            9.47859
# Median   54.00000     Variance                89.84359
# Mode     59.00000     Range                   36.00000
#                       Interquartile Range     14.50000

location_expected <- c(
  `Mean` = 52.775,
  `Median` = 54,
  `Mode` = 59
)

variability_expected = c(
  `Std Deviation` = 9.47859,
  `Variance` = 89.84359,
  `Range` = 36,
  `Interquartile Range` = 14.5
)

max(uni_stats$location - location_expected) < 0.00000001

# std deviation is printed with lower precision in SAS output, looks more off here
# variance is still different
# interquartile range is also different due to how we calculate quantiles
uni_stats$variability - variability_expected


# Quantiles (Definition 5)
# Quantile      Estimate
# 100% Max          67.0
# 99%               67.0
# 95%               65.0
# 90%               65.0
# 75% Q3            60.0
# 50% Median        54.0
# 25% Q1            45.5
# 10%               39.0
# 5%                35.5
# 1%                31.0
# 0% Min            31.0

quantiles_expected <- c(
  `0%` = 31,
  `1%` = 31,
  `5%` = 35.5,
  `10%` = 39,
  `25%` = 45.5,
  `50%` = 54.0,
  `75%` = 60,
  `90%` = 65,
  `95%` = 65,
  `99%` = 67,
  `100%` = 67
)

# we could use quantile(x, type=3) to get an exact match, but let's stick with the R default
# this has slight differences for the test data set at quantiles 5% and 25%.
uni_stats$quantiles - quantiles_expected

### Location Test #############################################################

#            Tests for Location: Mu0=0
#
# Test            -Statistic-    -----p Value------
# 
# Student's t     t  78.74077    Pr > |t|    <.0001
# Sign            M       100    Pr >= |M|   <.0001
# Signed Rank     S     10050    Pr >= |S|   <.0001


t_result <- t.test(x)
print(t_result)                      
t_result$statistic # 78.74077
t_result$p.value   # <.0001

s_result <- sign.test(x)
s_result$statistic # 100
s_result$p.value   # 1.244603e-60

# The wilcox test was tricky to match up with SAS.
# According to this page: http://rcompanion.org/rcompanion/d_10.html
# the results from wilcox.test match those from SAS.  However, the test
# statistic that is reported is different (as can be seen from the same
# analysis in SAS: http://www.biostathandbook.com/wilcoxonsignedrank.html).
# R reports a statistic that is labeled "V".  This is the same as the 
# statistic called "T" in [Wikipedia](https://en.wikipedia.org/wiki/Wilcoxon_signed-rank_test)
# and the statistic calculated by hand on this page: 
#  https://sphweb.bumc.bu.edu/otlt/mph-modules/bs/bs704_nonparametric/BS704_Nonparametric6.html#:~:text=The%20test%20statistic%20for%20the,W%2D%20would%20be%20similar).
# The Wikipedia page focuses on a test statistic called "W" which does
# seem more useful.  This is defined as "the sum of the positive ranks
# minus the sum of the negative ranks".  SAS uses a statistic called "S"
# which appears to be half of "W"  Instead of trying to reproduce the
# wilcox test I just report the V statistic and the p-value.

y <- rnorm(100)
y <- c(-3.1
-6.3,
1.2,
-2,
-1,
-7.2,
5.6,
2.2,
-12,
-12.3,
-5.3,
-0.1,
-23.4
)


w_result <- wilcox.test(x, mu=0, correct=F)
w_result$statistic
w_result$p.value

library(dplyr)
df <- tribble(
  ~name, ~aug, ~nov,
  "Balsam_Spire",	8.1,	11.2,
  "Beaupre",	10,	16.3,
  "Hazendans",	16.5,	15.3,
  "Hoogvorst",	13.6,	15.6,
  "Raspalje",	9.5,	10.5,
  "Unal",	8.3,	15.5,
  "Columbia_River",	18.3,	12.7,
  "Fritzi_Pauley",	13.3,	11.1,
  "Trichobel",	7.9,	19.9,
  "Gaver",	8.1,	20.4,
  "Gibecq",	8.9,	14.2,
  "Primo",	12.6,	12.7,
  "Wolterson",	13.4,	36.8
)
wilcox.test(df$aug, df$nov, paired=T)
wilcox.test(df$aug - df$nov)

sign.test(df$aug - df$nov)
