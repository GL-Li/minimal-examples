# ref: https://rpubs.com/davoodastaraky/mtRegression
#      http://www.stat.yale.edu/Courses/1997-98/101/linreg.htm 
#      http://r-statistics.co/Linear-Regression.html

# workflow ====================================================================
#
# - Formulate the question or hypothesis, iterate and update after the
#   following steps.
#
# - Examine the data, pay attention to
#   - colinearity of independent variables
#   - lurking variables, those missing from data but influence the observation
#     such as time that cause seasonal variation.
#   - outlier
#   - influential data, for example, the data point at x = 10, while all other 
#     100 data points in x ~ (0, 1)
#
# - model (variable) selection
#   - stepwise selection
#   - nested likelihood ratio test
#
# - model diagnosis
#   - detecting colinearity of selected independent variables
#   - residual vs fitted value
#   - residual normality


# formulate the question ======================================================
# Dataset to use: mtcars.
#
# We are going to find out the set of the independent variables that account for
# the most of the variation in dependent varaible mpg.


# mtcars dataset ==============================================================
# This dataset is ideal for demonstrate linear regression in R. The variabel 
# mpg depends on all other variables. 
# > ?mtcars to view the description of each variable. 
# am and vs are actually categorical variable. But they have only two levels so
# it is ok to set them as numerical variables with values 0 and 1.
str(mtcars)

# .. colinearity ====
# variables are highly correlated
cor(mtcars)  # must be all numerical variables
pairs(mtcars)

# .. outliers winsorization ====
# how to quickly check the outliers of each variable?
# boxplot(mtcars) plot boxplot of each column but the scale varies too much.
# built-in function scale() works like a charm.
# variables hp, wt, qsec and carb has outliers, as seen in the plot
boxplot(scale(mtcars))

# Check each of them with box-whisker-point plot
boxplot_point <- function(x, width = 0.1){
    # x: numeric vector
    # width: numeric, width of jitered scatter plot
    n <- length(x)
    boxplot(x, outline = FALSE, ylim = c(min(x), max(x)))
    points(rep(1, n) + (runif(n) - 0.5) * width, x)
}

winsor_x <- function(x){
    # To replace outliers with less extreme values
    # x: numeric vector
    outliers <- boxplot(x, plot = FALSE)$out
    x_good <- x[!x %in% outliers]
    x_min <- min(x_good)
    x_max <- max(x_good)
    x[x < x_min] <- x_min
    x[x > x_max] <- x_max
    return(x)
}

winsor_df <- function(df){
    # df: numeric data.frame
    as.data.frame(lapply(df, winsor_x))
}

df <- winsor_df(mtcars)

boxplot_point(mtcars$hp)    # continuous, 1 true outlier
boxplot_point(df$hp)
boxplot_point(mtcars$wt)    # 3 points are separated from the rest 29
boxplot_point(mtcars$qsec)  # continuous, 1 true outlier
boxplot_point(mtcars$carb)  # discrete values, 1 outlier



# Build linear model ==========================================================
# .. effect of winsorization ====
# instant improvement
lr0 <- lm(mpg ~ ., mtcars)   # adjusted R^2: 0.8066
lr1 <- lm(mpg ~., df)        #               0.8232

# .. stepwise selection ====
lr0_step <- step(lm(mpg ~ ., mtcars))      # 0.8336
lr1_step <- step(lm(mpg ~ ., df))          # 0.8455

# .. Nested likelihood ratio test ====
# The purpose is to verify the model selected by step() is correct by adding
# and removing variables.
lr1_1 <- lm(mpg ~ wt, df)
lr1_2 <- lm(mpg ~ wt + qsec, df)
lr1_3 <- lm(mpg ~ wt + qsec + am, df)
lr1_4 <- lm(mpg ~ wt + qsec + am + disp, df)

# anova shows that addition of disp to the model is insignificant
anova(lr1_1, lr1_2, lr1_3, lr1_4)
    # Model 1: mpg ~ wt
    # Model 2: mpg ~ wt + qsec
    # Model 3: mpg ~ wt + qsec + am
    # Model 4: mpg ~ wt + qsec + am + disp
    #   Res.Df    RSS Df Sum of Sq       F    Pr(>F)    
    # 1     30 268.87                                   
    # 2     29 180.79  1    88.077 15.6019 0.0005052 ***   # qsec p = 0.000505
    # 3     28 157.10  1    23.689  4.1963 0.0503483 .     # am p = 0.0505
    # 4     27 152.42  1     4.683  0.8295 0.3704730       # disp p = 0.37


# model diagnosis ============================================================
# check the quality of final model

# .. colinearity of selected independent variables ====
# wt and am are correlated
pairs(df[, c("wt", "qsec", "am")])
cor(df[, c("wt", "qsec", "am")])

# .. residual distribution ====
plot(lr1_1$fitted.values, lr1_1$residuals)
qqnorm(lr1_1$residuals)
lines(c(-1, 1), c(-1, 1))
