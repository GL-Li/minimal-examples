# Tutorial materials: https://www.guru99.com/r-anova-tutorial.html

# Workflow of ANOVA (also applies to any statistical analysis)
# - glimpse(df) to check the data
# - descriptive statistics to get an idea of the data
# - visualize the data, this place a check on subsequent analysis
# - ANOVA

# Concepts =====================================================================
# ANOVA stands for ANalysis Of VAriance. It is an enhanced version of t-test as 
# it is able to compare the means of more than two groups.

# One-way ANOVA takes one numerical variable and one categorical variable
# and compare the means in each level. 

# Two-way ANOVA takes one numercial variable and two categorical variabels and 
# compares the means in each level within a categorical varibale. It looks like
# two one-way ANOVA but actually is different when the two categorical variabls
# have interactions.

# The calculation of ANOVA is based on lm() of the data, which can explain the 
# difference between one-way and two-way variables as additional variable will 
# change the fitting parameters of existing variables.

# data ========================================================================
# Three treatment groups A, B, and C and three poison levels 1 < 2 < 3. 

library(dplyr)
library(ggplot2)

df <- structure(
    list(
        time = c(
            0.31, 0.45, 0.46, 0.43, 0.36, 0.29, 0.4, 
            0.23, 0.22, 0.21, 0.18, 0.23, 0.82, 1.1, 0.88, 0.72, 0.92, 0.61, 
            0.49, 1.24, 0.3, 0.37, 0.38, 0.29, 0.43, 0.45, 0.63, 0.76, 0.44, 
            0.35, 0.31, 0.4, 0.23, 0.25, 0.24, 0.22, 0.45, 0.71, 0.66, 0.62, 
            0.56, 1.02, 0.71, 0.38, 0.3, 0.36, 0.31, 0.33
        ), 
        poison = structure(
            c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 1L, 1L, 1L, 1L, 
              2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 
              3L, 3L, 3L, 3L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L), 
            .Label = c("1", "2", "3"), 
            class = c("ordered", "factor")
        ), 
        treat = structure(
            c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 
              2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 
              3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L), 
            .Label = c("A", "B", "C", "D"), 
            class = "factor"
        )
    ), 
    row.names = c(NA, -48L), 
    class = "data.frame"
)
glimpse(df)



# One-way ANOVA ===============================================================
# The objective: To test the hypothesis:
# - H0: There is no difference in average time among groups of poison levels.
# - HA: The average time is different in at least one group of poison levels.

# .. descriptive statistics ====
df %>% 
    group_by(poison) %>%
    summarise(count = n(),
              avg_time = mean(time, na.rm = TRUE),
              sd_time = sd(time, na.rm = TRUE))

# .. boxplot visualization ====
# poison 3 is very different
ggplot(df, aes(poison, time, color = poison)) + 
    geom_boxplot() + 
    geom_jitter(width = 0.1)


# .. ANOVA ====
# .... aov() ====
aov_pos <- aov(time ~ poison, df)

# .... overall hypothesis test ====
# The p-value is << 0.05, so we have enough evidence to reject the null 
# hypotheis, that is, there is a significant difference in time among
# the groups of poison levels.
summary(aov_pos)

# .... pair-wise comparison ====
# Compare the difference between each pair of poison level. The result shows 
# that pairs 3-1 and 3-2 are significantly different while pair 2-1 is not. The
# result agrees to the visualization.
TukeyHSD(aov_pos)


# Two-way ANOVA ================================================================
# time ~ poison + treat
# do not analyze cross group such as A-1, B-3

# .. descriptive stats ====
# add descriptive stats for treat
df %>%
    group_by(treat) %>%
    summarise(count = n(),
              mean = mean(time, na.rm = TRUE),
              sd = sd(time, na.rm = TRUE))

# .. boxplot visualization ====
# Same as in one-way ANOVA, we only plot for a single variable, poison or treat

df %>% ggplot(aes(treat, time, color = treat)) + 
    geom_boxplot() +
    geom_jitter(width = 0.1)


# .. ANOVA ====

# .... aov() ====
aov_pos_tre <- aov(time ~ poison + treat, df)

# .... overall hypothesis
# Both poison and treat significantly change the survival time. If there is 
# interaction between poison and treat variables, the effect of each will be
# different from their respective one-way ANOVA
summary(aov_pos_tre)

# .... pair-wise comparison ====

TukeyHSD(aov_pos_tre)

