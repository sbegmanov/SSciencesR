## Covariance and Correlation
# 1. Learn to separate data preparation and analysis into different program files.
# 2. Learn to prepare data for analysis.
# 3. Learn to visualize the relationship between two variables using scatter plot.
# 4. Learn about covariance and correlation coefficient, both sample and population versions.
# 5. Learn to make statistical inferences from sample coefficient estimate to population parameter.
# 6. Learn to compute and visualize correlation across different levels of a group variable.

library(tidyverse)

pwt7 <- read.csv("data/pwt70_w_country_names.csv", header = TRUE, 
                 strip.white = TRUE, stringsAsFactors = FALSE,
                 na.strings = c("NA", ""))

library(DataCombine)

pwt7 <- pwt7[order(pwt7$country, pwt7$year), ]

pwt7 <- slide(pwt7, Var = "rgdpl", NewVar = "rgdplead", GroupVar = "country", slideBy = 1)
pwt7 <- slide(pwt7, Var = "rgdpl", NewVar = "rgdplag", GroupVar = "country", slideBy = -1)

pwt7$growth <- (pwt7$rgdpl - pwt7$rgdplag) * 100 / pwt7$rgdplag
pwt7g <- pwt7[, c("isocode", "country", "year", "rgdpl", "openk", "growth")]

pwt7 <- pwt7[pwt7$isocode != "CH2",]
pwt7g <- pwt7g[pwt7g$year >= 1960,]

names(pwt7g)
class(pwt7g)
save(pwt7g, file = "data/pwt7g.RData")

# before running program, remove all objects in workspace
rm(list = ls(all = TRUE))
load("data/pwt7g.RData")

summary(pwt7g$growth)
summary(pwt7g$openk)


library(ggplot2)

# scatter plot
ggplot(pwt7g, aes(x = openk, y = growth))+
  geom_point() +
  geom_vline(xintercept = 73.59) + # a line through the mean of openk
  geom_hline(yintercept = 2.304) # aline through the mean of growth


# sample covariance, only non-missing observations
cov(pwt7g$growth, pwt7g$openk, use = "complete.obs")

# sample correlation coefficient, non-missing observations
cor(pwt7g$openk, pwt7g$growth,use = "complete.obs")


# making inferences over population correlation between openness and growth
cor.test(pwt7g$openk, pwt7g$growth, use = "complete.obs", "two.sided", "pearson")


library(dplyr)

# correlation coefficient for each year export output
table1 <- pwt7g %>% 
  group_by(year) %>%
  summarize(corgo = cor(growth, openk, use = "complete.obs")) %>% 
  as.data.frame()

# single-year calculation
cor(pwt7g$openk[pwt7g$year == 2008], pwt7g$growth[pwt7g$year == 2008], 
    use = "complete.obs")
  
library(ggplot2)
library(gridExtra)

# produce annual sample correlation plot objects
corgo1 <- qplot(year, corgo, data = table1)
corgo2 <- ggplot(table1, aes(year, corgo)) +
  geom_line()

grid.arrange(corgo1, corgo2, ncol = 2)

library(broom)
# annual correlation coefficient export output
table2 <- pwt7g %>%
  group_by(year) %>%
  do(tidy(cor.test(.$growth, .$openk, use = "complete.obs"))) %>%
  as.data.frame()

names(table2)

# descriptive statistics for estimate and p value
summary(table2$estimate)
summary(table2$p.value)


library(stargazer)

# display statistically significant cases
stargazer(table2[table2$p.value <= 0.05, 
                 c("year", "estimate", "p.value")],
          type = "text", summary = FALSE, rownames = FALSE)

table2 %>% 
  ggplot(aes(year, p.value)) +
  geom_point() +
  geom_hline(yintercept=0.05)

### Miscellaneous 

# deviation of growth & openness from mean
d1 <- pwt7g$growth - mean(pwt7g$growth, na.rm = TRUE)
d2 <- pwt7g$openk - mean(pwt7g$openk, na.rm = TRUE)

# sample size minus one
s1 <- sum(!is.na(pwt7g$growth) & !is.na(pwt7g$openk)) - 1

# sample covariance
sum(d1 * d2, na.rm = TRUE) / s1

# doing everything in one step
(sum((pwt7g$growth - mean(pwt7g$growth, na.rm = TRUE))*
       (pwt7g$openk - mean(pwt7g$openk,na.rm = TRUE)), na.rm = TRUE)) /
  (sum(!is.na(pwt7g$growth)&!is.na(pwt7g$openk)) - 1)

# confirm step-by-step calculation
cov(pwt7g$growth, pwt7g$openk, use = "complete.obs")

# compute Pearson correlation coefficient step by step
c1 <- cov(pwt7g$openk, pwt7g$growth, use = "complete.obs")
v1 <- var(pwt7g$openk, na.rm = TRUE)
v2 <- var(pwt7g$growth, na.rm = TRUE)
correlation <- c1 / sqrt(v1 * v2)

# display computed results
cbind(c1, v1, v2, correlation)

# confirm results above
cor(pwt7g$openk, pwt7g$growth, use = "complete.obs")


### Anscombe’s Quartet and the Failures of the Correlation Coefficient
anscombe

# display identical correlation coefficient
cor(anscombe$x1, anscombe$y1)
cor(anscombe$x2, anscombe$y2)
cor(anscombe$x3, anscombe$y3)
cor(anscombe$x4, anscombe$y4)

library(ggplot2)
library(gridExtra)

# generate four scatter plots with regression lines
f1 <- ggplot(anscombe) + aes(x1, y1) + geom_point()
f2 <- ggplot(anscombe) + aes(x2, y2) + geom_point()
f3 <- ggplot(anscombe) + aes(x3, y3) + geom_point()
f4 <- ggplot(anscombe) + aes(x4, y4) + geom_point()

grid.arrange(f1, f2, f3, f4, ncol = 2)

## Other correlation test statistics
# making inferences over population correlation between openness and growth
cor.test(pwt7g$openk, pwt7g$growth, use = "complete.obs", "two.sided", "spearman")

# making inferences over population correlation between openness and growth
cor.test(pwt7g$openk, pwt7g$growth, use = "complete.obs", "two.sided", "kendall")









  

