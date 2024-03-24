# 1. Learn to separate data preparation and analysis into different program files.
# 2. Learn about the logic of regression models and identify both population and sample models.
# 3. Learn to estimate sample regression models using ordinary least squares (OLS) and interpret regression coefficient estimates.
# 4. Learn to make statistical inferences from sample coefficient estimates to population parameters.
# 5. Learn about three types of sum of squares and overall model fit.
# 6. Learn to present regression results.

# 1. How do we identify a statistical model from a theoretical argument?
# 2. How do we understand the logic of regression analysis?
# 3. How do we prepare data for the variables in the statistical model?
# 4. How do we estimate the statistical model?
# 5. How do we interpret the model estimates?
# 6. How do we make statistical inferences based on the model estimates?
# 7. How do we explain overall model fit?

# Based on pwt package's 5.6 version and models based on the following article:
# Frankel, Jeffrey A., and David Romer. 1999. "Does trade cause growth?"
# American Economic Review 89(3): 379–99.


# install.packages(c("devtools", "data.table", "stargazer", "ggplot2", "gridExtra", 
#                    "car", "pwt", "broom", "GGally"), dependencies = TRUE)
rm(list = ls(all = TRUE))

library(pwt)

data("pwt5.6")
names(pwt5.6)

# economically active population
pwt5.6$labor <- pwt5.6$rgdpch * pwt5.6$pop / pwt5.6$rgdpwok

# total population and new population variables
summary(pwt5.6$pop)
summary(pwt5.6$labor)

# subset of pwt5.6
pwt.85 <- pwt5.6[pwt5.6$year == 1985,
                 c("wbcode", "country", "year", "rgdpwok", "rgdpch",
                   "open", "labor", "continent")]

# land area variable from World Bank statistics

# devtools::install_github("GIST-ORNL/wbstats")

library(wbstats)
library(data.table)

# search for a list of area related variables choose one needed
wb_search(pattern = "total area")

# extract land area variable based on indicator, sq km
area <- data.table(
  wb_data(country = "all", indicator = c("AG.LND.TOTL.K2"))
)

tail(area, n=1)

names(area)[names(area) == "AG.LND.TOTL.K2"] <- "landarea"
names(area)[names(area) == "date"] <- "year"

# country mappings
countries <- data.table(wb_countries())
names(countries)

# merge area and countries according to iso2c
area <- merge(area, countries, by = c("iso2c"), sort = TRUE)
area <- data.frame(area)

# keep needed variables and observations from area dataset
area <- area[area$year == 1985,
             c("iso3c.x", "landarea", "year", "region", "latitude", "longitude")]

names(area)[names(area) == "iso3c.x"] <- "wbcode"
names(area)[names(area) == "latitude"] <- "lat"
names(area)[names(area) == "longitude"] <- "long"


# keep only observations that match with pwt.85 data
final.85 <- merge(pwt.85, area, by = c("wbcode", "year"),
                  all.x = TRUE, sort = TRUE)

class(final.85)
names(final.85)
tail(final.85, n = 1)

# new variables for analysis
final.85$logy <- log(final.85$rgdpch)
final.85$loglab <- log(final.85$labor)
final.85$logland <- log(final.85$landarea)

# save R dataset
save(final.85, file = "data/final.85.RData")

### Visualize and inspect data
rm(list = ls(all = TRUE))
load("data/final.85.RData")

library(stargazer)

# formatted descriptive statistics
stargazer(final.85, type = "text", median = TRUE)

library(ggplot2)
library(gridExtra)

# histogram of rgdpch as data object
hist1 <- ggplot(final.85, aes(rgdpch)) +
  geom_histogram() +
  geom_vline(xintercept = 4423, color = "red") # mean

# histogram of logy as data object
hist2 <- ggplot(final.85, aes(logy)) +
  geom_histogram() +
  geom_vline(xintercept = 7.891, color = "red") # mean

# display plots together
grid.arrange(hist1, hist2, ncol = 2)


# open and log of real income per capita
ggplot(final.85, aes(x = open, y = logy))+
  geom_point() +
  geom_vline(xintercept = 73.874) +
  geom_hline(yintercept = 7.891) +
  geom_text(aes(label = country), hjust = 0, vjust = 0)


## OLS model
model1 <- lm(logy ~ open + loglab + logland, data = final.85)
summary(model1)

# SD error of coefficients
# variance-covariance matrix of B(open var) in model1
vcov(model1)

## Population parameter of interest
# t-test from model1 and CI below

confint(model1)

## Overall model fit
# TSS = ESS + RSS. OLS minimizes the RSS following Gauss-Markov theorem.

# ESS and RSS from model output
anova(model1)

# Statistical  results of the regression
library(stargazer)

stargazer(model1, type = "text", no.space=TRUE,
          omit.stat = c("ser"), model.names = FALSE,
          dep.var.labels.include = FALSE, dep.var.caption = "")

library(broom)
library(GGally)

# plot coefficients, intercept has a large value so excluded
ggcoef(model1, exclude_intercept = TRUE)


### Miscellaneous 

# OLS model
model1 <- lm(logy ~ open + loglab + logland, data = final.85)

# nature of model output
names(model1)
class(model1)
mode(model1)

# anova table and results
anova(model1)

# model coefficients
coefficients(model1)

# CI of coefficient estimates, 95% and 99%
confint(model1)
confint(model1, level = 0.99)

# covariance matrix for model coefficient estimates
vcov(model1)

# list predicted values of the fitted model
fitted(model1)

# list residuals of the fitted model
residuals(model1)

# use fitted model to get predicted y
predict(model1)


## Matrix algebra for OLS

# observations without missing values
final.85 <- final.85[complete.cases(final.85), ]

# ones is for the intercept term
x <- cbind(1, as.matrix(final.85[, c("open", "loglab", "logland")]))

# dependent variable
y <- final.85[, "logy"]

# matrix calculation of OLS estimates
# B = (X'X) ^−1 X'Y
solve(t(x) %*% x) %*% t(x) %*% y

# compute TSS, ESS, and RSS TSS-total sum of squares in y
sum((final.85$logy - mean(final.85$logy))^2)

# ESS-explained sum of squares in y
sum((predict(model1) - mean(final.85$logy))^2)

# RSS-residual sum of squares in y
sum((final.85$logy - predict(model1))^2)


## Partial regression
# obtain logy unexplained by population and land
model2 <- lm(logy ~ loglab + logland,
             data = final.85, na.action = na.exclude)
final.85$yres <- residuals(model2)

# obtain open unexplained by population and land
model3 <- lm(open ~ loglab + logland,
             data = final.85, na.action = na.exclude)
final.85$openres <- residuals(model3)

# effect of open on logy (population and land held constant)
model4 <- lm(yres ~ openres, data = final.85)
summary(model4)

# partial regression plot
ggplot(final.85, aes(x = openres, y = yres)) +
  geom_point() +
  geom_text(aes(label = country, hjust = 0, vjust = 0)) +
  stat_smooth(method = "lm")

# avPlot() function to demonstrate the partial regression plot 
# for each independent variable
library(car)

model1 <- lm(logy ~ open + loglab + logland, data = final.85)
avPlots(model1)

# pairwise relationships among variables
library(GGally)

# ggscatmat() for numeric data, for categorical data, use ggpairs()
ggscatmat(final.85, columns = c("logy", "open", "loglab", "logland"))




























