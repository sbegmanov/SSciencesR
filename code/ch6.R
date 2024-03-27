# 1. Learn about the assumptions of OLS modeling and their impact.
# 2. Learn to diagnose assumption violations in OLS modeling.
# 3. Learn to conduct sensitivity analysis to assumption violations.

# According to the Gauss-Markov theorem, the OLS estimator b is the 
# best linear unbiased estimator (BLUE) of the population parameter 
# β among all unbiased estimators if the following conditions are satisfied:

# 1. The model is linear in parameter and correctly specified.
# 2. The independent variables are not perfectly correlated.
# 3. The long-run average of the error term is zero.
# 4. The error term and the independent variables are uncorrelated.
# 5. The error variance is constant across observations.
# 6. The observations of the error term are not correlated.

# display regression results from Anscombe quartet
a1 <- lm(y1 ~ x1, data = anscombe)
a2 <- lm(y2 ~ x2, data = anscombe)
a3 <- lm(y3 ~ x3, data = anscombe)
a4 <- lm(y4 ~ x4, data = anscombe)

library(stargazer)

stargazer(a1, a2, a3, a4, type = "text", no.space = TRUE,
          model.names = FALSE, notes = "standard errors in parentheses")

library(ggplot2)
library(gridExtra)

# generate four scatter plots with regression lines
F1 <- ggplot(anscombe) + 
  aes(x1, y1) + 
  geom_point() +
  geom_abline(intercept = 3, slope = 0.5)

F2 <- ggplot(anscombe) +
  aes(x2,y2) +
  geom_point() + 
  geom_abline(intercept = 3, slope = 0.5)

F3 <- ggplot(anscombe) +
  aes(x3,y3) +
  geom_point() +
  geom_abline(intercept = 3, slope = 0.5)

F4 <- ggplot(anscombe) +
  aes(x4, y4) +
  geom_point() +
  geom_abline(intercept = 3, slope = 0.5)

# display four scatter plots together
grid.arrange(F1, F2, F3, F4, ncol = 2)


### final.85 data analysis

# install.packages(c("stargazer", "ggplot2", "gridExtra", "broom", "car", 
#                    "lmtest", "sandwich", "interplot", "ape"), dependencies = TRUE)

rm(list = ls(all = TRUE))
load("data/final.85.RData")

library(ggplot2)
library(gridExtra)
library(stargazer)

# formatted descriptive statistics
stargazer(final.85, type = "text", median = TRUE)

# OLS model
model1 <- lm(logy ~ open + loglab + logland, data = final.85)
summary(model1)

library(broom)

# tidy regression output
tidy(model1)

# add additional statistics to original data
final.85v2 <- augment_columns(model1, final.85)

# descriptive statistics for new data
stargazer(final.85v2, type = "text", median = TRUE)

attach(final.85v2)

# residuals against fitted values: check linearity
ggplot(final.85v2, aes(x = .fitted, y = .resid)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_smooth(method = 'loess', se = TRUE)

# the linearity and model specification assumption via 
# the residuals-versus-independent-variables 

# residuals against trade
ropen <- ggplot(final.85v2, aes(x = open, y = .resid)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_smooth(method='loess', se = TRUE) # curvilinear, worth further exploring

# residuals against land
rland <- ggplot(final.85v2, aes(x = logland, y = .resid)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_smooth(method='loess', se = TRUE) # RESET regression

# residuals against labor
rlab <- ggplot(final.85v2, aes(x = loglab, y = .resid)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_smooth(method='loess', se = TRUE) # RESET regression

# display plots together
grid.arrange(ropen, rland, rlab, ncol = 3)

# regression specification error test expanded model
model1.q1 <- lm(logy ~ open + loglab + logland + I(.fittedˆ2), data = final.85v2)











