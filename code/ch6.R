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
model1.q1 <- lm(logy ~ open + loglab + logland + I(.fitted^2), data = final.85v2)

# F test of model difference
anova(model1, model1.q1)


# estimate a curvilinear model for trade openness
model1.q2 <- lm(logy ~ open + loglab + logland + I(open^2), data = final.85v2)

# F test of model difference
anova(model1, model1.q2)
summary(model1.q2)

## Perfect and high multicollinearity

# a variable perfectly correlating with open
final.85v2$open4 <- 4 * final.85v2$open

# correlation between two variables
cor(final.85v2$open, final.85v2$open4, use = "complete.obs")

# estimate model1 adding open4 in two different orderings OLS model
lm(logy ~ open + open4 + loglab + logland, data = final.85v2)


# VIF stat, VIF value of 10 is a sign of serious multicollinearity
library(car)

vif(model1)


## Constant ERROR variance
# non-constant error variance = heteroskedastic error
# test heteroskedasticity, estimate OLS model and create output object

model1 <- lm(logy ~ open + loglab + logland, data = final.85v2)

# Cook/Weisberg score test of constant error variance
ncvTest(model1)

# Breush/Pagan test of constant error variance
library(lmtest)
library(sandwich)

bptest(model1)

# weighted least squares
# corrections for non-constant error variance
model1.wls <- lm(logy ~ open+loglab+logland, weights = 1 / open, data = final.85v2)
summary(model1.wls)

# report default HC3 robust standard errors
model1.hc3 <- coeftest(model1, vcov = vcovHC)
model1.hc3

# report HC1 robust standard errors as Stata
# variants:"HC3","HC","HC0","HC1","HC2","HC4","HC4m","HC5"
model1.hc1 <- coeftest(model1, vcov = vcovHC(model1, type = "HC1"))
model1.hc1

# request the variance-covariance matrix HC3
vcovHC(model1, type = "HC3")

# request the variance-covariance matrix HC1
vcovHC(model1, type = "HC1")


## Independence of ERROR Term Observations

# distribution of residuals in each region
ggplot(final.85v2, aes(.fitted, .resid)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  facet_wrap(~continent)

# scatter plot of trade and income by region
ggplot(final.85, aes(open, logy)) +
  geom_point() +
  facet_wrap( ~ continent) +
  stat_smooth(method = "lm", se = FALSE)

# show attributes
attributes(final.85v2$continent)

# change value labels of factor variable continent
levels(final.85v2$continent) <- c(".Africa", ".C.N.America",
                                  ".S.America", ".Asia", ".Europe", ".Oceania")
# model with all regional dummies and no intercept
lm(logy ~ 0 + continent + open + loglab + logland,  data = final.85v2)

# display model with all regional dummies but one region will be automatically 
# excluded due to perfect multicollinearity
lm(logy ~ continent + open + loglab + logland, data = final.85v2)

# display full model ouput
model1.reg <- lm(logy ~ continent + open + loglab + logland, data = final.85v2)

# display model output
summary(model1.reg)

# compare against original model
anova(model1, model1.reg)

# interaction model
model1.int <- lm(logy ~ open * continent + loglab + logland, data = final.85v2)

# compare models
anova(model1, model1.int)
anova(model1.reg, model1.int)

# display output
summary(model1.int)

# The effect maybe due to chance or actually different from zero in the population. 
library(interplot)

# plot effect of trade conditional on region
interplot(model1.int, var1 = "open", var2 = "continent") +
  geom_hline(yintercept = 0, linetype = "dashed")



library(lmtest)
library(multiwayvcov)

# estimate OLS model and create output object
model1 <- lm(logy ~ open + loglab + logland, data = final.85v2)

# show OLS results for comparison
summary(model1)

#  robust standard errors clustered over a particular unit-level 
# request clustered variance and covariance matrix
# for 6 regions are appropriate, here demonstrated for pedagogical purposes
vcov_region <- cluster.vcov(model1, final.85v2$continent)

# display clustered matrix
vcov_region

# request test statistics and p values
model1.cl <- coeftest(model1, vcov_region)
model1.cl


### Influential observations
library(car)


# influence plot for influential observations
influencePlot(model1)

# create observation id
final.85v2$id <- as.numeric(row.names(final.85v2))

library(ggplot2)

# identify obs with Cook's D above cutoff
ggplot(final.85v2, aes(id, .cooksd)) +
  geom_bar(stat = "identity", position = "identity") +
  xlab("Obs. Number") + 
  ylab("Cook's distance") + 
  geom_hline(yintercept = 0.03) +
  geom_text(aes(label = ifelse((.cooksd > 0.03), id, "")), 
            vjust = -0.2, hjust = 0.5)

# list observations whose cook's D above threshold
final.85v2[final.85v2$.cooksd > 0.03, 
           c("id", "country", "logy", "open", ".std.resid", ".hat", ".cooksd")]

# re-estimate model1 without Singapore
model1.no1 <- lm(logy ~ open + loglab + logland, 
                 data = final.85v2[final.85v2$.cooksd < 0.18, ])
summary(model1.no1)

# re-estimate model 1 without Singapore and five others
model1.no2 <- lm(logy ~ open + loglab + logland, 
                 data = final.85v2[final.85v2$.cooksd < 0.03,])
summary(model1.no2)


### Normality Test

qqPlot(model1, distribution = "t", simulate = TRUE)

# Shapiro-Wilk normality test
# normality test
shapiro.test(final.85v2$.resid)

# test log-transformed open variable
model1.no3 <- lm(logy ~ log(open) + loglab + logland, data = final.85v2)

# normality test
shapiro.test(residuals(model1.no3))

# show model output
summary(model1.no3)

### Report findings
# robustness checks I
stargazer(model1, model1.q2, model1.wls, model1.hc1, model1.hc3, model1.no1, model1.no2, 
          type = "text", no.space = TRUE, covariate.labels = NULL, label = "",
          omit.stat = c("f", "ser"), model.names = FALSE, 
          dep.var.labels.include = FALSE, dep.var.caption = "",
          column.labels = c("OLS", "Quadratic", "WLS", "Robust.hc1", "Robust.hc3", 
                            "Singapore", "outliers"))

# robustness checks II
stargazer(model1.cl, model1.reg, model1.int, model1.no3, 
          type = "text", no.space = TRUE, covariate.labels = NULL, label = "", 
          omit.stat = c("f", "ser"), model.names = FALSE,
          dep.var.labels.include = FALSE, dep.var.caption = "",
          column.labels = c("cluster.se", "regions", "interaction", "logopen"))


### Miscellaneous

par(mfrow = c(3, 2))
plot(model1, which = 1:6)


library(GGally)
library(ggplot2)

# pairwise correlation, distribution, and scatter plots
ggscatmat(final.85v2, columns = c("logy", "open", "loglab", "logland"))

# independent variables vs. diagnostic statistics: residual, sigma, hat, cooksd
ggnostic(model1)


# Test spatial correlation using Moran’s I

morany <- final.85v2[, c("country", "wbcode", "logy", "lat",  "long")]
head(morany, n = 1)

morany.dist <- as.matrix(dist(cbind(morany$long, morany$lat)))

# find an inverse distance matrix with each off-diagonal entry equal 
# to 1/(distance between two points)
morany.dist.inv <- 1 / morany.dist

# replace diagonal entries with zero
diag(morany.dist.inv) <- 0

library(ape)

# Moran's I, null hypothesis: no spatial correlation  formula z=(I-e(I))/sqr(var(I))
Moran.I(morany$logy, morany.dist.inv)

## Panel data models
library(plm)

# ols with country fixed effects
fixed1 <- plm(log(rgdpl) ~ openk + log(POP), 
              data = pwt7, index  = "country", model = "within") # data from ch4

# ols with country and year fixed effects
fixed2 <- plm(log(rgdpl) ~ openk + log(POP),
              data = pwt7, index = c("country", "year"), model = "within",
              effect = "twoways")

# random effects model
random <- plm(log(rgdpl) ~ openk + log(POP), data = pwt7, 
              index = c("country", "year"), model = "random")
phtest(fixed1, random)
























