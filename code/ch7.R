# Based on the following articles

# Braithwaite, A. “The Geographic Spread of Militarized Disputes.” Journal of Peace Research
# What influences the geographic spread of military conflict?

### first article is based on:
# H1: Territorial disputes are more spread out than non-territorial disputes.
# H2: Disputes in countries with valuable natural resources are more spread out
# than those in other countries.
# H3: Disputes in areas of passable terrain are more spread out than those in
# areas of impassable terrain.
# H4: Disputes between geographically large states are more spread out than
# those between small or between large and small states.
# H5: Disputes between states sharing a vital border are more spread out than
# those between states not sharing a vital border.

rm(list = ls(all = TRUE))
library(foreign)


mid <- read.dta("data/file48280_braith_final_data.dta")

library(stargazer)
# formatted table of descriptive statistics

stargazer(mid, type = "text", title = "Summary Statistics")

# logsize from the book = jointsize from the file
table2 <- lm(log_radius_area ~ territory + log(jointsize) + host_mt + host_for + water + cwpceyrs 
   + bord_vital + host_resource, data = mid[mid$final_hostile > 0,])


library(car)
library(lmtest)
library(sandwich)

# White robust standard errors
table2.r<-coeftest(table2, vcov = vcovHC(table2, type = "HC1"))


# Diagnostic tests and robustness checks
library(broom)
# additional diagnostics statistics to original data
mid.v2 <- augment_columns(table2, mid)

# diagnostic test for model specification
library(ggplot2)

# residuals against fitted values
ggplot(mid.v2, aes(x = .fitted, y = .resid)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_smooth(method = 'loess', se = TRUE)

# RESET test
# OLS estimates for model 2
# Test possible non-linearity in the residuals
table2.q <- lm(log_radius_area ~ territory + log(jointsize) + host_mt + host_for 
               + water + cwpceyrs + bord_vital + host_resource + I(.fitted^2), 
               data = mid.v2)

# F test of model difference
anova(table2, table2.q)

# diagnose multicollinearity (vif statistics)
library(car)

vif(table2)

# Constant error variance
# test heteroskedasticity
# Cook/Weisberg score test of constant error variance
ncvTest(table2)

# Breush/Pagan test of constant error variance
bptest(table2)

# Influential Observations
# diagnose influential observations create observation id
mid.v2$id <- as.numeric(row.names(mid.v2))

# identify obs with Cook's D above cutoff
ggplot(mid.v2, aes(id, .cooksd)) +
  geom_bar(stat = "identity", position = "identity") +
  xlab("Obs. Number") +
  ylab("Cook's distance") +
  geom_hline(yintercept = 0.014) +
  geom_text(aes(label = ifelse((.cooksd > 0.014), id, "")), vjust = -0.2, hjust = 0.5) # check colnames(mid.v2) instead id


# list observations whose Cook's D above threshold
mid.v2[mid.v2$.cooksd > 0.014, 
       c("dispnum", "ccode1", "ccode2", "year", "log_radius_area", ".std.resid",
         ".hat", ".cooksd")]

# re-estimate model without influential obs
table2.no <- lm(log_radius_area ~ territory + log(jointsize) + host_mt + 
                  host_for + water + cwpceyrs + bord_vital + host_resource, 
                data = mid.v2[mid.v2$.cooksd < 0.014, ])

# estimation results without outliers
summary(table2.no)

# Normality diagnostics
qqPlot(table2, distribution = "t", simulate = TRUE, grid = TRUE)
qqPlot(table2.no, distribution = "t", simulate = TRUE, grid = TRUE)



# normality test
shapiro.test(residuals(table2))
shapiro.test(residuals(table2.no))

# Report and discuss estimation results
stargazer(table2, table2.r, table2.no, type = "text", no.space = TRUE,
          omit.stat = c("f", "ser"), model.names = FALSE,
          dep.var.labels.include = FALSE, dep.var.caption = " ")


# Second article:
# Bénabou, R. etc. “Religion and Innovation.” American Economic Review
# Does religiosity influence individual attitudes toward innovation?
rm(list = ls(all = TRUE))
library(foreign)
library(stargazer)
library(car)
library(lmtest)
library(sandwich)

benabou <- read.dta("data/aerpp2015btv-dataset.dta", convert.factors = FALSE)

stargazer(benabou, type = "text", title = "Summary Statistics")

# obtain variable labels
var.labels <- attr(benabou, "var.labels")
bendata.key <- data.frame(var.name = names(benabou), var.labels)
stargazer(bendata.key, type = "text", summary = F, rownames = F)

# five models for A029
c1m1 <- lm(A029 ~ F034rp + X001m + X003 + X025 + X045m + X047 + factor(X049m) 
           + factor(F025) + factor(S003) + factor(year), 
           weights = S017, data = benabou)
# White robust standard errors
c1m1.r <- coeftest(c1m1, vcov = vcovHC(c1m1, type = "HC1"))

### five models for A029
c1m2 <- lm(A029 ~ A006m + X001m + X003 + X025 + X045m + X047 + factor(X049m) 
           + factor(F025) + factor(S003) + factor(year), 
           weights = S017, data = benabou)
# White robust standard errors
c1m2.r <- coeftest(c1m2, vcov = vcovHC(c1m2, type = "HC1"))


c1m3 <- lm(A029 ~ F050 + X001m + X003 + X025 + X045m + X047 + factor(X049m) 
           + factor(F025) + factor(S003) + factor(year), 
           weights = S017, data = benabou)
# White robust standard errors
c1m3.r <- coeftest(c1m3, vcov = vcovHC(c1m3, type = "HC1"))


c1m4 <- lm(A029 ~ F063 + X001m + X003 + X025 + X045m + X047 + factor(X049m) 
           + factor(F025) + factor(S003) + factor(year), 
           weights = S017, data = benabou)
# White robust standard errors
c1m4.r <- coeftest(c1m4, vcov = vcovHC(c1m4, type = "HC1"))


c1m5 <- lm(A029 ~ F028m + X001m + X003 + X025 + X045m + X047 + factor(X049m) 
           + factor(F025) + factor(S003) + factor(year), 
           weights = S017, data = benabou)
# White robust standard errors
c1m5.r <- coeftest(c1m5, vcov = vcovHC(c1m5, type = "HC1"))

### five models for A034
c2m1 <- lm(A034 ~ F034rp + X001m + X003 + X025 + X045m + X047 + factor(X049m) 
           + factor(F025) + factor(S003) + factor(year), 
           weights = S017, data = benabou)
# White robust standard errors
c2m1.r <- coeftest(c2m1, vcov = vcovHC(c2m1, type = "HC1"))


c2m2 <- lm(A034 ~ A006m + X001m + X003 + X025 + X045m + X047 + factor(X049m) 
           + factor(F025) + factor(S003) + factor(year), 
           weights = S017, data = benabou)
# White robust standard errors
c2m2.r <- coeftest(c2m2, vcov = vcovHC(c2m2, type = "HC1"))


c2m3 <- lm(A034 ~ F050 + X001m + X003 + X025 + X045m + X047 + factor(X049m) 
           + factor(F025) + factor(S003) + factor(year), 
           weights = S017, data = benabou)
# White robust standard errors
c2m3.r <- coeftest(c2m3, vcov = vcovHC(c2m3, type="HC1"))


c2m4 <- lm(A034 ~ F063 + X001m + X003 + X025 + X045m + X047
           + factor(X049m) + factor(F025) + factor(S003)
           + factor(year), weights = S017, data = benabou)
# White robust standard errors
c2m4.r <- coeftest(c2m4, vcov=vcovHC(c2m4, type="HC1"))


c2m5 <- lm(A034 ~ F028m + X001m + X003 + X025 + X045m + X047 + factor(X049m) 
           + factor(F025) + factor(S003) + factor(year), 
           weights = S017, data = benabou)
# White robust standard errors
c2m5.r <- coeftest(c2m5, vcov = vcovHC(c2m5, type = "HC1"))


### five models for A039
c3m1 <- lm(A039 ~ F034rp + X001m + X003 + X025 + X045m + X047 + factor(X049m) 
           + factor(F025) + factor(S003) + factor(year), 
           weights = S017, data = benabou)
# White robust standard errors
c3m1.r <- coeftest(c3m1, vcov = vcovHC(c3m1, type = "HC1"))


c3m2 <- lm(A039 ~ A006m + X001m + X003 + X025 + X045m + X047 + factor(X049m) 
           + factor(F025) + factor(S003) + factor(year), 
           weights = S017, data = benabou)
# White robust standard errors
c3m2.r <- coeftest(c3m2, vcov = vcovHC(c3m2, type = "HC1"))


c3m3 <- lm(A039 ~ F050 + X001m + X003 + X025 + X045m + X047 + factor(X049m) 
           + factor(F025) + factor(S003) + factor(year), 
           weights = S017, data = benabou)
# White robust standard errors
c3m3.r <- coeftest(c3m3, vcov = vcovHC(c3m3, type = "HC1"))


c3m4 <- lm(A039 ~ F063 + X001m + X003 + X025 + X045m + X047 + factor(X049m) 
           + factor(F025) + factor(S003) + factor(year), 
           weights = S017, data = benabou)
# White robust standard errors
c3m4.r <- coeftest(c3m4, vcov = vcovHC(c3m4, type = "HC1"))


c3m5 <- lm(A039 ~ F028m + X001m + X003 + X025 + X045m + X047 + factor(X049m) 
           + factor(F025) + factor(S003) + factor(year), 
           weights = S017, data = benabou)
# White robust standard errors
c3m5.r <- coeftest(c3m5, vcov = vcovHC(c3m5, type = "HC1"))

# Report replicated Results
library(stargazer)
stargazer(c1m1.r, c1m2.r, c1m3.r, c1m4.r, c1m5.r, type = "text",
          no.space = TRUE, keep = c("F034rp", "A006m", "F050", "F063", "F028m"),
          dep.var.labels = "Importance of child independence (A029)",
          covariate.labels = c("Religious person (F034rp)", 
                               "Importance of religion (A006m)", 
                               "Belief in God (F050)",
                               "Importance of God (F063)", 
                               "Church attendance (F028m)"),
          out = "benabou1")

stargazer(c2m1.r, c2m2.r, c2m3.r, c2m4.r, c2m5.r, type = "text",
          no.space = TRUE, keep = c("F034rp", "A006m", "F050", "F063", "F028m"),
          dep.var.labels = "Importance of child imagination (A034)",
          covariate.labels = c("Religious person (F034rp)",
                               "Importance of religion (A006m)", 
                               "Belief in God (F050)", 
                               "Importance of God (F063)", 
                               "Church attendance (F028m)"),
          out = "benabou2")

stargazer(c3m1.r, c3m2.r, c3m3.r, c3m4.r, c3m5.r, type = "text",
          no.space = TRUE, keep = c("F034rp", "A006m", "F050", "F063", "F028m"),
          dep.var.labels = "Importance of child determination (A039)",
          covariate.labels = c("Religious person (F034rp)",
                               "Importance of religion (A006m)", 
                               "Belief in God (F050)",
                               "Importance of God (F063)", 
                               "Church attendance (F028m)"),
          out = "benabou3")



























