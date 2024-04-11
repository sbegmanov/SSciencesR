# Analyzing categorical data and finding more data
# Description at data/files/F00001101-WV6_Official_Questionnaire_v4_June2012.pdf
# Do men and women differ in self-reported happiness?
rm(list = ls(all = TRUE))
load("data/WV6_Data_R_v_2016_01_01.rdata")

# Null Hypothesis: Gender and happiness are statistically independent of
# each other.
# Alternative Hypothesis: Gender and happiness are not statistically
# independent of each other.

# self-reported happiness variable
table(WV6_Data_R$V10)

# recode missing values
WV6_Data_R$V10[WV6_Data_R$V10 == -5 | WV6_Data_R$V10 == -2 | WV6_Data_R$V10 == -1] <- NA
table(WV6_Data_R$V10)

# tabulate self-reported happiness variable
100 * (prop.table(table(WV6_Data_R$V10)))

# a dichotomous and dummy variable happy
WV6_Data_R$happy <- ifelse(WV6_Data_R$V10 <= 2, 1, 0)
table(WV6_Data_R$happy, WV6_Data_R$V10)

# tabulate frequency count of dichotomous gender variable
table(WV6_Data_R$V240)


# create a copy of V240
WV6_Data_R$male <- WV6_Data_R$V240

# recode missing values
WV6_Data_R$male[WV6_Data_R$male == -2 | WV6_Data_R$male == -5] <- NA

# recode value for women
WV6_Data_R$male[WV6_Data_R$male == 2] <- 0
table(WV6_Data_R$male)

# Cross-tabulate two discrete variables
# Two-way contingency table
table(WV6_Data_R$male, WV6_Data_R$happy)

# cross tabulate sample proportions
100 * prop.table(table(WV6_Data_R$male, WV6_Data_R$happy))

library(gmodels)
CrossTable(WV6_Data_R$male, WV6_Data_R$happy)


### Statistical inference
# cross tabulate frequency count
freq.output2 <- table(WV6_Data_R$male, WV6_Data_R$happy)
chisq.test(freq.output2)

# Do believers in God and non-believers differ in self-reported happiness?
# tabulate belief in God V148
table(WV6_Data_R$V148)

# create a belief variable
WV6_Data_R$belief <- WV6_Data_R$V148

# recode missing values
WV6_Data_R$belief[WV6_Data_R$belief %in% c(-5, -4, -2, -1)] <- NA

# recode non-believers as equal zero
WV6_Data_R$belief[WV6_Data_R$belief == 2] <- 0

# double check frequency count
table(WV6_Data_R$belief)

# cross tabulation
table(WV6_Data_R$belief, WV6_Data_R$happy)
100 * prop.table(table(WV6_Data_R$belief, WV6_Data_R$happy))

# show cross tabulations of belief and happiness
CrossTable(table(WV6_Data_R$belief, WV6_Data_R$happy))

# test statistical independence
freq.output2 <- table(WV6_Data_R$belief, WV6_Data_R$happy)
chisq.test(freq.output2)


# Sources of self-reported happiness: Logistic Regression
# tabulate income variable
table(WV6_Data_R$V239)

# create an income variable
WV6_Data_R$income <- WV6_Data_R$V239

# recode missing values
WV6_Data_R$income[WV6_Data_R$income %in% c(-5, -2, -1)] <- NA
table(WV6_Data_R$income)

# estimate model and assign output to a data object
model1.logit <- glm(happy ~ male + belief + income, weights = V258,
                    family = binomial("logit"), data = WV6_Data_R)
summary(model1.logit)

# estimate model and assign output to a data object
model2.logit <- glm(happy ~ male + belief + income + factor(V2),
                    weights = V258, family = binomial("logit"), 
                    data = WV6_Data_R )
summary(model2.logit)

# report regression results
library(stargazer)
stargazer(model1.logit, model2.logit, type = "text", no.space = TRUE,
          omit = "factor", ci = TRUE)

# Interpret size of effect
# odds ratio
exp(coef(model2.logit))

# odds ratio
(exp(coef(model2.logit)) - 1) * 100






























