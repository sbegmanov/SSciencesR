# install following packages only once, then comment out code

# install.packages(c('reshape2', 'DataCombine', 'Hmisc', 'haven', 'foreign', 
#                    'gdata', 'XLConnect', 'pwt', 'reshape', 'doBy'),
#                  dependencies=TRUE)

# remove all objects from workspace
rm(list = ls(all = TRUE))

# Do countries that are more open to international trade grow faster economically 
# and have higher income?
library(tidyverse)

# import comma-delimited file, create data object pwt7
pwt7 <- read.csv("data/pwt70_w_country_names.csv", header = TRUE, strip.white = TRUE,
                stringsAsFactors = FALSE, na.strings = c("NA", ""))

# list first one observation in dataset pwt7
head(pwt7, n = 1)

# list last one observation in dataset pwt7
tail(pwt7, n = 1)

# dimensions of pwt7: number of observations and number of variables
dim(pwt7)

# variable names in dataset pwt7
names(pwt7)

# structure of dataset pwt7
str(pwt7)


# create a figure with three plots set graphic parameters for figure of two rows, 
# two columns
par(mfrow=c(2, 2))

# graph distribution of variable rgdpl in data frame pwt7
hist(pwt7$rgdpl)
boxplot(pwt7$rgdpl)
qqnorm(pwt7$rgdpl)

# reference fifth observation of pwt7$POP
pwt7$POP[5]

# reference specific observation of pwt7$POP
pwt7$POP[pwt7$country == "Afghanistan" & pwt7$year == 1954]

# reference fifth observation of fourth variable in pwt7
pwt7[5, 4]

# sort data first by country and then by year
pwt7 <- pwt7[order(pwt7$country, pw7$year), ]

# sort data by country (ascending) and by year (descending)
pwt7 <- pwt7[order(pwt7$country, -pwt7$year), ]

# show select rows; example: observations 100 and 102
pwt7[c(100, 102), ]

# show select rows; example: observations from 100 to 102
pwt7[c(100:102), ]

# show select variables
pwt7[, c("country", "year", "rgdpl")]

# combine two selections: certain rows and certain columns
pwt7[c(100:102), c("country", "year", "rgdpl")]

# show certain observations and certain variables that meet conditions: 
# Afghanistan since 2006 for three variables

pwt7[pwt7$year >= 2006 & pwt7$country == "Afghanistan", 
     c("country", "year", "rgdpl")]

# select multiple countries and non-consecutive years for select variables
pwt7[pwt7$year %in% c(1970, 1980, 1990, 2000, 2009) &
       pwt7$country %in% c("India", "China Version 1"),
     c("country", "year", "rgdpl")]

# create a new dataset with three select variables
pwt7new <- pwt7[, c("country", "year", "rgdpl")]

# create two temporary pwt7 subsets for merging example
pwt7.tmp1 <- pwt7[, c("isocode", "year", "rgdpl")]
pwt7.tmp2 <- pwt7[, c("isocode", "year", "openk")]

# merge two datasets
pwt7.m <- merge(pwt7.tmp1, pwt7.tmp2, by = c("isocode", "year"),
                all = TRUE, sort = TRUE)


library(reshape2)

# create a subset of pwt7 (India and Pakistan, three variables and six years)
pwt7.ip <- pwt7[pwt7$year %in% c(1950, 1960, 1970, 1980, 1990, 2000) & 
                  pwt7$country %in% c("India", "Pakistan"),
                c("country", "year", "rgdpl")]

# reshape pwt7.ip from long to wide form
pwt7.ip2 <- dcast(pwt7.ip, year ~ country, value.var = "rgdpl")

# reshape a dataset from wide to long
melt(pwt7.ip2, id.vars = "year", variable.name = "country", value.name = "rgdpl")


# remove observations with isocode equal CH2
pwt7.nc <- pwt7[pwt7$isocode != "CH2", ]

# display the number of years under CH2
pwt7$year[pwt7$isocode == "CH2"]
pwt7.nc$year[pwt7.nc$isocode == "CH2"]

# create dataset with duplicate India observations
pwt7.dup <- rbind(pwt7.ip, pwt7.ip[pwt7.ip$country == "India", ])

# remove duplicate observations
pwt7.dup[!duplicated(pwt7.dup[, c("country", "year")]), ]

# create real per capita investment in 2005 international $
pwt7$investpc <- pwt7$rgdpl * pwt7$ki / 100

# create total real investment in 2005 international $
pwt7$invest <- pwt7$rgdpl * pwt7$POP * pwt7$ki * 10

# create a character variable income.group by WB parameter
# ki - investment Share of PPP ConvertedGDP Per Capita
# rgdpl - PPP Converted GDP per capita (Laspeyres)
# POP - population (in thousands)
# real investment per capita = rgdpl*ki/100, 
# total real investment = rgdpl*POP*1000*ki/100=rgdpl*POP*ki*10

pwt7$income.group <- NA
pwt7$income.group[pwt7$rgdpl < 1000] <- "low-income"
pwt7$income.group[pwt7$rgdpl > 1000 & pwt7$rgdpl < 4000] <- "low-middle"
pwt7$income.group[pwt7$rgdpl > 4000 & pwt7$rgdpl < 12000] <- "up-middle"
pwt7$income.group[pwt7$rgdpl > 12000] <- "high-income"

# show variable type using class() function
class(pwt7$income.group)

# convert character variable into factor type
pwt7$income.group <- factor(pwt7$income.group, 
                            levels = c("low-income", "low-middle", "up-middle"),
                            ordered = TRUE)

# show frequency count in each category
table(pwt7$income.group)

# create a numeric variable income.group2
pwt7$income.group2 <- NA
pwt7$income.group2[pwt7$rgdpl < 1000] <- 1
pwt7$income.group2[pwt7$rgdpl > 1000 & pwt7$rgdpl < 4000] <- 2
pwt7$income.group2[pwt7$rgdpl > 4000 & pwt7$rgdpl < 12000] <- 3
pwt7$income.group2[pwt7$rgdpl > 12000] <- 4

# show variable type using class() function
class(pwt7$income.group2)

# convert numeric variable into factor variable
pwt7$income.group2 <- factor(pwt7$income.group2,
                             labels = c("low-income", "low-middle", 
                                        "up-middle", "high-income"))

# create a character variable for decade
pwt7$decade <- NA
pwt7$decade[pwt7$year >= 1950 & pwt7$year <= 1959] <- "1950s"
pwt7$decade[pwt7$year >= 1960 & pwt7$year <= 1969] <- "1960s"
pwt7$decade[pwt7$year >= 1970 & pwt7$year <= 1979] <- "1970s"
pwt7$decade[pwt7$year >= 1980 & pwt7$year <= 1989] <- "1980s"
pwt7$decade[pwt7$year >= 1990 & pwt7$year <= 1999] <- "1990s"
pwt7$decade[pwt7$year >= 2000] <- "2000s"

# convert character variable into factor variable
pwt7$decade <- factor(pwt7$decade, levels = c("1950s", "1960s", "1970s", "1980s", 
                                              "1990s", "2000s"),
                      ordered = TRUE)
# show frequency count in each decade
table(pwt7$decade)


### create leading and lagging variables in a panel data
## load DataCombine package in order to use slide function
library(DataCombine)

# sort data first by country and then by year
pwt7 <- pwt7[order(pwt7$country, pwt7$year), ]

# create a one-year leading variable for rgdpl
pwt7 <- slide(pwt7, Var = "rgdpl", NewVar = "rgdplead",
              GroupVar = "country", slideBy = 1)

# create a one-year lagged variable for rgdpl
pwt7 <- slide(pwt7, Var="rgdpl", NewVar="rgdplag",
              GroupVar="country", slideBy=-1)

# create annual growth rate for rgdpl
pwt7$growth <- (pwt7$rgdpl - pwt7$rgdplag)/pwt7$rgdplag

# create world average annual growth using by() function
pwt7$growth.w <- by(pwt7$growth, pwt7$year, FUN = mean, na.rm = TRUE)

# compute by-group economic growth rates
aggregate(growth ~ decade + income.group, data = pwt7, FUN = mean)

# save by-group economic growth rates to a dataset
pwt7.ag <- aggregate(growth ~ decade + income.group, data=pwt7, FUN=mean)

# rename the growth variable in pwt7.ag
names(pwt7.ag)[names(pwt7.ag) == "growth"] <- "growth.di"

# merge pwt7.ag into pwt7
pwt7 <- merge(pwt7, pwt7.ag, by = c("decade", "income.group"),
              all = TRUE, sort = TRUE)

### generate multiple group-statistics for multiple variables
library(doBy)

pwt7.cs <- summaryBy(growth + openk + POP ~ isocode + decade, FUN = c(mean, sd),
                     data = pwt7, na.rm = TRUE)

# display last observation
tail(pwt7.cs, n = 1)

























