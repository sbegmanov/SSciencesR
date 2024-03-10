#### Strata files
# Tab-delimited file

dataframe.name <- read.table("filename.txt", header=TRUE, sep="\t", 
                             na.strings=".", strip.white=TRUE,
                             stringsAsFactors=False)

library(foreign)
library(Hmisc)

dataframe.name <- stata.get("filename.dta")
dataframe.name <- read.dta("filename.dta")

dataframe.name <- stata.get("filename.dta", convert.underscore=TRUE,
                            convert.factors=FALSE)
dataframe.name <- read.dta("filename.dta", convert.underscore=TRUE,
                           convert.factors=FALSE)
library(haven)
dataframe.name <- read_dta("data/pwt1001.dta")

# STATA13 format
library(readstata13)

dataframe.name <- read.dta13("filename.dta")

# for files with informative variable labels

dataframe.name <- read.dta("filename.dta")
var.labels <- attr(dataframe.name, "var.labels")
codebook <- data.frame(var.name=names(dataframe.name), var.labels)


### SPSS files
library(foreign)
library(Hmisc)

dataframe.name <- spss.get("filename.sav")
dataframe.name <- read.xport("filename.xport")

library(haven)
dataframe.name <- read_sas("filename.sas7bdat")


### Excel files
library(RODBC)

channel <- odbcConnectExcel("filename.xls")
dataframe1 <- sqlFetch(channel, "worsheet1")
odbcClose(channel)

library(gdata)
dataframe <- read.xls("datafile.xls", sheet = 1)

library(XLConnet)
workbook <- loadWorkbook("datafile.xls")
dataframe <- readWorksheet(workbook, sheet = "Sheet1")


### R data format (.rdata or .rda)
save(data.object, file = "datafile.RData")
load("datafile.RData")




































