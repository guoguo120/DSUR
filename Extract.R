
#Index

#################################################
#Extract

# - default r data
# - .csv
# - .RData
# - .sav
# - SiteCatalyst
# - sql
# - sap
# - world bank
# - quandl
# - (create data)


#################################################

setwd('\\Users\\vanvlietben\\Dropbox\\20140101_FDMG\\R\\') #macbook
setwd('C:\\Users\\bvliet\\Desktop\\BU\\R\\') #fdmg desktop
setwd('G:\\Users\\BEN\\R\\') #fdmg fileserver
getwd()

install.packages("dplyr")
install.packages("data.table")
install.packages("car")
install.packages("")
install.packages("foreign")
install.packages("reshape")
install.packages("ggplot2")
#install.packages('ggplot2', dependencies = TRUE)

library("dplyr")
library("data.table")
library("car")
library("")
library("foreign") #read .sav
library("reshape") #rename vars
library("ggplot2")


#################################################
#default r data

#http://vincentarelbundock.github.io/Rdatasets/datasets.html

data <- mtcars
data <- airquality


#################################################
#.csv

data <- fread("name.csv", stringsAsFactors = F, ,header = T)

data <- data.table(data)
data <- data.frame(data)



#################################################
#.RData

load('data/data.RData')


#################################################
#.sav

library("foreign") #read .sav

data <- spss.get("name.sav", use.value.labels=T)
# use.value.labels argument converts value labels to R factors





#################################################
#SiteCatalyst

#https://github.com/randyzwitch/RSiteCatalyst




#################################################
#World bank, WDI

#http://cran.r-project.org/web/packages/WDI/README.html
#https://github.com/vincentarelbundock/WDI

install.packages("WDI")
library("WDI")

WDIsearch(string='gdp', field='name', cache=NULL)[1:100,] #search

data <- WDI(indicator = 'MS.MIL.XPND.GD.ZS', country=c('MX','CA','US', 'NL'), start=1960, end=2012)

ggplot(data, aes(year, BG.GSR.NFSV.GD.ZS, color=country)) + geom_line()

