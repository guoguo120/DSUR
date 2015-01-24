
library("data.table")
library("dplyr")
library("ggplot2")

setwd("/Users/vanvlietben/Dropbox/") #macbook
getwd()

####
rm(list = ls())

health <- read.csv(file="20150201_BU/20150201_health.csv", head=TRUE, sep=";")

health <- select(health, month = Month, weight = Weight)
health <- data.frame(health)

date <- as.POSIXct(strptime(health$month, "%m/%d/%y"), tz = "CET")
year <- as.numeric(year(date))
week <- as.numeric(week(date))

birth <- rep(1983, (nrow(health)))
age <- year - birth

perc <- as.integer((health$weight / 85)*100)
bmi <- as.integer(health$weight / (1.85 * 1.85))

min <- as.integer(rep(62, (nrow(health))))
max <- as.integer(rep(62, (nrow(health))))
max2 <- ifelse(health$weight > 85, (health$weight - 85), 0)

x <- data.frame(date, year, age, week, bmi, perc, min, max, max2)
health <- data.frame(c(health, x))

health <- health[c("date", "year", "week", "weight", "bmi", "perc", "min", "max", "max2", "age")] #reorder vars
rm(x, age, birth, bmi, date, max, max2, min, perc, week, year)

qplot(data = filter(health, date > "2004-05-01"), date, weight, colour=year, geom="path")
qplot(data = health, date, weight, colour = year, geom="path")
qplot(data = health, y = weight, x = year, facets = age ~ ., geom="boxplot")
qplot(data = filter(health, date > "2004-05-01"), date, weight, colour=year, geom = c("point", "smooth"))
qplot(data = filter(health, date > "2004-05-01"), date, weight, colour=bmi, geom = c("point", "smooth"))
