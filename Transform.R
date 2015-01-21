
#Index

#################################################
#Extract

# - explore data
# - missing
# - rename vars
# - select / filter
# - datums
# - rename, recode, autorecode,
# - 
# - custom tables (spss)
# - 
# - 


#################################################

setwd('\\Users\\vanvlietben\\Dropbox\\20140101_FDMG\\R\\') #macbook
setwd('C:\\Users\\bvliet\\Desktop\\BU\\R\\') #fdmg desktop
setwd('G:\\Users\\BEN\\R\\') #fdmg fileserver
getwd()

install.packages("dplyr")
install.packages("data.table")
install.packages("")
install.packages("")
install.packages("hmisc") #?
install.packages("foreign") #read .sav
install.packages("reshape") #rename vars
install.packages("shiny")
install.packages("ggplot2")

library("dplyr")
library("data.table")
library("")
library("")
library("hmisc") #?
library("foreign") #read .sav
library("reshape") #rename vars
library("shiny")
library("ggplot2")


#################################################
#




data <- airquality #load data
ls(data)
summary(data) #scan data

names <- names(airquality)
view(names)


#rename vars
data <- rename(data, c("Month"="maand", "Day"="dag"))


good <- complete.cases(data) #rows with missings
data <- cbind(data, good); rm(good)
data <- filter(data, good == TRUE)

data <- group_by(data, Month)
sum <- summarise(data, mean(Ozone), mean(Solar.R), mean(Wind), mean(Temp), mean(Temp))
sum
rm(sum)







abs(data$Wind)
data$Wind
sqrt(data$Wind)
ceiling(data$Wind)
trunc(data$Wind)




#subsetting

dataset[name %like% 'jantje']
dataset[name %in% c('jantje','peit')]



Setkey() #Voor join eerst de key var aanwijzen / verschillende joints doornemen
unique(data, mpg) #aantal unieke records op basis van vector


