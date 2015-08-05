








data <- rename(data, c("Month"="maand", "Day"="dag")) #rename vars


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


