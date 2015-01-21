

#Index

#################################################
#http://www.statmethods.net






data <- mtcars
model1 <- lm(formula(hp ~ .), data = data)
y2 <- predict(model1, data); data <- cbind(data, y2)
rm(model1, y2)
plot(data$hp, data$y2)

#fit lines
abline(lm(data$hp~data$y2), col="red")
lines(lowess(data$hp,data$y2), col="blue")




# Enhanced Scatterplot of MPG vs. Weight, by Number of Car Cylinders 
library(car)
scatterplot(mpg ~ wt | cyl, data = mtcars)

pairs(~mpg+disp+drat+wt,data=mtcars)
lines(lowess(.))

dta2 <- mtcars[c(1,3,5,6,8,9)]


# Creating a Graph
attach(mtcars)
plot(wt, mpg) 
abline(lm(mpg~wt))
title("Regression of MPG on Weight")



t.test(mpg~vs)


#http://www.statmethods.net



# Simple Dotplot
dotchart(mtcars$mpg,labels=row.names(mtcars),cex=.7, main="Gas Milage for Car Models", xlab="Miles Per Gallon")

# Dotplot: Grouped Sorted and Colored; sort by mpg, group and color by cylinder 
x <- mtcars[order(mtcars$mpg),] # sort by mpg
x$cyl <- factor(x$cyl) # it must be a factor
x$color[x$cyl==4] <- "red"
x$color[x$cyl==6] <- "blue"
x$color[x$cyl==8] <- "darkgreen"  
dotchart(x$mpg,labels=row.names(x),cex=.7,groups= x$cyl,
         main="Gas Milage for Car Models\ngrouped by cylinder",
         xlab="Miles Per Gallon", gcolor="black", color=x$color)



