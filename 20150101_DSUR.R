

# - H1/2: statistics

Samenvatting hier..



# - H3: get to know R
# - H4: graphs
# - H5: Exploring assumptions
# - H6: Correlation
# - H7: Regression
# - H8: Log regression
# - H9: Comparing two means

# - H10: ANOVA /GLM1
# - H11: ANCOVA /GLM2
# - H12: Factorial ANOVA /GLM3
# - H13: Repeated measures designs /GLM4
# - H14: Mixed designs /GLM5

# - H15: Non-parametric tests
# - H16: MANCOVA
# - H17: Exploratory factor analysis
# - H18: Categorical data
# - H19: Multilevel linear models

# - Extra: H2O
# - Extar: decision trees?




#################################################
#H1/2: statistics

#################################################
#H3: get to know R

#################################################
#H4: graphs

#get data
data = mtcars
summary(data)
ls(data) #alfabetische volgorde
names(data)

plot(data$carb, data$mpg, type = "p", col = "red", main = "Titel", sub = "ondertitel", 
      xlab = "xlabel", ylab = "ylabel", asp = NA)
hist(data$mpg, labels=T, breaks = 10, freq = T, col = "lightblue", border = "black" , density = 25)
hist(x, labels=T, breaks = 100, freq = T, col = "lightblue", border = "black" , density = 25)
barplot(data$mpg, type="h", labels=T, , freq = T, col = "lightblue", border = "black", density = 25)

ds <- ddply(df, .(gp), summarise, mean = mean(y), sd = sd(y))

ggplot(data, aes(x = disp, y = qsec)) +
  geom_point() +
  scale_xlog10()


#qplot
qplot(displ, cty, data = mpg, color = class) + geom_jitter()


qplot(mpg, wt, data=mtcars)
qplot(mpg, wt, data=mtcars)
qplot(factor(cyl), mpg, data=mtcars, colour=cyl)
qplot(mpg, wt, data=mtcars, size=cyl)
qplot(mpg, wt, data=mtcars, facets=vs ~ am)

qplot(factor(cyl), wt, data = mtcars, geom=c("boxplot"))
qplot(mpg, data = mtcars, geom = "dotplot")

x <- c(1:5); y <- x
xy <- data.frame(x, y)
xy







