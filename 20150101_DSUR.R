

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



#################################################
#H5: Exploring assumptions

#################################################
#H6: Correlation

#################################################
#H7: Regression

install.packages("boot")
install.packages("car")
install.packages("QuantPsyc")
install.packages("Rcmdr")

library("boot")
library("car")
library("QuantPsyc")
library("Rcmdr")

#get data
data = mtcars
summary(data)
names(data)

#na.action (not available i.e. missing data), zie p.257
lm(hp ~ ., data = data)
model1 <- lm(data = filter(data, cyl == 5|6), hp ~ mpg + disp, na.action = na.fail)
model2 <- lm(data = filter(data, cyl == 5|6), hp ~ mpg + disp + wt, na.action = na.fail)
plot(model1)
summary(model1)

#get standardized beta's, p.283
lm.beta(model1)

#get confidence intervals
confint(model1)

#compare models
anova(model1, model2)

#voorbeeld plot residuals
mod <- lm(mpg ~ wt, data=mtcars)
qplot(resid(mod), fitted(mod))




#################################################
#H8: Log regression

#################################################
#H9: Comparing two means

#################################################
#H10: ANOVA/GLM1

#################################################
#H11: ANCOVA/GLM2

#################################################
#H12: Factorial ANOVA/GLM3

#################################################
#H13: Repeated measures designs/GLM4

#################################################
#H14: Mixed designs/GLM5

#################################################
#H15: Non-parametric tests

#################################################
#H16: MANCOVA

#################################################
#H17: Exploratory factor analysis

#################################################
#H18: Categorical data

#################################################
#H19: Multilevel linear models





#################################################
#Extra: H2O


# The following two commands remove any previously installed H2O packages for R.
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download, install and initialize the H2O package for R.
install.packages("h2o", repos=(c("http://s3.amazonaws.com/h2o-release/h2o/master/1660/R", getOption("repos"))))
library(h2o)
localH2O = h2o.init()

# Finally, let's run a demo to see H2O at work.
demo(h2o.glm)

#in browser: "http://localhost:54321"





