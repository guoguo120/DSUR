
#---------------------------------------------------------------------------------------
#-- H8 Logistic regression -------------------------------------------------------------
#---------------------------------------------------------------------------------------

# - When assumption of linear relationship is violated -> logarithmic transformation
# - Equation is prob of Y occuring

# - Assessing model: Log-likelihood (LL), deviance (-2LL), r2, Akaike info criterion (AIC/BIC)
# - Assessing predictors: z-score, odds ratio
# - Methods: forced entry, forward or backward stepwise entry
# - Assumptions: 1.) lineairity, 2.) independence of errors, 3.) multicollinearity
# - Caution: 1.) Incomplete information (cells with low n), 2.) complete seperation



#---------------------------------------------------------------------------------------
#-- Load libraries ---------------------------------------------------------------------
#---------------------------------------------------------------------------------------

install.packages("car")
install.packages("mlogit")
install.packages("Rcmdr")

library(car)
library(mlogit)
library(Rcmdr)

setwd('G:\\Users\\BEN\\dsur')


#---------------------------------------------------------------------------------------
#prep data

eelData <- read.delim("eel.dat", header = T)
head(eelData)

eelData$Cured <- relevel(eelData$Cured, "Not Cured") #set baseline
eelData$Intervention <- relevel(eelData$Intervention, "No Treatment")


#---------------------------------------------------------------------------------------
#create model

eelModel.0 <- glm(Cured ~ 1, data = eelData, family = binomial()) #get the null deviance
eelModel.1 <- glm(Cured ~ Intervention, data = eelData, family = binomial())
eelModel.2 <- glm(Cured ~ Intervention + Duration, data = eelData, family = binomial())

summary(eelModel.0) #get the null deviance
summary(eelModel.1)
summary(eelModel.2)

modelChi <- eelModel.1$null.deviance - eelModel.1$deviance #get delta deviance
chidf <- eelModel.1$df.null - eelModel.1$df.residual #get df
chisq.prob <- 1 - pchisq(modelChi, chidf) #calc probability assiciated with chi-square statistic
modelChi; chidf; chisq.prob #check p-value (<0.05) of model improvement

R2.hl <- modelChi/eelModel.1$null.deviance #Hosmer & Lemeshow's r2
R2.cs <- 1 - exp((eelModel.1$deviance - eelModel.1$null.deviance)/(nrow(eelData))) #Cox & Snell's r2
R2.n <- R.cs /( 1- ( exp (-(eelModel.1$null.deviance/(nrow(eelData)))))) #Nagelkerke's r2
R2.hl; R2.cs; R2.n

#function that computes pseudo r2's
logisticPseudoR2s <- function(LogModel) {
    dev <- LogModel$deviance 
    nullDev <- LogModel$null.deviance 
    modelN <- length(LogModel$fitted.values)
    R.hl <- 1 - dev / nullDev
    R.cs <- 1- exp ( -(nullDev - dev) / modelN)
    R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
    cat("Pseudo R^2 for logistic regression\n")
    cat("Hosmer and Lemeshow R^2  ", round(R.hl, 3), "\n")
    cat("Cox and Snell R^2        ", round(R.cs, 3), "\n")
    cat("Nagelkerke R^2           ", round(R.n, 3),  "\n")
    }

logisticPseudoR2s(eelModel.1)

#Compute odds ratio of predictors, increase of 1 in b = increase / decrease of odds  by ...
exp(eelModel.2$coefficients)
exp(confint(eelModel.2))

#compare model1 and model 2
modelChi <- eelModel.1$deviance - eelModel.2$deviance
chidf <- eelModel.1$df.residual - eelModel.2$df.residual
chisq.prob <- 1 - pchisq(modelChi, chidf)
modelChi; chidf; chisq.prob

anova(eelModel.1, eelModel.2) #alternative, compare model1 and model 2

#casewise diagnostics
#check if cases exert undue influence on model; check 7.7 for more info
eelData$predicted.probabilities <- fitted(eelModel.1)
eelData$standardized.residuals <- rstandard(eelModel.1) #only 5% should lie outside 1.96, above 3 is cause for concern
eelData$studentized.residuals <- rstudent(eelModel.1) #only 5% should lie outside 1.96, above 3 is cause for concern
eelData$dfbeta <- dfbeta(eelModel.1) #should be less than 1
eelData$dffit <- dffits(eelModel.1) #should be less than 1
eelData$leverage <- hatvalues(eelModel.1) #lies between 0-1, expected value: number of predictors + 1 / n (e.g. 2/113 = 0.018)
head(eelData)


#********************* Penalty Example ********************

penaltyData <- read.delim("penalty.dat", header = T)
head(penaltyData); summary(penaltyData)

penaltyModel.1 <- glm(Scored ~ Previous + PSWQ, data = penaltyData, family = binomial())
penaltyModel.2 <- glm(Scored ~ Previous + PSWQ + Anxious, data = penaltyData, family = binomial())

summary(penaltyModel.1)
summary(penaltyModel.2)

modelChi <- penaltyModel.1$null.deviance - penaltyModel.1$deviance #compute model 1 improvement
chidf <- penaltyModel.1$df.null - penaltyModel.1$df.residual
chisq.prob <- 1 - pchisq(modelChi, chidf)
modelChi; chidf; chisq.prob

logisticPseudoR2s(penaltyModel.1) 

exp(penaltyModel.1$coefficients) #compute odds ratio
exp(confint(penaltyModel.1)) #both lower and upper limit should be above or below 1

modelChi <- penaltyModel.1$deviance - penaltyModel.2$deviance #compare model1 and model 2
chidf <- penaltyModel.1$df.residual - penaltyModel.2$df.residual
chisq.prob <- 1 - pchisq(modelChi, chidf)
modelChi; chidf; chisq.prob

anova(penaltyModel.1, penaltyModel.2) #alternative, compare model 1 and 2

logisticPseudoR2s(penaltyModel.2)

exp(penaltyModel.2$coefficients) #compute odds ratio model 2
exp(confint(penaltyModel.2))

vif(penaltyModel.2) #testing multicollinearity, VIF over 10 is problematic; no statistical grounds for ommitting one var over another; factor analysis is possible solution
1/vif(penaltyModel.2) #reciprocal of VIF
cor(penaltyData[, c("Previous", "PSWQ", "Anxious")]) #check correlation of predictors

penaltyData$logPSWQInt <- log(penaltyData$PSWQ)*penaltyData$PSWQ #test linearity of logit with interaction var; create interaction vars
penaltyData$logAnxInt <- log(penaltyData$Anxious)*penaltyData$Anxious
penaltyData$logPrevInt <- log(penaltyData$Previous + 1)*penaltyData$Previous #zero has nog log, hence the addition of a constant (1)
head(penaltyData)
penaltyTest.1 <- glm(Scored ~ PSWQ + Anxious + Previous + logPSWQInt + logAnxInt + logPrevInt, data=penaltyData, family=binomial())
summary(penaltyTest.1) #check if interaction vars are significant, if so: main effect has violated assumption of linearity


#********************* Chat Up Lines Example **************

#multinominal log regression, dep var with three categories
chatData <- read.delim("Chat-Up Lines.dat", header = T)
chatData$Gender <- relevel(chatData$Gender, ref = 2) #set ref category
head(chatData); summary(chatData); table(chatData$Gender, chatData$Success)
is.factor(chatData$Success) #check if var is factor, if F, use as.factor()
is.factor(chatData$Gender)

mlChat <- mlogit.data(chatData, choice="Success", shape="wide") #rearrange data, each case three rows
head(mlChat); summary(mlChat)

chatModel <- mlogit(Success ~ 1 | Good_Mate + Funny + Gender + Sex + Gender:Sex + Funny:Gender , data = mlChat, reflevel=3) #including the interaction vars
summary(chatModel)

data.frame(odds1 = exp(chatModel$coefficients), odds2 = 1 / exp(chatModel$coefficients)) #check coefficients (p.353, gender)
exp(confint(chatModel))

chatData <- read.delim("Chat-Up Lines.dat", header = T); chatData$Gender <- relevel(chatData$Gender, ref = 2) #multicolinearity
chatModel <- glm(Success ~ Funny + Good_Mate + Sex + Gender, data = chatData, family = binomial())
vif(chatModel); 1/vif(chatModel)
cor(chatData[, c("Funny", "Good_Mate", "Sex")])

mlChat$logFunny <- log(mlChat$Funny +1) #testing the linearity of the logit
mlChat$logGood <- log(mlChat$Good_Mate +1)
mlChat$logSex <- log(mlChat$Sex +1)
head(mlChat)
chatTest.1 <- mlogit(Success ~ 1 | Good_Mate + Funny + Sex + Funny:logFunny + Good_Mate:logGood + Sex:logSex, data = mlChat, reflevel=3)
summary(chatTest.1)

