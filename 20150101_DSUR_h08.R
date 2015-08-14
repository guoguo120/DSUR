
#---------------------------------------------------------------------------------------
#-- H8 Logistic regression -------------------------------------------------------------
#---------------------------------------------------------------------------------------

# - When assumption of linear relationship is violated -> logarithmic transformation
# - Equation is prob of Y occuring

# - Assessing model: log-likelihood (LL), deviance (-2LL), r2, Akaike info criterion (AIC/BIC)
# - Assessing predictors: z-score, odds ratio
# - Methods: forced entry, forward or backward stepwise entry
# - Assumptions: lineairity, independence of errors, multicollinearity
# - Caution: incomplete information (cells with low n), complete seperation


#---------------------------------------------------------------------------------------

install.packages("car")
install.packages("mlogit")

library(car)
library(mlogit)


#--------------------------------------------------------------------------------------- 
# Eel example
#--------------------------------------------------------------------------------------- 

setwd('G:\\Users\\BEN\\dsur')

eelData <- read.delim("eel.dat", header = T)
head(eelData)

eelData$Cured <- relevel(eelData$Cured, "Not Cured") #set baseline
eelData$Intervention <- relevel(eelData$Intervention, "No Treatment")


#---------------------------------------------------------------------------------------

eelModel.0 <- glm(Cured ~ 1, data = eelData, family = binomial()) #create model
eelModel.1 <- glm(Cured ~ Intervention, data = eelData, family = binomial())
eelModel.2 <- glm(Cured ~ Intervention + Duration, data = eelData, family = binomial())

summary(eelModel.0) #get the null deviance
summary(eelModel.1)
summary(eelModel.2)


modelChi <- eelModel.1$null.deviance - eelModel.1$deviance #get delta deviance
chidf <- eelModel.1$df.null - eelModel.1$df.residual #get delta df
chisq.prob <- 1 - pchisq(modelChi, chidf) #get prob associated with chi-square statistic
modelChi; chidf; chisq.prob #check p-value of model improvement

anova(eelModel.1, eelModel.2) #alternative, compare model 1 and model 2


R2.hl <- modelChi/eelModel.1$null.deviance #get Hosmer & Lemeshow's r2
R2.cs <- 1 - exp((eelModel.1$deviance - eelModel.1$null.deviance)/(nrow(eelData))) #get Cox & Snell's r2
R2.n <- R.cs / ( 1 - ( exp (-(eelModel.1$null.deviance/(nrow(eelData)))))) #get Nagelkerke's r2
R2.hl; R2.cs; R2.n


#---------------------------------------------------------------------------------------

logisticPseudoR2s <- function(LogModel) {
    dev <- LogModel$deviance 
    nullDev <- LogModel$null.deviance 
    modelN <- length(LogModel$fitted.values)
    R.hl <- 1 - dev / nullDev
    R.cs <- 1 - exp ( -(nullDev - dev) / modelN)
    R.nk <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
    cat("Pseudo R^2 for logistic regression\n")
    cat("Hosmer and Lemeshow R^2  ", round(R.hl, 3), "\n")
    cat("Cox and Snell R^2        ", round(R.cs, 3), "\n")
    cat("Nagelkerke R^2           ", round(R.nk, 3), "\n")
    }

logisticPseudoR2s(eelModel.0)
logisticPseudoR2s(eelModel.1)
logisticPseudoR2s(eelModel.2)


#---------------------------------------------------------------------------------------

#Compute odds ratio of predictors, conf int: both lower and upper limit above or below 1
exp(eelModel.2$coefficients)
exp(confint(eelModel.2))


#---------------------------------------------------------------------------------------

#casewise diagnostics
eelData$predicted.probabilities <- fitted(eelModel.1)
eelData$standardized.residuals <- rstandard(eelModel.1) #5% >1.96, >3 is cause for concern
eelData$studentized.residuals <- rstudent(eelModel.1) #5% >1.96, >3 is cause for concern
eelData$dfbeta <- dfbeta(eelModel.1) #<1
eelData$dffit <- dffits(eelModel.1) #<1
eelData$leverage <- hatvalues(eelModel.1) #<1, expected value: number of predictors + 1 / n




#---------------------------------------------------------------------------------------
# Penalty Example
#---------------------------------------------------------------------------------------

setwd('G:\\Users\\BEN\\dsur')

penaltyData <- read.delim("penalty.dat", header = T)
head(penaltyData)
summary(penaltyData)


#---------------------------------------------------------------------------------------

penaltyModel.1 <- glm(Scored ~ Previous + PSWQ, data = penaltyData, family = binomial())
penaltyModel.2 <- glm(Scored ~ Previous + PSWQ + Anxious, data = penaltyData, family = binomial())

summary(penaltyModel.1)
summary(penaltyModel.2)


modelChi <- penaltyModel.1$null.deviance - penaltyModel.1$deviance
chidf <- penaltyModel.1$df.null - penaltyModel.1$df.residual
chisq.prob <- 1 - pchisq(modelChi, chidf)
modelChi; chidf; chisq.prob

logisticPseudoR2s(penaltyModel.1) 


#---------------------------------------------------------------------------------------

exp(penaltyModel.1$coefficients)
exp(confint(penaltyModel.1)) 


#---------------------------------------------------------------------------------------

modelChi <- penaltyModel.1$deviance - penaltyModel.2$deviance
chidf <- penaltyModel.1$df.residual - penaltyModel.2$df.residual
chisq.prob <- 1 - pchisq(modelChi, chidf)
modelChi; chidf; chisq.prob

anova(penaltyModel.1, penaltyModel.2)

logisticPseudoR2s(penaltyModel.2)


#---------------------------------------------------------------------------------------

exp(penaltyModel.2$coefficients)
exp(confint(penaltyModel.2))


#---------------------------------------------------------------------------------------

#test multicollinearity, VIF >10 is problematic; 1/VIF is reciprocal of VIF
vif(penaltyModel.2)
1/vif(penaltyModel.2)

cor(penaltyData[, c("Previous", "PSWQ", "Anxious")]) #check correlation of predictors


#---------------------------------------------------------------------------------------
#

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

