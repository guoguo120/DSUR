
#------------------------------------------------------------------------------------------------------------------
#install h2o

install.packages("h2o")

library(h2o)


#------------------------------------------------------------------------------------------------------------------
#create train + validation set

rm(list=ls())

data <- iris

smp_size <- floor(0.75 * nrow(data))

set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[train_ind, ]
test <- data[-train_ind, ]

rm(data, smp_size, train_ind)


#------------------------------------------------------------------------------------------------------------------

write.csv(test, "test.csv", row.names = F)
write.csv(train, "train.csv", row.names = F)


#------------------------------------------------------------------------------------------------------------------
# initialize an h2o instance

localH2O = h2o.init()

h2o.clusterInfo(localH2O)

#upload
train.hex = h2o.uploadFile(localH2O, path = "train.csv", destination_frame = "train.hex")
test.hex = h2o.uploadFile(localH2O, path = "test.csv", destination_frame = "test.hex")

#summary
summary(train.hex)


#------------------------------------------------------------------------------------------------------------------
# model training 

xVar = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")

yVar = "Species"

iris.rf = h2o.randomForest(x= xVar, 
                           y = yVar, 
                           training_frame = train.hex, 
                           validation_frame = test.hex)

print(iris.rf)

rm(xVar, yVar)


#------------------------------------------------------------------------------------------------------------------
# model analysis

VarImportance = h2o.varimp(iris.rf)

View(VarImportance)


#------------------------------------------------------------------------------------------------------------------
# predicting
 
train_predict = h2o.predict(iris.rf, train.hex)

train_predict = as.data.frame(train_predict)

data <- cbind(train, train_predict)

table(data$Species, data$predict)
