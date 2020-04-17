
## just playing with stuff

################################################
# Ensembling different classifiers
################################################

# source: https://machinelearningmastery.com/machine-learning-ensembles-with-r/

library(data.table)
library(caret)
library(caretEnsemble)

train.set1 <- fread("Data/train_set1.csv")
train.set2 <- fread("Data/train_set2.csv")
test.set <- fread("Data/test_set.csv")

# combine training sets
train.set <- rbind(train.set1, train.set2)

# Example of Stacking algorithms
# create submodels

train.set <- train.set[1:500,]
control <- trainControl(method="repeatedcv", number=10, repeats=3, classProbs=TRUE)
algorithmList <- c('lda', 'svmRadial', 'rf')
set.seed(557)
models <- caretList(class~u_g + g_r + r_i + i_z, data=train.set, trControl=control, methodList=algorithmList)
results <- resamples(models)
summary(results)
dotplot(results)



# correlation between results
modelCor(results)
splom(results)

# stacking using rf
stackControl <- trainControl(method="repeatedcv", number=10, repeats=3, classProbs=TRUE)
set.seed(557)
stack.rf <- caretStack(models, method="rf", metric="Accuracy", trControl=stackControl)
print(stack.rf)



################################################
# Front Matter
################################################
