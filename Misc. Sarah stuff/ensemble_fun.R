
## just playing with stuff

################################################
# Stacking different classifiers
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
# Averaging over perturbed sets
################################################
library(e1071)

#toy example

source("Toys/svm_measurement_error_toy.R")
set.seed(557)

toy.dat <- make_base_data()
dat <- noisify_data(toy.dat)

n <- 500
train.idx <- sample(1:n, round(.7*n))
train.set <- dat[train.idx,]

# tune SVM

set.seed(557)

tune.linear <- 
  tune.svm(x = train.set[, c("x1", "x2")], y = train.set$label,
           kernel = "linear", cost = seq(0.05, 0.5, by = 0.05))

tune.linear

# tune.poly <-
#   tune.svm(x = train.set[, c("x1", "x2")], y = train.set$label,
#            kernel = "poly", cost = seq(0.1, 1.6, by = 0.2),
#            degree = 2,
#            gamma = seq(0.2, 2.4, by = 0.3), coef0 = c(0.2, 0.4, 0.6, 0.9, 1.2, 1.5))
# tune.poly
# degree gamma coef0 cost
# 2     1   0.1   10

# reference svm

svm.fit <- svm(label ~ ., data = dat[train.idx,], kernel = "linear", cost = tune.linear$best.parameters$cost, probability = TRUE)
preds <- predict(svm.fit, dat[-train.idx,], probability = TRUE)
confusionMatrix(preds, dat[-train.idx, ]$label)

# polynomial
# svm.fit <- svm(label ~ ., data = train.set, kernel = "poly",
#                degree = 2,
#                gamma = tune.poly$best.parameters$gamma,
#                coef0 = tune.poly$best.parameters$coef0,
#                cost = tune.poly$best.parameters$cost,
#                probability = TRUE)
# preds <- predict(svm.fit, dat[-train.idx,], probability = TRUE)
# confusionMatrix(preds, dat[-train.idx, ]$label)

########### GP, avg probabilities

set.seed(557)
n.sets <- 30
probs.pert <- sapply(1:n.sets, FUN = function(set.num){
  # perturb data
  dat.noisy <- noisify_data(dat)
  
  # tune SVM
  tune.linear <- 
    tune.svm(x = dat.noisy[train.idx, c("x1", "x2")], y = dat.noisy[train.idx,]$label,
             kernel = "linear", cost = seq(0.05, 0.5, by = 0.05))

  # fit SVM
  svm.fit <- svm(label ~ ., data = dat.noisy[train.idx,], kernel = "linear", cost = tune.linear$best.parameters$cost, probability = TRUE)
  probs <- predict(svm.fit, dat.noisy[-train.idx, ], probability = TRUE)
  return(attr(probs, "probabilities")[, "1"])
})

avg.probs <- rowMeans(probs.pert)
new.preds <- ifelse(avg.probs > 0.5, 1, 2)
confusionMatrix(new.preds %>% as.factor, dat[-train.idx, ]$label)

########### bootstrap

set.seed(557)
n.sets <- 30
probs.pert <- sapply(1:n.sets, FUN = function(set.num){
  # perturb data
  dat.noisy <- noisify_data(dat)
  
  train.idx.boot <- sample(train.idx, length(train.idx)*0.7, replace = TRUE)
  
  # tune SVM
  tune.linear <- 
    tune.svm(x = dat.noisy[train.idx.boot, c("x1", "x2")], y = dat.noisy[train.idx.boot,]$label,
             kernel = "linear", cost = seq(0.05, 0.5, by = 0.05))
  
  # fit SVM
  svm.fit <- svm(label ~ ., data = dat.noisy[train.idx.boot,], kernel = "linear", cost = tune.linear$best.parameters$cost, probability = TRUE)
  probs <- predict(svm.fit, dat.noisy[-train.idx, ], probability = TRUE)
  return(attr(probs, "probabilities")[, "1"])
})

avg.probs <- rowMeans(probs.pert)
new.preds <- ifelse(avg.probs > 0.5, 1, 2)
confusionMatrix(new.preds %>% as.factor, dat[-train.idx, ]$label)


################################################
# Stacking perturbed sets
################################################



