
## just playing with stuff

################################################
# Stacking different classifiers
################################################
# just seeing how things work. this is all crap though
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
control <- trainControl(method="cv", number = 5, classProbs=FALSE, savePredictions = "final")
algorithmList <- c('svmRadial', 'rf')
set.seed(557)
models <- caretList(class ~ u_g + g_r + r_i + i_z,
                    data = train.set,
                    trControl = control,
                    methodList = algorithmList)
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
# Averaging over perturbed sets: toy example
################################################
# for linear, GP and bootstrap bagging doesn't improve prediction (linear is too stable? not enough bias?)
# interesting, GP and B improve polynomials of degree 5, 7, .. but not others. idk man
# revisit with a different example

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
#            kernel = "poly", cost = seq(0.1, 1.6, by = 0.5),
#            degree = 5,
#            gamma = seq(0.3, 2.4, by = 0.6), coef0 = c(0.2,0.6, 1.2, 1.5))
# tune.poly
# degree gamma coef0 cost
# 2     1   0.1   10

# reference svm

svm.fit <- svm(label ~ ., data = dat[train.idx,], kernel = "linear", cost = tune.linear$best.parameters$cost, probability = TRUE)
preds <- predict(svm.fit, dat[-train.idx,], probability = TRUE)
confusionMatrix(preds, dat[-train.idx, ]$label)

# polynomial
svm.fit <- svm(label ~ ., data = train.set, kernel = "poly",
               degree = 3,
               #gamma = tune.poly$best.parameters$gamma,
               #coef0 = tune.poly$best.parameters$coef0,
               #cost = tune.poly$best.parameters$cost,
               #probability = TRUE
               )
preds <- predict(svm.fit, dat[-train.idx,])
confusionMatrix(preds, dat[-train.idx, ]$label)

########### GP, avg probabilities

set.seed(557)
n.sets <- 30
probs.pert <- sapply(1:n.sets, FUN = function(set.num){
  # perturb data
  dat.noisy <- noisify_data(dat)
  
  # tune SVM
  # tune.linear <- 
  #   tune.svm(x = dat.noisy[train.idx, c("x1", "x2")], y = dat.noisy[train.idx,]$label,
  #            kernel = "linear", cost = seq(0.05, 0.5, by = 0.05))

  # fit SVM
  # svm.fit <- svm(label ~ ., data = dat.noisy[train.idx,], kernel = "linear", cost = tune.linear$best.parameters$cost, probability = TRUE)
  svm.fit <- svm(label ~ ., data = dat.noisy[train.idx,], kernel = "poly", degree = 3, probability = TRUE)
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
  # bootstrap training points
  train.idx.boot <- sample(train.idx, length(train.idx)*0.7, replace = TRUE)

  # fit SVM
  svm.fit <- svm(label ~ ., data = dat[train.idx.boot,], kernel = "poly", degree = 3, probability = TRUE) 
  probs <- predict(svm.fit, dat[-train.idx, ], probability = TRUE)
  return(attr(probs, "probabilities")[, "1"])
})

avg.probs <- rowMeans(probs.pert)
new.preds <- ifelse(avg.probs > 0.5, 1, 2)
confusionMatrix(new.preds %>% as.factor, dat[-train.idx, ]$label)


set.seed(557)
n.sets <- 30
probs.pert <- sapply(1:n.sets, FUN = function(set.num){
  # bootstrap training points
  train.idx.boot <- sample(train.idx, length(train.idx)*0.7, replace = TRUE)
  
  # tune SVM
  tune.linear <- 
    tune.svm(x = dat[train.idx.boot, c("x1", "x2")], y = dat[train.idx.boot,]$label,
             kernel = "linear", cost = seq(0.05, 0.5, by = 0.05))
  
  # fit SVM
  svm.fit <- svm(label ~ ., data = dat[train.idx.boot,], kernel = "linear", cost = tune.linear$best.parameters$cost, probability = TRUE)
  probs <- predict(svm.fit, dat[-train.idx, ], probability = TRUE)
  return(attr(probs, "probabilities")[, "1"])
})

avg.probs <- rowMeans(probs.pert)
new.preds <- ifelse(avg.probs > 0.5, 1, 2)
confusionMatrix(new.preds %>% as.factor, dat[-train.idx, ]$label)


################################################
# Stacking perturbed sets
################################################
library(parallel)
library(randomForest)

source("Astro/perturb_astro_fn.R") # different from the perturb function in perturb_predict.R
set.seed(248)
p.test <- create.perturbed.dat(dat = test.set)

n.sets <- 2
start <- Sys.time()
rf.pert.preds <- mclapply(1:n.sets, FUN = function(set.idx){
  # create perturbed training sets

  p.train1 <- create.perturbed.dat(dat = train.set1)
  p.train2 <- create.perturbed.dat(dat = train.set2)

  # run rf
  rf.fit <- randomForest(class ~ u_g + g_r + r_i + i_z, data = p.train1, ntree = 100, mtry = 1)
  
  # rf predictions
  predict(rf.fit, p.train2, type = "response")
}, mc.cores = 2)
Sys.time() - start # print time elapsed

# convert list to df of predictions
rf.preds <- rf.pert.preds %>% unlist %>% matrix(ncol = n.sets) %>% as.data.frame

######### change to save all rf models from the loop. Will need for test set
########### okay now fit model on rf.preds. Response is true class of train.set2
###### Then run test through all models
