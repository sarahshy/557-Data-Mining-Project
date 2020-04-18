# bagging random forest is stupid cuz it's already bagged
# would make more sense for a higher variance classifer
################################################
# Front Matter
################################################

library(data.table)
library(dplyr)
library(randomForest)
library(parallel)
library(tidyselect)
library(pROC)
library(plyr)
library(caret)

train.set1 <- fread("Data/train_set1.csv")
train.set2 <- fread("Data/train_set2.csv")
test.set <- fread("Data/test_set.csv")

# combine training sets
train.set <- rbind(train.set1, train.set2)

# convert class to factor
train.set$class <- as.factor(train.set$class)
test.set$class <- as.factor(test.set$class)

print("finished loading")


################################################
# Useful functions
################################################

# create colors from magnitude columns
create.colors <- function(dat){ # takes full clean set with adjusted mags
  dr12.recode <- dat %>%
    mutate(u_g = u_adj - g_adj,
           g_r = g_adj - r_adj,
           r_i = r_adj - i_adj,
           i_z = i_adj - z_adj)
  
  return(dr12.recode) # returns colors only
}

##### The lines below create one full perturbed set
# can alternatively (and more concisely) just sum measurement errors,
# but this way we can keep the perturbed magnitudes

# prep column names
mag.names <- c("u_adj", "g_adj", "r_adj", "i_adj", "z_adj")
err.names <- c("psfMagErr_u", "psfMagErr_g", "psfMagErr_r", "psfMagErr_i", "psfMagErr_z")

# pull only necessary columns from trains and test sets
train.mags <- train.set %>% select(all_of(mag.names))
train.errors <- train.set %>% select(all_of(err.names))
test.mags <- test.set %>% select(all_of(mag.names))
test.errors <- test.set %>% select(all_of(err.names))

# perturb
create.perturbed.dat <- function(mags.df, errors.df){
  mapply(FUN = function(mag, error, i, j){ mag + rnorm(1, 0, error) },
         mags.df,
         errors.df) %>%
    as.data.frame
}


################################################
# Parallelized Pipeline
################################################


# run parallel prediction
n.sets <- 500
start <- Sys.time()
rf.pert.probs <- mclapply(1:n.sets, FUN = function(set.idx){
  # create perturbed sets
  pert.train <- create.perturbed.dat(train.mags, train.errors)
  pert.test <- create.perturbed.dat(test.mags, test.errors)
  
  # create colors
  pert.train <- create.colors(pert.train) # add color columns
  pert.test <- create.colors(pert.test)
  
  # attach class column
  pert.train$class <- train.set$class
  
  # fit rf (tuned, mtry = 1)
  rf.fit <- randomForest(class ~ u_g + g_r + r_i + i_z, data = pert.train, ntree = 100, mtry = 1)
  
  # rf predictions
  preds <- predict(rf.fit, pert.test, type = "prob") %>% as.data.frame
  preds$GALAXY
  
}, mc.cores = 20)
Sys.time() - start # print time elapsed

print("ran parallel prediction")

# convert list to df of predictions
rf.probs <- rf.pert.probs %>% unlist %>% matrix(ncol = n.sets) %>% as.data.frame

################################################
# Calculate measures
################################################


pull.meas <- function(labels, predictions){
  predictions <- predictions %>% as.factor
  labels <- labels %>% as.factor
  
  auc <- roc(labels %>% as.numeric, predictions %>% as.numeric, quiet = T)$auc
  perf <- confusionMatrix(predictions, labels)
  measures <- c(AUC = auc, perf$overall[1], perf$byClass[c(1, 2, 7)])
  return(measures)
}

################################################
# Average probabilities
################################################

avg.probs <- rowMeans(rf.probs)
avg.preds <- ifelse(avg.probs > 0.5, "GALAXY", "STAR")
bag.meas <- pull.meas(labels = test.set$class, predictions = avg.preds)

################################################
# Reference RF
################################################

rf.fit <- randomForest(class ~ u_g + g_r + r_i + i_z, data = train.set, ntree = 100, mtry = 1)
test.preds <- predict(rf.fit, test.set, type = "response")

rf.meas <- pull.meas(labels = test.set$class, predictions = test.preds)

rbind(rf.meas, bag.meas)

