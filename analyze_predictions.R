# Put together confidence intervals and class probabilities
################################################
# Front Matter
################################################

library(data.table)
library(caret)
library(randomForest)
library(pROC)
library(dplyr)
library(parallel)
library(plyr)
set.seed(10101)

train.set1 <- fread("Data/train_set1.csv")
train.set2 <- fread("Data/train_set2.csv")
test.set <- fread("Data/test_set.csv")

# combine training sets
train.set <- rbind(train.set1, train.set2)

# convert class to factor
train.set$class <- as.factor(train.set$class)
test.set$class <- as.factor(test.set$class)

preds <- fread("Data/preds_500.csv")

################################################
# Somewhat useful functions
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
# Reference RF
################################################

rf.fit <- randomForest(class ~ u_g + g_r + r_i + i_z, data = train.set, ntree = 100)
test.preds <- predict(rf.fit, test.set, type = "response")

rf.meas <- pull.meas(labels = test.set$class, predictions = test.preds)

save(rf.meas, file = "Data/reference_rf_measures.RData")

################################################
# Loops over all sets
################################################

start <- Sys.time()
df.meas <- mclapply(preds, FUN = pull.meas, labels = test.set$class, mc.cores = 5)
Sys.time() - start

df <- data.frame(Reduce(rbind, df.meas), row.names = NULL)

write.csv(df, "Data/measures500.csv", row.names = FALSE)

################################################
# "Probabilities"
################################################

# big mess

counts <- apply(preds, 1, FUN = function(x){
  table(factor(x, levels = c("GALAXY", "STAR")))
}) %>% t

counts <- counts %>%
  as.data.frame %>%
  mutate(prob_gal = GALAXY/ncol(preds))

write.csv(counts, "Data/counts_prob.csv", row.names = F)


##### After lunch: compare with RF probabilities



