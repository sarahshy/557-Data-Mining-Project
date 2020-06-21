# Compare predictions of straight Random Forest and ME Random Forest


############################################
# Front Matter
############################################

library(data.table)
library(randomForest)
library(caret)
library(dplyr)

train.set1 <- fread("../Data/train_set1.csv")
train.set2 <- fread("../Data/train_set2.csv")
test.set <- fread("../Data/test_set.csv")

# combine training sets
train.set <- rbind(train.set1, train.set2)

# convert class to factor
train.set$class <- as.factor(train.set$class)
test.set$class <- as.factor(test.set$class)


############################################
# Fit RF on original data
############################################

rf.fit <- randomForest(class ~ u_g + g_r + r_i + i_z, data = train.set, ntree = 100, mtry = 2)
rf.preds <- predict(rf.fit, test.set, type = "prob")
rf.preds.fac <- predict(rf.fit, test.set)
confusionMatrix(rf.preds.fac, test.set$class)

# perturbed RF predictions
probs <- fread("../Data/counts_prob.csv")
preds <- ifelse(probs$prob_gal > 0.5, "GALAXY", "STAR")
confusionMatrix(preds %>% as.factor, test.set$class)

# overall classification doesn't improve

prob.diff <- rf.preds[,1] - probs$prob_gal
summary(prob.diff) # diff between original RF probs and perturbed probs
hist(prob.diff)

# which points switch classes?
switch.idx <- which(rf.preds.fac != preds) 
switch.idx %>% length # 1900 objects switched classes
switch.obj <- test.set[switch.idx,]
summary(switch.obj[,32:35])
summary(test.set[,32:35]) # no interesting differences

summary(switch.obj[,27:31])
summary(test.set[,27:31]) # no interesting differences

summary(switch.obj[,20:24])
summary(test.set[,20:24])

probs$prob_gal[switch.idx] %>% hist
rf.preds[switch.idx] %>% hist

# on the objects that disagreed, who got it more correct?
confusionMatrix(rf.preds.fac[switch.idx], test.set$class[switch.idx]) # RF prediction
confusionMatrix(preds[switch.idx] %>% as.factor, test.set$class[switch.idx]) # ME prediction

# adjust for bandwidth
ggplot() + geom_density(data = switch.obj, aes(x = psfMagErr_z), adjust = 2, alpha = 0.5, fill = "blue") +
  xlim(0, 2) +
  ylim(0, 4)

ggplot() + geom_density(data = test.set, aes(x = psfMagErr_z), alpha = 0.5, fill = "red") +
  xlim(0, 2) +
  ylim(0, 4)

