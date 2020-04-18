# better predictions without ensembling

library(data.table)
library(ranger)
library(caret)

train.set1 <- fread("Data/train_set1.csv")
train.set2 <- fread("Data/train_set2.csv")
test.set <- fread("Data/test_set.csv")

# combine training sets
train.set <- rbind(train.set1, train.set2)

train.set$class <- as.factor(train.set$class)
test.set$class <- as.factor(test.set$class)

# create color errors
train.set2 <- train.set %>% mutate(u_g_err = psfMagErr_g + psfMag_u,
                                     g_r_err = psfMagErr_g + psfMagErr_r,
                                     r_i_err = psfMagErr_r + psfMagErr_i,
                                     i_z_err = psfMagErr_i + psfMag_z)
# define a weight for each observation
train.set3 <- train.set2 %>%
  mutate(err = 1/(u_g_err + g_r_err + r_i_err + i_z_err))

train.set3$err <- (train.set3$err - mean(train.set3$err))/sd(train.set3$err)

train.set3$err <- (train.set3$err- min(train.set3$err))/max(train.set3$err)/max((train.set3$err- min(train.set3$err))/max(train.set3$err))

##### tuning
tuneRF(train.set[, c("u_g", "g_r", "r_i", "i_z")], train.set$class, ntreeTry = 50, trace = TRUE, plot = TRUE)

##### basic RF
train.set$class <- as.factor(train.set$class)
rf.fit <- randomForest(class ~ u_g + g_r + r_i + i_z, data = train.set, ntree = 100, mtry = 2)
pred <- predict(rf.fit, test.set)
confusionMatrix(pred, test.set$class)
varImpPlot(rf.fit)

##### case weights in ranger
rf.fit.wt <- ranger(class ~ u_g + g_r + r_i + i_z,
                    data = train.set3,
                    case.weights = train.set3$err,
                    num.trees = 100,
                    mtry = 2)
pred.wt <- predict(rf.fit.wt, test.set)
confusionMatrix(pred.wt$predictions, test.set$class)



