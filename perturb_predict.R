# Perturb train and test observations
# Fit RF to each perturbed train set
# Save predictions on test set to CSV as dataframe
# (Parallelized)

################################################
# Front Matter
################################################

library(data.table)
library(dplyr)
library(randomForest)
library(parallel)
library(tidyselect)

train.set1 <- fread("Data/train_set1.csv")
train.set2 <- fread("Data/train_set2.csv")
train.set <- rbind(train.set1, train.set2)

test.set <- fread("Data/test_set.csv")

print("finished loading")

################################################
# Useful functions
################################################

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
rf.pert.preds <- mclapply(1:n.sets, FUN = function(set.idx){
  # create perturbed sets
  pert.train <- create.perturbed.dat(train.mags, train.errors)
  pert.test <- create.perturbed.dat(test.mags, test.errors)
  
  # create colors
  pert.train <- create.colors(pert.train) # add color columns
  pert.test <- create.colors(pert.test)
  
  # attach class column
  pert.train$class <- train.set$class
  
  # run rf
  rf.fit <- randomForest(class ~ u_g + g_r + r_i + i_z, data = pert.train, ntree = 100)
  
  # rf predictions
  predict(rf.fit, pert.test, type = "response")
}, mc.cores = 20)
Sys.time() - start # print time elapsed

print("ran parallel prediction")

# convert list to df of predictions
rf.preds <- rf.pert.preds %>% unlist %>% matrix(ncol = n.sets) %>% as.data.frame

################################################
# Save predictions
################################################

# save predictions to csv
write.csv(rf.preds, "Data/preds_500.csv", row.names = F)

print("wrote preds to csv")

