# load libraries
library(tidyr)
library(randomForest)
library(dplyr)
source("clean_sdss.R")

# load data
train.set <- read.csv("dr12_train_V5.csv")
test.set <- read.csv("dr12_test_V5.csv")

array3d.to.list <- function(array){ 
  lapply(seq(dim(array)[3]), function(num){ array[ , , num]})
}

# perturb the training sets
mag.vec <- c("u", "g", "r", "i", "z")

create.perturbed.dat <- function(dat, mags){
  perturb <- sapply(mag.vec, FUN = function(mag){
    name.adj <- paste(mag, "_adj", sep = "")
    name.err <- paste("psfMagErr_", mag, sep = "")
    vars <- dat %>% select(name.adj, name.err)
    pert <- apply(vars, MARGIN = 1, FUN = function(row){
      row[1] + rnorm(1, 0, row[2])
    })
    pert
  })
}

n.sets <- 200 # number of perturbed sets to make
pert.train <- n.sets %>%
  replicate(create.perturbed.dat(train.set, mag.vec)) %>%
  array3d.to.list()

pert.train <- lapply(pert.train, FUN = function(set){ 
  colnames(set) <- paste(mag.vec, "_adj", sep = "")
  return(set)
})

# observation 23258 has huge error


# create set for classification

# create colors
pert.col <- lapply(pert.train, FUN = function(set){
  df <- set %>% as.data.frame
  create.colors(df)
})

# attach class labels as factor
train.sets <- lapply(pert.col, FUN = function(set){
  set$class <- train.set$class %>% as.factor
  return(set)
})


# run random forest on each perturbed training set

# save predictions on test set
preds <- lapply(train.sets, FUN = function(set){
  rf.fit <- randomForest(class ~ u_g + g_r + r_i + i_z, data = set, ntree = 100)
  test.preds <- predict(rf.fit, test.set, type = "response")
  return(test.preds)
})

# convert to df
rf.preds <- preds %>% unlist %>% matrix(ncol = n.sets)

# save predictions
write.csv(rf.preds, "rf_predictions.csv", row.names = F)
