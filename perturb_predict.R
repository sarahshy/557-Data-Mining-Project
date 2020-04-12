# Perturb train and test observations
# Fit RF to each perturbed train set
# Save predictions on test set to CSV as dataframe
# (Parallelized)

################################################
# Front Matter
################################################

library(data.table)

train.set1 <- fread("Data/train_set1.csv")
train.set2 <- fread("Data/train_set2.csv")
train.set <- rbind(train.set1, train.set2)

test.set <- fread("Data/test_set.csv")

print("finished loading")

################################################
# Useful functions
################################################

