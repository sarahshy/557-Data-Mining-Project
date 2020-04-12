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




