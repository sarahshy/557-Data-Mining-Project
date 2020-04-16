library(data.table)
library(dplyr)

dat <- fread("/Users/sarahshy/Desktop/Spring 2020/Clustering/Data/DR12_sample_V6/dr12_sample_V6_clean.csv")
names(dat)[1] <- "specObjID"

# remove quasars
dat.clean <- dat %>% filter(class != "QSO")

# create train/test sets
set.seed(557)
n <- nrow(dat.clean)
train.idx <- sample(1:n, round(.7*n))
test.idx <- setdiff(1:n, train.idx)

train.set <- dat.clean %>% slice(train.idx)
test.set <- dat.clean %>% slice(test.idx)

# split train set for GitHub storage
n.train <- nrow(train.set)
train.idx1 <- sample(1:n.train, round(0.5*n.train))
train.idx2 <- setdiff(1:n.train, train.idx1)
train.set1 <- train.set %>% slice(train.idx1)
train.set2 <- train.set %>% slice(train.idx2)

# save as CSV
write.csv(train.set, "/Users/sarahshy/Desktop/Spring 2020/Data Mining/557-Data-Mining-Project/Data/train_set.csv", row.names = F)
write.csv(test.set, "/Users/sarahshy/Desktop/Spring 2020/Data Mining/557-Data-Mining-Project/Data/test_set.csv", row.names = F)

write.csv(train.set1, "/Users/sarahshy/Desktop/Spring 2020/Data Mining/557-Data-Mining-Project/Data/train_set1.csv", row.names = F)
write.csv(train.set2, "/Users/sarahshy/Desktop/Spring 2020/Data Mining/557-Data-Mining-Project/Data/train_set2.csv", row.names = F)
