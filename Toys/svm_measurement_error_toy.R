library(mvtnorm)
library(e1071)
library(tidyverse)

n <- 500
train <- sample(1:n, round(.8*n))
test <- setdiff(1:n, train)

mean_1 <- c(2, 5)
sigma_1 <- matrix(c(.8, -.4, -.4, 1.2), nrow = 2)
mean_2 <- c(1, 0)
sigma_2 <- matrix(c(1, .3, .3, .4), nrow = 2)

sigma_noise <- matrix(c(1, 0, 0, 3), nrow = 2)

make_base_data <- function() {
  c1 <- rmvnorm(ceiling(n/2), mean = mean_1, sigma = sigma_1)
  c2 <- rmvnorm(floor(n/2), mean = mean_2, sigma = sigma_2)
  
  data <- rbind(cbind(c1, 1), cbind(c2, 2))
  colnames(data) <- c("x", "y", "label")
  
  as_tibble(data) %>% mutate(label = as.factor(label))
}

noisify_data <- function(data, noise_rounds = 1) {
  if (noise_rounds < 1) return(data) # no noise needed!
  
  data_noisy <- data
  for(i in 1:noise_rounds) {
    noise <- rmvnorm(nrow(data), c(0, 0), sigma_noise)
    data_noisy[,c("x", "y")] <- data_noisy[,c("x", "y")] + noise
  }
  data_noisy
}

plot_data <- function(data) {
  ggplot(data) +
    geom_point(aes(x = x, y = y, color = label, shape = label))
}

# blah, blah, DRY...
plot_data_with_error <- function(data) {
  ggplot(data) +
    geom_errorbar(aes(x = x,
                    ymin = y - sigma_noise[2,2]/2,
                    ymax = y + sigma_noise[2,2]/2,
                    color = label)
                , alpha = 0.2) +
    geom_errorbarh(aes(y = y,
                       xmin = x - sigma_noise[1,1]/2,
                       xmax = x + sigma_noise[1,1]/2,
                       color = label),
                   alpha = 0.2) +
    geom_point(aes(x = x, y = y, color = label, shape = label))
}

svm_metrics <- function(data) {
  # uses global train, test
  svm_result <- svm(label ~ ., data = data[train,], kernel = "linear", cost = 1)
  predicted <- predict(svm_result, newdata = data[test,])
  
  counts <- table(predicted, data[test,]$label) %>% as.vector()
  total <- sum(counts)
  list(
    accuracy = (counts[1] + counts[4]) / total
  )
}

run_simulations <- function(base_data, m, noise_rounds) {
  metrics <- NULL
  for(i in 1:m) {
    if(is.function(base_data)) {
      data <- base_data()
    } else {
      data <- base_data
    }
    obs <- noisify_data(data, noise_rounds)
    metrics <- bind_rows(metrics, svm_metrics(obs))
  }
  metrics
}

## moved the rest of the stuff into the slides
