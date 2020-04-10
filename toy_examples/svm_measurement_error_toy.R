library(mvtnorm)
library(e1071)
library(tidyverse)

set.seed(557)
n <- 500
train <- sample(1:n, round(.8*n))
test <- setdiff(1:n, train)

mean_1 <- c(5, 5)
sigma_1 <- matrix(c(8, 4, 4, 12), nrow = 2)
mean_2 <- c(15, 0)
sigma_2 <- matrix(c(10, 3, 3, 4), nrow = 2)

sigma_noise <- 50 * matrix(c(1, .5, .5, 2), nrow = 2)


make_base_data <- function() {
  c1 <- rmvnorm(ceiling(n/2), mean = mean_1, sigma = sigma_1)
  c2 <- rmvnorm(floor(n/2), mean = mean_2, sigma = sigma_2)
  
  data <- rbind(cbind(c1, 1), cbind(c2, 2))
  colnames(data) <- c("x", "y", "label")
  
  as_tibble(data) %>% mutate(label = as.factor(label))
}

noisify_data <- function(data, noise_rounds = 1) {
  # uses global data
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

data <- make_base_data()
plot_data(data)

noisy <- noisify_data(data)
plot_data(noisy)

svm_metrics(data)
svm_metrics(noisy)

run_simulations <- function(base_data, m, noise_rounds = 1) {
  metrics <- NULL
  for(i in 1:m) {
    obs <- noisify_data(base_data, noise_rounds)
    metrics <- bind_rows(metrics, svm_metrics(obs))
  }
  metrics
}

# simulate actual measurement error
sim_results <- run_simulations(data, 500)

# simulate measurement error on top of a given noisy observation
sim_results_noisy <- run_simulations(noisy, 500)

# simulate measurement error with double noise
sim_results_double <- run_simulations(data, 500, 2)

sim_results_combined <- bind_rows(
  sim_results %>% add_column(noise = "on true data"),
  sim_results_double %>% add_column(noise = "on true data twice"),
  sim_results_noisy %>% add_column(noise = "on noisy observation")
)

ggplot(sim_results_combined) + geom_boxplot(aes(x = noise, y = accuracy))
