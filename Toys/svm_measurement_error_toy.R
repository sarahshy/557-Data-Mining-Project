library(mvtnorm)
library(e1071)
library(tidyverse)

n <- 500
train <- sample(1:n, round(.7*n))
test <- setdiff(1:n, train)

mean_1 <- c(1, 2)
sigma_1 <- matrix(c(3, -.5, -.5, 2), nrow = 2)
mean_2 <- c(-1, -1)
sigma_2 <- matrix(c(4, -1, -1, 1), nrow = 2)

sigma_noise <- matrix(c(1, 0, 0, 3), nrow = 2)

make_base_data <- function() {
  c1 <- rmvnorm(ceiling(n/2), mean = mean_1, sigma = sigma_1)
  c2 <- rmvnorm(floor(n/2), mean = mean_2, sigma = sigma_2)
  
  data <- rbind(cbind(c1, 1), cbind(c2, 2))
  colnames(data) <- c("x1", "x2", "label")
  
  as_tibble(data) %>% mutate(label = as.factor(label))
}

noisify_data <- function(data, noise_rounds = 1) {
  if (noise_rounds < 1) return(data) # no noise needed!
  
  data_noisy <- data
  for(i in 1:noise_rounds) {
    noise <- rmvnorm(nrow(data), c(0, 0), sigma_noise)
    data_noisy[,c("x1", "x2")] <- data_noisy[,c("x1", "x2")] + noise
  }
  data_noisy
}

plot_data <- function(data) {
  ggplot(data) +
    geom_point(aes(x = x1, y = x2, color = label, shape = label)) +
    xlim(-10, 10) +
    ylim(-10, 10)
}

# blah, blah, DRY...
plot_data_with_error <- function(data) {
  plot_data(data) +
    geom_errorbar(aes(x = x1,
                      ymin = x2 - sigma_noise[2,2]/2,
                      ymax = x2 + sigma_noise[2,2]/2,
                      color = label)
                  , alpha = 0.2) +
    geom_errorbarh(aes(y = x2,
                       xmin = x1 - sigma_noise[1,1]/2,
                       xmax = x1 + sigma_noise[1,1]/2,
                       color = label),
                   alpha = 0.2)
}

plot_data_with_decision_boundaries <- function(data, results, alpha = 0.1) {
  # (used in presentation, keeping so as not to break it
  # even though we'll probably never knit that thing again)
  intercepts <- -results$beta_2^-1 * results$beta_0
  slopes <- -results$beta_2^-1 * results$beta_1
  plot_data(data) +
    geom_abline(intercept = intercepts, slope = slopes, alpha = alpha, color = "#999999")
}

plot_data_with_decision_boundaries_and_soft_classifications <- function(data, results) {
  # I've always been fond of long function names.
  classified <- soft_classify_set(sim_results_noisy, noisy)
  intercepts <- -results$beta_2^-1 * results$beta_0
  slopes <- -results$beta_2^-1 * results$beta_1
  ggplot(classified) +
    geom_abline(intercept = intercepts, slope = slopes, alpha = 0.05, color = "#777777") +
    geom_point(aes(x = x1, y = x2, color = p), size = 1) +
    scale_color_gradientn(colors = c("#f1cc11", "#99cc11", "#337755", "#225588"),
                          values = c(0, 0.1, 0.9, 1),
                          name = "P(class 1)") +
    xlim(-10, 10) +
    ylim(-10, 10)
}

soft_classify <- function(results, x1, x2) {
  results %>%
    transmute(fitted_sign = pmax(0, sign(beta_0 + beta_1 * x1 + beta_2 * x2))) %>%
    summarize(p = mean(fitted_sign)) %>%
    as_vector() %>%
    unname()
}

# This is terrible.
soft_classify_set <- function(results, data) {
  scv <- Vectorize(function(x1, x2) soft_classify(results, x1, x2))
  data %>% mutate(p = scv(x1, x2))
}

# This is even worse.
all_predictions <- function(results, data) {
  n <- nrow(data)
  B <- nrow(results)
  preds <- matrix(NA, nrow = n, ncol = B)
  for(i in 1:n) {
    for(j in 1:B) {
      preds[i,j] <- pmax(0, sign(results$beta_0[j] + results$beta_1[j] * data$x1[i] + results$beta_2[j] * data$x2[i]))
    }
  }
  preds
}

svm_metrics <- function(data) {
  # uses global train, test
  svm_result <- svm(label ~ ., data = data[train,], kernel = "linear", cost = 0.18)
  
  # my cutsie tuned polynomial svm
  # svm_result <- svm(label ~ ., data = data[train,], kernel = "poly", degree = 2, gamma = 1, coef0 = 0.1, cost = 10)
  coefs <- coef(svm_result)
  predicted <- predict(svm_result, newdata = data[test,])
  acc <- mean(predicted == data[test, ]$label)
  list(
    accuracy = acc,
    beta_0 = coefs[1],
    beta_1 = coefs[2],
    beta_2 = coefs[3]
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
