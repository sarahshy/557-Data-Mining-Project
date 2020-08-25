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

noisify_data <- function(data, noise_rounds = 1, hetero = FALSE) {
  if (noise_rounds < 1) return(data) # no noise needed!
  
  data_noisy <- data
  for(i in 1:noise_rounds){
    if(hetero == FALSE){ # homoscedastic error
      noise <- rmvnorm(n, c(0, 0), sigma_noise)
      data_noisy[,c("x1", "x2")] <- data_noisy[,c("x1", "x2")] + noise
      print(head(data_noisy))
    } else { # heteroscedastic error
      # sigma_noise <- matrix(c(1, 0, 0, 3), nrow = 2)
      noise_x1 <- rnorm(n, 0, sigma_x1)
      noise_x2 <- rnorm(n, 0, sigma_x2)
      noise <- cbind(noise_x1, noise_x2)
      data_noisy[,c("x1", "x2")] <- data_noisy[,c("x1", "x2")] + noise
    }
  }
  data_noisy
}

plot_data <- function(data) {
  ggplot(data) +
    geom_point(aes(x = x1, y = x2, color = label, shape = label)) +
    scale_color_manual(values = c("#225588", "#f1cc11")) +
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
  intercepts <- -results$theta_2^-1 * results$theta_0
  slopes <- -results$theta_2^-1 * results$theta_1
  plot_data(data) +
    geom_abline(intercept = intercepts, slope = slopes, alpha = alpha, color = "#999999")
}

plot_data_with_decision_boundaries_and_soft_classifications <- function(data, results) {
  # I've always been fond of long function names.
  classified <- soft_classify_set(sim_results_noisy, noisy)
  intercepts <- -results$theta_2^-1 * results$theta_0
  slopes <- -results$theta_2^-1 * results$theta_1
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
    transmute(fitted_sign = pmax(0, sign(theta_0 + theta_1 * x1 + theta_2 * x2))) %>%
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
      preds[i,j] <- pmax(0, sign(results$theta_0[j] + results$theta_1[j] * data$x1[i] + results$theta_2[j] * data$x2[i]))
    }
  }
  preds
}

svm_metrics <- function(data, cost = 1, gamma = 1/n) {
  # uses global train, test
  svm_result <- svm(label ~ ., data = data[train,], kernel = "linear", cost = 0.18)
  
  # my cutsie tuned polynomial svm
  # svm_result <- svm(label ~ ., data = data[train,], kernel = "poly", degree = 2, gamma = 1, coef0 = 0.1, cost = 10)
  coefs <- coef(svm_result)
  
  # Internally, e1071 centers and scales the data, then performs analysis
  # on this scaled data.
  # `coef(svm_result)` therefore gives us the separating hyperplane relative
  # to the scaled data, i.e.,:
  # beta_0 + beta_1 * x1 + beta_2 * x2 = 0
  # where x1 = (x - center_x)/scale_x, x2 = (y - center_x)/scale_y
  # To put things in terms of the original (unscaled) x and y, we have some ugly
  # work to do. First, we have to extract the centers and scales from `svm_result`:
  # (This doesn't seem like it could possibly be the best way to do it, but I don't
  # see any exposed function that gives this information back.)
  centers <- svm_result$x.scale$`scaled:center`
  scales <- svm_result$x.scale$`scaled:scale`
  
  # Then do some algebra to get a hyperplane in terms of x and y:
  # beta_0 + beta_1 * x1 + beta_2 * x2 = 0
  #   ==>
  # (beta_0 - beta_1/scale_x*mean_x - beta_2/scale_y*mean_y)
  #   + beta_1/scale_x * x
  #   + beta_2/scale_y * y
  #   = 0
  
  predicted <- predict(svm_result, newdata = data[test,])
  acc <- mean(predicted == data[test, ]$label)
  
  list(
    accuracy = acc,
    # separating hyperplane relative to scaled data
    beta_0 = coefs[1],
    beta_1 = coefs[2],
    beta_2 = coefs[3],
    # separating hyperplane relative to original (unscaled) data
    theta_0 = coefs[1] - coefs[2]/scales[1]*centers[1] - coefs[3]/scales[2]*centers[2],
    theta_1 = coefs[2]/scales[1],
    theta_2 = coefs[3]/scales[2]
  )
}

run_simulations <- function(base_data, m = 500, noise_rounds) { # m is number of perturbations
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
