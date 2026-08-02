simulation_results <- read_csv("bestResult.csv")
# c_hat_mean
x1_count_mean <- mean(simulation_results$target1_count)
n_count_mean <- mean(simulation_results$marker_count)
c1_hat_mean <- (x1_count_mean/n_count_mean) * lambda_marker # E(x)/E(n)
cat('The estimated concentration is:', c1_hat_mean, '\n')

# standard deviation
c1_hat_sd <- sd(simulation_results$c1_hat)
cat('The standard deviation is:', c1_hat_sd, '\n')

# coefficient of variation
cv1 <- 100*c1_hat_sd/c1_hat_mean
cat('The coefficient of variation(%) is:', cv1, '\n')

# total standard error
x1_error <- sqrt(x1_count_mean)/x1_count_mean
n_error <- sqrt(n_count_mean)/n_count_mean
total_se1 <- 100*sqrt(x1_error^2+n_error^2)
cat('The total standard error(%) is:', total_se1, '\n')

# accuracy
accuracy1 <- 100*(1 - abs(c1_hat_mean - lambda_target1) / lambda_target1)
cat('The accuracy(%) is:', accuracy1, '\n')

# data collection effort
time <- as.numeric(difftime(end_t, start_t, units = "secs")) 
cat('The data collection effort is:', time, '\n')
