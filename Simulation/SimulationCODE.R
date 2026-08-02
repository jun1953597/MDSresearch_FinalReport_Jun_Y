library(tidyverse)
set.seed(42)
output_file <- "sim_result_multi.csv"
# =========================================================
# Simulation Window
xmin <- 0
xmax <- 1
ymin <- 0
ymax <- 1

# Simulate the two-dimensional homogeneous Poisson process point
simulate_one_type <- function(lambda, type_name,
                              xmin = 0, xmax = 1,
                              ymin = 0, ymax = 1) {
  area <- (xmax - xmin) * (ymax - ymin)
  n <- lambda # rpois(1, lambda * area)
  
  tibble(
    x = runif(n, xmin, xmax),
    y = runif(n, ymin, ymax),
    type = type_name)}

simulate_marked_ppp <- function(lambda_target1, lambda_marker, lambda_target2,
                                xmin = 0, xmax = 1,
                                ymin = 0, ymax = 1) {
  bind_rows(
    simulate_one_type(lambda_target1, "target1", xmin, xmax, ymin, ymax),
    simulate_one_type(lambda_marker, "marker", xmin, xmax, ymin, ymax),
    simulate_one_type(lambda_target2,  "target2",  xmin, xmax, ymin, ymax)
  ) %>%
    mutate(id = row_number()) %>%
    select(id, x, y, type)
}

# Setting of the Counting Window
left_x <- 0.10          # Left boundary
initial_width <- 0.1   # Initial width
y_lower <- 0.45         # lower boundary
y_upper <- 0.55         # upper boundary
step_width <- 0.002      # The width each time it extends to the right

# =========================================================
# Filter points within the counting window
filtered_points <- function(data, x_left, x_right, y_lower, y_upper) {
  data %>%
    filter(
      x >= x_left,
      x <= x_right,
      y >= y_lower,
      y <= y_upper)
}

expand_rectangle <- function(data, x_left, initial_width, y_lower, y_upper,
                             marker_goal, step_width, xmax) {
  x_right <- x_left + initial_width
  
  repeat {
    inside <- filtered_points(data, x_left, x_right, y_lower, y_upper)
    
    marker_count <- sum(inside$type == "marker")
    target1_count <- sum(inside$type == "target1")
    target2_count  <- sum(inside$type == "target2")
    
    if (marker_count >= marker_goal || x_right >= xmax) {
      return(list(
        x_left = x_left,
        x_right = x_right,
        y_lower = y_lower,
        y_upper = y_upper,
        width = x_right - x_left,
        area = (x_right - x_left) * (y_upper - y_lower),
        marker_count = marker_count,
        target1_count = target1_count,
        target2_count = target2_count,
        total_count = nrow(inside),
        points_inside = inside
      ))
    }
    
    x_right <- min(x_right + step_width, xmax)
  }
}

# =========================================================
start_t <- Sys.time() # start time
# parameter setting
lambda_target1 <- 10000
lambda_target2 <- 0
lambda_marker <- 5000
lambda_total <- lambda_target1 + lambda_marker + lambda_target2
marker_goal <- lambda_marker/20
nsim <- 500 # Number of simulations

simulation_list <- map(seq_len(nsim), function(sim) {
  data <- simulate_marked_ppp(
    lambda_target1 = lambda_target1,
    lambda_marker = lambda_marker,
    lambda_target2 = lambda_target2,
    xmin = xmin, xmax = xmax,
    ymin = ymin, ymax = ymax)
  
  count_res <- expand_rectangle(
    data = data,
    x_left = left_x,
    initial_width = initial_width,
    y_lower = y_lower,
    y_upper = y_upper,
    marker_goal = marker_goal,
    step_width = step_width,
    xmax = xmax
  )
  
  x1_count <- count_res$target1_count
  x2_count <- count_res$target2_count
  n_count <- count_res$marker_count
  
  c1_hat <- if (n_count > 0) (x1_count / n_count) * lambda_marker else NA_real_
  c2_hat <- if (n_count > 0) (x2_count / n_count) * lambda_marker else NA_real_
  
  
  list(
    sim = sim,
    data = data,
    count_summary = tibble(
      sim = sim,
      total = lambda_total,
      target1 = lambda_target1,
      target2 = lambda_target2,
      marker = lambda_marker,
      marker_count = n_count,
      target1_count = x1_count,
      target2_count = x2_count,
      c1_hat = c1_hat,
      c2_hat = c2_hat)
  )
})

simulation_results <- map_dfr(simulation_list, "count_summary")
print(head(simulation_results))

# =========================================================
# c_hat_mean
x1_count_mean <- mean(simulation_results$target1_count)
x2_count_mean <- mean(simulation_results$target2_count)
n_count_mean <- mean(simulation_results$marker_count)
c1_hat_mean <- (x1_count_mean/n_count_mean) * lambda_marker # E(x)/E(n)
c2_hat_mean <- (x2_count_mean/n_count_mean) * lambda_marker # E(x)/E(n)

# standard deviation
c1_hat_sd <- sd(simulation_results$c1_hat)
c2_hat_sd <- sd(simulation_results$c2_hat)

# coefficient of variation
cv1 <- 100*c1_hat_sd/c1_hat_mean
cv2 <- 100*c2_hat_sd/c2_hat_mean

# total standard error
x1_error <- sqrt(x1_count_mean)/x1_count_mean
x2_error <- sqrt(x2_count_mean)/x2_count_mean
n_error <- sqrt(n_count_mean)/n_count_mean
total_se1 <- 100*sqrt(x1_error^2+n_error^2)
total_se2 <- 100*sqrt(x2_error^2+n_error^2)

# accuracy
accuracy1 <- 100*(1 - abs(c1_hat_mean - lambda_target1) / lambda_target1)
accuracy2 <- 100*(1 - abs(c2_hat_mean - lambda_target2) / lambda_target2)

# correlation
cov <- cov(simulation_results$c1_hat, simulation_results$c2_hat)
cor <- cor(simulation_results$c1_hat, simulation_results$c2_hat)

# data collection effort
end_t <- Sys.time() # end time 
time <- as.numeric(difftime(end_t, start_t, units = "secs")) 

# store one row for this run
run_summary <- tibble(
  nsim = nsim,
  lambda_target1 = lambda_target1,
  lambda_target2 = lambda_target2,
  lambda_marker = lambda_marker,
  lambda_total = lambda_total,
  marker_goal = marker_goal,
  
  
  c1_hat_mean = c1_hat_mean,
  c2_hat_mean = c2_hat_mean,
  
  c1_hat_sd = c1_hat_sd,
  c2_hat_sd = c2_hat_sd,
  
  cv1 = cv1,
  total_standard_error1 = total_se1,
  cv2 = cv2,
  total_standard_error2 = total_se2,
  
  accuracy1 = accuracy1,
  accuracy2 = accuracy2,
  
  cov = cov,
  cor = cor,
  
  runtime_seconds = time
)
cat(sum(simulation_results$target2_count > 0))

# append to CSV
write_csv(run_summary, output_file, append = TRUE)
