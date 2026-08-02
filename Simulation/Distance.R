# The distance graph is only applicable to one type
fossil_type_for_distance <- "target"

lambda_distance <- c(target = lambda_target, marker = lambda_marker, other  = lambda_other)[[fossil_type_for_distance]]

subset_type <- function(dat, fossil_type = "target") {
  dat %>%
    filter(type == fossil_type) %>%
    mutate(id = row_number())
}

# =========================================================
# inter-event distances
inter_event_distances <- function(dat) {
  n <- nrow(dat)
  
  if (n < 2) {
    return(tibble(i = integer(), j = integer(), distance = double()))
  }
  
  pairs <- combn(n, 2)
  
  tibble(i = pairs[1, ], j = pairs[2, ],
         distance = as.vector(dist(dat %>% select(x, y))))}

# nearest neighbor distances
nearest_neighbour_distances <- function(dat) {
  n <- nrow(dat)
  
  if (n < 2) {return(dat %>% mutate(nn_id = NA_integer_, nn_distance = NA_real_))}
  
  dmat <- as.matrix(dist(dat %>% select(x, y)))
  diag(dmat) <- Inf
  
  dat %>%
    mutate(nn_id = apply(dmat, 1, which.min), nn_distance = apply(dmat, 1, min))}

# =========================================================
# G(r) 
G_nn_theory <- function(r, lambda) {
  1 - exp(-lambda * pi * r^2)
}
# g(r)
g_nn_theory <- function(r, lambda) {
  2 * pi * lambda * r * exp(-lambda * pi * r^2)
}

# =========================================================
# Nearest Neighbor Distance
nn_all <- map_dfr(simulation_list, function(obj) {
  fossil_dat <- subset_type(obj$data, fossil_type_for_distance)
  
  nearest_neighbour_distances(fossil_dat) %>%
    transmute(sim = obj$sim, nn_distance = nn_distance)
})

if (nrow(nn_all) > 0) {
  p_nn_all_hist <- ggplot(nn_all, aes(x = nn_distance)) +
    geom_histogram(aes(y = after_stat(density)), bins = 40) +
    stat_function(
      fun = function(x) g_nn_theory(x, lambda_distance),
      linewidth = 1) +
    theme_minimal() +
    labs(
      title = paste("Density Histogram of the Nearest Neighbor Distance"),
      x = "distance", y = "density") + 
    theme(plot.title = element_text(hjust = 0.5))
  
  print(p_nn_all_hist)}

# =========================================================
# Inter-event distances
pair_all <- map_dfr(simulation_list, function(obj) {
  fossil_dat <- subset_type(obj$data, fossil_type_for_distance)
  
  inter_event_distances(fossil_dat) %>%
    transmute(sim = obj$sim, distance = distance)
})

if (nrow(pair_all) > 0) {
  p_pair_all_hist <- ggplot(pair_all, aes(x = distance)) +
    geom_histogram(bins = 50) +
    theme_minimal() +
    labs(
      title = paste("Frequency histogram of Inter-event Distances"),
      x = "distance", y = "count") +
    theme(plot.title = element_text(hjust = 0.5))
  
  print(p_pair_all_hist)}