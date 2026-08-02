# =========================================================
one_sim <- read.csv("example.csv")

one_count <- expand_rectangle(
  dat = one_sim,
  x_left = left_x,
  initial_width = initial_width,
  y_lower = y_lower,
  y_upper = y_upper,
  marker_goal = marker_goal,
  step_width = step_width,
  xmax = xmax)

rect_df <- tibble(
  xmin = one_count$x_left,
  xmax = one_count$x_right,
  ymin = one_count$y_lower,
  ymax = one_count$y_upper)

one_c_hat <- if (one_count$marker_count > 0) {
  (one_count$target_count / one_count$marker_count) * lambda_marker} else {NA_real_}

p_demo <- ggplot(one_sim, aes(x = x, y = y, color = type)) +
  geom_point(size = 2, alpha = 0.85) +
  geom_rect(
    data = rect_df,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE,
    fill = NA,
    color = "black",
    linewidth = 1) +
  coord_equal(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  theme_minimal() +
  labs(
    title = "Example diagram of simulation",
    subtitle = bquote(
      marker == .(one_count$marker_count) * "," ~
        target == .(one_count$target_count) * "," ~
        hat(c) == .(round(one_c_hat, 2))),
    x = "x",
    y = "y",
    color = "type") +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5))

print(p_demo)

# =========================================================
one_sim2 <- read.csv("example2.csv")

one_count2 <- expand_rectangle(
  dat = one_sim2,
  x_left = left_x,
  initial_width = initial_width,
  y_lower = y_lower,
  y_upper = y_upper,
  marker_goal = marker_goal,
  step_width = step_width,
  xmax = xmax)

rect_df2 <- tibble(
  xmin = one_count$x_left,
  xmax = one_count$x_right,
  ymin = one_count$y_lower,
  ymax = one_count$y_upper)

p_demo2 <- ggplot(one_sim2, aes(x = x, y = y, color = type)) +
  geom_point(size = 2, alpha = 0.85) +
  geom_rect(
    data = rect_df2,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE,
    fill = NA,
    color = "black",
    linewidth = 1
  ) +
  coord_equal(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  theme_minimal() +
  labs(
    title = "Example diagram of simulation in multiple target scenarios",
    subtitle = bquote(
      marker == .(one_count2$marker_count) * "," ~
        target1 == .(one_count2$target1_count) * "," ~
        target2 == .(one_count2$target2_count)),
    x = "x",
    y = "y",
    color = "type") +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5))

print(p_demo2)
