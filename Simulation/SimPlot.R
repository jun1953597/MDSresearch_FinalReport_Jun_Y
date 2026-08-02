# =========================================================
cor_results <- read.csv("cor_results.csv")
cor_c <- ggplot(cor_results,aes(x = c1_hat, y = c2_hat)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Correlation between concentration estimates",
       subtitle = bquote(Correlation == 0.8332),
       x = expression(hat(c)[1]),
       y = expression(hat(c)[2])) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(size = 14, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

print(cor_c)

# =========================================================
cumc_results <- read.csv("cumc_results.csv")
cumc_plot <- ggplot(cumc_results, aes(x = sim, y = cum_c_hat)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_hline(
    yintercept = 1000,
    linetype = "dashed",
    color = "red"
  ) +
  labs(
    title = "Convergence of cumulative concentration estimate",
    x = "Simulation times",
    y = expression(bar(c))
  ) +
  theme_minimal()

print(cumc_plot)

# =========================================================
df <- read_csv("sim_result.csv")
df <- df[-1, ]
df_ratio <- df[1:7, ]
df_ratio$`x:n ratio` <- factor(df_ratio$`x:n ratio`, levels = unique(df_ratio$`x:n ratio`))
# scale
scale_factor <- max(c(df$cov, df$total_standard_error), na.rm = TRUE) /
  max(df$c_hat_sd, na.rm = TRUE)
# plot
ratio <- ggplot(df_ratio, aes(x = `x:n ratio`)) +
  # cov curve
  geom_line(aes(y = cov, color = "CV", group = 1),linewidth = 1.2) +
  geom_point(aes(y = cov,color = "CV"),size = 3) +
  # total_standard_error curve
  geom_line(aes(y = total_standard_error,color = "sigma",group = 1),
            linewidth = 1.2,linetype = "dashed") +
  geom_point(aes(y = total_standard_error,color = "sigma"), size = 3) +
  
  # c_hat_sd 
  geom_col(aes(y = c_hat_sd * scale_factor,fill = "sd"),alpha = 0.4,width = 0.6) +
  # axis
  scale_y_continuous(name = "CV, sigma",sec.axis = sec_axis(~ . / scale_factor, name = "sd")) +
  scale_color_manual( values = c("CV" = "#D55E00","sigma" = "#0072B2")) +
  scale_fill_manual(values = c("sd" = "grey60" ) ) +
  
  labs(title = "The variation of statistics under different ratios u",
       x = "target-to-marker (u) ratio") +
  
  theme_minimal() +
  theme(
    axis.title.y.left = element_text(color = "black"),
    axis.title.y.right = element_text(color = "black"),
    legend.title = element_blank(),
    legend.position = "top")

print(ratio)

# =========================================================
df_runtime <- df[8:14, ]
df_runtime$precision <- 100 - df_runtime$total_standard_error
df_runtime <- df_runtime[order(df_runtime$runtime_seconds), ]

fit <- smooth.spline(df_runtime$runtime_seconds, df_runtime$precision)
x_new <- seq(min(df_runtime$runtime_seconds), max(df_runtime$runtime_seconds),length.out = 200)
y_new <- predict(fit, x = x_new)$y
smooth_df <- data.frame(runtime_seconds = x_new,precision = y_new)

runtime <- ggplot() +
  geom_point(data = df_runtime,
             aes(x = runtime_seconds, y = precision),
             size = 3, color = "#D55E00") +
  geom_line(data = smooth_df,
            aes(x = runtime_seconds, y = precision),
            linewidth = 1.2, color = "#0072B2") +
  geom_text(data = df_runtime,
            aes(x = runtime_seconds, y = precision, label =`x:n ratio`),
            vjust = -1,size = 4) +
  labs(
    title = "The relationship between Data collection effort and Precision",
    x = "data collection effort",
    y = "precision") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin = margin(r = 20, l = 20))

print(runtime)


# =========================================================
df_marker <- df[16:25, ]
df_marker$marker_goal <- as.numeric(as.character(df_marker$marker_goal))
scale_factor_marker <- max(df_marker$total_standard_error, na.rm = TRUE) / 
  max(df_marker$runtime_seconds, na.rm = TRUE)

threshold_m <- ggplot(df_marker, aes(x = marker_goal)) +
  geom_line(aes(y = cov, color = "sigma", group = 1), linewidth = 1) +
  geom_point(aes(y = cov, color = "sigma"), size = 2) +
  
  geom_line(aes(y = runtime_seconds * scale_factor_marker, color = "effort", group = 1), linewidth = 1) +
  geom_point(aes(y = runtime_seconds * scale_factor_marker, color = "effort"), size = 2) +
  
  scale_y_continuous(name = "sigma",sec.axis = sec_axis(~ . / scale_factor_marker, name = "effort")) +
  scale_color_manual( values = c("sigma" = "#D55E00","effort" = "#0072B2")) +
  
  labs(x = "marker_threshold",title = "The influence of the marker threshold on the results") +
  theme_minimal(base_size = 14)+
  theme(legend.title = element_blank(),
        legend.position = "top")

print(threshold_m)

# =========================================================
df_multi <- read.csv("sim_result_multi.csv")
df_cor <- df_multi[3:13, ]
df_cor$x1.x2 <- factor(df_cor$x1.x2, levels = df_cor$x1.x2)

corr <- ggplot(df_cor, aes(x = x1.x2, y = cor, group = 1)) +
  geom_line(linewidth = 1.1, color = "#0072B2") +
  geom_point(size = 3.2, color = "#0072B2") +
  geom_text(
    aes(label = sprintf("%.3f", cor)),vjust = -0.8,size = 4) +
  scale_y_continuous(
    limits = c(0.6, 0.9),
    breaks = seq(0.6, 0.9, by = 0.05),
    expand = expansion(mult = c(0.02, 0.08))) +
  labs(x = expression("target1-to-target2 (" * u[x] * ") ratio "),y = "Correlation",
    title = expression("Correlation under different ratios " * u[x])) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 15, hjust = 0.5, face = "bold"),
    axis.title.x = element_text(size = 13, margin = margin(t = 10)),
    axis.title.y = element_text(size = 13, margin = margin(r = 10)),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 15, 10, 15))

print(corr)

# =========================================================
df_cor_long <- df_cor %>%
  mutate(x1.x2 = factor(x1.x2, levels = x1.x2)) %>%
  pivot_longer(
    cols = c(total_standard_error1, total_standard_error2),
    names_to = "se_type", values_to = "se") %>%
  mutate(se_type = recode(se_type,
                          total_standard_error1 = "target1",total_standard_error2 = "target2"))

cv <- ggplot(df_cor_long, aes(x = x1.x2, y = se, fill = se_type)) +
  geom_col(
    position = position_dodge(width = 0.75),
    width = 0.65, alpha = 0.8) +
  coord_cartesian(ylim = c(14.5, 20.5)) +
  scale_y_continuous(
    breaks = seq(14, 20, by = 1)) +
  labs(x = expression("target1-to-target2 (" * u[x] * ") ratio"), y = expression(sigma),
       title = expression("The variation of " * sigma ~" under different ratios " * u[x] )) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "top",
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 16, hjust = 0.5))

print(cv)

# =========================================================
df_cor2 <- df_multi[15:19, ]
df_cor_long2 <- df_cor2 %>%
  mutate(x1.x2 = factor(x1.x2, levels = c("9", "7", "4", "3", "1"))) %>%
  pivot_longer(
    cols = c(total_standard_error1, total_standard_error2),
    names_to = "se_type", values_to = "se") %>%
  mutate(se_type = recode(se_type,
                          total_standard_error1 = "target1",total_standard_error2 = "target2"))
cv2 <- ggplot(df_cor_long2, aes(x = x1.x2, y = se, fill = se_type)) +
  geom_col(
    position = position_dodge(width = 0.75),
    width = 0.5, alpha = 0.8) +
  coord_cartesian(ylim = c(6.5, 18.5)) +
  scale_y_continuous(
    breaks = seq(6, 20, by = 2)) +
  labs(x = "x1:x2", y = "SE",
       title = "The variation of SE in scenario2") +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "top",
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 16, hjust = 0.5))

print(cv2)

# =========================================================
df_cor3 <- df_multi[21:26, ]
df_cor_long3 <- df_cor2 %>%
  mutate(x1.x2 = factor(x1.x2, levels = c("9", "7", "4", "3", "1"))) %>%
  pivot_longer(
    cols = c(total_standard_error1, total_standard_error2),
    names_to = "se_type", values_to = "se") %>%
  mutate(se_type = recode(se_type,
                          total_standard_error1 = "target1",total_standard_error2 = "target2"))
cv3 <- ggplot(df_cor_long3, aes(x = x1.x2, y = se, fill = se_type)) +
  geom_col(
    position = position_dodge(width = 0.75),
    width = 0.5, alpha = 0.8) +
  coord_cartesian(ylim = c(6.5, 18.5)) +
  scale_y_continuous(
    breaks = seq(6, 20, by = 2)) +
  labs(x = "x1:x2", y = "SE",
       title = "The variation of SE in scenario3") +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "top",
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 16, hjust = 0.5))

print(cv3)
# =========================================================
df_rare <- df_multi[28:40, ]
df_rare$x1.x2 <- factor(df_rare$x1.x2, levels = df_rare$x1.x2)
df_rare1 <- df_rare[1:6, ]
ggplot(df_rare1, aes(x = x1.x2, y = total_standard_error2)) +
  geom_line(color = "#2C7FB8",linewidth = 1.1,group = 1) +
  geom_point(color = "#2C7FB8",size = 3) +
  labs(
    x = expression("marker-to-target2 (" * u[r] * ") ratio"),
    y = expression(sigma ~ "of rare target"),
    title = "Variation in SE of rare target",
    subtitle = expression("ratio " * u[r]* " from 1 to 20")) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text( size = 15, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text = element_text(color = "black"),
    axis.line = element_line(linewidth = 0.6),
    axis.ticks = element_line(linewidth = 0.6))

# =========================================================
df_rare2 <- df_rare[7:13, ]
df_rare2$detection_probability <- df_rare2[, ncol(df_rare2)] / 1000
ggplot(df_rare2, aes(x = x1.x2, y = detection_probability, group = 1)) +
  geom_line(color = "#D95F0E",linewidth = 1.1) +
  geom_point(color = "#D95F0E",size = 3) +
  geom_hline(
    yintercept = df_rare2$detection_probability[as.character(df_rare2$x1.x2) == "100"],
    linetype = "dashed",
    color = "red") +
  geom_text(
    data = df_rare2[df_rare2$x1.x2 == 100, ],
    aes(label = paste0(round(detection_probability * 100, 1), "%")),vjust = -1,color = "black",size = 4) +
  scale_y_continuous(limits = c(0.6, 1),breaks = seq(0.6, 1, by = 0.1),labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = expression("marker-to-target2 (" * u[r] * ") ratio "),
    y = "Detection probability",
    title = "Detection probability of rare target",
    subtitle = expression("ratio " * u[r]* " from 25 to 200")) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text( size = 15, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text = element_text(color = "black"),
    axis.line = element_line(linewidth = 0.6),
    axis.ticks = element_line(linewidth = 0.6))
# =========================================================
