library(tidyverse)
library(mvtnorm)

# 2d representation
mean_s1 <- c(0, 1)
sigma_s1 <- matrix(c(1, 0, 0, 1.5^2), ncol = 2)
mean_s2 <- c(1, 0)
sigma_s2 <- matrix(c(1.5^2, 0, 0, 1), ncol = 2)

x <- seq(-4, 4, length.out = 3000)
y <- seq(-4, 4, length.out = 3000)
grid <- expand.grid(S1 = x, S2 = y)

grid$dens_s1 <- dmvnorm(grid[, 1:2], mean = mean_s1, sigma = sigma_s1)
grid$dens_s2 <- dmvnorm(grid[, 1:2], mean = mean_s2, sigma = sigma_s2)

grid_long <- grid %>%
  select(S1, S2, dens_s1, dens_s2) %>%
  tidyr::pivot_longer(cols = starts_with("dens_"),
                      names_to = "condition", values_to = "density") %>%
  mutate(condition = recode(condition,
                            dens_s1 = "Target = S1",
                            dens_s2 = "Target = S2"))

ggplot(grid_long, aes(x = S1, y = S2, z = density, fill = condition)) +
  geom_contour(aes(color = condition), bins = 4, size = 0.4) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", linewidth = 0.4) +
    labs(x = "Signals from the lower visual field", y = "Signals from the upper visual field", color = "Condition", fill = "Condition") +
  coord_fixed() + theme_minimal(base_size = 8) + 
  theme(legend.position = "none",
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6)) +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(breaks = seq(-4, 4, by = 2), limits = c(-4, 4)) +
  scale_y_continuous(breaks = seq(-4, 4, by = 2), limits = c(-4, 4)) -> g1

# S1 detection
x_vals <- seq(-4, 4, length.out = 1000)
df <- tibble(
  x = x_vals,
  density_1 = dnorm(x, mean = 0, sd = 1),
  density_2 = dnorm(x, mean = 1, sd = 1.5)
) %>%
  pivot_longer(cols = starts_with("density"),
               names_to = "distribution", values_to = "density") %>%
  mutate(distribution = recode(distribution,
                             density_1 = "mean = 0, sd = 1.0",
                             density_2 = "mean = 1, sd = 1.5"))

ggplot(df, aes(x = x, y = density, color = distribution)) +
  geom_line(size = 0.5) +
  labs(x = NULL, y = NULL) +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(breaks = seq(-4, 4, by = 2), limits = c(-4, 4)) +
  scale_y_continuous(breaks = seq(0, 0.5, by = 0.2)) +
  theme_minimal(base_size = 8) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6)) -> g2

# S2 detection
x_vals <- seq(-4, 4, length.out = 1000)
df <- tibble(
  x = x_vals,
  density_1 = dnorm(x, mean = 0, sd = 1),
  density_2 = dnorm(x, mean = 1, sd = 1.5)
) %>%
  pivot_longer(cols = starts_with("density"),
               names_to = "distribution", values_to = "density") %>%
  mutate(distribution = recode(distribution,
                               density_1 = "mean = 0, sd = 1.0",
                               density_2 = "mean = 1, sd = 1.5")
  ) %>%
  mutate(distribution = factor(distribution,
                               levels = c("mean = 1, sd = 1.5", "mean = 0, sd = 1.0")
  ))

ggplot(df, aes(x = x, y = density, color = distribution)) +
  geom_line(size = 0.5) +
  labs(x = NULL, y = NULL) +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(breaks = seq(-4, 4, by = 2), limits = c(-4, 4)) +
  scale_y_continuous(breaks = seq(0, 0.5, by = 0.2)) +
  theme_minimal(base_size = 8) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6)) -> g3

# Relative discrimination
x_vals <- seq(-7, 7, length.out = 1000)

df <- tibble(
  x = x_vals,
  dist1 = dnorm(x, mean = -1.0, sd = sqrt(1 + 1.5^2)),
  dist2 = dnorm(x, mean =  1.0, sd = sqrt(1 + 1.5^2))) %>%
  pivot_longer(cols = starts_with("dist"),
               names_to = "distribution", values_to = "density") %>%
  mutate(distribution = recode(distribution,
                               dist1 = "mean = -1, sd = 1.8",
                               dist2 = "mean = 1, sd = 1.8")) %>%
  mutate(distribution = factor(distribution, levels = c("mean = -1, sd = 1.8", "mean = 1, sd = 1.8")))

ggplot(df, aes(x = x, y = density, color = distribution)) +
  geom_line(size = 0.5) +
  labs(x = NULL, y = NULL) +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(breaks = seq(-4, 4, by = 2), limits = c(-5, 5)) +
  scale_y_continuous(breaks = seq(0, 0.5, by = 0.2)) +
  theme_minimal(base_size = 8) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6)
  ) -> g4


# save figures
range_g3 <- c(-4, 4)
range_g4 <- c(-5, 5)
span_g3 <- diff(range_g3)
span_g4 <- diff(range_g4)

ggsave("g1.jpg", plot = g1, width = 2, height = 2, units = "in", dpi = 500)
ggsave("g2.jpg", plot = g2, width = 2, height = 0.6, units = "in", dpi = 500)
ggsave("g3.jpg", plot = g3, width = 2, height = 0.6, units = "in", dpi = 500)
ggsave("g4.jpg", plot = g4, width = 2 * span_g4 / span_g3, height = 0.48, units = "in", dpi = 500)


# sdt measures
da <- 1/sqrt((1 + 1.5^2)/2)
da
dp <- 2/sqrt(1 + 1.5^2)
dp