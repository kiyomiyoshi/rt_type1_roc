library(tidyverse)
library(data.table)
library(grid)
library(cowplot)
library(sjPlot)

#'#
nr_s1 <- c(10,  7, 16, 27, 29, 10)
nr_s2 <- c(43, 21, 10, 12,  8,  3)
sum(nr_s1)
sum(nr_s2)

uvsdt_conf <- fit_uvsdt_mle(nr_s1, nr_s2, add_constant = F)
uvsdt_conf
zh <- qnorm(sum(nr_s2[1:(length(nr_s2)/2)]) / sum(nr_s2))
zf <- qnorm(sum(nr_s1[1:(length(nr_s1)/2)]) / sum(nr_s1))
dp <- zh - zf
dp

#'# empirical roc
nfa <- cumsum(nr_s1)
nhit <- cumsum(nr_s2)
pfa <- nfa/sum(nr_s1)
phit <- nhit/sum(nr_s2)
roc <- data.frame(phit, pfa)[-6,]

p1 <- ggplot(roc, aes(x = pfa, y = phit)) + geom_point() +
  xlim(0, 1) + ylim(0, 1) + xlab("FA rate") + ylab("Hit rate")

#'# auc
auc <- phit[1]*pfa[1]/2
for (n in 1:11) {
  auc <- auc + (phit[n] + phit[n + 1])*(pfa[n + 1] - pfa[n])/2
}
auc

#'# sdt model
mu <-    as.numeric(uvsdt_conf[1]) 
sigma <- as.numeric(uvsdt_conf[2])

x_vals <- seq(-4, 6, length.out = 1000)
df <- tibble(
  x = x_vals,
  density_1 = dnorm(x_vals, mean = 0, sd = 1),
  density_2 = dnorm(x_vals, mean = mu, sd = sigma)
) %>%
  pivot_longer(cols = starts_with("density"),
               names_to = "distribution", values_to = "density")

x_vals <- seq(-4, 6, length.out = 1000)
df <- tibble(
  x = x_vals,
  density_1 = dnorm(x_vals, mean = 0, sd = 1),
  density_2 = dnorm(x_vals, mean = mu, sd = sigma)
) %>%
  pivot_longer(cols = starts_with("density"),
               names_to = "distribution", values_to = "density")

color_1 <- "red" # "#E41A1C"
color_2 <- "blue" # "#377EB8"

y1_peak <- dnorm(0, mean = 0, sd = 1)
y2_peak <- dnorm(mu, mean = mu, sd = sigma)

ggplot(df, aes(x = x, y = density, color = distribution)) +
  geom_line(size = 0.5, alpha = 0.7) +
  annotate("segment", x = 0, xend = 0 + 1, y = y1_peak * 0.6, yend = y1_peak * 0.6,
           arrow = arrow(ends = "both", length = unit(0.1, "cm")),
           color = rgb(1, 0, 0, alpha = 0.7), size = 0.3) +
  annotate("text", x = 0.5, y = y1_peak * 0.6 - 0.015, label = "1", size = 3, color = rgb(1, 0, 0, alpha = 0.7)) +
  annotate("segment", x = mu, xend = mu + sigma, y = y2_peak * 0.6, yend = y2_peak * 0.6,
           arrow = arrow(ends = "both", length = unit(0.1, "cm")),
           color = rgb(0, 0, 1, alpha = 0.7), size = 0.3) +
  annotate("text", x = mu + sigma / 2, y = y2_peak * 0.6 - 0.015, label = "1.25", size = 3, color = rgb(0, 0, 1, alpha = 0.7)) +
  annotate("text",
           x = c(-0.18, mu - 0.52),
           y = c(y1_peak, y2_peak) + 0.015,
           label = c(0, 1.23),
           size = 3,
           hjust = 0,
           color = c(rgb(1, 0, 0, alpha = 0.7), rgb(0, 0, 1, alpha = 0.7))) +
  labs(x = NULL, y = "Density") +
  scale_color_manual(values = c("density_1" = rgb(1, 0, 0, alpha = 0.7), "density_2" = rgb(0, 0, 1, alpha = 0.7))) +
  scale_x_continuous(breaks = seq(-4, 6, by = 2), limits = c(-5, 6)) +
  scale_y_continuous(breaks = seq(0, 0.5, by = 0.2)) +
  theme_minimal(base_size = 8) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6)) -> p2

p2

#'# model roc
criteria <- criteria <- seq(-4, 4, length.out = 100000)
uv_far <- c(0, as.vector(pnorm(0 - criteria, 0, 1)), 1)
uv_hr <- c(0, as.vector(pnorm((mu - criteria)/sigma, 0, 1)), 1)
uv_roc <- as.data.frame(cbind(uv_hr, uv_far))
ev_far <- c(0, as.vector(pnorm(0  - criteria,  0, 1)), 1)
ev_hr <- c(0, as.vector(pnorm((dp - criteria), 0, 1)), 1)
ev_roc <- as.data.frame(cbind(uv_hr, uv_far))

p3 <- p1 + 
  geom_line(uv_roc, mapping = aes(x = uv_far, y = uv_hr), color = "#3b5f8f") +
  # geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  coord_equal() +
  theme_minimal(base_size = 8)
p3

g <- cowplot::plot_grid(p3, p2, labels = c("(a)", "(b)"), nrow = 1, label_x = -0.02, label_y = 1.01, label_size = 10)
ggsave("figure.jpg", g, width = 5.33, height = 2.33, units = "in", dpi = 500)