library(tidyverse)

sigma <- 1.5
cri1 <- seq(-4, 4, length.out = 100000)
cutoffs <- seq(0, 2, 1)
mus <- c(3, 2, 1)
(da <- mus / sqrt((1 + 1.5^2) / 2))

roc_all <- data.frame()
points_all <- data.frame()

for (mu in mus) {
  uv_far <- c(0, as.vector(pnorm(0 - cri1, 0, 1)), 1)
  uv_hr <- c(0, as.vector(pnorm((mu - cri1)/sigma, 0, 1)), 1)
  temp_roc <- data.frame(phit = uv_hr, pfa = uv_far, mu = as.factor(mu))
  roc_all <- rbind(roc_all, temp_roc)
  
  uv_far_cut <- as.vector(pnorm(0 - cutoffs, 0, 1))
  uv_hr_cut <- as.vector(pnorm((mu - cutoffs)/sigma, 0, 1))
  temp_points <- data.frame(phit = uv_hr_cut, pfa = uv_far_cut, mu = as.factor(mu))
  points_all <- rbind(points_all, temp_points)
}

points_all <- points_all %>%
  mutate(dp = qnorm(phit) - qnorm(pfa))
points_all

label = round(points_all$dp, 2)

ev_far1 <- c(0, as.vector(pnorm(0  - cri1,  0, 1)), 1)
ev_hr1 <- c(0, as.vector(pnorm((1.33 - cri1), 0, 1)), 1)
ev_roc1 <- data.frame(phit = ev_hr1, pfa = ev_far1)

ev_far2 <- c(0, as.vector(pnorm(0  - cri1,  0, 1)), 1)
ev_hr2 <-  c(0, as.vector(pnorm((0.67 - cri1), 0, 1)), 1)
ev_roc2 <- data.frame(phit = ev_hr2, pfa = ev_far2)

p1 <- ggplot(roc_all, aes(x = pfa, y = phit, color = mu)) +
  geom_line(ev_roc1, mapping = aes(x = pfa, y = phit), color = "gray", linetype = "dashed") +
  geom_line(ev_roc2, mapping = aes(x = pfa, y = phit), color = "gray", linetype = "dashed") +
  geom_line(size = 0.8) +
  geom_point(data = points_all, aes(x = pfa, y = phit, color = mu), size = 1.8) +
  geom_text(data = points_all,
            aes(x = pfa, y = phit, label = label, color = mu),
            hjust = -0.32,
            vjust =  1.05,
            size = 3.5,
            show.legend = FALSE) +
  coord_equal() +
  theme_minimal(base_size = 10) +
  labs(x = "FA rate",
       y = "Hit rate",
       color = NULL) +
  scale_color_manual(
    values = c("1" = "#364B6B", "2" = "#3B6A55", "3" = "#8C3A3C"), 
    labels = c(
      expression(d[a] == 2.35 * "," ~ mu == 3.0 * "," ~ sigma == 1.5),
      expression(d[a] == 1.57 * "," ~ mu == 2.0 * "," ~ sigma == 1.5),
      expression(d[a] == 0.78 * "," ~ mu == 1.0 * "," ~ sigma == 1.5)
    )
  ) +
  theme(
    legend.position = c(0.68, 0.2),
    legend.background = element_rect(fill = "white", color = NA),
    legend.text = element_text(size = 9) 
  ) 
p1

ggsave("figure_2.jpg", p1, width = 4, height = 4, units = "in", dpi = 500)