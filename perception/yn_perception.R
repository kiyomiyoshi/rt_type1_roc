library(data.table)
library(tidyverse)
library(grid)
library(cowplot)
library(gridExtra)
library(sjPlot)
library(ggpubr)
library(scales)

script <- c("Dijkstra_2024_Expt1_1.R",
            "Dijkstra_2024_Expt1_2.R",
            "Dijkstra_2024_Expt1_3.R",
            "Dijkstra_2024_Expt2_1.R",
            "Dijkstra_2024_Expt2_2.R",
            "Dijkstra_2024_Expt2_3.R",
            "Mazor_2020_Detection.R",
            "Mazor_2021_Expt7.R",
            "Mazor_2025_Expt2.R",
            "Sherman_2016_JOCN_1.R",
            "Sherman_2016_JOCN_2.R")

dataset <- c("Dijkstra_2024_Expt1_1",
             "Dijkstra_2024_Expt1_2",
             "Dijkstra_2024_Expt1_3",
             "Dijkstra_2024_Expt2_1",
             "Dijkstra_2024_Expt2_2",
             "Dijkstra_2024_Expt2_3",
             "Mazor_2020_Detection",
             "Mazor_2021_Expt7",
             "Mazor_2025_Expt2",
             "Sherman_2016_JOCN_1",
             "Sherman_2016_JOCN_2")

source("uvsdt.R")

sdt_conf <- c()
sdt_rt <- c()
wide <- c()

for (j in 1:length(script)) {
  source(script[j]) 
  uvsdt_conf$data <- dataset[j]
  uvsdt_rt$data <- dataset[j]
  df_wide$data <- dataset[j]
  sdt_conf <- rbind(sdt_conf, uvsdt_conf)
  sdt_rt <- rbind(sdt_rt, uvsdt_rt)
  wide <- rbind(wide, df_wide)
}  

sdt_conf$var <- "Confidence"
sdt_rt$var <- "RT"
colnames(sdt_conf) <- c("mu", "sigma", "da", "logL", "id", "dp", "dataset", "var")
colnames(sdt_rt) <-   c("mu", "sigma", "da", "logL", "id", "dp", "dataset", "var")
sdt <- rbind(sdt_conf, sdt_rt)
sdt$var <- factor(sdt$var, levels = c("RT", "Confidence"))
sdt <- subset(sdt, sdt$dp > 0)
wide <- subset(wide, wide$dp...5 > 0)

# density plots
# sigma
mean_sigma <- sdt %>%
  group_by(var) %>%
  summarise(sigma_mean = mean(sigma, na.rm = TRUE), .groups = "drop")

ggplot(sdt, aes(x = sigma, fill = var, color = var)) +
  geom_density(alpha = 0.4, size = 0.4) +
  geom_vline(
    data = mean_sigma,
    aes(xintercept = sigma_mean, color = var),
    linetype = "dashed", size = 0.4, show.legend = FALSE) +
  labs(x = "σ",
       y = "Density",
       fill = NULL,
       color = NULL) +
  theme_minimal() +
  coord_cartesian(xlim = c(-0.5, 5)) +
  theme(
    legend.position = c(0.5, 1.1),
    legend.justification = c(0, 1),
    legend.text = element_text(size = 6),
    legend.key.size = unit(0.9, "lines"),
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    axis.text.x  = element_text(size = 6),
    axis.text.y  = element_text(size = 6)) +
  scale_fill_manual(values = c("RT" = "#F8766D", "Confidence" = "#00BA38")) +
  scale_color_manual(values = c("RT" = "#F8766D", "Confidence" = "#00BA38")) -> p1
p1

# mu
mean_mu <- sdt %>%
  group_by(var) %>%
  summarise(mu_mean = mean(mu, na.rm = TRUE), .groups = "drop")

ggplot(sdt, aes(x = mu, fill = var, color = var)) +
  geom_density(alpha = 0.4, size = 0.4) +
  geom_vline(
    data = mean_mu,
    aes(xintercept = mu_mean, color = var),
    linetype = "dashed", size = 0.4) +
  labs(x = "μ",
       y = "Density",
       fill = NULL,
       color = NULL) +
  theme_minimal() +
  coord_cartesian(xlim = c(-0.5, 5)) +
  scale_fill_manual(values = c("RT" = "#F8766D", "Confidence" = "#00BA38")) +
  scale_color_manual(values = c("RT" = "#F8766D", "Confidence" = "#00BA38")) +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    axis.text.x  = element_text(size = 6),
    axis.text.y  = element_text(size = 6)) -> p2
p2

# da
wide %>% filter(!is.infinite(dp...5)) -> wide
long <- wide %>%
  select(da_conf, da_rt,  dp...5) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")
long$variable <- factor(long$variable, levels = c("da_rt", "da_conf", "dp...5"))

means <- long %>%
  group_by(variable) %>%
  summarise(mean_value = mean(value, na.rm = TRUE))
means_solid <- subset(means, variable == levels(long$variable)[1])
means_dashed <- subset(means, variable != levels(long$variable)[1])

ggplot(long, aes(x = value, fill = variable, color = variable)) +
  geom_density(alpha = 0.4, size = 0.4) +
  geom_vline(data = means_solid,
             aes(xintercept = mean_value, color = variable),
             linetype = "dashed", size = 0.4, show.legend = FALSE) +
  geom_vline(data = means_dashed,
             aes(xintercept = mean_value, color = variable),
             linetype = "dashed", size = 0.4, show.legend = FALSE) +
  labs(x = "Value",
       y = "Density",
       fill = NULL,
       color = NULL) + 
  coord_cartesian(xlim = c(-0.5, 5)) +
  scale_fill_manual(
    values = c("#F8766D", "#00BA38", "#619CFF"),
    labels = c(expression(d[a] * "RT"),
               expression(d[a] * "Conf"),
               "d'")
  ) +
  scale_color_manual(
    values = c("#F8766D", "#00BA38", "#619CFF"),
    labels = c(expression(d[a] * "RT"),
               expression(d[a] * "Conf"),
               "d'")
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.65, 1.1),
    legend.justification = c(0, 1),
    legend.text = element_text(size = 6),
    legend.key.size = unit(0.9, "lines"),
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    axis.text.x  = element_text(size = 6),
    axis.text.y  = element_text(size = 6)
  ) -> p3

p3

p1 <- p1 + theme(plot.margin = margin(5, 5, 5, 5))
p2 <- p2 + theme(plot.margin = margin(5, 5, 5, 5))
p3 <- p3 + theme(plot.margin = margin(5, 5, 5, 5))

figure_3 <- cowplot::plot_grid(
  p1, p2, p3,
  nrow = 1,
  labels = c("(a)", "(b)", "(c)"),
  label_size = 10,
  label_fontface = "plain",
  label_x = c(-0.03, -0.03, -0.03),
  label_y = c(0.98, 0.98, 0.98)
)

sjPlot::save_plot("figure_3.png", figure_3, width = 16, height = 4, dpi = 300)

# stats
# sigma
t.test(log(wide$sigma_rt), mu = 0)
t.test(wide$sigma_rt, mu = 1)
sd(wide$sigma_rt)/sqrt(length(wide$sigma_rt))

t.test(log(wide$sigma_conf), mu = 0)
t.test(wide$sigma_conf, mu = 1)
sd(wide$sigma_conf)/sqrt(length(wide$sigma_conf))

t.test(wide$sigma_conf, wide$sigma_rt, paired = T)

# mu
t.test(wide$mu_conf, wide$mu_rt, paired = T)
mean(wide$mu_conf)
sd(wide$mu_conf)/sqrt(length(wide$mu_conf))
mean(wide$mu_rt)
sd(wide$mu_rt)/sqrt(length(wide$mu_rt))

# da
t.test(wide$da_conf, wide$da_rt, paired = T)
t.test(wide$da_conf, wide$dp...5, paired = T)
t.test(wide$da_rt, wide$dp...5, paired = T)

mean(wide$da_conf)
sd(wide$da_conf)/sqrt(length(wide$da_conf))
mean(wide$da_rt)
sd(wide$da_rt)/sqrt(length(wide$da_rt))
mean(wide$dp...5)
sd(wide$dp...5)/sqrt(length(wide$dp...5))

# correlation plots
cowplot::plot_grid(sigma_Dijkstra_2024_Expt1_1, sigma_Dijkstra_2024_Expt1_2, sigma_Dijkstra_2024_Expt1_3, sigma_Dijkstra_2024_Expt2_1, 
                   sigma_Dijkstra_2024_Expt2_2, sigma_Dijkstra_2024_Expt2_3, sigma_Mazor_2020_Detection,  sigma_Mazor_2021_Expt7,
                   sigma_Mazor_2025_Expt2, sigma_Sherman_2016_JOCN_1, sigma_Sherman_2016_JOCN_2, ncol = 4) -> sigma

cowplot::plot_grid(mu_Dijkstra_2024_Expt1_1, mu_Dijkstra_2024_Expt1_2, mu_Dijkstra_2024_Expt1_3, mu_Dijkstra_2024_Expt2_1, 
                   mu_Dijkstra_2024_Expt2_2, mu_Dijkstra_2024_Expt2_3, mu_Mazor_2020_Detection,  mu_Mazor_2021_Expt7,
                   mu_Mazor_2025_Expt2, mu_Sherman_2016_JOCN_1, mu_Sherman_2016_JOCN_2, ncol = 4) -> mu

cowplot::plot_grid(da_Dijkstra_2024_Expt1_1, da_Dijkstra_2024_Expt1_2, da_Dijkstra_2024_Expt1_3, da_Dijkstra_2024_Expt2_1, 
                   da_Dijkstra_2024_Expt2_2, da_Dijkstra_2024_Expt2_3, da_Mazor_2020_Detection,  da_Mazor_2021_Expt7,
                   da_Mazor_2025_Expt2, da_Sherman_2016_JOCN_1, da_Sherman_2016_JOCN_2, ncol = 4) -> da

sigma <- ggdraw() +
  draw_plot(sigma, x = 0.05, y = 0.05, width = 0.96, height = 0.96) +
  draw_label("RT-based σ", x = 0.03, y = 0.5, angle = 90, vjust = 0.5, size = 10) +
  draw_label("Confidence-based σ", x = 0.5, y = 0.03, vjust = 0.5, size = 10)

mu <- ggdraw() +
  draw_plot(mu, x = 0.05, y = 0.05, width = 0.96, height = 0.96) +
  draw_label("RT-based μ", x = 0.03, y = 0.5, angle = 90, vjust = 0.5, size = 10) +
  draw_label("Confidence-based μ", x = 0.5, y = 0.03, vjust = 0.5, size = 10)

# da
da <- ggdraw() +
  draw_plot(da, x = 0.05, y = 0.05, width = 0.96, height = 0.96) +
  draw_label(expression("RT-based d"[a]), x = 0.03, y = 0.5, angle = 90, vjust = 0.5, size = 10) +
  draw_label(expression("Confidence-based d"[a]), x = 0.5, y = 0.03, vjust = 0.5, size = 10)

# save figure
sjPlot::save_plot("figure_4.png", sigma, width = 16, height = 13.6, dpi = 500)
sjPlot::save_plot("figure_5.png", mu,    width = 16, height = 13.6, dpi = 500)
sjPlot::save_plot("figure_6.png", da,    width = 16, height = 13.6, dpi = 500)

#
sdt$dataset <- as.factor(sdt$dataset)
sdt$var <- as.factor(sdt$var)

#
comparisons_list <- combn(levels(sdt$var), 2, simplify = FALSE)
default_colors <- hue_pal()(3) 

#
sdt$dataset <- factor(sdt$dataset, 
                      levels = c("Dijkstra_2024_Expt1_1", "Dijkstra_2024_Expt1_2", "Dijkstra_2024_Expt1_3", 
                                 "Dijkstra_2024_Expt2_1", "Dijkstra_2024_Expt2_2", "Dijkstra_2024_Expt2_3",
                                 "Mazor_2020_Detection",  "Mazor_2021_Expt7",      "Mazor_2025_Expt2",
                                 "Sherman_2016_JOCN_1",   "Sherman_2016_JOCN_2"))
ggplot(sdt, aes(x = var, y = sigma, fill = var)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.1) +
  stat_compare_means(
    comparisons = comparisons_list,
    method = "t.test", 
    label = "p.signif",
    size = 3,
    symnum.args = list(
      cutpoints = c(0, 0.001, 0.01, 0.05, 1),
      symbols = c("< .001", "< .01", "< .05", "ns")
    )
  ) +
  facet_wrap(~dataset, ncol = 4) +
  coord_cartesian(ylim = c(0, 4.5)) +
  scale_fill_manual(values = default_colors[1:2]) + 
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = NULL,
       y = "Estimate of σ") -> g1

#
ggplot(sdt, aes(x = var, y = mu, fill = var)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.1) +
  stat_compare_means(
    comparisons = comparisons_list,
    method = "t.test", 
    label = "p.signif",
    size = 3,
    symnum.args = list(
      cutpoints = c(0, 0.001, 0.01, 0.05, 1),
      symbols = c("< .001", "< .01", "< .05", "ns")
    )
  ) +
  facet_wrap(~dataset, ncol = 4) +
  coord_cartesian(ylim = c(0, 6.5)) +
  scale_fill_manual(values = default_colors[1:2]) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = NULL,
       y = "Estimate of μ") -> g2

#
dat1 <- select(sdt, da, dataset, var)
colnames(dat1) <- c("value", "dataset", "index")
dat2 <- select(sdt, dp, dataset)
dat2$index <- "dp"
colnames(dat2) <- c("value", "dataset", "index")
dat3 <- rbind(dat1, dat2)
dat3$dataset <- factor(dat3$dataset, 
                       levels = c("Dijkstra_2024_Expt1_1", "Dijkstra_2024_Expt1_2", "Dijkstra_2024_Expt1_3", 
                                  "Dijkstra_2024_Expt2_1", "Dijkstra_2024_Expt2_2", "Dijkstra_2024_Expt2_3",
                                  "Mazor_2020_Detection",  "Mazor_2021_Expt7",      "Mazor_2025_Expt2",
                                  "Sherman_2016_JOCN_1",   "Sherman_2016_JOCN_2"))
dat3 <- dat3 %>%
  mutate(index = factor(index),
         value = as.numeric(value))

comparisons_list <- combn(levels(dat3$index), 2, simplify = FALSE)

ggplot(dat3, aes(x = index, y = value, fill = index)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.1) +
  stat_compare_means(
    comparisons = comparisons_list,
    method = "t.test",
    label = "p.signif",
    size = 3,
    step.increase = 0.17,
    symnum.args = list(
      cutpoints = c(0, 0.001, 0.01, 0.05, 1),
      symbols = c("< .001", "< .01", "< .05", "ns"))) +
  facet_wrap(~dataset, ncol = 4) + 
  coord_cartesian(ylim = c(0, 7)) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = NULL, y = "Estimate") +
  scale_x_discrete(labels = c(
    "RT"         = expression(paste(d[a], "\nRT")),
    "Confidence" = expression(paste(d[a], "\nConf")),
    "dp"         = expression("d'")
  )) -> g3

ggsave("figure_a1.jpg", g1, width = 6.5, height = 5, units = "in", dpi = 500)
ggsave("figure_a2.jpg", g2, width = 6.5, height = 5, units = "in", dpi = 500)
ggsave("figure_a3.jpg", g3, width = 6.5, height = 5, units = "in", dpi = 500)