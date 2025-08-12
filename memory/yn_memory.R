library(data.table)
library(tidyverse)
library(grid)
library(cowplot)
library(gridExtra)
library(sjPlot)
library(ggpubr)
library(scales)

script <- c("Kantner_2014_E2_Face.R",
            "Kantner_2014_E2_Word.R",
            "Sadeghi_2017_memory_control.R",
            "Sadeghi_2017_memory_patient.R",
            "Schmidt_2019_memory_MM_pre.R",
            "Schmidt_2019_memory_MM_post.R",
            "Schmidt_2019_memory_SoB_pre.R",
            "Schmidt_2019_memory_SoB_post.R",
            "Siedlecka_2019_Exp2_0.R",
            "Weidemann_2016.R")

dataset <- c("Kantner_2014_E2_Face",
             "Kantner_2014_E2_Word",
             "Sadeghi_2017_control",
             "Sadeghi_2017_patient",
             "Schmidt_2019_MM_pre",
             "Schmidt_2019_MM_post",
             "Schmidt_2019_SoB_pre",
             "Schmidt_2019_SoB_post",
             "Siedlecka_2019_Exp2_0",
             "Weidemann_2016")

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

figure_s1 <- cowplot::plot_grid(
  p1, p2, p3,
  nrow = 1,
  labels = c("(a)", "(b)", "(c)"),
  label_size = 10,
  label_fontface = "plain",
  label_x = c(-0.03, -0.03, -0.03),
  label_y = c(0.98, 0.98, 0.98)
)

sjPlot::save_plot("figure_s1.jpg", figure_s1, width = 16, height = 4, dpi = 300)

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
cowplot::plot_grid(sigma_Weidemann_2016, sigma_Siedlecka_2019_Exp2_0, 
                   sigma_Kantner_2014_E2_Face, sigma_Kantner_2014_E2_Word, 
                   sigma_Schmidt_2019_MM_pre,  sigma_Schmidt_2019_MM_post, 
                   sigma_Schmidt_2019_SoB_pre, sigma_Schmidt_2019_SoB_post,
                   sigma_Sadeghi_2017_control, sigma_Sadeghi_2017_patient, ncol = 4) -> sigma

cowplot::plot_grid(mu_Weidemann_2016, mu_Siedlecka_2019_Exp2_0, 
                   mu_Kantner_2014_E2_Face, mu_Kantner_2014_E2_Word,
                   mu_Schmidt_2019_MM_pre,  mu_Schmidt_2019_MM_post, 
                   mu_Schmidt_2019_SoB_pre, mu_Schmidt_2019_SoB_post,
                   mu_Sadeghi_2017_control, mu_Sadeghi_2017_patient, ncol = 4) -> mu

cowplot::plot_grid(da_Weidemann_2016,  da_Siedlecka_2019_Exp2_0, 
                   da_Kantner_2014_E2_Face, da_Kantner_2014_E2_Word,
                   da_Schmidt_2019_MM_pre,  da_Schmidt_2019_MM_post, 
                   da_Schmidt_2019_SoB_pre, da_Schmidt_2019_SoB_post,
                   da_Sadeghi_2017_control, da_Sadeghi_2017_patient) -> da

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

sjPlot::save_plot("figure_s5.png",  sigma, width = 16, height = 13.6, dpi = 300)
sjPlot::save_plot("figure_s6.png",  mu,    width = 16, height = 13.6, dpi = 300)
sjPlot::save_plot("figure_s7.png", da,    width = 16, height = 13.6, dpi = 300)

#
sdt$dataset <- as.factor(sdt$dataset)
sdt$var <- as.factor(sdt$var)
sdt <- subset(sdt, sdt$sigma < 5) ####

#
comparisons_list <- combn(levels(sdt$var), 2, simplify = FALSE)
default_colors <- hue_pal()(3) 

#
sdt$dataset <- factor(sdt$dataset, levels = c("Weidemann_2016", "Siedlecka_2019_Exp2_0", 
                                              "Kantner_2014_E2_Face", "Kantner_2014_E2_Word", 
                                              "Schmidt_2019_MM_pre",  "Schmidt_2019_MM_post", 
                                              "Schmidt_2019_SoB_pre", "Schmidt_2019_SoB_post",
                                              "Sadeghi_2017_control", "Sadeghi_2017_patient"))
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
  coord_cartesian(ylim = c(0, 5)) +
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
  coord_cartesian(ylim = c(0, 7.5)) +
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
dat3$dataset <- factor(dat3$dataset, levels = c("Weidemann_2016", "Siedlecka_2019_Exp2_0", 
                                                "Kantner_2014_E2_Face", "Kantner_2014_E2_Word", 
                                                "Schmidt_2019_MM_pre",  "Schmidt_2019_MM_post", 
                                                "Schmidt_2019_SoB_pre", "Schmidt_2019_SoB_post",
                                                "Sadeghi_2017_control", "Sadeghi_2017_patient"))
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
  coord_cartesian(ylim = c(0, 6.5)) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = NULL, y = "Estimate") +
  scale_x_discrete(labels = c(
    "RT"         = expression(paste(d[a], "\nRT")),
    "Confidence" = expression(paste(d[a], "\nConf")),
    "dp"         = expression("d'")
  )) -> g3

ggsave("figure_s2.jpg", g1, width = 6.5, height = 5, units = "in", dpi = 500)
ggsave("figure_s3.jpg", g2, width = 6.5, height = 5, units = "in", dpi = 500)
ggsave("figure_s4.jpg", g3, width = 6.5, height = 5, units = "in", dpi = 500)