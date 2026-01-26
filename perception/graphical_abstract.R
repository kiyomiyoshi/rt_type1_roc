library(tidyverse)
library(data.table)
library(grid)
library(cowplot)
library(sjPlot)
library(cowplot)
library(patchwork)
source("uvsdt.R")

#'#
df <- fread("data_Mazor_2020.csv", header = T) # sub 46, ava 39
df <- subset(df, df$Condition == "Detection") # 200 trials / sub

i = 1
d <- subset(df, df$Subj_idx == i)
d$RT_bin <- ntile(d$RT_dec, 3)

nr_s1 <- c(sum(d$Stimulus == "0" & d$Response == "1" & d$RT_bin == 1, na.rm = TRUE),
           sum(d$Stimulus == "0" & d$Response == "1" & d$RT_bin == 2, na.rm = TRUE),
           sum(d$Stimulus == "0" & d$Response == "1" & d$RT_bin == 3, na.rm = TRUE),
           sum(d$Stimulus == "0" & d$Response == "0" & d$RT_bin == 3, na.rm = TRUE),
           sum(d$Stimulus == "0" & d$Response == "0" & d$RT_bin == 2, na.rm = TRUE),
           sum(d$Stimulus == "0" & d$Response == "0" & d$RT_bin == 1, na.rm = TRUE))

nr_s2 <- c(sum(d$Stimulus == "1" & d$Response == "1" & d$RT_bin == 1, na.rm = TRUE),
           sum(d$Stimulus == "1" & d$Response == "1" & d$RT_bin == 2, na.rm = TRUE),
           sum(d$Stimulus == "1" & d$Response == "1" & d$RT_bin == 3, na.rm = TRUE),
           sum(d$Stimulus == "1" & d$Response == "0" & d$RT_bin == 3, na.rm = TRUE),
           sum(d$Stimulus == "1" & d$Response == "0" & d$RT_bin == 2, na.rm = TRUE),
           sum(d$Stimulus == "1" & d$Response == "0" & d$RT_bin == 1, na.rm = TRUE))

uvsdt <- fit_uvsdt_mle(nr_s1, nr_s2, add_constant = TRUE)
uvsdt
roc <- cbind(c(0, cumsum(nr_s1) / sum(nr_s1)), c(0, cumsum(nr_s2) / sum(nr_s2)))
roc 


#'#
d <- na.omit(d)
d <- mutate(d, 
            Stimulus = ifelse(Stimulus == 0, "absent", "present"),
            Response = ifelse(Response == 0, "No", "Yes"))
d$Stimulus <- factor(d$Stimulus, levels = c("present", "absent"))
d$Response <- factor(d$Response, levels = c("Yes", "No"))

#'#
cuts <- c(0.79, 0.99)
labels_1 <- c("fast", "mid", "slow") 
x_pos_1 <- c(0.68, 0.89, 1.12)

color_1 <- "#E41A1C"
color_2 <- "#377EB8"

ggplot(d) +
  geom_histogram(
    aes(x = RT_dec, fill = Stimulus),
    binwidth = diff(range(d$RT_dec)) / 30,
    alpha = 1
  ) +
  facet_wrap(. ~ Response, nrow = 2) +
  geom_vline(xintercept = cuts, linetype = "dashed", color = "gray70", size = 0.55) +
  annotate("text", fontface = "bold", x = 0.68, y = 13, label = "fast") + 
  annotate("text", fontface = "bold", x = 0.89, y = 13, label = "mid") +
  annotate("text", fontface = "bold", x = 1.12, y = 13, label = "slow") + 
  scale_fill_manual(
    values = c(color_2, color_1),
    labels = c("target present", "target absent"),
    name = NULL,
    guide = guide_legend(
      keywidth = unit(0.8, "lines"),
      keyheight = unit(0.8, "lines"))) +
  theme_minimal(base_size = 9.5) +
  xlab("Empirical response time") +
  ylab("Frequency") +
  theme(
    axis.text.x  = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_text(color = "black", size = 11, face = "bold"),
    legend.title = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 9, face = "bold"),
    legend.position = c(0.85, 0.86),
    strip.text = element_text(size = 11, face = "bold")) + 
  theme(
    panel.border = element_rect(color = "darkgrey", fill = NA, size = 1),
    strip.text = element_text(size = 11, face = "bold"),
    strip.background = element_rect(fill = "grey90", color = NA)
  ) +
  ylim(0, 15) -> g1
g1

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

y1_peak <- dnorm(0, mean = 0, sd = 1)
y2_peak <- dnorm(mu, mean = mu, sd = sigma)

labels_2 <- c("fast", "mid", "slow", "mid", "fast") 
x_pos_2 <- c(-1.83, -0.81, 0.25, 1.28, 2.26)

labels_3 <- c("―No―", "―Yes―") 
x_pos_3 <- c(-0.60, 1.38)

ggplot(df, aes(x = x, y = density, color = distribution)) +
  geom_line(size = 0.7, alpha = 1) +
  geom_segment(x =  0.33, xend =  0.33, y = 0, yend = 0.47, linetype = "solid",  color = "black",  size = 0.6) +
  geom_segment(x = -1.27, xend = -1.27, y = 0, yend = 0.53, linetype = "dashed", color = "grey70", size = 0.5) +
  geom_segment(x = -0.36, xend = -0.36, y = 0, yend = 0.53, linetype = "dashed", color = "grey70", size = 0.5) +
  geom_segment(x =  0.84, xend =  0.84, y = 0, yend = 0.53, linetype = "dashed", color = "grey70", size = 0.5) +
  geom_segment(x =  1.73, xend =  1.73, y = 0, yend = 0.53, linetype = "dashed", color = "grey70", size = 0.5) +
  annotate(
    "text",
    fontface = "bold",
    x = x_pos_2,
    y = 0.51,
    label = labels_2) +
  annotate(
    "text",
    fontface = "bold",
    x = x_pos_3,
    y = 0.45,
    size = 4,
    label = labels_3) +
  scale_color_manual(values = c("density_1" = color_1, "density_2" = color_2)) +
  scale_x_continuous(breaks = seq(-4, 6, by = 2), limits = c(-5, 6)) +
  scale_y_continuous(limits = c(0, 0.52)) +
  labs(x = "Model signal strength", y = "Density") +
  theme_minimal(base_size = 9.5) +
  theme(
    legend.position = "none",
    axis.text.x  = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "darkgrey", fill = NA, size = 1),
    axis.title = element_text(color = "black", size = 11, face = "bold")) -> g2
g2

g <- cowplot::plot_grid(g1, g2, nrow = 2)
ggsave("graphical_abstract.png", g, width = 4, height = 4,    units = "in", dpi = 300)