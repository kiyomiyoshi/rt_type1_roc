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
source("meta_sdt_bal.R")

meta_c <- c()
meta_r <- c()

for (j in 1:length(script)) {
  source(script[j]) 
  meta_conf <-      as.data.frame(meta_conf)
  meta_conf$data <- dataset[j]
  meta_rt <-        as.data.frame(meta_rt)
  meta_rt$data <-   dataset[j]
  meta_c <- rbind(meta_c, meta_conf)
  meta_r <- rbind(meta_r, meta_rt)
}  

meta_c <- meta_c[, 1:6]
meta_r <- meta_r[, 1:6]
meta_c$var <- "Conf"
meta_r$var <- "RT"
meta_sdt <- rbind(meta_c, meta_r)
colnames(meta_sdt) <- c("dp", "mdpb", "Yes", "No", "mratio", "mdiff", "var")
meta_sdt[, 1:6] <- lapply(meta_sdt[, 1:6], as.numeric)
meta_long <- meta_sdt %>%
  pivot_longer(cols = c(Yes, No), names_to = "measure", values_to = "value")
meta_long$measure <- factor(meta_long$measure, levels = c("Yes", "No"))
meta_long$var <- factor(meta_long$var, levels = c("RT", "Conf"))

comparisons_list <- combn(levels(meta_long$var), 2, simplify = FALSE)
default_colors <- hue_pal()(3) 
p2 <- ggplot(meta_long, aes(x = var, y = value, fill = var)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.02) +
# stat_compare_means(
#   comparisons = comparisons_list,
#   method = "t.test", 
#   label = "p.signif",
#   size = 3,
#   label.y = 7,
#   step.increase = 0,
#   symnum.args = list(
#     cutpoints = c(0, 0.001, 0.01, 0.05, 1),
#     symbols = c("< .001", "< .01", "< .05", "ns")
#   )
# ) +
  facet_wrap(~measure, ncol = 2) +
  coord_cartesian(ylim = c(-2.5, 10)) +
  scale_fill_manual(values = default_colors[1:2]) + 
  theme_bw(base_size = 8) +
  theme(legend.position = "none") +
  labs(x = NULL, y = "meta-d'", title = "Recognition memory")
p2

p <- cowplot::plot_grid(p1, p2)
ggsave("figure_s8.jpg", p, width = 6, height = 2, units = "in", dpi = 500)