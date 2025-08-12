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

roc_c <- c()
roc_r <- c()

for (j in 1:length(script)) {
  source(script[j]) 
  roc_conf <-      as.data.frame(roc_conf)
  roc_conf$data <- dataset[j]
  roc_rt <-        as.data.frame(roc_rt)
  roc_rt$data <-   dataset[j]
  roc_c <- rbind(roc_c, roc_conf)
  roc_r <- rbind(roc_r, roc_rt)
}  

roc_c$var <- "Conf"
roc_r$var <- "RT"
roc <- rbind(roc_c, roc_r)
colnames(roc) <- c("far", "hr", "id", "data", "var")
roc[, 1:3] <- lapply(roc[, 1:3], as.numeric)

p2 <- ggplot(roc, aes(x = far, y = hr, color = var)) +
  geom_smooth(method = "loess", se = FALSE, span = 0.7) +
  labs(x = "far", y = "hr", color = "Variable") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
  coord_equal() +
  theme_minimal(base_size = 8) + ggtitle("Memory")

p2

p <- cowplot::plot_grid(p1, p2)
ggsave("figure_s2.jpg", p, width = 4, height = 2, units = "in", dpi = 500)