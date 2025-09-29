library(data.table)
library(tidyverse)
library(grid)
library(cowplot)
library(gridExtra)
library(sjPlot)
library(ggpubr)
library(scales)

script <- c("Dijkstra_2024_expt1_1.R",
            "Dijkstra_2024_expt1_2.R",
            "Dijkstra_2024_expt1_3.R",
            "Mazor_2020_Detection.R",
            "Dijkstra_2024_expt2_1.R",
            "Dijkstra_2024_expt2_2.R",
            "Dijkstra_2024_expt2_3.R")

dataset <- c("Dijkstra_2024_expt1_1",
             "Dijkstra_2024_expt1_2",
             "Dijkstra_2024_expt1_3",
             "Mazor_2020_Detection",
             "Dijkstra_2024_expt2_1",
             "Dijkstra_2024_expt2_2",
             "Dijkstra_2024_expt2_3")

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

p1 <- ggplot(roc, aes(x = far, y = hr, color = var)) +
  geom_smooth(method = "loess", se = FALSE, span = 0.7) +
  labs(x = "far", y = "hr", color = "Variable") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
  coord_equal() +
  theme_minimal(base_size = 8) + ggtitle("Perception")

p1