library(tidyverse)
library(data.table)
library(grid)
library(cowplot)
library(sjPlot)

#'# perception
mur <- 1.63 
muc <- 1.74
sigmar <-  1.31
sigmac <- 1.39
dp <- 1.77637

criteria <- criteria <- seq(-4, 4, length.out = 100000)
far <- c(0, as.vector(pnorm(0 - criteria, 0, 1)), 1)
hr  <- c(0, as.vector(pnorm((mur - criteria)/sigmar, 0, 1)), 1)
uv_rocr <- as.data.frame(cbind(hr, far))
uv_rocr$index <- "RT"

far <- c(0, as.vector(pnorm(0 - criteria, 0, 1)), 1)
hr  <- c(0, as.vector(pnorm((muc - criteria)/sigmac, 0, 1)), 1)
uv_rocc <- as.data.frame(cbind(hr, far))
uv_rocc$index <- "Conf"

far <- c(0, as.vector(pnorm(0  - criteria,  0, 1)), 1)
hr <- c(0, as.vector(pnorm((dp - criteria), 0, 1)), 1)
ev_roc <- as.data.frame(cbind(hr, far))
ev_roc$index <- "EV"
roc <- rbind(uv_rocr, uv_rocc, ev_roc)
roc$index <- factor(roc$index, levels = c("RT", "Conf", "EV"))

p1 <- ggplot(roc) + 
  geom_line(aes(x = far, y = hr, color = index)) +
  coord_equal() +
  theme_minimal(base_size = 8) + 
  xlab("FA rate") + ylab("Hit rate") + ggtitle("Visual detection") +
  theme(
    legend.title = element_blank(),                 
    legend.position = c(0.95, 0.05),
    legend.justification = c("right", "bottom") )
p1

#'# memory
mur <- 1.64 
muc <- 2.28
sigmar <-  1.15
sigmac <-  1.46
dp <- 1.8086

criteria <- criteria <- seq(-4, 4, length.out = 100000)
far <- c(0, as.vector(pnorm(0 - criteria, 0, 1)), 1)
hr  <- c(0, as.vector(pnorm((mur - criteria)/sigmar, 0, 1)), 1)
uv_rocr <- as.data.frame(cbind(hr, far))
uv_rocr$index <- "RT"

far <- c(0, as.vector(pnorm(0 - criteria, 0, 1)), 1)
hr  <- c(0, as.vector(pnorm((muc - criteria)/sigmac, 0, 1)), 1)
uv_rocc <- as.data.frame(cbind(hr, far))
uv_rocc$index <- "Conf"

far <- c(0, as.vector(pnorm(0  - criteria,  0, 1)), 1)
hr <- c(0, as.vector(pnorm((dp - criteria), 0, 1)), 1)
ev_roc <- as.data.frame(cbind(hr, far))
ev_roc$index <- "EV"
roc <- rbind(uv_rocr, uv_rocc, ev_roc)
roc$index <- factor(roc$index, levels = c("Conf", "RT", "EV"))

p2 <- ggplot(roc) + 
  geom_line(aes(x = far, y = hr, color = index)) +
  coord_equal() +
  theme_minimal(base_size = 8) + 
  theme(legend.position = "none") +
  xlab("FA rate") + ylab("Hit rate") + ggtitle("Recognition memory")
p2

p <- cowplot::plot_grid(p1, p2)
ggsave("figure_s7.jpg", p, width = 6, height = 2, units = "in", dpi = 500)