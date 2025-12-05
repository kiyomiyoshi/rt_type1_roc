library(tidyverse)
library(data.table)
library(grid)
library(cowplot)
library(sjPlot)

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
            Response = ifelse(Response == 0, "no", "yes"))
d$Response <- factor(d$Response, levels = c("yes", "no"))

d %>%
  group_by(Stimulus, Response, RT_bin) %>%
summarise(n = n())

d <- d %>%
  mutate(grp = paste0(Response, ", b", RT_bin))

desired_order <- c("yes, b1","yes, b2","yes, b3",
                   "no, b3", "no, b2","no, b1")

d$grp <- factor(d$grp, levels = desired_order)

ggplot(d, aes(x = grp, fill = Stimulus)) +
  geom_bar(position = "dodge", alpha = 07) +
  ylab("Frequency") +
  xlab(NULL) +
  labs(fill = NULL) +
  scale_fill_manual(values = c("red", "blue")) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8.5, color = "black"),
    axis.text.y = element_text(size = 8.5, color = "black"),
    legend.position = "none") -> g1
g1

#'#
cuts <- quantile(d$RT_dec, probs = seq(0, 1, length.out = 4))
cuts <- cuts[2:3]
cuts

labels_1 <- lapply(1:2, function(i) bquote(t[.(i)]))
labels_2 <- c("b1", "b2", "b3") 
x_pos <- c(0.7, 0.9, 1.1)

ggplot(d) +
  geom_histogram(
    aes(x = RT_dec, fill = Stimulus),
    binwidth = diff(range(d$RT_dec)) / 30,
    alpha = 0.7
  ) +
  geom_vline(xintercept = cuts, linetype = "dashed", color = "black", size = 0.55) +
  annotate(
    "text",
    x = cuts,
    y = Inf,
    label = labels_1,
    hjust = 1.3,
    vjust = 1.1,
    parse = TRUE) +
  annotate(
    "text",
    x = x_pos,
    y = 21,
    label = labels_2) +
  scale_fill_manual(values = c("red", "blue")) +
  theme_minimal(base_size = 10) +
  xlab("RT") +
  ylab("Frequency") +
  theme(
    axis.text.x = element_text(size = 8.5, color = "black"),
    axis.text.y = element_text(size = 8.5, color = "black"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    legend.title = element_blank(),
    legend.position = c(1, 1),
    legend.justification = c("right", "top")) + 
  ylim(0, 25) -> g2

g2

#'#
d <- d %>%
  mutate(grp = paste0(Response, ", b", RT_bin),
         grp = factor(grp, levels = desired_order))

cum_data <- d %>%
  group_by(Stimulus, grp) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Stimulus) %>%
  arrange(grp) %>%
  mutate(cum_ratio = cumsum(n) / sum(n))

ggplot(cum_data, aes(x = grp, y = cum_ratio, fill = Stimulus)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = c("red", "blue")) +
  theme_minimal(base_size = 10) +
  xlab(NULL) +
  ylab("Cumulative ratio") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 8.5),
    axis.text.y = element_text(color = "black", size = 8.5),
    legend.position = "none") +
  ylab("Cumulative proportion") -> g3

g <- cowplot::plot_grid(g2, g1, g3, labels = c("(a)", "(b)", "(c)"), nrow = 1, label_x = -0.02, label_y = 1.01, label_size = 10)
ggsave("figure_s1.jpg", g, width = 8, height = 7/3,    units = "in", dpi = 500)