library(tidyverse)
library(data.table)
library(grid)

df <- fread("data_Mazor_2021_Expt7.csv", header = T) # sub 136, ava 124
df %>%
  group_by(Subj_idx) %>%
  summarise(n = n()) %>%
  print(n = 99999)

#'# confidence
uvsdt_conf <- c()
meta_conf <- c()
dp <- c()
roc_conf <- c()
for (i in unique(df$Subj_idx)) {
  
  d <- subset(df, df$Subj_idx == i)
  d$Conf_bin <- ntile(d$Confidence, 3)
  
  nr_s1 <- c(sum(d$Stimulus == "2" & d$Response == "1" & d$Conf_bin == 3, na.rm = TRUE),
             sum(d$Stimulus == "2" & d$Response == "1" & d$Conf_bin == 2, na.rm = TRUE),
             sum(d$Stimulus == "2" & d$Response == "1" & d$Conf_bin == 1, na.rm = TRUE),
             sum(d$Stimulus == "2" & d$Response == "2" & d$Conf_bin == 1, na.rm = TRUE),
             sum(d$Stimulus == "2" & d$Response == "2" & d$Conf_bin == 2, na.rm = TRUE),
             sum(d$Stimulus == "2" & d$Response == "2" & d$Conf_bin == 3, na.rm = TRUE))
  
  nr_s2 <- c(sum(d$Stimulus == "1" & d$Response == "1" & d$Conf_bin == 3, na.rm = TRUE),
             sum(d$Stimulus == "1" & d$Response == "1" & d$Conf_bin == 2, na.rm = TRUE),
             sum(d$Stimulus == "1" & d$Response == "1" & d$Conf_bin == 1, na.rm = TRUE),
             sum(d$Stimulus == "1" & d$Response == "2" & d$Conf_bin == 1, na.rm = TRUE),
             sum(d$Stimulus == "1" & d$Response == "2" & d$Conf_bin == 2, na.rm = TRUE),
             sum(d$Stimulus == "1" & d$Response == "2" & d$Conf_bin == 3, na.rm = TRUE))
  
  result <- tryCatch({
    fit_uvsdt_mle(nr_s1, nr_s2, add_constant = TRUE)
  }, error = function(e) {
    return(NA)
  })
  
  result2 <- tryCatch({
    fit_meta_d_bal(nr_s2, nr_s1, add_constant = TRUE)
  }, error = function(e) {
    return(NA)
  })
  
  nr_s1 <- nr_s1 + (1 / length(nr_s1))
  nr_s2 <- nr_s2 + (1 / length(nr_s2))
  zh <- qnorm(sum(nr_s2[1:(length(nr_s2)/2)]) / sum(nr_s2))
  zf <- qnorm(sum(nr_s1[1:(length(nr_s1)/2)]) / sum(nr_s1))
  dp <- c(dp, zh - zf)
  result_vec <- as.numeric(unlist(result))
  uvsdt_conf <- rbind(uvsdt_conf, c(result_vec, i))
  result2_vec <- as.numeric(unlist(result2))
  meta_conf <- rbind(meta_conf, c(result2_vec, i))
  roc <- cbind(c(0, cumsum(nr_s1) / sum(nr_s1)), c(0, cumsum(nr_s2) / sum(nr_s2)))
  roc <- cbind(roc, rep(i, length(nr_s1) + 1))
  roc_conf <- rbind(roc_conf, roc)
  
}


#'# RT
uvsdt_rt <- c()
meta_rt <- c()
roc_rt <- c()
for (i in unique(df$Subj_idx)) {
  
  d <- subset(df, df$Subj_idx == i)
  d$RT_bin <- ntile(d$RT_dec, 3)
  nr_s1 <- c(sum(d$Stimulus == "2" & d$Response == "1" & d$RT_bin == 1, na.rm = TRUE),
             sum(d$Stimulus == "2" & d$Response == "1" & d$RT_bin == 2, na.rm = TRUE),
             sum(d$Stimulus == "2" & d$Response == "1" & d$RT_bin == 3, na.rm = TRUE),
             sum(d$Stimulus == "2" & d$Response == "2" & d$RT_bin == 3, na.rm = TRUE),
             sum(d$Stimulus == "2" & d$Response == "2" & d$RT_bin == 2, na.rm = TRUE),
             sum(d$Stimulus == "2" & d$Response == "2" & d$RT_bin == 1, na.rm = TRUE))

  nr_s2 <- c(sum(d$Stimulus == "1" & d$Response == "1" & d$RT_bin == 1, na.rm = TRUE),
             sum(d$Stimulus == "1" & d$Response == "1" & d$RT_bin == 2, na.rm = TRUE),
             sum(d$Stimulus == "1" & d$Response == "1" & d$RT_bin == 3, na.rm = TRUE),
             sum(d$Stimulus == "1" & d$Response == "2" & d$RT_bin == 3, na.rm = TRUE),
             sum(d$Stimulus == "1" & d$Response == "2" & d$RT_bin == 2, na.rm = TRUE),
             sum(d$Stimulus == "1" & d$Response == "2" & d$RT_bin == 1, na.rm = TRUE))
  
  result <- tryCatch({
    fit_uvsdt_mle(nr_s1, nr_s2, add_constant = TRUE)
  }, error = function(e) {
    return(NA)
  })
  result_vec <- as.numeric(unlist(result))
  uvsdt_rt <- rbind(uvsdt_rt, c(result_vec, i))
  result2_vec <- as.numeric(unlist(result2))
  meta_rt <- rbind(meta_rt, c(result2_vec, i))
  roc <- cbind(c(0, cumsum(nr_s1) / sum(nr_s1)), c(0, cumsum(nr_s2) / sum(nr_s2)))
  roc <- cbind(roc, rep(i, length(nr_s1) + 1))
  roc_rt <- rbind(roc_rt, roc)
  
}


#'# visualization
uvsdt_conf <- cbind(uvsdt_conf, dp)
uvsdt_rt <- cbind(uvsdt_rt, dp)
uvsdt_conf <- na.omit(as.data.frame(uvsdt_conf[, c(1:3, (ncol(uvsdt_conf) - 2):ncol(uvsdt_conf))]))
uvsdt_rt <-   na.omit(as.data.frame(uvsdt_rt[, c(1:3, (ncol(uvsdt_rt) - 2):ncol(uvsdt_rt))]))
colnames(uvsdt_conf) <- c("mu_conf", "sigma_conf", "da_conf", "logL_conf", "id", "dp")
colnames(uvsdt_rt) <-   c("mu_rt",   "sigma_rt",   "da_rt",   "logL_rt",   "id", "dp")

# wide format
common_ids <- intersect(uvsdt_conf$id, uvsdt_rt$id)
uvsdt_conf <- filter(uvsdt_conf, id %in% common_ids)
uvsdt_rt   <- filter(uvsdt_rt,   id %in% common_ids)
df_wide <- bind_cols(dplyr::select(uvsdt_conf, -id), uvsdt_rt)
df_wide <- subset(df_wide, df_wide$dp...5 > 0)
nrow(df_wide) # 127
roc_rt <- roc_rt[roc_rt[, 3] %in% df_wide$id, ]
roc_conf <- roc_conf[roc_conf[, 3] %in% df_wide$id, ]
meta_rt <- na.omit(meta_rt)
meta_conf <- na.omit(meta_conf)

# sigma
correlation_sigma <- cor(df_wide$sigma_conf, df_wide$sigma_rt)
ggplot(df_wide) + 
  geom_point(aes(x = sigma_conf, y = sigma_rt), alpha = 0.5) + xlim(0, 4) + ylim(0, 4) +
  geom_smooth(aes(x = sigma_conf, y = sigma_rt), method = "lm", se = FALSE, size = 0.7) +
  annotation_custom(
    grob = textGrob(
      label = paste("y =", round(coef(lm(sigma_rt ~ sigma_conf, data = df_wide))[2], 2), 
                    "x +", round(coef(lm(sigma_rt ~ sigma_conf, data = df_wide))[1], 2), 
                    "\nCorrelation: ", round(correlation_sigma, 2)),
      x = 0, y = 1, just = c("left", "top"), gp = gpar(fontsize = 8, col = "black")),
    xmin = -Inf, xmax = Inf, ymin = Inf, ymax = Inf) +
  ggtitle("Mazor_2021_Expt7") + 
  theme_minimal() +
  theme(plot.title = element_text(size = 8),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x  = element_text(size = 6),
        axis.text.y  = element_text(size = 6)) -> sigma_Mazor_2021_Expt7

# mu
correlation_mu <- cor(df_wide$mu_conf, df_wide$mu_rt)
ggplot(df_wide) + 
  geom_point(aes(x = mu_conf, y = mu_rt), alpha = 0.5) + xlim(0, 5) + ylim(0, 5) +
  geom_smooth(aes(x = mu_conf, y = mu_rt), method = "lm", se = FALSE, size = 0.7) +
  annotation_custom(
    grob = textGrob(
      label = paste("y =", round(coef(lm(mu_rt ~ mu_conf, data = df_wide))[2], 2), 
                    "x +", round(coef(lm(mu_rt ~ mu_conf, data = df_wide))[1], 2), 
                    "\nCorrelation: ", round(correlation_mu, 2)),
      x = 0, y = 1, just = c("left", "top"), gp = gpar(fontsize = 8, col = "black")),
    xmin = -Inf, xmax = Inf, ymin = Inf, ymax = Inf) +
  ggtitle("Mazor_2021_Expt7") + 
  theme_minimal() +
  theme(plot.title = element_text(size = 8),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x  = element_text(size = 6),
        axis.text.y  = element_text(size = 6)) -> mu_Mazor_2021_Expt7

# da
correlation_da <- cor(df_wide$da_conf, df_wide$da_rt)
ggplot(df_wide) + 
  geom_point(aes(x = da_conf, y = da_rt), alpha = 0.5) + xlim(0, 5) + ylim(0, 5) +
  geom_smooth(aes(x = da_conf, y = da_rt), method = "lm", se = FALSE, size = 0.7) +
  annotation_custom(
    grob = textGrob(
      label = paste("y =", round(coef(lm(da_rt ~ da_conf, data = df_wide))[2], 2), 
                    "x +", round(coef(lm(da_rt ~ da_conf, data = df_wide))[1], 2), 
                    "\nCorrelation: ", round(correlation_da, 2)),
      x = 0, y = 1, just = c("left", "top"), gp = gpar(fontsize = 8, col = "black")),
    xmin = -Inf, xmax = Inf, ymin = Inf, ymax = Inf) + 
  ggtitle("Mazor_2021_Expt7") + 
  theme_minimal() +
  theme(plot.title = element_text(size = 8),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x  = element_text(size = 6),
        axis.text.y  = element_text(size = 6)) -> da_Mazor_2021_Expt7