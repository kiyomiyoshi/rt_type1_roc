library(tidyverse)
library(data.table)
library(grid)

df <- fread("data_Mazor_2020.csv", header = T) # sub 46, ava 39
df <- subset(df, df$Condition == "Detection") # 200 trials / sub
target_ids <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20,
                21, 22, 23, 24, 25, 26, 27, 28, 29, 31, 32, 33, 34, 35, 36, 38, 39,
                40, 41, 42, 44, 45, 46)
df <- df %>% filter(Subj_idx %in% target_ids)


#'# confidence
uvsdt_conf_odd <- c()
for (i in unique(df$Subj_idx)) {
  
  d <- subset(df, df$Subj_idx == i)
  d <- filter(d, row_number() %% 2 == 1)
 
  nr_s1 <- c(sum(d$Stimulus == "0" & d$Response == "1" & d$Confidence == 6, na.rm = TRUE),
             sum(d$Stimulus == "0" & d$Response == "1" & d$Confidence == 5, na.rm = TRUE),
             sum(d$Stimulus == "0" & d$Response == "1" & d$Confidence == 4, na.rm = TRUE),
             sum(d$Stimulus == "0" & d$Response == "1" & d$Confidence == 3, na.rm = TRUE),
             sum(d$Stimulus == "0" & d$Response == "1" & d$Confidence == 2, na.rm = TRUE),
             sum(d$Stimulus == "0" & d$Response == "1" & d$Confidence == 1, na.rm = TRUE),
             sum(d$Stimulus == "0" & d$Response == "0" & d$Confidence == 1, na.rm = TRUE),
             sum(d$Stimulus == "0" & d$Response == "0" & d$Confidence == 2, na.rm = TRUE),
             sum(d$Stimulus == "0" & d$Response == "0" & d$Confidence == 3, na.rm = TRUE),
             sum(d$Stimulus == "0" & d$Response == "0" & d$Confidence == 4, na.rm = TRUE),
             sum(d$Stimulus == "0" & d$Response == "0" & d$Confidence == 5, na.rm = TRUE),
             sum(d$Stimulus == "0" & d$Response == "0" & d$Confidence == 6, na.rm = TRUE))
  
  nr_s2 <- c(sum(d$Stimulus == "1" & d$Response == "1" & d$Confidence == 6, na.rm = TRUE),
             sum(d$Stimulus == "1" & d$Response == "1" & d$Confidence == 5, na.rm = TRUE),
             sum(d$Stimulus == "1" & d$Response == "1" & d$Confidence == 4, na.rm = TRUE),
             sum(d$Stimulus == "1" & d$Response == "1" & d$Confidence == 3, na.rm = TRUE),
             sum(d$Stimulus == "1" & d$Response == "1" & d$Confidence == 2, na.rm = TRUE),
             sum(d$Stimulus == "1" & d$Response == "1" & d$Confidence == 1, na.rm = TRUE),
             sum(d$Stimulus == "1" & d$Response == "0" & d$Confidence == 1, na.rm = TRUE),
             sum(d$Stimulus == "1" & d$Response == "0" & d$Confidence == 2, na.rm = TRUE),
             sum(d$Stimulus == "1" & d$Response == "0" & d$Confidence == 3, na.rm = TRUE),
             sum(d$Stimulus == "1" & d$Response == "0" & d$Confidence == 4, na.rm = TRUE),
             sum(d$Stimulus == "1" & d$Response == "0" & d$Confidence == 5, na.rm = TRUE),
             sum(d$Stimulus == "1" & d$Response == "0" & d$Confidence == 6, na.rm = TRUE))
  
  result <- tryCatch({
    fit_uvsdt_mle(nr_s1, nr_s2, add_constant = TRUE)
  }, error = function(e) {
    return(NA)
  })
  result_vec <- as.numeric(unlist(result))
  uvsdt_conf_odd <- rbind(uvsdt_conf_odd, c(result_vec, i))
  
}


#'# RT
uvsdt_rt_odd <- c()
for (i in unique(df$Subj_idx)) {
  
  d <- subset(df, df$Subj_idx == i)
  d <- filter(d, row_number() %% 2 == 1)
  d$RT_bin <- ntile(d$RT_dec, 6)
  
  nr_s1 <- c(sum(d$Stimulus == "0" & d$Response == "1" & d$RT_bin == 1, na.rm = TRUE),
             sum(d$Stimulus == "0" & d$Response == "1" & d$RT_bin == 2, na.rm = TRUE),
             sum(d$Stimulus == "0" & d$Response == "1" & d$RT_bin == 3, na.rm = TRUE),
             sum(d$Stimulus == "0" & d$Response == "1" & d$RT_bin == 4, na.rm = TRUE),
             sum(d$Stimulus == "0" & d$Response == "1" & d$RT_bin == 5, na.rm = TRUE),
             sum(d$Stimulus == "0" & d$Response == "1" & d$RT_bin == 6, na.rm = TRUE),
             sum(d$Stimulus == "0" & d$Response == "0" & d$RT_bin == 6, na.rm = TRUE),
             sum(d$Stimulus == "0" & d$Response == "0" & d$RT_bin == 5, na.rm = TRUE),
             sum(d$Stimulus == "0" & d$Response == "0" & d$RT_bin == 4, na.rm = TRUE),
             sum(d$Stimulus == "0" & d$Response == "0" & d$RT_bin == 3, na.rm = TRUE),
             sum(d$Stimulus == "0" & d$Response == "0" & d$RT_bin == 2, na.rm = TRUE),
             sum(d$Stimulus == "0" & d$Response == "0" & d$RT_bin == 1, na.rm = TRUE))
  
  nr_s2 <- c(sum(d$Stimulus == "1" & d$Response == "1" & d$RT_bin == 1, na.rm = TRUE),
             sum(d$Stimulus == "1" & d$Response == "1" & d$RT_bin == 2, na.rm = TRUE),
             sum(d$Stimulus == "1" & d$Response == "1" & d$RT_bin == 3, na.rm = TRUE),
             sum(d$Stimulus == "1" & d$Response == "1" & d$RT_bin == 4, na.rm = TRUE),
             sum(d$Stimulus == "1" & d$Response == "1" & d$RT_bin == 5, na.rm = TRUE),
             sum(d$Stimulus == "1" & d$Response == "1" & d$RT_bin == 6, na.rm = TRUE),
             sum(d$Stimulus == "1" & d$Response == "0" & d$RT_bin == 6, na.rm = TRUE),
             sum(d$Stimulus == "1" & d$Response == "0" & d$RT_bin == 5, na.rm = TRUE),
             sum(d$Stimulus == "1" & d$Response == "0" & d$RT_bin == 4, na.rm = TRUE),
             sum(d$Stimulus == "1" & d$Response == "0" & d$RT_bin == 3, na.rm = TRUE),
             sum(d$Stimulus == "1" & d$Response == "0" & d$RT_bin == 2, na.rm = TRUE),
             sum(d$Stimulus == "1" & d$Response == "0" & d$RT_bin == 1, na.rm = TRUE))
  
  result <- tryCatch({
    fit_uvsdt_mle(nr_s1, nr_s2, add_constant = TRUE)
  }, error = function(e) {
    return(NA)
  })
  result_vec <- as.numeric(unlist(result))
  uvsdt_rt_odd <- rbind(uvsdt_rt_odd, c(result_vec, i))
  
}


#'# visualization
uvsdt_conf_odd <- na.omit(as.data.frame(uvsdt_conf_odd[, c(1:3, ncol(uvsdt_conf_odd) - 1, ncol(uvsdt_conf_odd))]))
uvsdt_rt_odd <-   na.omit(as.data.frame(uvsdt_rt_odd[, c(1:3, ncol(uvsdt_rt_odd) - 1, ncol(uvsdt_rt_odd))]))
colnames(uvsdt_conf_odd) <- c("mu_conf", "sigma_conf", "da_conf", "logL_conf", "id")
colnames(uvsdt_rt_odd) <-   c("mu_rt",   "sigma_rt",   "da_rt",   "logL_rt",   "id")

# wide format
common_ids <- intersect(uvsdt_conf_odd$id, uvsdt_rt_odd$id)
conf_filtered <- filter(uvsdt_conf_odd, id %in% common_ids)
rt_filtered   <- filter(uvsdt_rt_odd,   id %in% common_ids)
df_wide_odd <- bind_cols(conf_filtered %>% select(-id), rt_filtered)
df_wide_odd %>% filter(mu_conf > 0 & mu_rt > 0 & sigma_conf < 4 & sigma_rt < 4) -> df_wide_odd
df_wide <- subset(df_wide, df_wide$dp...5 > 0)
nrow(df_wide_odd) # 38

# sigma
correlation_sigma <- cor(df_wide_odd$sigma_conf, df_wide_odd$sigma_rt)
ggplot(df_wide_odd) + 
  geom_point(aes(x = sigma_conf, y = sigma_rt)) + xlim(0, 3.5) + ylim(0, 3.5) +
  geom_smooth(aes(x = sigma_conf, y = sigma_rt), method = "lm", se = FALSE) +
  annotation_custom(
    grob = textGrob(
      label = paste("y =", round(coef(lm(sigma_rt ~ sigma_conf, data = df_wide_odd))[2], 2), 
                    "x +", round(coef(lm(sigma_rt ~ sigma_conf, data = df_wide_odd))[1], 2), 
                    "\nCorrelation: ", round(correlation_sigma, 2)),
      x = 0, y = 1, just = c("left", "top"), gp = gpar(fontsize = 8, col = "black")
    ),
    xmin = -Inf, xmax = Inf, ymin = Inf, ymax = Inf
  ) + ggtitle("Mazor_2020_Detection") + theme(plot.title = element_text(size = 10)) -> sigma_Mazor_2020_Detection_odd

# mu
correlation_mu <- cor(df_wide_odd$mu_conf, df_wide_odd$mu_rt)
ggplot(df_wide_odd) + 
  geom_point(aes(x = mu_conf, y = mu_rt)) + xlim(0, 6) + ylim(0, 6) +
  geom_smooth(aes(x = mu_conf, y = mu_rt), method = "lm", se = FALSE) +
  annotation_custom(
    grob = textGrob(
      label = paste("y =", round(coef(lm(mu_rt ~ mu_conf, data = df_wide_odd))[2], 2), 
                    "x +", round(coef(lm(mu_rt ~ mu_conf, data = df_wide_odd))[1], 2), 
                    "\nCorrelation: ", round(correlation_mu, 2)),
      x = 0, y = 1, just = c("left", "top"), gp = gpar(fontsize = 8, col = "black")
    ),
    xmin = -Inf, xmax = Inf, ymin = Inf, ymax = Inf
  ) + ggtitle("Mazor_2020_Detection") + theme(plot.title = element_text(size = 10)) -> mu_Mazor_2020_Detection_odd

# da
correlation_da <- cor(df_wide_odd$da_conf, df_wide_odd$da_rt)
ggplot(df_wide_odd) + 
  geom_point(aes(x = da_conf, y = da_rt)) + xlim(0, 3) + ylim(0, 3) +
  geom_smooth(aes(x = da_conf, y = da_rt), method = "lm", se = FALSE) +
  annotation_custom(
    grob = textGrob(
      label = paste("y =", round(coef(lm(da_rt ~ da_conf, data = df_wide_odd))[2], 2), 
                    "x +", round(coef(lm(da_rt ~ da_conf, data = df_wide_odd))[1], 2), 
                    "\nCorrelation: ", round(correlation_da, 2)),
      x = 0, y = 1, just = c("left", "top"), gp = gpar(fontsize = 8, col = "black")
    ),
    xmin = -Inf, xmax = Inf, ymin = Inf, ymax = Inf
  ) + ggtitle("Mazor_2020_Detection") + theme(plot.title = element_text(size = 10)) -> da_Mazor_2020_Detection_odd