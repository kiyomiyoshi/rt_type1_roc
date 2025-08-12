library(tidyverse)
library(data.table)
library(grid)

df <- fread("data_Weidemann_2016.csv", header = T)
length(unique(df$Subj_idx)) # 171
df %>%
  group_by(Subj_idx) %>%
  summarise(n = n()) %>%
  print(n = 99999) # 2109-5866


#'# confidence
uvsdt_conf_even <- c()
for (i in unique(df$Subj_idx)) {
  
  d <- subset(df, df$Subj_idx == i)
  d <- filter(d, row_number() %% 2 == 0)
  
  nr_s1 <- c(sum(d$Stimulus == "new" & d$Response == "old" & d$Confidence == 5, na.rm = TRUE),
             sum(d$Stimulus == "new" & d$Response == "old" & d$Confidence == 4, na.rm = TRUE),
             sum(d$Stimulus == "new" & d$Response == "old" & d$Confidence == 3, na.rm = TRUE),
             sum(d$Stimulus == "new" & d$Response == "old" & d$Confidence == 2, na.rm = TRUE),
             sum(d$Stimulus == "new" & d$Response == "old" & d$Confidence == 1, na.rm = TRUE),
             sum(d$Stimulus == "new" & d$Response == "new" & d$Confidence == 1, na.rm = TRUE),
             sum(d$Stimulus == "new" & d$Response == "new" & d$Confidence == 2, na.rm = TRUE),
             sum(d$Stimulus == "new" & d$Response == "new" & d$Confidence == 3, na.rm = TRUE),
             sum(d$Stimulus == "new" & d$Response == "new" & d$Confidence == 4, na.rm = TRUE),
             sum(d$Stimulus == "new" & d$Response == "new" & d$Confidence == 5, na.rm = TRUE))
  
  nr_s2 <- c(sum(d$Stimulus == "old" & d$Response == "old" & d$Confidence == 5, na.rm = TRUE),
             sum(d$Stimulus == "old" & d$Response == "old" & d$Confidence == 4, na.rm = TRUE),
             sum(d$Stimulus == "old" & d$Response == "old" & d$Confidence == 3, na.rm = TRUE),
             sum(d$Stimulus == "old" & d$Response == "old" & d$Confidence == 2, na.rm = TRUE),
             sum(d$Stimulus == "old" & d$Response == "old" & d$Confidence == 1, na.rm = TRUE),
             sum(d$Stimulus == "old" & d$Response == "new" & d$Confidence == 1, na.rm = TRUE),
             sum(d$Stimulus == "old" & d$Response == "new" & d$Confidence == 2, na.rm = TRUE),
             sum(d$Stimulus == "old" & d$Response == "new" & d$Confidence == 3, na.rm = TRUE),
             sum(d$Stimulus == "old" & d$Response == "new" & d$Confidence == 4, na.rm = TRUE),
             sum(d$Stimulus == "old" & d$Response == "new" & d$Confidence == 5, na.rm = TRUE))
  
  result <- tryCatch({
    fit_uvsdt_mle(nr_s1, nr_s2, add_constant = TRUE)
  }, error = function(e) {
    return(NA)
  })
  result_vec <- as.numeric(unlist(result))
  uvsdt_conf_even <- rbind(uvsdt_conf_even, c(result_vec, i))
  
}


#'# RT
uvsdt_rt_even <- c()
for (i in unique(df$Subj_idx)) {
  
  d <- subset(df, df$Subj_idx == i)
  d <- filter(d, row_number() %% 2 == 0)
  d$RT_bin <- ntile(d$RT_dec, 5)
  
  nr_s1 <- c(sum(d$Stimulus == "new" & d$Response == "old" & d$RT_bin == 1, na.rm = TRUE),
             sum(d$Stimulus == "new" & d$Response == "old" & d$RT_bin == 2, na.rm = TRUE),
             sum(d$Stimulus == "new" & d$Response == "old" & d$RT_bin == 3, na.rm = TRUE),
             sum(d$Stimulus == "new" & d$Response == "old" & d$RT_bin == 4, na.rm = TRUE),
             sum(d$Stimulus == "new" & d$Response == "old" & d$RT_bin == 5, na.rm = TRUE),
             sum(d$Stimulus == "new" & d$Response == "new" & d$RT_bin == 5, na.rm = TRUE),
             sum(d$Stimulus == "new" & d$Response == "new" & d$RT_bin == 4, na.rm = TRUE),
             sum(d$Stimulus == "new" & d$Response == "new" & d$RT_bin == 3, na.rm = TRUE),
             sum(d$Stimulus == "new" & d$Response == "new" & d$RT_bin == 2, na.rm = TRUE),
             sum(d$Stimulus == "new" & d$Response == "new" & d$RT_bin == 1, na.rm = TRUE))
  
  nr_s2 <- c(sum(d$Stimulus == "old" & d$Response == "old" & d$RT_bin == 1, na.rm = TRUE),
             sum(d$Stimulus == "old" & d$Response == "old" & d$RT_bin == 2, na.rm = TRUE),
             sum(d$Stimulus == "old" & d$Response == "old" & d$RT_bin == 3, na.rm = TRUE),
             sum(d$Stimulus == "old" & d$Response == "old" & d$RT_bin == 4, na.rm = TRUE),
             sum(d$Stimulus == "old" & d$Response == "old" & d$RT_bin == 5, na.rm = TRUE),
             sum(d$Stimulus == "old" & d$Response == "new" & d$RT_bin == 5, na.rm = TRUE),
             sum(d$Stimulus == "old" & d$Response == "new" & d$RT_bin == 4, na.rm = TRUE),
             sum(d$Stimulus == "old" & d$Response == "new" & d$RT_bin == 3, na.rm = TRUE),
             sum(d$Stimulus == "old" & d$Response == "new" & d$RT_bin == 2, na.rm = TRUE),
             sum(d$Stimulus == "old" & d$Response == "new" & d$RT_bin == 1, na.rm = TRUE))
  
  result <- tryCatch({
    fit_uvsdt_mle(nr_s1, nr_s2, add_constant = TRUE)
  }, error = function(e) {
    return(NA)
  })
  result_vec <- as.numeric(unlist(result))
  uvsdt_rt_even <- rbind(uvsdt_rt_even, c(result_vec, i))
  
}


#'# visualization
uvsdt_conf_even <- na.omit(as.data.frame(uvsdt_conf_even[, c(1:3, ncol(uvsdt_conf_even) - 1, ncol(uvsdt_conf_even))]))
uvsdt_rt_even <-   na.omit(as.data.frame(uvsdt_rt_even[, c(1:3, ncol(uvsdt_rt_even) - 1, ncol(uvsdt_rt_even))]))
colnames(uvsdt_conf_even) <- c("mu_conf", "sigma_conf", "da_conf", "logL_conf", "id")
colnames(uvsdt_rt_even) <-   c("mu_rt",   "sigma_rt",   "da_rt",   "logL_rt",   "id")

# wide format
common_ids <- intersect(uvsdt_conf_even$id, uvsdt_rt_even$id)
conf_filtered <- filter(uvsdt_conf_even, id %in% common_ids)
rt_filtered   <- filter(uvsdt_rt_even,   id %in% common_ids)
df_wide_even <- bind_cols(conf_filtered %>% select(-id), rt_filtered)
df_wide_even <- df_wide_even %>%
  mutate(across(-last_col(), as.numeric))
df_wide_even %>% filter(mu_conf > 0 & mu_rt > 0 & sigma_conf < 4 & sigma_rt < 4) -> df_wide_even
nrow(df_wide_even)
length(unique(df$Subj_idx))

# sigma
correlation_sigma <- cor(df_wide_even$sigma_conf, df_wide_even$sigma_rt)
ggplot(df_wide_even) + 
  geom_point(aes(x = sigma_conf, y = sigma_rt)) + xlim(0, 3) + ylim(0, 3) +
  geom_smooth(aes(x = sigma_conf, y = sigma_rt), method = "lm", se = FALSE) +
  annotation_custom(
    grob = textGrob(
      label = paste("y =", round(coef(lm(sigma_rt ~ sigma_conf, data = df_wide_even))[2], 2), 
                    "x +", round(coef(lm(sigma_rt ~ sigma_conf, data = df_wide_even))[1], 2), 
                    "\nCorrelation: ", round(correlation_sigma, 2)),
      x = 0, y = 1, just = c("left", "top"), gp = gpar(fontsize = 8, col = "black")
    ),
    xmin = -Inf, xmax = Inf, ymin = Inf, ymax = Inf
  ) + ggtitle("Weidemann_2016") + theme(plot.title = element_text(size = 10)) -> sigma_Weidemann_2016_even

# mu
correlation_mu <- cor(df_wide_even$mu_conf, df_wide_even$mu_rt)
ggplot(df_wide_even) + 
  geom_point(aes(x = mu_conf, y = mu_rt)) + xlim(0, 6) + ylim(0, 6) +
  geom_smooth(aes(x = mu_conf, y = mu_rt), method = "lm", se = FALSE) +
  annotation_custom(
    grob = textGrob(
      label = paste("y =", round(coef(lm(mu_rt ~ mu_conf, data = df_wide_even))[2], 2), 
                    "x +", round(coef(lm(mu_rt ~ mu_conf, data = df_wide_even))[1], 2), 
                    "\nCorrelation: ", round(correlation_mu, 2)),
      x = 0, y = 1, just = c("left", "top"), gp = gpar(fontsize = 8, col = "black")
    ),
    xmin = -Inf, xmax = Inf, ymin = Inf, ymax = Inf
  ) + ggtitle("Weidemann_2016") + theme(plot.title = element_text(size = 10)) -> mu_Weidemann_2016_even

# da
correlation_da <- cor(df_wide_even$da_conf, df_wide_even$da_rt)
ggplot(df_wide_even) + 
  geom_point(aes(x = da_conf, y = da_rt)) + xlim(0, 3) + ylim(0, 3) +
  geom_smooth(aes(x = da_conf, y = da_rt), method = "lm", se = FALSE) +
  annotation_custom(
    grob = textGrob(
      label = paste("y =", round(coef(lm(da_rt ~ da_conf, data = df_wide_even))[2], 2), 
                    "x +", round(coef(lm(da_rt ~ da_conf, data = df_wide_even))[1], 2), 
                    "\nCorrelation: ", round(correlation_da, 2)),
      x = 0, y = 1, just = c("left", "top"), gp = gpar(fontsize = 8, col = "black")
    ),
    xmin = -Inf, xmax = Inf, ymin = Inf, ymax = Inf
  ) + ggtitle("Weidemann_2016") + theme(plot.title = element_text(size = 10)) -> da_Weidemann_2016_even