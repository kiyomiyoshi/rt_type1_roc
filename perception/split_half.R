source("uvsdt.R")
source("Mazor_2020_detection.R")
source("Mazor_2020_detection_odd.R")
source("Mazor_2020_detection_even.R")

common_ids <- intersect(df_wide_odd$id, df_wide_even$id)
length(common_ids) # 29
df_wide_sub <- df_wide[df_wide$id %in% common_ids, ]
odd_common  <- df_wide_odd[df_wide_odd$id %in% common_ids, ]
even_common <- df_wide_even[df_wide_even$id %in% common_ids, ]


# sigma 
mean(df_wide$sigma_conf) + 2.5 * sd(df_wide$sigma_conf)
df_wide_sub <- subset(df_wide_sub, df_wide_sub$sigma_conf <  2.886)
odd_common  <- subset(odd_common, odd_common$sigma_conf <  2.886)
even_common <- subset(even_common, even_common$sigma_conf <  2.886)

correlation_sigma_conf <- cor(odd_common$sigma_conf, even_common$sigma_conf)
correlation_sigma_rt   <- cor(odd_common$sigma_rt, even_common$sigma_rt)
reliability_sigma_conf <- (2 * correlation_sigma_conf) / (1 + correlation_sigma_conf)
reliability_sigma_rt   <- (2 * correlation_sigma_rt) / (1 + correlation_sigma_rt)
cor(df_wide_sub$sigma_conf, df_wide_sub$sigma_rt) / sqrt(reliability_sigma_conf * reliability_sigma_rt) 

# mu
correlation_mu_conf <- cor(odd_common$mu_conf, even_common$mu_conf)
correlation_mu_rt   <- cor(odd_common$mu_rt, even_common$mu_rt)
reliability_mu_conf <- (2 * correlation_mu_conf) / (1 + correlation_mu_conf)
reliability_mu_rt   <- (2 * correlation_mu_rt) / (1 + correlation_mu_rt)
cor(df_wide_sub$mu_conf, df_wide_sub$mu_rt) / sqrt(reliability_mu_conf * reliability_mu_rt) 

# da
correlation_da_conf <- cor(odd_common$da_conf, even_common$da_conf)
correlation_da_rt   <- cor(odd_common$da_rt, even_common$da_rt)
reliability_da_conf <- (2 * correlation_da_conf) / (1 + correlation_da_conf)
reliability_da_rt   <- (2 * correlation_da_rt) / (1 + correlation_da_rt)
cor(df_wide_sub$da_conf, df_wide_sub$da_rt) / sqrt(reliability_da_conf * reliability_da_rt) 